//! Implements scope management to improve the quality of the generated AST

use crate::circuits::ast::types::*;

pub fn types_compatible(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Field(_), Type::Field(_)) => true,
        (Type::Boolean(_), Type::Boolean(_)) => true,
        (Type::Integer(i1), Type::Integer(i2)) => i1.bits == i2.bits && i1.signed == i2.signed,
        (Type::Array(a1), Type::Array(a2)) => {
            a1.size == a2.size && types_compatible(&a1.ty, &a2.ty)
        }
        (Type::Slice(s1), Type::Slice(s2)) => types_compatible(&s1.ty, &s2.ty),
        (Type::Tuple(t1), Type::Tuple(t2)) => {
            t1.inner.len() == t2.inner.len() &&
                t1.inner.iter().zip(t2.inner.iter()).all(|(a, b)| types_compatible(a, b))
        }
        (Type::Struct(s1), Type::Struct(s2)) => {
            s1.name == s2.name &&
                s1.fields.len() == s2.fields.len() &&
                s1.fields
                    .iter()
                    .zip(s2.fields.iter())
                    .all(|(f1, f2)| f1.name == f2.name && types_compatible(&f1.ty, &f2.ty))
        }
        _ => false,
    }
}

// ────────────────────────────────────────────────────────────────────────────────
// Variable
// ────────────────────────────────────────────────────────────────────────────────

/// A variable in scope
#[derive(Clone, Debug)]
pub struct Variable {
    pub name: String,
    pub ty: Type,
    pub mutable: bool,
}

// ────────────────────────────────────────────────────────────────────────────────
// Access Path
// ────────────────────────────────────────────────────────────────────────────────

/// Represents an accessible path that yields a specific type
#[derive(Clone, Debug)]
pub struct AccessPath {
    /// The variable name
    pub base: String,
    /// The access expression (e.g., ".field", ".0", "[2]")
    pub access: String,
    /// The type of the accessed element
    pub ty: Type,
}

// ────────────────────────────────────────────────────────────────────────────────
// Scope
// ────────────────────────────────────────────────────────────────────────────────

/// Manages variable scopes with stack-based nesting for blocks/loops
#[derive(Clone, Default)]
pub struct Scope {
    /// Stack of scopes, each scope is a list of variables
    pub scopes: Vec<Vec<Variable>>,
    /// Input parameters (always visible)
    pub inputs: Vec<Variable>,
    /// Structs (always visible)
    pub structs: Vec<Struct>,
}

impl Scope {
    pub fn new() -> Self {
        Self { scopes: vec![Vec::new()], inputs: Vec::new(), structs: Vec::new() }
    }

    /// Create a scope with initial variables (flat, for backwards compatibility)
    pub fn with_variables(variables: Vec<Variable>, inputs: Vec<Variable>) -> Self {
        Self { scopes: vec![variables], inputs, structs: Vec::new() }
    }

    /// Push a new scope (entering a block)
    pub fn push(&mut self) {
        self.scopes.push(Vec::new());
    }

    /// Pop the current scope (exiting a block)
    pub fn pop(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Add a variable to the current scope
    pub fn add(&mut self, var: Variable) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.push(var);
        }
    }

    /// Get all variables visible in current scope (including inputs)
    pub fn all_variables(&self) -> Vec<&Variable> {
        let mut vars: Vec<&Variable> = self.scopes.iter().flatten().collect();
        vars.extend(self.inputs.iter());
        vars
    }

    /// Get all mutable variables visible in current scope
    pub fn mutable_variables(&self) -> Vec<&Variable> {
        self.all_variables().into_iter().filter(|v| v.mutable).collect()
    }

    /// Find variables of a specific type
    pub fn variables_of_type(&self, ty: &Type) -> Vec<&Variable> {
        self.all_variables().into_iter().filter(|v| types_compatible(&v.ty, ty)).collect()
    }

    /// Find mutable variables of a specific type
    pub fn mutable_of_type(&self, ty: &Type) -> Vec<&Variable> {
        self.mutable_variables().into_iter().filter(|v| types_compatible(&v.ty, ty)).collect()
    }

    /// Check if a variable name exists
    pub fn exists(&self, name: &str) -> bool {
        self.all_variables().iter().any(|v| v.name == name)
    }

    /// Get a variable by name
    pub fn get(&self, name: &str) -> Option<&Variable> {
        self.all_variables().into_iter().find(|v| v.name == name)
    }

    /// Find all struct field accesses that yield a specific type
    pub fn struct_field_accesses(&self, target_ty: &Type) -> Vec<AccessPath> {
        let mut result = Vec::new();
        for var in self.all_variables() {
            if let Type::Struct(Struct { fields, .. }) = &var.ty {
                for field in fields {
                    if types_compatible(&field.ty, target_ty) {
                        result.push(AccessPath {
                            base: var.name.clone(),
                            access: format!(".{}", field.name),
                            ty: (*field.ty).clone(),
                        });
                    }
                }
            }
        }
        result
    }

    /// Find all tuple index accesses that yield a specific type
    pub fn tuple_index_accesses(&self, target_ty: &Type) -> Vec<AccessPath> {
        let mut result = Vec::new();
        for var in self.all_variables() {
            if let Type::Tuple(Tuple { inner }) = &var.ty {
                for (idx, elem_ty) in inner.iter().enumerate() {
                    if types_compatible(elem_ty, target_ty) {
                        result.push(AccessPath {
                            base: var.name.clone(),
                            access: format!(".{}", idx),
                            ty: elem_ty.clone(),
                        });
                    }
                }
            }
        }
        result
    }

    /// Find all array/slice index accesses that yield a specific type
    pub fn index_accesses(&self, target_ty: &Type) -> Vec<AccessPath> {
        let mut result = Vec::new();
        for var in self.all_variables() {
            match &var.ty {
                Type::Array(Array { ty, size }) if *size > 0 => {
                    if types_compatible(ty, target_ty) {
                        result.push(AccessPath {
                            base: var.name.clone(),
                            access: format!("[{}]", size - 1),
                            ty: (**ty).clone(),
                        });
                    }
                }
                Type::Slice(Slice { ty, size }) if *size > 0 => {
                    if types_compatible(ty, target_ty) {
                        result.push(AccessPath {
                            base: var.name.clone(),
                            access: format!("[{}]", size - 1),
                            ty: (**ty).clone(),
                        });
                    }
                }
                _ => {}
            }
        }
        result
    }
}
