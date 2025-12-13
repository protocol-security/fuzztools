use super::{
    context::Context,
    misc::random_string,
    operators::Operator,
    types::{Boolean, Field, Integer, Type},
};
use crate::math::random_field_element;
use rand::{seq::IndexedRandom, Rng};
use std::collections::VecDeque;

// ============================================================
// Expression Type System
// ============================================================

/// Simplified type for expression type-checking
#[derive(Clone, Debug, PartialEq)]
pub enum ExprType {
    Field,
    Integer {
        bits: u8,
        signed: bool,
    },
    Boolean,
    /// A lambda/closure type: params -> return_type
    Lambda {
        params: Vec<ExprType>,
        ret: Box<ExprType>,
    },
    /// Unit type (for statements that don't return a value)
    Unit,
}

impl ExprType {
    pub fn random_numeric(random: &mut impl Rng) -> Self {
        if random.random_bool(0.5) {
            ExprType::Field
        } else {
            let bits = *[8u8, 16, 32, 64].choose(random).unwrap();
            let signed = random.random_bool(0.5);
            ExprType::Integer { bits, signed }
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, ExprType::Field | ExprType::Integer { .. })
    }

    pub fn to_full_type(&self, mutable: bool) -> Option<Type> {
        match self {
            ExprType::Field => Some(Type::Field(Field { mutable })),
            ExprType::Integer { bits, signed } => {
                Some(Type::Integer(Integer { bits: *bits, signed: *signed, mutable }))
            }
            ExprType::Boolean => Some(Type::Boolean(Boolean { mutable })),
            _ => None,
        }
    }
}

impl std::fmt::Display for ExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprType::Field => write!(f, "Field"),
            ExprType::Integer { bits, signed } => {
                write!(f, "{}{}", if *signed { "i" } else { "u" }, bits)
            }
            ExprType::Boolean => write!(f, "bool"),
            ExprType::Lambda { params, ret } => {
                let params_str: Vec<_> = params.iter().map(|p| p.to_string()).collect();
                write!(f, "fn({}) -> {}", params_str.join(", "), ret)
            }
            ExprType::Unit => write!(f, "()"),
        }
    }
}

// ============================================================
// Variable Scope
// ============================================================

/// A variable in scope
#[derive(Clone, Debug)]
pub struct Variable {
    pub name: String,
    pub ty: ExprType,
    pub mutable: bool,
}

/// Manages variable scopes (for blocks, loops, etc.)
#[derive(Clone, Default)]
pub struct Scope {
    /// Stack of scopes, each scope is a list of variables
    scopes: Vec<Vec<Variable>>,
}

impl Scope {
    pub fn new() -> Self {
        Self { scopes: vec![Vec::new()] }
    }

    /// Push a new scope (entering a block)
    pub fn push(&mut self) {
        self.scopes.push(Vec::new());
    }

    /// Pop the current scope (exiting a block)
    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    /// Add a variable to the current scope
    pub fn add(&mut self, var: Variable) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.push(var);
        }
    }

    /// Get all variables visible in current scope
    pub fn all_variables(&self) -> Vec<&Variable> {
        self.scopes.iter().flatten().collect()
    }

    /// Get all mutable variables visible in current scope
    pub fn mutable_variables(&self) -> Vec<&Variable> {
        self.scopes.iter().flatten().filter(|v| v.mutable).collect()
    }

    /// Find variables of a specific type
    pub fn variables_of_type(&self, ty: &ExprType) -> Vec<&Variable> {
        self.all_variables().into_iter().filter(|v| types_compatible(&v.ty, ty)).collect()
    }

    /// Find mutable variables of a specific type
    pub fn mutable_of_type(&self, ty: &ExprType) -> Vec<&Variable> {
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
}

fn types_compatible(a: &ExprType, b: &ExprType) -> bool {
    match (a, b) {
        (ExprType::Field, ExprType::Field) => true,
        (ExprType::Boolean, ExprType::Boolean) => true,
        (
            ExprType::Integer { bits: b1, signed: s1 },
            ExprType::Integer { bits: b2, signed: s2 },
        ) => b1 == b2 && s1 == s2,
        (ExprType::Unit, ExprType::Unit) => true,
        _ => false,
    }
}

// ============================================================
// Function Definition (for callable functions)
// ============================================================

/// A function that can be called
#[derive(Clone)]
pub struct FunctionDef {
    pub name: String,
    pub inputs: Vec<(String, ExprType)>,
    pub return_type: Option<ExprType>,
}

// ============================================================
// Expression AST
// ============================================================

/// A typed expression
#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: ExprType,
}

#[derive(Clone)]
pub enum ExprKind {
    /// Literal value: `42`, `true`, `0x1234`
    Literal(String),

    /// Variable reference: `foo`
    Variable(String),

    /// Binary operation: `a + b`
    Binary { op: Operator, left: Box<Expr>, right: Box<Expr> },

    /// Unary operation: `!x`, `-y`
    Unary { op: Operator, operand: Box<Expr> },

    /// Parenthesized expression: `(expr)`
    Paren(Box<Expr>),

    /// Function call: `foo(a, b, c)`
    Call { func: String, args: Vec<Expr> },

    /// Lambda call: `add_1(x)`
    LambdaCall { lambda: String, args: Vec<Expr> },
}

// ============================================================
// Statement AST
// ============================================================

#[derive(Clone)]
pub enum Statement {
    /// Let binding: `let name: ty = expr;` or `let name = expr;`
    Let { name: String, ty: Option<Type>, value: Expr, mutable: bool },

    /// Assignment to existing mutable variable: `name = expr;`
    Assign { name: String, value: Expr },

    /// Compound assignment: `name += expr;`
    CompoundAssign { name: String, op: Operator, value: Expr },

    /// Assertion: `assert(condition);` or `assert(condition, "message");`
    Assert { condition: Expr, message: Option<String> },

    /// Lambda definition: `let name = |params| body;`
    Lambda { name: String, params: Vec<(String, ExprType)>, body: Expr },

    /// If statement: `if cond { ... } else if cond { ... } else { ... }`
    If { branches: Vec<(Expr, Block)>, else_block: Option<Block> },

    /// For loop: `for i in start..end { ... }`
    For { var: String, start: Expr, end: Expr, body: Block },

    /// Expression statement (for side effects)
    Expr(Expr),
}

/// A block of statements
#[derive(Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

// ============================================================
// Operator Classification
// ============================================================

impl Operator {
    /// Arithmetic operators safe for Field (no modulo)
    pub fn arithmetic_field() -> &'static [Operator] {
        &[Operator::Add, Operator::Sub, Operator::Mul, Operator::Div]
    }

    /// Arithmetic operators for integers (includes modulo)
    pub fn arithmetic_integer() -> &'static [Operator] {
        &[Operator::Add, Operator::Sub, Operator::Mul, Operator::Div, Operator::Mod]
    }

    /// Bitwise operators for integers only
    pub fn bitwise() -> &'static [Operator] {
        &[Operator::And, Operator::Or, Operator::Xor, Operator::Shl, Operator::Shr]
    }

    /// Comparison operators for integers (full set)
    pub fn comparison_integer() -> &'static [Operator] {
        &[
            Operator::Less,
            Operator::LessOrEqual,
            Operator::Greater,
            Operator::GreaterOrEqual,
            Operator::Equal,
            Operator::NotEqual,
        ]
    }

    /// Comparison operators safe for Field (equality only)
    pub fn comparison_field() -> &'static [Operator] {
        &[Operator::Equal, Operator::NotEqual]
    }

    /// Boolean binary operators
    pub fn boolean_binary() -> &'static [Operator] {
        &[Operator::And, Operator::Or, Operator::Xor]
    }

    /// Unary arithmetic operators
    pub fn unary_numeric() -> &'static [Operator] {
        &[Operator::Neg]
    }

    /// Unary boolean operators
    pub fn unary_boolean() -> &'static [Operator] {
        &[Operator::Not]
    }

    /// Compound assignment operators for Field (no modulo)
    pub fn compound_assign_field() -> &'static [Operator] {
        &[Operator::AddAssign, Operator::SubAssign, Operator::MulAssign, Operator::DivAssign]
    }

    /// Compound assignment operators for integers
    pub fn compound_assign_integer() -> &'static [Operator] {
        &[
            Operator::AddAssign,
            Operator::SubAssign,
            Operator::MulAssign,
            Operator::DivAssign,
            Operator::ModAssign,
        ]
    }

    /// Compound assignment operators for integers (bitwise)
    pub fn compound_assign_bitwise() -> &'static [Operator] {
        &[
            Operator::AndAssign,
            Operator::OrAssign,
            Operator::XorAssign,
            Operator::ShlAssign,
            Operator::ShrAssign,
        ]
    }
}

// ============================================================
// Work Items for Iterative Generation
// ============================================================

enum ExprWorkItem {
    /// Generate an expression of given type at slot
    Generate { slot_idx: usize, target_type: ExprType, depth: usize },
    /// Finalize a binary expression
    FinalizeBinary {
        slot_idx: usize,
        op: Operator,
        left_slot: usize,
        right_slot: usize,
        result_type: ExprType,
    },
    /// Finalize a unary expression
    FinalizeUnary { slot_idx: usize, op: Operator, operand_slot: usize, result_type: ExprType },
    /// Finalize parenthesized expression
    FinalizeParen { slot_idx: usize, inner_slot: usize },
    /// Finalize function call
    FinalizeCall { slot_idx: usize, func: String, arg_slots: Vec<usize>, result_type: ExprType },
    /// Finalize lambda call
    FinalizeLambdaCall {
        slot_idx: usize,
        lambda: String,
        arg_slots: Vec<usize>,
        result_type: ExprType,
    },
}

// ============================================================
// Expression Generation
// ============================================================

impl Expr {
    /// Generate a random expression of the target type using iterative approach
    pub fn random(
        random: &mut impl Rng,
        ctx: &Context,
        target_type: ExprType,
        scope: &Scope,
        functions: &[FunctionDef],
    ) -> Self {
        let mut slots: Vec<Option<Expr>> = vec![None];
        let mut work: VecDeque<ExprWorkItem> = VecDeque::new();

        work.push_back(ExprWorkItem::Generate { slot_idx: 0, target_type, depth: 0 });

        while let Some(item) = work.pop_front() {
            match item {
                ExprWorkItem::Generate { slot_idx, target_type, depth } => {
                    // Base case: max depth reached
                    if depth >= ctx.max_expression_depth {
                        slots[slot_idx] = Some(Self::random_leaf(random, ctx, &target_type, scope));
                        continue;
                    }

                    // Build list of available expression types
                    let mut choices: Vec<&str> = vec!["literal", "paren"];

                    // Check for variables of matching type
                    if !scope.variables_of_type(&target_type).is_empty() {
                        choices.push("variable");
                    }

                    // Check for functions that return this type
                    let matching_funcs: Vec<_> = functions
                        .iter()
                        .filter(|f| {
                            f.return_type.is_some() &&
                                types_compatible(&f.return_type.as_ref().unwrap(), &target_type)
                        })
                        .collect();
                    if !matching_funcs.is_empty() {
                        choices.push("call");
                    }

                    // Check for lambdas that return this type
                    let matching_lambdas: Vec<_> = scope
                        .all_variables()
                        .into_iter()
                        .filter(|v| {
                            if let ExprType::Lambda { ret, .. } = &v.ty {
                                types_compatible(ret, &target_type)
                            } else {
                                false
                            }
                        })
                        .collect();
                    if !matching_lambdas.is_empty() {
                        choices.push("lambda_call");
                    }

                    match &target_type {
                        ExprType::Field | ExprType::Integer { .. } => {
                            choices.extend(["binary_arith", "unary_neg"]);
                        }
                        ExprType::Boolean => {
                            choices.extend(["comparison", "binary_bool", "unary_not"]);
                        }
                        _ => {}
                    }

                    match *choices.choose(random).unwrap() {
                        "literal" => {
                            slots[slot_idx] =
                                Some(Self::random_leaf(random, ctx, &target_type, scope));
                        }

                        "variable" => {
                            let vars = scope.variables_of_type(&target_type);
                            if vars.is_empty() {
                                slots[slot_idx] =
                                    Some(Self::random_leaf(random, ctx, &target_type, scope));
                            } else {
                                let var = *vars.choose(random).unwrap();
                                slots[slot_idx] = Some(Expr {
                                    kind: ExprKind::Variable(var.name.clone()),
                                    ty: target_type,
                                });
                            }
                        }

                        "binary_arith" => {
                            // Choose operator based on type:
                            // - Field: no modulo allowed
                            // - Integer: all arithmetic operators allowed
                            let op = match &target_type {
                                ExprType::Field => {
                                    *Operator::arithmetic_field().choose(random).unwrap()
                                }
                                ExprType::Integer { .. } => {
                                    *Operator::arithmetic_integer().choose(random).unwrap()
                                }
                                _ => *Operator::arithmetic_field().choose(random).unwrap(),
                            };

                            let left_slot = slots.len();
                            let right_slot = slots.len() + 1;
                            slots.push(None);
                            slots.push(None);

                            work.push_front(ExprWorkItem::FinalizeBinary {
                                slot_idx,
                                op,
                                left_slot,
                                right_slot,
                                result_type: target_type.clone(),
                            });
                            // Both operands must have the same type as the result
                            work.push_front(ExprWorkItem::Generate {
                                slot_idx: right_slot,
                                target_type: target_type.clone(),
                                depth: depth + 1,
                            });
                            work.push_front(ExprWorkItem::Generate {
                                slot_idx: left_slot,
                                target_type,
                                depth: depth + 1,
                            });
                        }

                        "comparison" => {
                            // For comparisons, we need to pick a numeric type for operands
                            // Field: only == and != allowed
                            // Integer: all comparisons allowed
                            // To ensure type consistency, pick Integer type for full comparisons
                            // or Field with equality-only operators
                            let use_integer = random.random_bool(0.7); // Prefer integers for more operators

                            let (op, operand_type) = if use_integer {
                                let bits = *[8u8, 16, 32, 64].choose(random).unwrap();
                                let signed = random.random_bool(0.5);
                                (
                                    *Operator::comparison_integer().choose(random).unwrap(),
                                    ExprType::Integer { bits, signed },
                                )
                            } else {
                                (
                                    *Operator::comparison_field().choose(random).unwrap(),
                                    ExprType::Field,
                                )
                            };

                            let left_slot = slots.len();
                            let right_slot = slots.len() + 1;
                            slots.push(None);
                            slots.push(None);

                            work.push_front(ExprWorkItem::FinalizeBinary {
                                slot_idx,
                                op,
                                left_slot,
                                right_slot,
                                result_type: ExprType::Boolean,
                            });
                            // Both operands must have the same type
                            work.push_front(ExprWorkItem::Generate {
                                slot_idx: right_slot,
                                target_type: operand_type.clone(),
                                depth: depth + 1,
                            });
                            work.push_front(ExprWorkItem::Generate {
                                slot_idx: left_slot,
                                target_type: operand_type,
                                depth: depth + 1,
                            });
                        }

                        "binary_bool" => {
                            let op = *Operator::boolean_binary().choose(random).unwrap();
                            let left_slot = slots.len();
                            let right_slot = slots.len() + 1;
                            slots.push(None);
                            slots.push(None);

                            work.push_front(ExprWorkItem::FinalizeBinary {
                                slot_idx,
                                op,
                                left_slot,
                                right_slot,
                                result_type: ExprType::Boolean,
                            });
                            work.push_front(ExprWorkItem::Generate {
                                slot_idx: right_slot,
                                target_type: ExprType::Boolean,
                                depth: depth + 1,
                            });
                            work.push_front(ExprWorkItem::Generate {
                                slot_idx: left_slot,
                                target_type: ExprType::Boolean,
                                depth: depth + 1,
                            });
                        }

                        "unary_neg" => {
                            let op = *Operator::unary_numeric().choose(random).unwrap();
                            let operand_slot = slots.len();
                            slots.push(None);

                            work.push_front(ExprWorkItem::FinalizeUnary {
                                slot_idx,
                                op,
                                operand_slot,
                                result_type: target_type.clone(),
                            });
                            work.push_front(ExprWorkItem::Generate {
                                slot_idx: operand_slot,
                                target_type,
                                depth: depth + 1,
                            });
                        }

                        "unary_not" => {
                            let op = *Operator::unary_boolean().choose(random).unwrap();
                            let operand_slot = slots.len();
                            slots.push(None);

                            work.push_front(ExprWorkItem::FinalizeUnary {
                                slot_idx,
                                op,
                                operand_slot,
                                result_type: ExprType::Boolean,
                            });
                            work.push_front(ExprWorkItem::Generate {
                                slot_idx: operand_slot,
                                target_type: ExprType::Boolean,
                                depth: depth + 1,
                            });
                        }

                        "paren" => {
                            let inner_slot = slots.len();
                            slots.push(None);

                            work.push_front(ExprWorkItem::FinalizeParen { slot_idx, inner_slot });
                            work.push_front(ExprWorkItem::Generate {
                                slot_idx: inner_slot,
                                target_type,
                                depth: depth + 1,
                            });
                        }

                        "call" => {
                            let func = *matching_funcs.choose(random).unwrap();
                            let arg_slots: Vec<usize> =
                                (0..func.inputs.len()).map(|i| slots.len() + i).collect();
                            for _ in 0..func.inputs.len() {
                                slots.push(None);
                            }

                            work.push_front(ExprWorkItem::FinalizeCall {
                                slot_idx,
                                func: func.name.clone(),
                                arg_slots: arg_slots.clone(),
                                result_type: target_type.clone(),
                            });

                            for (i, (_, param_ty)) in func.inputs.iter().enumerate().rev() {
                                work.push_front(ExprWorkItem::Generate {
                                    slot_idx: arg_slots[i],
                                    target_type: param_ty.clone(),
                                    depth: depth + 1,
                                });
                            }
                        }

                        "lambda_call" => {
                            let lambda_var = *matching_lambdas.choose(random).unwrap();
                            if let ExprType::Lambda { params, ret } = &lambda_var.ty {
                                let arg_slots: Vec<usize> =
                                    (0..params.len()).map(|i| slots.len() + i).collect();
                                for _ in 0..params.len() {
                                    slots.push(None);
                                }

                                work.push_front(ExprWorkItem::FinalizeLambdaCall {
                                    slot_idx,
                                    lambda: lambda_var.name.clone(),
                                    arg_slots: arg_slots.clone(),
                                    result_type: (**ret).clone(),
                                });

                                for (i, param_ty) in params.iter().enumerate().rev() {
                                    work.push_front(ExprWorkItem::Generate {
                                        slot_idx: arg_slots[i],
                                        target_type: param_ty.clone(),
                                        depth: depth + 1,
                                    });
                                }
                            } else {
                                // Fallback
                                slots[slot_idx] =
                                    Some(Self::random_leaf(random, ctx, &target_type, scope));
                            }
                        }

                        _ => {
                            slots[slot_idx] =
                                Some(Self::random_leaf(random, ctx, &target_type, scope));
                        }
                    }
                }

                ExprWorkItem::FinalizeBinary {
                    slot_idx,
                    op,
                    left_slot,
                    right_slot,
                    result_type,
                } => {
                    let left = slots[left_slot].take().unwrap();
                    let right = slots[right_slot].take().unwrap();
                    slots[slot_idx] = Some(Expr {
                        kind: ExprKind::Binary { op, left: Box::new(left), right: Box::new(right) },
                        ty: result_type,
                    });
                }

                ExprWorkItem::FinalizeUnary { slot_idx, op, operand_slot, result_type } => {
                    let operand = slots[operand_slot].take().unwrap();
                    slots[slot_idx] = Some(Expr {
                        kind: ExprKind::Unary { op, operand: Box::new(operand) },
                        ty: result_type,
                    });
                }

                ExprWorkItem::FinalizeParen { slot_idx, inner_slot } => {
                    let inner = slots[inner_slot].take().unwrap();
                    let ty = inner.ty.clone();
                    slots[slot_idx] = Some(Expr { kind: ExprKind::Paren(Box::new(inner)), ty });
                }

                ExprWorkItem::FinalizeCall { slot_idx, func, arg_slots, result_type } => {
                    let args: Vec<Expr> =
                        arg_slots.iter().map(|&s| slots[s].take().unwrap()).collect();
                    slots[slot_idx] =
                        Some(Expr { kind: ExprKind::Call { func, args }, ty: result_type });
                }

                ExprWorkItem::FinalizeLambdaCall { slot_idx, lambda, arg_slots, result_type } => {
                    let args: Vec<Expr> =
                        arg_slots.iter().map(|&s| slots[s].take().unwrap()).collect();
                    slots[slot_idx] =
                        Some(Expr { kind: ExprKind::LambdaCall { lambda, args }, ty: result_type });
                }
            }
        }

        slots[0].take().unwrap()
    }

    /// Generate a leaf expression (literal or variable)
    fn random_leaf(
        random: &mut impl Rng,
        ctx: &Context,
        target_type: &ExprType,
        scope: &Scope,
    ) -> Self {
        // 50% chance to use a variable if one exists
        if random.random_bool(0.5) {
            let matching = scope.variables_of_type(target_type);
            if !matching.is_empty() {
                let var = *matching.choose(random).unwrap();
                return Expr { kind: ExprKind::Variable(var.name.clone()), ty: target_type.clone() };
            }
        }

        // Generate literal
        let literal = match target_type {
            ExprType::Field => {
                let exclude_prime = random.random_bool(ctx.exclude_prime_probability);
                random_field_element(
                    "bn254",
                    random,
                    exclude_prime,
                    ctx.boundary_value_probability,
                    ctx.small_upper_bound_probability,
                    ctx.max_small_upper_bound,
                )
                .to_string()
            }
            ExprType::Integer { bits, signed } => {
                Integer { bits: *bits, signed: *signed, mutable: false }.random_value(random, ctx)
            }
            ExprType::Boolean => if random.random_bool(0.5) { "true" } else { "false" }.to_string(),
            _ => "()".to_string(),
        };

        Expr { kind: ExprKind::Literal(literal), ty: target_type.clone() }
    }
}

// ============================================================
// Statement Generation
// ============================================================

impl Statement {
    /// Generate a random statement
    pub fn random(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &mut Scope,
        functions: &[FunctionDef],
        depth: usize,
    ) -> Self {
        // At max depth, only generate simple statements
        let max_depth_reached = depth >= ctx.max_expression_depth;

        let mut choices: Vec<&str> = vec!["let", "assert"];

        // Can only assign if we have mutable variables
        if !scope.mutable_variables().is_empty() {
            choices.push("assign");
            choices.push("compound_assign");
        }

        // Only generate complex statements if not at max depth
        if !max_depth_reached {
            choices.push("lambda");
            choices.push("if");
            choices.push("for");
        }

        match *choices.choose(random).unwrap() {
            "let" => Self::random_let(random, ctx, scope, functions),
            "assign" => Self::random_assign(random, ctx, scope, functions),
            "compound_assign" => Self::random_compound_assign(random, ctx, scope, functions),
            "assert" => Self::random_assert(random, ctx, scope, functions),
            "lambda" => Self::random_lambda(random, ctx, scope, functions),
            "if" => Self::random_if(random, ctx, scope, functions, depth),
            "for" => Self::random_for(random, ctx, scope, functions, depth),
            _ => Self::random_let(random, ctx, scope, functions),
        }
    }

    /// Generate a let statement: `let name: ty = expr;`
    fn random_let(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &mut Scope,
        functions: &[FunctionDef],
    ) -> Self {
        let name = random_string(random, ctx.max_name_characters_count);
        let mutable = random.random_bool(ctx.mutable_probability);

        // Choose expression type
        let expr_type = if random.random_bool(0.7) {
            ExprType::random_numeric(random)
        } else {
            ExprType::Boolean
        };

        let value = Expr::random(random, ctx, expr_type.clone(), scope, functions);

        // Optionally include type annotation
        let ty = if random.random_bool(0.5) { expr_type.to_full_type(mutable) } else { None };

        // Add to scope
        scope.add(Variable { name: name.clone(), ty: expr_type, mutable });

        Statement::Let { name, ty, value, mutable }
    }

    /// Generate an assignment to existing mutable variable: `name = expr;`
    fn random_assign(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        functions: &[FunctionDef],
    ) -> Self {
        let mutable_vars = scope.mutable_variables();
        if mutable_vars.is_empty() {
            // Fallback to let if no mutable variables
            return Self::random_let(random, ctx, &mut scope.clone(), functions);
        }

        let var = *mutable_vars.choose(random).unwrap();
        let value = Expr::random(random, ctx, var.ty.clone(), scope, functions);

        Statement::Assign { name: var.name.clone(), value }
    }

    /// Generate a compound assignment: `name += expr;`
    fn random_compound_assign(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        functions: &[FunctionDef],
    ) -> Self {
        let mutable_vars: Vec<_> =
            scope.mutable_variables().into_iter().filter(|v| v.ty.is_numeric()).collect();

        if mutable_vars.is_empty() {
            return Self::random_let(random, ctx, &mut scope.clone(), functions);
        }

        let var = *mutable_vars.choose(random).unwrap();

        // Choose operator based on variable type:
        // - Field: no modulo allowed
        // - Integer: all compound operators allowed
        let op = match &var.ty {
            ExprType::Field => *Operator::compound_assign_field().choose(random).unwrap(),
            ExprType::Integer { .. } => {
                *Operator::compound_assign_integer().choose(random).unwrap()
            }
            _ => *Operator::compound_assign_field().choose(random).unwrap(),
        };

        let value = Expr::random(random, ctx, var.ty.clone(), scope, functions);

        Statement::CompoundAssign { name: var.name.clone(), op, value }
    }

    /// Generate an assert statement: `assert(condition);` or `assert(condition, "msg");`
    fn random_assert(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &Scope,
        functions: &[FunctionDef],
    ) -> Self {
        let condition = Expr::random(random, ctx, ExprType::Boolean, scope, functions);

        let message = if random.random_bool(0.5) {
            Some(random_string(random, ctx.max_name_characters_count))
        } else {
            None
        };

        Statement::Assert { condition, message }
    }

    /// Generate a lambda: `let name = |params| body;`
    fn random_lambda(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &mut Scope,
        functions: &[FunctionDef],
    ) -> Self {
        let name = random_string(random, ctx.max_name_characters_count);

        // Generate 1-3 parameters
        let param_count = random.random_range(1..=3);
        let params: Vec<(String, ExprType)> = (0..param_count)
            .map(|_| {
                let pname = random_string(random, ctx.max_name_characters_count);
                let pty = if random.random_bool(0.7) {
                    ExprType::random_numeric(random)
                } else {
                    ExprType::Boolean
                };
                (pname, pty)
            })
            .collect();

        // Create a temporary scope with lambda parameters
        let mut lambda_scope = scope.clone();
        lambda_scope.push();
        for (pname, pty) in &params {
            lambda_scope.add(Variable { name: pname.clone(), ty: pty.clone(), mutable: false });
        }

        // Generate return type and body
        let ret_type = if random.random_bool(0.7) {
            ExprType::random_numeric(random)
        } else {
            ExprType::Boolean
        };
        let body = Expr::random(random, ctx, ret_type.clone(), &lambda_scope, functions);

        // Add lambda to outer scope
        let lambda_type = ExprType::Lambda {
            params: params.iter().map(|(_, t)| t.clone()).collect(),
            ret: Box::new(ret_type),
        };
        scope.add(Variable { name: name.clone(), ty: lambda_type, mutable: false });

        Statement::Lambda { name, params, body }
    }

    /// Generate an if statement: `if cond { ... } else if cond { ... } else { ... }`
    fn random_if(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &mut Scope,
        functions: &[FunctionDef],
        depth: usize,
    ) -> Self {
        // 1-3 branches (if, else if, else if...)
        let branch_count = random.random_range(1..=3);
        let mut branches = Vec::with_capacity(branch_count);

        for _ in 0..branch_count {
            let condition = Expr::random(random, ctx, ExprType::Boolean, scope, functions);
            let body = Block::random(random, ctx, scope, functions, depth + 1);
            branches.push((condition, body));
        }

        // 50% chance for else block
        let else_block = if random.random_bool(0.5) {
            Some(Block::random(random, ctx, scope, functions, depth + 1))
        } else {
            None
        };

        Statement::If { branches, else_block }
    }

    /// Generate a for loop: `for i in start..end { ... }`
    fn random_for(
        random: &mut impl Rng,
        ctx: &Context,
        scope: &mut Scope,
        functions: &[FunctionDef],
        depth: usize,
    ) -> Self {
        let var = random_string(random, ctx.max_name_characters_count);

        // Generate range bounds as simple literals
        let start_val = random.random_range(0u64..10);
        let end_val = random.random_range((start_val + 1)..20);

        let start = Expr {
            kind: ExprKind::Literal(start_val.to_string()),
            ty: ExprType::Integer { bits: 64, signed: false },
        };
        let end = Expr {
            kind: ExprKind::Literal(end_val.to_string()),
            ty: ExprType::Integer { bits: 64, signed: false },
        };

        // Create body scope with loop variable
        let mut body_scope = scope.clone();
        body_scope.push();
        body_scope.add(Variable {
            name: var.clone(),
            ty: ExprType::Integer { bits: 64, signed: false },
            mutable: false,
        });

        let body = Block::random(random, ctx, &mut body_scope, functions, depth + 1);

        Statement::For { var, start, end, body }
    }
}

// ============================================================
// Block Generation
// ============================================================

impl Block {
    /// Generate a random block of statements
    pub fn random(
        random: &mut impl Rng,
        ctx: &Context,
        outer_scope: &Scope,
        functions: &[FunctionDef],
        depth: usize,
    ) -> Self {
        // Create a new scope for this block
        let mut scope = outer_scope.clone();
        scope.push();

        // Generate 1-max_block_expressions_count statements
        let count = random.random_range(1..=ctx.max_block_expressions_count);
        let statements: Vec<Statement> = (0..count)
            .map(|_| Statement::random(random, ctx, &mut scope, functions, depth))
            .collect();

        Block { statements }
    }
}

// ============================================================
// Display Implementations
// ============================================================

impl Expr {
    /// Format expression, wrapping in parens if needed for precedence
    fn format_with_parens(&self, needs_parens: bool) -> String {
        let inner = match &self.kind {
            ExprKind::Literal(s) => s.clone(),
            ExprKind::Variable(name) => name.clone(),
            ExprKind::Binary { op, left, right } => {
                // Always parenthesize binary sub-expressions to avoid precedence issues
                let left_str =
                    left.format_with_parens(matches!(left.kind, ExprKind::Binary { .. }));
                let right_str =
                    right.format_with_parens(matches!(right.kind, ExprKind::Binary { .. }));
                format!("{} {} {}", left_str, op, right_str)
            }
            ExprKind::Unary { op, operand } => {
                let operand_str =
                    operand.format_with_parens(matches!(operand.kind, ExprKind::Binary { .. }));
                format!("{}{}", op, operand_str)
            }
            ExprKind::Paren(inner) => format!("({})", inner),
            ExprKind::Call { func, args } => {
                let args_str: Vec<_> = args.iter().map(|a| a.to_string()).collect();
                format!("{}({})", func, args_str.join(", "))
            }
            ExprKind::LambdaCall { lambda, args } => {
                let args_str: Vec<_> = args.iter().map(|a| a.to_string()).collect();
                format!("{}({})", lambda, args_str.join(", "))
            }
        };

        if needs_parens {
            format!("({})", inner)
        } else {
            inner
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format_with_parens(false))
    }
}

impl Statement {
    /// Format statement with proper indentation for nested blocks
    pub fn format_indented(&self, indent: usize) -> String {
        match self {
            Statement::Let { name, ty, value, mutable } => {
                let mut_kw = if *mutable { "mut " } else { "" };
                match ty {
                    Some(t) => format!("let {}{}: {} = {};", mut_kw, name, t, value),
                    None => format!("let {}{} = {};", mut_kw, name, value),
                }
            }
            Statement::Assign { name, value } => {
                format!("{} = {};", name, value)
            }
            Statement::CompoundAssign { name, op, value } => {
                format!("{} {} {};", name, op, value)
            }
            Statement::Assert { condition, message } => match message {
                Some(msg) => format!("assert({}, \"{}\");", condition, msg),
                None => format!("assert({});", condition),
            },
            Statement::Lambda { name, params, body } => {
                let params_str: Vec<_> =
                    params.iter().map(|(n, t)| format!("{}: {}", n, t)).collect();
                format!("let {} = |{}| {};", name, params_str.join(", "), body)
            }
            Statement::If { branches, else_block } => {
                let mut result = String::new();

                for (i, (cond, block)) in branches.iter().enumerate() {
                    if i == 0 {
                        result.push_str(&format!("if {} {}", cond, block.format_indented(indent)));
                    } else {
                        result.push_str(&format!(
                            " else if {} {}",
                            cond,
                            block.format_indented(indent)
                        ));
                    }
                }

                if let Some(else_blk) = else_block {
                    result.push_str(&format!(" else {}", else_blk.format_indented(indent)));
                }

                result
            }
            Statement::For { var, start, end, body } => {
                format!("for {} in {}..{} {}", var, start, end, body.format_indented(indent))
            }
            Statement::Expr(expr) => format!("{};", expr),
        }
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format_indented(0))
    }
}

impl Block {
    /// Format block with proper indentation
    pub fn format_indented(&self, indent: usize) -> String {
        let indent_str = "    ".repeat(indent);
        let inner_indent = "    ".repeat(indent + 1);

        if self.statements.is_empty() {
            "{}".to_string()
        } else {
            let stmts: Vec<_> = self
                .statements
                .iter()
                .map(|s| format!("{}{}", inner_indent, s.format_indented(indent + 1)))
                .collect();
            format!("{{\n{}\n{}}}", stmts.join("\n"), indent_str)
        }
    }
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format_indented(0))
    }
}
