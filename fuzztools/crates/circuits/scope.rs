use crate::circuits::{
    ast::{forest::ForestType, types::*},
    functions::Function,
};
use std::collections::{HashSet, VecDeque};

#[derive(Clone, Default)]
pub struct Scope {
    /// (input name, type, whether it is public or not)
    pub inputs: Vec<(String, Type, bool)>,

    pub structs: Vec<Struct>,
    pub functions: Vec<Function>,

    /// (global name, type, value)
    pub globals: Vec<(String, Type, String)>,

    /// (return type, whether it is pub or not)
    pub ret: Option<(Type, bool)>,

    // This is used to prevent infinite recursion due to Type::random() -> Lambda ->
    // Type::random_value() -> Type::random() with depth reset to 0...
    pub lambda_depth: usize,

    /// Type kinds to favor during generation (for biased generation towards input/return types, so
    /// that forests are more "consistent")
    pub type_bias: HashSet<TypeKind>,

    pub forest_type: ForestType,
}

impl Scope {
    pub fn compute_type_bias(types: &[Type]) -> HashSet<TypeKind> {
        let mut bias = HashSet::new();
        let mut work: VecDeque<&Type> = types.iter().collect();

        while let Some(ty) = work.pop_front() {
            let kind = ty.kind();
            bias.insert(kind);

            // If kind is primitive, bias towards the whole primitive family (Field/Int/Bool)
            if matches!(
                kind,
                TypeKind::Field | TypeKind::Boolean | TypeKind::Signed | TypeKind::Unsigned
            ) {
                bias.extend([
                    TypeKind::Field,
                    TypeKind::Boolean,
                    TypeKind::Signed,
                    TypeKind::Unsigned,
                ]);
            }

            match ty {
                Type::Array(a) => work.push_back(&a.ty),
                Type::Slice(s) => work.push_back(&s.ty),
                Type::Tuple(t) => work.extend(&t.elements),
                Type::Struct(s) => work.extend(s.fields.iter().map(|f| f.ty.as_ref())),
                Type::Lambda(l) => {
                    work.extend(l.params.iter().map(|(_, t)| t));
                    work.push_back(l.ret.as_ref());
                }
                _ => {}
            }
        }
        bias
    }
}
