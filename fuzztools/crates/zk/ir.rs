use crate::{
    math::random_field_element, mutations::Phantom, utils::{RandomChoice, random_id, random_name}, zk::config::Config
};
use alloy::primitives::U256;
use rand::Rng;
use secp256k1::hashes::hash160::Hash;
use std::{collections::HashMap, fmt::{Display, Formatter}};

/// Represents an operator in the IR. @audit should be changed to support arbitrary set of operators
/// disjoint to this one
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    // Boolean operators
    And,
    Or,
    Xor,
    Not,

    // Bitwise operators
    Shl,
    Shr,

    // Comparison operators
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
}

impl Operator {
    pub const ARITHMETIC_OPERATORS: &'static [Operator] =
        &[Operator::Add, Operator::Sub, Operator::Mul, Operator::Div, Operator::Rem];
    pub const BITWISE_OPERATORS: &'static [Operator] = &[Operator::Shl, Operator::Shr];
    pub const BOOLEAN_OPERATORS: &'static [Operator] =
        &[Operator::And, Operator::Or, Operator::Xor, Operator::Not];
    pub const COMPARISON_OPERATORS: &'static [Operator] =
        &[Operator::Lt, Operator::Lte, Operator::Gt, Operator::Gte, Operator::Eq, Operator::Neq];
    // @audit what about references and dereferences
    pub const UNARY_OPERATORS: &'static [Operator] = &[Operator::Not, Operator::Sub];
}

impl From<&str> for Operator {
    fn from(value: &str) -> Self {
        match value {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "%" => Self::Rem,

            "&" => Self::And,
            "|" => Self::Or,
            "^" => Self::Xor,
            "!" => Self::Not,

            "<<" => Self::Shl,
            ">>" => Self::Shr,

            "<" => Self::Lt,
            "<=" => Self::Lte,
            ">" => Self::Gt,
            ">=" => Self::Gte,
            "==" => Self::Eq,
            "!=" => Self::Neq,
            _ => unimplemented!("Operator not supported {}", value),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let operator = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Rem => "%",

            Self::And => "&",
            Self::Or => "|",
            Self::Xor => "^",
            Self::Not => "!",

            Self::Shl => "<<",
            Self::Shr => ">>",

            Self::Lt => "<",
            Self::Lte => "<=",
            Self::Gt => ">",
            Self::Gte => ">=",
            Self::Eq => "==",
            Self::Neq => "!=",
        };

        write!(f, "{}", operator)
    }
}

// -------------------------
//
//      Noir Types
//
// -------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum NoirType {
    Field,
    Integer { signed: bool, size: u64 },
    Boolean,
    // @audit what about scape hatchs \r, \n, \t, \0, \", \\ and raw strings r"...", r#"...",
    // r######"..."###### as well as f"..." writing { with {{"
    String { size: u64 },
    // @audit you can call function in getters like
    // let _ = array[f(x)];
    //
    // fn f(x: u32) -> u32 {
    // x * 2
    // }
    Array { type_: Box<NoirType>, size: u64 },
    // @audit must be initialized as = &[1, 2, 3, ...] or &[0; 3]
    // @audit todo Slice,

    // @audit accedemos a los fields con .0, .1, .2...
    Tuple { types: Vec<Box<NoirType>> },
    // @audit access fields via struct.field_name
    // @audit todo Struct,
    // @audit todo Reference,
}

impl NoirType {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Field => "field",
            Self::Integer { .. } => "int",
            Self::Boolean => "bool",
            Self::String { .. } => "string",
            Self::Array { .. } => "array",
            Self::Tuple { .. } => "tuple"
        }
    }
}

impl Display for NoirType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Field => write!(f, "Field"),
            Self::Integer { signed, size } => {
                write!(f, "{}{}", if *signed { "i" } else { "u" }, size)
            },
            Self::Boolean => write!(f, "bool"),
            Self::String { size } => write!(f, "str<{}>", size),
            Self::Array { type_, size } => write!(f, "[{}; {}]", type_, size),
            Self::Tuple { types } => {
                write!(f, "(")?;
                for (i, type_) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", type_)?;
                }
                write!(f, ")")
            }, /* @audit todo Struct,
                * @audit todo Reference, */
        }
    }
}

const UNSIGNED_INTEGER_SIZES: &'static [u64] = &[1, 8, 16, 32, 64, 128];
const SIGNED_INTEGER_SIZES: &'static [u64] = &[1, 8, 16, 32, 64];

impl NoirType {
    // @audit need config for the max number of fields
    pub fn random(random: &mut impl Rng, config: &Config, depth: u64) -> Self {
        match random.random_range(0..=5) {
            0 => Self::Field,
            1 => {
                let signed = random.random_bool(config.signed_probability);
                let size = if signed {
                    *random.choice(SIGNED_INTEGER_SIZES)
                } else {
                    *random.choice(UNSIGNED_INTEGER_SIZES)
                };
                Self::Integer { signed, size }
            },
            2 => Self::Boolean,
            3 => Self::String { size: random.random_range(0..=config.max_string_size) },
            4 => {
                if depth >= config.max_type_depth {
                    // To avoid infinite recursion
                    Self::Integer {
                        signed: random.random_bool(config.signed_probability),
                        size: *random.choice(UNSIGNED_INTEGER_SIZES),
                    }
                } else {
                    Self::Array {
                        type_: Box::new(Self::random(random, config, depth + 1)),
                        size: random.random_range(0..=config.max_element_count),
                    }
                }
            },
            5 => Self::Tuple {
                types: (0..random.random_range(0..=config.max_element_count))
                    .map(|_| Box::new(Self::random(random, config, depth + 1)))
                    .collect(),
            },
            _ => unreachable!(),
        }
    }
}

// -------------------------
//
//      Visibility
//
// -------------------------
#[derive(Clone, Debug, PartialEq)]
pub enum Visibility {
    Public,
    Restricted,
    Private,
}

impl Display for Visibility {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Public => write!(f, "pub"),
            Self::Restricted => write!(f, "pub(crate)"),
            Self::Private => write!(f, ""),
        }
    }
}

// -------------------------
//
//      Nodes in the IR
//
// -------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum IRNode {
    Identifier {
        name: String,
    },
    Field {
        value: U256,
    },
    Integer {
        value: U256,
    },
    Boolean {
        value: bool,
    },
    String {
        value: String,
    },

    Array {
        value: Vec<Box<IRNode>>,
    },
    // @audit todo Slice,
    Tuple {
        value: Vec<Box<IRNode>>,
    },
    // @audit todo Struct(Struct),
    FunctionCall {
        name: String,
        arguments: Vec<Box<IRNode>>,
    },

    // @audit for example, !a or -a
    UnaryExpression {
        operator: Operator,
        operand: Box<IRNode>,
    },

    // @audit for example, a + b and similar
    BinaryExpression {
        operator: Operator,
        left: Box<IRNode>,
        right: Box<IRNode>,
    },

    ArrayAccessExpression {
        array: Box<IRNode>,
        index: Box<IRNode>,
    },
    TupleAccessExpression {
        tuple: Box<IRNode>,
        index: Box<IRNode>,
    },
    // @audit this is for structs FieldAccessExpression(FieldAccessExpression),

    // @audit missing specific likes oracle, while unconstrained...
    IfStatement {
        condition: Box<IRNode>,
        then_branch: Box<IRNode>,
        else_branch: Box<IRNode>,
    },
    ForStatement {
        index: String,
        start: Box<IRNode>,
        end: Box<IRNode>,
        body: Vec<Box<IRNode>>,
    },
    LetStatement {
        name: String,
        expr: Box<IRNode>,
        type_: Option<NoirType>,
        is_mutable: bool, /* @audit todo what about references and in-function arguments or
                           * argument calls that are &mut or mut or & */
    },
    AssignStatement {
        name: String,
        expr: Box<IRNode>,
    },
    AssertStatement {
        left: Box<IRNode>,
        right: Box<IRNode>,
        operator: Operator,
    },

    ReturnStatement {
        value: Box<IRNode>,
    },

    // @audit and constants, globals...
    GlobalDefinition {
        name: String,
        type_: NoirType,
        expr: Box<IRNode>,
        visibility: Visibility,
    },

    // @audit what about lambdas like || {}?
    FunctionDefinition {
        name: String,
        arguments: Vec<(String, NoirType)>,
        body: Vec<Box<IRNode>>,
        visibility: Visibility,
        return_type: Option<NoirType>,
    },
}

// @audit the idea is 1) you do a pass generation some linkage to inner variables and assertions to
// inputs, then you create complex statements from MIDDLE_NODES to manipulate them with END_NODES
// being the last statement in a { ... } block choosen from MIDDLE_NODES
impl IRNode {
    pub fn random_statement(
        random: &mut impl Rng,
        config: &Config,
        depth: u64,
        ctx: &mut Context,
    ) -> Self {
        // If we are too deep, we should prefer simpler statements or return
        if depth >= config.max_expression_depth {
            if ctx.parent_type.is_some() {
                let value = match ctx.parent_type.as_ref().unwrap().as_str() {
                    "field" => Self::Field {
                        value: U256::ONE
                    },
                    "int" => Self::Integer { 
                        value: U256::ONE
                    },
                    "bool" => Self::Boolean { value: random.random_bool(0.5) },
                    "string" => Self::String { value: String::new() },
                    "array" => Self::Array { value: vec![] },
                    "tuple" => Self::Array { value: vec![] },
                    _ => unreachable!()
                };

                return value;
            }
        }

        // Available statements:
        // Let, Assign, If, For, Assert, Return, Expression(FunctionCall)
        // We weight them to produce reasonable code.
        let choices = [
            ("let", 10),
            ("assign", 5),
            ("if", 3),
            ("for", 2),
            ("assert", 2),
            ("return", 1), // @audit same here
            ("call", 2),
        ];

        let choice = *Self::weighted_choice(random, &choices);

        match choice {
            "let" => {
                let name = random_name(random, 8);
                let type_ = NoirType::random(random, config, 0);
                let expr = Self::random_expression(random, config, depth + 1, ctx, Some(&type_));
                let is_mutable = random.random_bool(0.5);

                // Add to context
                ctx.add_variable(name.clone(), type_.clone());

                IRNode::LetStatement { name, expr: Box::new(expr), type_: Some(type_), is_mutable }
            },
            "assign" => {
                // Can only assign if we have mutable variables
                if let Some((name, type_)) = ctx.get_random_variable(random) { // @audit should return is_mutable too
                    let expr = Self::random_expression(random, config, depth + 1, ctx, Some(type_));
                    IRNode::AssignStatement { name: name.clone(), expr: Box::new(expr) }
                } else {
                    // Fallback to let if no variables
                    Self::random_statement(random, config, depth, ctx) // @audit eing
                }
            },
            "if" => {
                let condition = Box::new(Self::random_expression(
                    random,
                    config,
                    depth + 1,
                    ctx,
                    Some(&NoirType::Boolean),
                ));

                // Create new contexts for branches to avoid leaking variables (simple scoping)
                let mut then_ctx = ctx.clone();
                let mut else_ctx = ctx.clone();

                // For simplicity in this IR, branches in statements might be blocks.
                // But IRNode::IfStatement expects IRNode as branches.
                // We'll generate a single statement or a block (if we had a Block node, but we
                // don't). We will just generate a random statement for now. @audit BasicBlock or vec of statements
                let then_branch =
                    Box::new(Self::random_statement(random, config, depth + 1, &mut then_ctx));
                let else_branch =
                    Box::new(Self::random_statement(random, config, depth + 1, &mut else_ctx));

                IRNode::IfStatement { condition, then_branch, else_branch }
            },
            "for" => {
                let index_name = random_name(random, 4);
                // @audit really? should be logical? or na like <
                let start = Box::new(Self::random_expression(
                    random,
                    config,
                    depth + 1,
                    ctx,
                    Some(&NoirType::Integer { signed: false, size: 32 }),
                ));
                let end = Box::new(Self::random_expression(
                    random,
                    config,
                    depth + 1,
                    ctx,
                    Some(&NoirType::Integer { signed: false, size: 32 }),
                ));

                let mut loop_ctx = ctx.clone();
                // @audit check
                loop_ctx.add_variable(index_name.clone(), NoirType::Integer {
                    signed: false,
                    size: 32,
                });

                // @audit BasicBlock similar
                let body_size = random.random_range(1..=config.max_for_size);
                let body = (0..body_size)
                    .map(|_| {
                        Box::new(Self::random_statement(random, config, depth + 1, &mut loop_ctx))
                    })
                    .collect();

                IRNode::ForStatement { index: index_name, start, end, body }
            },
            "assert" => {
                let type_ = NoirType::random(random, config, 0);
                // @audit should assert on public variables and random the right or include in in left
                let left =
                    Box::new(Self::random_expression(random, config, depth + 1, ctx, Some(&type_)));
                let right =
                    Box::new(Self::random_expression(random, config, depth + 1, ctx, Some(&type_)));
                let operator = *random.choice(Operator::COMPARISON_OPERATORS);

                IRNode::AssertStatement { left, right, operator }
            },
            "return" => {
                // Return a random expression @audit no me gusta
                let expr = Self::random_expression(random, config, depth + 1, ctx, None);
                IRNode::ReturnStatement { value: Box::new(expr) }
            },
            "call" => {
                // Function call as a statement (ignoring return value) @audit should not and the name should be in an array of callable functions  check
                let name = random_name(random, 10);
                // Random arguments
                let arg_count = random.random_range(0..4);
                let arguments = (0..arg_count)
                    .map(|_| {
                        Box::new(Self::random_expression(random, config, depth + 1, ctx, None))
                    })
                    .collect();

                IRNode::FunctionCall { name, arguments }
            },
            _ => unreachable!(),
        }
    }

    // @audit check hacia abajo no lo he mirado pero todo es nuevo
    pub fn random_expression(
        random: &mut impl Rng,
        config: &Config,
        depth: u64,
        ctx: &Context,
        target_type: Option<&NoirType>,
    ) -> Self {
        // If depth is high, prefer terminals (literals or variables)
        let force_terminal = depth >= config.max_expression_depth;

        let mut choices = Vec::new();
        choices.push(("literal", 10));
        choices.push(("variable", 10));

        if !force_terminal {
            choices.push(("unary", 5));
            choices.push(("binary", 10));
            choices.push(("if_expr", 2));
            // choices.push(("call", 5)); // TODO: Add function calls in expressions
        }

        let choice = *Self::weighted_choice(random, &choices);

        // Determine the type we want to generate
        let type_to_gen =
            if let Some(t) = target_type { t.clone() } else { NoirType::random(random, config, 0) };
        match choice {
            "literal" => Self::random_literal(random, config, &type_to_gen),
            "variable" => {
                // Try to find a variable of the requested type
                if let Some(name) = ctx.get_random_variable_of_type(random, &type_to_gen) {
                    IRNode::Identifier { name: name.clone() }
                } else {
                    // Fallback to literal if no matching variable
                    Self::random_literal(random, config, &type_to_gen)
                }
            },
            "unary" => {
                // Unary ops usually apply to Booleans (Not) or Integers/Fields (Sub/Neg)
                // We need to pick an operator compatible with `type_to_gen`
                let op = match type_to_gen {
                    NoirType::Boolean => Operator::Not,
                    NoirType::Integer { .. } | NoirType::Field => Operator::Sub, // Negation
                    _ => return Self::random_literal(random, config, &type_to_gen), // Fallback
                };

                let operand = Box::new(Self::random_expression(
                    random,
                    config,
                    depth + 1,
                    ctx,
                    Some(&type_to_gen),
                ));
                IRNode::UnaryExpression { operator: op, operand }
            },
            "binary" => {
                // Binary ops: Result type depends on operator.
                // If target is Boolean, could be (Bool && Bool) or (Int < Int).
                // If target is Int, could be (Int + Int).

                match type_to_gen {
                    NoirType::Boolean => {
                        // 50% chance of Logic op (Bool op Bool -> Bool)
                        // 50% chance of Cmp op (Int op Int -> Bool)
                        if random.random_bool(0.5) {
                            let op = *random.choice(Operator::BOOLEAN_OPERATORS);
                            let left = Box::new(Self::random_expression(
                                random,
                                config,
                                depth + 1,
                                ctx,
                                Some(&NoirType::Boolean),
                            ));
                            let right = Box::new(Self::random_expression(
                                random,
                                config,
                                depth + 1,
                                ctx,
                                Some(&NoirType::Boolean),
                            ));
                            IRNode::BinaryExpression { operator: op, left, right }
                        } else {
                            let op = *random.choice(Operator::COMPARISON_OPERATORS);
                            // Compare two random things of same type
                            let cmp_type = NoirType::random(random, config, 0);
                            let left = Box::new(Self::random_expression(
                                random,
                                config,
                                depth + 1,
                                ctx,
                                Some(&cmp_type),
                            ));
                            let right = Box::new(Self::random_expression(
                                random,
                                config,
                                depth + 1,
                                ctx,
                                Some(&cmp_type),
                            ));
                            IRNode::BinaryExpression { operator: op, left, right }
                        }
                    },
                    NoirType::Integer { .. } | NoirType::Field => {
                        let op = *random.choice(Operator::ARITHMETIC_OPERATORS);
                        let left = Box::new(Self::random_expression(
                            random,
                            config,
                            depth + 1,
                            ctx,
                            Some(&type_to_gen),
                        ));
                        let right = Box::new(Self::random_expression(
                            random,
                            config,
                            depth + 1,
                            ctx,
                            Some(&type_to_gen),
                        ));
                        IRNode::BinaryExpression { operator: op, left, right }
                    },
                    _ => Self::random_literal(random, config, &type_to_gen),
                }
            },
            "if_expr" => {
                let condition = Box::new(Self::random_expression(
                    random,
                    config,
                    depth + 1,
                    ctx,
                    Some(&NoirType::Boolean),
                ));
                let then_branch = Box::new(Self::random_expression(
                    random,
                    config,
                    depth + 1,
                    ctx,
                    Some(&type_to_gen),
                ));
                let else_branch = Box::new(Self::random_expression(
                    random,
                    config,
                    depth + 1,
                    ctx,
                    Some(&type_to_gen),
                ));
                IRNode::IfStatement { condition, then_branch, else_branch }
            },
            _ => Self::random_literal(random, config, &type_to_gen),
        }
    }

    fn random_literal(random: &mut impl Rng, config: &Config, type_: &NoirType) -> Self {
        match type_ {
            NoirType::Field => {
                IRNode::Integer { value: U256::from(random.random_range(0u64..u64::MAX)) }
            }, // Simplified Field
            NoirType::Integer { size: _size, .. } => {
                // TODO: Respect size limits
                IRNode::Integer { value: U256::from(random.random_range(0u64..100)) }
            },
            NoirType::Boolean => IRNode::Boolean { value: random.random_bool(0.5) },
            NoirType::String { size } => {
                IRNode::String { value: random_id(random, *size as usize) }
            },
            NoirType::Array { type_, size } => {
                let elements = (0..*size)
                    .map(|_| Box::new(Self::random_literal(random, config, type_)))
                    .collect();
                IRNode::Array { value: elements }
            },
            NoirType::Tuple { types } => {
                let elements = types
                    .iter()
                    .map(|t| Box::new(Self::random_literal(random, config, t)))
                    .collect();
                IRNode::Tuple { value: elements }
            },
        }
    }

    /// @audit should be ordered by weight
    fn weighted_choice<'a, T>(random: &mut impl Rng, items: &'a [(T, u32)]) -> &'a T {
        let set 
    }
}

#[derive(Clone)]
pub struct Context {
    variables: Vec<(String, NoirType)>,
    functions: HashMap<String, Vec<NoirType>>,
    globals: Vec<(String, NoirType)>,
    parent_type: Option<String>,
}   

impl Context {
    pub fn new(field_modulus: U256) -> Self {
        Self { variables: Vec::new(), functions: HashMap::new(), globals: Vec::new(), parent_type: None }
    }

    pub fn add_variable(&mut self, name: String, type_: NoirType) {
        self.variables.push((name, type_));
    }

    pub fn get_random_variable(&self, random: &mut impl Rng) -> Option<&(String, NoirType)> {
        if self.variables.is_empty() {
            None
        } else {
            Some(random.choice(&self.variables))
        }
    }

    pub fn get_random_variable_of_type(
        self: &Self,
        random: &mut impl Rng,
        target_type: &NoirType,
    ) -> Option<&String> {
        // This equality check might be too strict for complex types, but works for basic ones
        let candidates: Vec<&String> = self
            .variables
            .iter()
            .filter(|(_, t)| self.types_match(t, target_type))
            .map(|(n, _)| n)
            .collect();

        if candidates.is_empty() {
            None
        } else {
            Some(*random.choice(&candidates))
        }
    }

    fn types_match(&self, t1: &NoirType, t2: &NoirType) -> bool {
        // Simple structural equality for now.
        // In a real fuzzer, we might allow implicit casts if the language supports them.
        match (t1, t2) {
            (NoirType::Field, NoirType::Field) => true,
            (NoirType::Boolean, NoirType::Boolean) => true,
            (
                NoirType::Integer { signed: s1, size: z1 },
                NoirType::Integer { signed: s2, size: z2 },
            ) => s1 == s2 && z1 == z2,
            (NoirType::String { size: s1 }, NoirType::String { size: s2 }) => s1 == s2,
            _ => false, // Ignore complex types for variable matching for now to keep it simple
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::{rngs::SmallRng, SeedableRng};

    #[test]
    fn test_random_ir_generation() {
        let mut rng = SmallRng::seed_from_u64(42);
        let config = Config {
            max_input_count: 5,
            max_node_count: 100,
            max_expression_depth: 5,
            max_element_count: 5,
            max_string_size: 10,
            max_type_depth: 3,
            max_for_size: 3,
            signed_probability: 0.5,
            boundary_value_probability: 0.1,
            small_upper_bound_probability: 0.1,
        };
        let mut ctx = Context::new();

        // Generate a few statements
        for _ in 0..10 {
            let stmt = IRNode::random_statement(&mut rng, &config, 0, &mut ctx);
            println!("{:#?}", stmt);
        }
    }
}
