use crate::circuits::ast::operators::Operator;

/// A rule that can be applied to transform expressions in the graph
#[derive(Debug, Clone)]
pub struct Rule {
    pub name: &'static str,
    pub kind: RuleKind,
}

// @todo add many more

#[derive(Debug, Clone)]
pub enum RuleKind {
    // ═══════════════════════════════════════════════════════════════════════════
    // STRUCTURAL TRANSFORMATIONS
    // ═══════════════════════════════════════════════════════════════════════════
    /// Swap operands of a commutative binary operation
    SwapOperands { ops: &'static [Operator] },

    /// Associate: ((a op b) op c) -> (a op (b op c))
    Associate { ops: &'static [Operator] },

    /// Distribute: (a inner b) outer c -> (a outer c) inner (b outer c)
    Distribute { outer: Operator, inner: Operator },

    // ═══════════════════════════════════════════════════════════════════════════
    // IDENTITY RULES (simplification)
    // ═══════════════════════════════════════════════════════════════════════════
    /// Identity element: (a op identity) -> a
    Identity { op: Operator, identity_right: bool },

    /// Absorbing element: (a op absorb) -> absorb
    Absorb { op: Operator },

    /// Self-inverse: (a op a) -> result
    SelfInverse { op: Operator },

    /// Idempotent: (a op a) -> a
    Idempotent { op: Operator },

    // ═══════════════════════════════════════════════════════════════════════════
    // UNARY TRANSFORMATIONS
    // ═══════════════════════════════════════════════════════════════════════════
    /// Add double unary: a -> op(op(a))
    /// e.g., a -> --a, a -> !!a
    DoubleUnary { op: Operator },

    /// Convert sub to add-neg: (a - b) -> (a + (-b))
    AddNegSub,

    /// Convert neg to sub: (-a) -> (0 - a)
    NegZeroSub,

    // ═══════════════════════════════════════════════════════════════════════════
    // COMPARISON TRANSFORMATIONS
    // ═══════════════════════════════════════════════════════════════════════════
    /// Flip comparison direction and swap operands
    /// e.g., (a < b) -> (b > a)
    FlipComparison,

    /// Negate comparison: (a cmp b) -> !(a cmp' b)
    NegateComparison,

    // ═══════════════════════════════════════════════════════════════════════════
    // DE MORGAN'S LAWS
    // ═══════════════════════════════════════════════════════════════════════════
    /// De Morgan expand: !(a & b) -> (!a | !b), !(a | b) -> (!a & !b)
    DeMorgan,

    // ═══════════════════════════════════════════════════════════════════════════
    // COMPLEMENT / XOR CONVERSIONS
    // ═══════════════════════════════════════════════════════════════════════════
    /// Complement to xor: (!a) -> (a ^ 1)
    ComplementXor,

    // ═══════════════════════════════════════════════════════════════════════════
    // RANDOM VALUE INJECTION (for obfuscation)
    // ═══════════════════════════════════════════════════════════════════════════
    /// Inject random add-sub: a -> ((a + r) - r)
    InjectRandomAddSub,

    /// Inject random sub-add: a -> ((a - r) + r)
    InjectRandomSubAdd,

    /// Inject random mul-div: a -> ((a * r) / r)
    InjectRandomMulDiv,

    /// Inject random xor-xor: a -> ((a ^ r) ^ r)
    InjectRandomXorXor,

    // ═══════════════════════════════════════════════════════════════════════════
    // MULTIPLICATION SHORTCUTS
    // ═══════════════════════════════════════════════════════════════════════════
    /// Double to mul-two: (a + a) -> (a * 2)
    DoubleMulTwo,

    /// Mul neg-one to neg: (a * -1) -> (-a)
    MulNegOneNeg,
}

impl Rule {
    pub const fn new(name: &'static str, kind: RuleKind) -> Self {
        Self { name, kind }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// COMMUTATIVITY
// ═══════════════════════════════════════════════════════════════════════════════

// a + b <=> b + a
pub const COMM_ADD: Rule = Rule::new("comm-add", RuleKind::SwapOperands { ops: &[Operator::Add] });
// a * b <=> b * a
pub const COMM_MUL: Rule = Rule::new("comm-mul", RuleKind::SwapOperands { ops: &[Operator::Mul] });
// a / b <=> (1 / b) * a
pub const COMM_DIV: Rule = Rule::new("comm-div", RuleKind::SwapOperands { ops: &[Operator::Div] });
// a % b <=> a - (a / b)
pub const COMM_MOD: Rule = Rule::new("comm-mod", RuleKind::SwapOperands { ops: &[Operator::Mod] });
// a & b <=> b & a
pub const COMM_AND: Rule = Rule::new("comm-and", RuleKind::SwapOperands { ops: &[Operator::And] });
// a | b <=> b | a
pub const COMM_OR: Rule = Rule::new("comm-or", RuleKind::SwapOperands { ops: &[Operator::Or] });
// a ^ b <=> b ^ a
pub const COMM_XOR: Rule = Rule::new("comm-xor", RuleKind::SwapOperands { ops: &[Operator::Xor] });
// a == b <=> b == a
pub const COMM_EQ: Rule = Rule::new("comm-eq", RuleKind::SwapOperands { ops: &[Operator::Equal] });
// a != b <=> b != a
pub const COMM_NEQ: Rule =
    Rule::new("comm-neq", RuleKind::SwapOperands { ops: &[Operator::NotEqual] });

// ═══════════════════════════════════════════════════════════════════════════════
// ASSOCIATIVITY
// ═══════════════════════════════════════════════════════════════════════════════

// (a + b) + c <=> a + (b + c)
pub const ASSOC_ADD: Rule = Rule::new("assoc-add", RuleKind::Associate { ops: &[Operator::Add] });
// (a * b) * c <=> a * (b * c)
pub const ASSOC_MUL: Rule = Rule::new("assoc-mul", RuleKind::Associate { ops: &[Operator::Mul] });
// (a / b) * c <=> a * (c / b)
pub const ASSOC_DIV: Rule = Rule::new("assoc-div", RuleKind::Associate { ops: &[Operator::Div] });
/* @todo (a % b) * c <=> (a * (c / b)) * c
pub const ASSOC_MOD: Rule =
    Rule::new("assoc-mod", RuleKind::Associate { ops: &[Operator::Mod] });*/
// (a & b) & c <=> a & (b & c)
pub const ASSOC_AND: Rule = Rule::new("assoc-and", RuleKind::Associate { ops: &[Operator::And] });
// (a | b) | c <=> a | (b | c)
pub const ASSOC_OR: Rule = Rule::new("assoc-or", RuleKind::Associate { ops: &[Operator::Or] });
// (a ^ b) ^ c <=> a ^ (b ^ c)
pub const ASSOC_XOR: Rule = Rule::new("assoc-xor", RuleKind::Associate { ops: &[Operator::Xor] });

// ═══════════════════════════════════════════════════════════════════════════════
// DISTRIBUTIVITY
// ═══════════════════════════════════════════════════════════════════════════════

// a * (b + c) <=> (a * b) + (a * c)
pub const DIST_MUL_ADD: Rule =
    Rule::new("dist-mul-add", RuleKind::Distribute { outer: Operator::Mul, inner: Operator::Add });
// a * (b - c) <=> (a * b) - (a * c)
pub const DIST_MUL_SUB: Rule =
    Rule::new("dist-mul-sub", RuleKind::Distribute { outer: Operator::Mul, inner: Operator::Sub });
// a & (b | c) <=> (a & b) | (a & c)
pub const DIST_AND_OR: Rule =
    Rule::new("dist-and-or", RuleKind::Distribute { outer: Operator::And, inner: Operator::Or });
// a | (b & c) <=> (a | b) & (a | c)
pub const DIST_OR_AND: Rule =
    Rule::new("dist-or-and", RuleKind::Distribute { outer: Operator::Or, inner: Operator::And });

// ═══════════════════════════════════════════════════════════════════════════════
// IDENTITY
// ═══════════════════════════════════════════════════════════════════════════════

// a + 0 <=> a
pub const ADD_ZERO: Rule =
    Rule::new("identity-add-zero", RuleKind::Identity { op: Operator::Add, identity_right: true });
// a - 0 <=> a
pub const SUB_ZERO: Rule =
    Rule::new("identity-sub-zero", RuleKind::Identity { op: Operator::Sub, identity_right: true });
// a * 1 <=> a
pub const MUL_ONE: Rule =
    Rule::new("identity-mul-one", RuleKind::Identity { op: Operator::Mul, identity_right: true });
// a / 1 <=> a
pub const DIV_ONE: Rule =
    Rule::new("identity-div-one", RuleKind::Identity { op: Operator::Div, identity_right: true });
// a | false <=> a
pub const OR_FALSE: Rule =
    Rule::new("identity-or-false", RuleKind::Identity { op: Operator::Or, identity_right: true });
// a & true <=> a
pub const AND_TRUE: Rule =
    Rule::new("identity-and-true", RuleKind::Identity { op: Operator::And, identity_right: true });
// a ^ 0 <=> a
pub const XOR_ZERO: Rule =
    Rule::new("identity-xor-zero", RuleKind::Identity { op: Operator::Xor, identity_right: true });
// a << 0 <=> a
pub const SHL_ZERO: Rule =
    Rule::new("identity-shl-zero", RuleKind::Identity { op: Operator::Shl, identity_right: true });
// a >> 0 <=> a
pub const SHR_ZERO: Rule =
    Rule::new("identity-shr-zero", RuleKind::Identity { op: Operator::Shr, identity_right: true });

// ═══════════════════════════════════════════════════════════════════════════════
// ABSORBING ELEMENTS
// ═══════════════════════════════════════════════════════════════════════════════

// a * 0 <=> 0
pub const MUL_ZERO: Rule = Rule::new("absorb-mul-zero", RuleKind::Absorb { op: Operator::Mul });
// a & false <=> false
pub const AND_FALSE: Rule = Rule::new("absorb-and-false", RuleKind::Absorb { op: Operator::And });
// a | true <=> true
pub const OR_TRUE: Rule = Rule::new("absorb-or-true", RuleKind::Absorb { op: Operator::Or });

// ═══════════════════════════════════════════════════════════════════════════════
// SELF-INVERSE
// ═══════════════════════════════════════════════════════════════════════════════

// a - a <=> 0
pub const SELF_SUB: Rule = Rule::new("self-sub", RuleKind::SelfInverse { op: Operator::Sub });
// a / a <=> 1
pub const SELF_DIV: Rule = Rule::new("self-div", RuleKind::SelfInverse { op: Operator::Div });
// a ^ a <=> 0
pub const SELF_XOR: Rule = Rule::new("self-xor", RuleKind::SelfInverse { op: Operator::Xor });
// a % a <=> 0
pub const SELF_MOD: Rule = Rule::new("self-mod", RuleKind::SelfInverse { op: Operator::Mod });

// ═══════════════════════════════════════════════════════════════════════════════
// IDEMPOTENT
// ═══════════════════════════════════════════════════════════════════════════════

// a & a <=> a
pub const IDEM_AND: Rule = Rule::new("idem-and", RuleKind::Idempotent { op: Operator::And });
// a | a <=> a
pub const IDEM_OR: Rule = Rule::new("idem-or", RuleKind::Idempotent { op: Operator::Or });

// ═══════════════════════════════════════════════════════════════════════════════
// UNARY TRANSFORMATIONS
// ═══════════════════════════════════════════════════════════════════════════════

// --a <=> a
pub const DOUBLE_NEG: Rule = Rule::new("double-neg", RuleKind::DoubleUnary { op: Operator::Neg });
// !!a <=> a
pub const DOUBLE_NOT: Rule = Rule::new("double-not", RuleKind::DoubleUnary { op: Operator::Not });

// ═══════════════════════════════════════════════════════════════════════════════
// ADD NEGATED SUB
// ═══════════════════════════════════════════════════════════════════════════════

// a - b <=> a + (-b)
pub const ADD_NEG_SUB: Rule = Rule::new("add-neg-sub", RuleKind::AddNegSub);
// (-a) <=> (0 - a)
pub const NEG_ZERO_SUB: Rule = Rule::new("neg-zero-sub", RuleKind::NegZeroSub);

// ═══════════════════════════════════════════════════════════════════════════════
// FLIP COMPARISON
// ═══════════════════════════════════════════════════════════════════════════════

// a < b <=> b > a
pub const FLIP_LT_GT: Rule = Rule::new("flip-lt-gt", RuleKind::FlipComparison);
// a <= b <=> b >= a
pub const FLIP_LE_GE: Rule = Rule::new("flip-le-ge", RuleKind::FlipComparison);
// a < b <=> b >= a
pub const NEGATE_LT: Rule = Rule::new("negate-lt", RuleKind::NegateComparison);
// a > b <=> b <= a
pub const NEGATE_GT: Rule = Rule::new("negate-gt", RuleKind::NegateComparison);

// ═══════════════════════════════════════════════════════════════════════════════
// DE MORGAN'S LAWS
// ═══════════════════════════════════════════════════════════════════════════════

// !(a & b) <=> (!a | !b)
pub const DEMORGAN_AND_EXPAND: Rule = Rule::new("demorgan-and-expand", RuleKind::DeMorgan);
// !(a | b) <=> (!a & !b)
pub const DEMORGAN_OR_EXPAND: Rule = Rule::new("demorgan-or-expand", RuleKind::DeMorgan);

// ═══════════════════════════════════════════════════════════════════════════════
// COMPLEMENT / XOR
// ═══════════════════════════════════════════════════════════════════════════════

// (!a) -> (a ^ 1)
pub const COMPLEMENT_XOR: Rule = Rule::new("complement-xor", RuleKind::ComplementXor);

// ═══════════════════════════════════════════════════════════════════════════════
// RANDOM VALUE INJECTION
// ═══════════════════════════════════════════════════════════════════════════════

// a -> ((a + r) - r)
pub const INJECT_ADD_SUB: Rule = Rule::new("inject-add-sub", RuleKind::InjectRandomAddSub);
// a -> ((a - r) + r)
pub const INJECT_SUB_ADD: Rule = Rule::new("inject-sub-add", RuleKind::InjectRandomSubAdd);
// a -> ((a * r) / r)
pub const INJECT_MUL_DIV: Rule = Rule::new("inject-mul-div", RuleKind::InjectRandomMulDiv);
// a -> ((a ^ r) ^ r)
pub const INJECT_XOR_XOR: Rule = Rule::new("inject-xor-xor", RuleKind::InjectRandomXorXor);

// ═══════════════════════════════════════════════════════════════════════════════
// MULTIPLICATION SHORTCUTS
// ═══════════════════════════════════════════════════════════════════════════════

// (a + a) -> (a * 2)
pub const DOUBLE_MUL_TWO: Rule = Rule::new("double-mul-two", RuleKind::DoubleMulTwo);
// (a * -1) -> (-a)
pub const MUL_NEG_ONE_NEG: Rule = Rule::new("mul-neg-one-neg", RuleKind::MulNegOneNeg);

// ═══════════════════════════════════════════════════════════════════════════════
// RULE COLLECTIONS
// ═══════════════════════════════════════════════════════════════════════════════

/// All equivalence-preserving rules
pub const EQUIVALENCE_RULES: &[Rule] = &[
    // Commutativity
    COMM_ADD,
    COMM_MUL,
    COMM_DIV,
    COMM_MOD,
    COMM_AND,
    COMM_OR,
    COMM_XOR,
    COMM_EQ,
    COMM_NEQ,
    // Associativity
    ASSOC_ADD,
    ASSOC_MUL,
    ASSOC_DIV,
    ASSOC_AND,
    ASSOC_OR,
    ASSOC_XOR,
    // Distributivity
    DIST_MUL_ADD,
    DIST_MUL_SUB,
    DIST_AND_OR,
    DIST_OR_AND,
    // Identity
    ADD_ZERO,
    SUB_ZERO,
    MUL_ONE,
    DIV_ONE,
    OR_FALSE,
    AND_TRUE,
    XOR_ZERO,
    SHL_ZERO,
    SHR_ZERO,
    // Absorbing elements
    MUL_ZERO,
    AND_FALSE,
    OR_TRUE,
    // Self-inverse
    SELF_SUB,
    SELF_DIV,
    SELF_XOR,
    SELF_MOD,
    // Idempotent
    IDEM_AND,
    IDEM_OR,
    // Unary
    DOUBLE_NEG,
    DOUBLE_NOT,
    // Sub/neg conversions
    ADD_NEG_SUB,
    NEG_ZERO_SUB,
    // Comparison flips
    FLIP_LT_GT,
    FLIP_LE_GE,
    NEGATE_LT,
    NEGATE_GT,
    // De Morgan
    DEMORGAN_AND_EXPAND,
    DEMORGAN_OR_EXPAND,
    // Complement/XOR
    COMPLEMENT_XOR,
    // Shifts
    SHL_ZERO,
    SHR_ZERO,
    // Random injection
    INJECT_ADD_SUB,
    INJECT_SUB_ADD,
    INJECT_MUL_DIV,
    INJECT_XOR_XOR,
    // Multiplication shortcuts
    DOUBLE_MUL_TWO,
    MUL_NEG_ONE_NEG,
];
