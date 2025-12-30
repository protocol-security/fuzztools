use crate::circuits::ast::operators::Operator;

#[derive(Debug, Clone)]
pub struct Rule {
    pub kind: RuleKind,
}

#[derive(Debug, Clone)]
pub enum RuleKind {
    // Structural
    SwapOperands { ops: &'static [Operator] },
    Associate { ops: &'static [Operator] },
    Distribute { outer: Operator, inner: Operator },

    // Identity/Absorbing
    Identity { op: Operator, identity_right: bool },
    Absorb { op: Operator },
    SelfInverse { op: Operator },
    Idempotent { op: Operator },

    // Unary
    DoubleUnary { op: Operator },
    AddNegSub,
    NegZeroSub,

    // Comparison
    FlipComparison,
    NegateComparison,

    // Boolean
    DeMorgan,
    ComplementXor,

    // Obfuscation
    InjectAddSub,
    InjectSubAdd,
    InjectMulDiv,
    InjectXorXor,

    // Simplification
    DoubleMulTwo,
    MulNegOneNeg,
}

impl Rule {
    pub const fn new(kind: RuleKind) -> Self {
        Self { kind }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// RULE CONSTANTS
// ═══════════════════════════════════════════════════════════════════════════════
// @todo simplify this by merging rules
pub const COMM_ADD: Rule = Rule::new(RuleKind::SwapOperands { ops: &[Operator::Add] });
pub const COMM_MUL: Rule = Rule::new(RuleKind::SwapOperands { ops: &[Operator::Mul] });
pub const COMM_AND: Rule = Rule::new(RuleKind::SwapOperands { ops: &[Operator::And] });
pub const COMM_OR: Rule = Rule::new(RuleKind::SwapOperands { ops: &[Operator::Or] });
pub const COMM_XOR: Rule = Rule::new(RuleKind::SwapOperands { ops: &[Operator::Xor] });
pub const COMM_EQ: Rule = Rule::new(RuleKind::SwapOperands { ops: &[Operator::Equal] });
pub const COMM_NEQ: Rule = Rule::new(RuleKind::SwapOperands { ops: &[Operator::NotEqual] });

pub const ASSOC_ADD: Rule = Rule::new(RuleKind::Associate { ops: &[Operator::Add] });
pub const ASSOC_MUL: Rule = Rule::new(RuleKind::Associate { ops: &[Operator::Mul] });
pub const ASSOC_AND: Rule = Rule::new(RuleKind::Associate { ops: &[Operator::And] });
pub const ASSOC_OR: Rule = Rule::new(RuleKind::Associate { ops: &[Operator::Or] });
pub const ASSOC_XOR: Rule = Rule::new(RuleKind::Associate { ops: &[Operator::Xor] });

pub const DIST_MUL_ADD: Rule =
    Rule::new(RuleKind::Distribute { outer: Operator::Mul, inner: Operator::Add });
pub const DIST_MUL_SUB: Rule =
    Rule::new(RuleKind::Distribute { outer: Operator::Mul, inner: Operator::Sub });
pub const DIST_AND_OR: Rule =
    Rule::new(RuleKind::Distribute { outer: Operator::And, inner: Operator::Or });
pub const DIST_OR_AND: Rule =
    Rule::new(RuleKind::Distribute { outer: Operator::Or, inner: Operator::And });

pub const ADD_ZERO: Rule =
    Rule::new(RuleKind::Identity { op: Operator::Add, identity_right: true });
pub const SUB_ZERO: Rule =
    Rule::new(RuleKind::Identity { op: Operator::Sub, identity_right: true });
pub const MUL_ONE: Rule = Rule::new(RuleKind::Identity { op: Operator::Mul, identity_right: true });
pub const DIV_ONE: Rule = Rule::new(RuleKind::Identity { op: Operator::Div, identity_right: true });
pub const XOR_ZERO: Rule =
    Rule::new(RuleKind::Identity { op: Operator::Xor, identity_right: true });

pub const MUL_ZERO: Rule = Rule::new(RuleKind::Absorb { op: Operator::Mul });
pub const AND_FALSE: Rule = Rule::new(RuleKind::Absorb { op: Operator::And });
pub const OR_TRUE: Rule = Rule::new(RuleKind::Absorb { op: Operator::Or });

pub const SELF_SUB: Rule = Rule::new(RuleKind::SelfInverse { op: Operator::Sub });
pub const SELF_XOR: Rule = Rule::new(RuleKind::SelfInverse { op: Operator::Xor });
pub const SELF_DIV: Rule = Rule::new(RuleKind::SelfInverse { op: Operator::Div });

pub const IDEM_AND: Rule = Rule::new(RuleKind::Idempotent { op: Operator::And });
pub const IDEM_OR: Rule = Rule::new(RuleKind::Idempotent { op: Operator::Or });

pub const DOUBLE_NEG: Rule = Rule::new(RuleKind::DoubleUnary { op: Operator::Neg });
pub const DOUBLE_NOT: Rule = Rule::new(RuleKind::DoubleUnary { op: Operator::Not });

pub const ADD_NEG_SUB: Rule = Rule::new(RuleKind::AddNegSub);
pub const NEG_ZERO_SUB: Rule = Rule::new(RuleKind::NegZeroSub);

pub const FLIP_COMPARISON: Rule = Rule::new(RuleKind::FlipComparison);
pub const NEGATE_COMPARISON: Rule = Rule::new(RuleKind::NegateComparison);

pub const DEMORGAN: Rule = Rule::new(RuleKind::DeMorgan);
pub const COMPLEMENT_XOR: Rule = Rule::new(RuleKind::ComplementXor);

pub const INJECT_ADD_SUB: Rule = Rule::new(RuleKind::InjectAddSub);
pub const INJECT_SUB_ADD: Rule = Rule::new(RuleKind::InjectSubAdd);
pub const INJECT_MUL_DIV: Rule = Rule::new(RuleKind::InjectMulDiv);
pub const INJECT_XOR_XOR: Rule = Rule::new(RuleKind::InjectXorXor);

pub const DOUBLE_MUL_TWO: Rule = Rule::new(RuleKind::DoubleMulTwo);
pub const MUL_NEG_ONE_NEG: Rule = Rule::new(RuleKind::MulNegOneNeg);

pub const RULES: &[Rule] = &[
    COMM_ADD,
    COMM_MUL,
    COMM_AND,
    COMM_OR,
    COMM_XOR,
    COMM_EQ,
    COMM_NEQ,
    ASSOC_ADD,
    ASSOC_MUL,
    ASSOC_AND,
    ASSOC_OR,
    ASSOC_XOR,
    DIST_MUL_ADD,
    DIST_MUL_SUB,
    DIST_AND_OR,
    DIST_OR_AND,
    ADD_ZERO,
    SUB_ZERO,
    MUL_ONE,
    DIV_ONE,
    XOR_ZERO,
    MUL_ZERO,
    AND_FALSE,
    OR_TRUE,
    SELF_SUB,
    SELF_XOR,
    SELF_DIV,
    IDEM_AND,
    IDEM_OR,
    DOUBLE_NEG,
    DOUBLE_NOT,
    ADD_NEG_SUB,
    NEG_ZERO_SUB,
    FLIP_COMPARISON,
    NEGATE_COMPARISON,
    DEMORGAN,
    COMPLEMENT_XOR,
    INJECT_ADD_SUB,
    INJECT_SUB_ADD,
    INJECT_MUL_DIV,
    INJECT_XOR_XOR,
    DOUBLE_NEG,
    DOUBLE_NOT,
    DOUBLE_NEG,
    DOUBLE_NOT,
    ADD_ZERO,
    MUL_ONE,
    MUL_ZERO,
    SELF_SUB,
    SELF_XOR,
    IDEM_AND,
    IDEM_OR,
];
