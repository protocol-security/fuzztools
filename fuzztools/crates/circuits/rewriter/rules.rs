use crate::circuits::ast::operators::Operator;

#[derive(Debug, Clone)]
pub struct Rule {
    pub kind: RuleKind,
}

#[derive(Debug, Clone)]
pub enum RuleKind {
    // ═══════════════════════════════════════════════════════════════════════════════
    // Structural transformations
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Swap operands for commutative operators: (a op b) ↔ (b op a)
    /// Covers: comm-add, comm-mul, comm-and, comm-or, comm-xor, commutativity-equ
    SwapOperands {
        ops: &'static [Operator],
    },

    /// Re-associate operators: ((a op b) op c) ↔ (a op (b op c))
    /// Covers: assoc-add, assoc-mul, assoc-and, assoc-or, assoc-xor, assoc-lor, assoc-land
    Associate {
        ops: &'static [Operator],
    },

    /// Subtraction associativity: ((a - b) - c) ↔ (a - (b + c))
    /// Covers: inv-assoc-neg2pos, inv-assoc-pos2neg
    AssociateSub,

    /// Division associativity: ((a / b) * c) ↔ (a * (c / b))
    /// Covers: assoc-div, assoc-div-rev
    /// NOTE: Only valid for Field types (integer division has different semantics)
    AssociateDiv,

    /// Division commutativity: (a / b) → ((1 / b) * a)
    /// Covers: comm-div
    /// NOTE: Only valid for Field types (integer division truncates 1/b to 0)
    DivCommute,

    /// Distribute operators: ((a inner b) outer c) ↔ ((a outer c) inner (b outer c))
    /// Covers: dist-mul-add, dist-add-mul, dist-lor-land, dist-land-lor
    Distribute {
        outer: Operator,
        inner: Operator,
    },

    // ═══════════════════════════════════════════════════════════════════════════════
    // Identity and absorbing element rules
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Remove/inject identity element: (a op identity) ↔ a
    /// Covers: zero-add-des/con, zero-or-rev/con, zero-xor-rev/con, one-mul-des/con,
    ///         one-div-des/con, inv-zero-add-des/con, zero-lor-des/con, zero-land-des/con
    /// NOTE: For And (a & 1 = a), only valid for booleans (for integers, a & 1 keeps only LSB)
    Identity {
        op: Operator,
        identity_right: bool,
    },

    /// Replace with absorbing element: (a op absorbing) → absorbing
    /// Covers: and-zero, mul-zero, taut-lor, contra-land
    /// NOTE: For Or (a | 1 = 1), only valid for booleans (for integers, a | 1 sets only LSB)
    Absorb {
        op: Operator,
    },

    /// Self inverse: (a op a) → identity
    /// Covers: inv-xor (a ^ a → 0), inv-add-des (a - a → 0), inv-div-des (a / a → 1)
    /// NOTE: For Div, only applied to known non-zero literals (a / a is UB if a = 0)
    SelfInverse {
        op: Operator,
    },

    /// Idempotent: (a op a) ↔ a
    /// Covers: idem-and, idem-or, double-land-des/con, double-lor-des/con
    Idempotent {
        op: Operator,
    },

    // ═══════════════════════════════════════════════════════════════════════════════
    // Unary and negation transformations
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Double unary cancellation: op(op(a)) ↔ a
    /// Covers: double-negation-des/con, double-negation-add-des/con
    DoubleUnary {
        op: Operator,
    },

    /// Addition to negation: (a - b) ↔ (a + (-b))
    /// Covers: inv-addition-inl, inv-addition-exp
    AddNegSub,

    /// Negation to subtraction: (-a) ↔ (0 - a)
    /// Covers: neg-zero-add-des, neg-zero-add-con
    NegZeroSub,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Comparison transformations
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Flip comparison by swapping operands: (a < b) ↔ (b > a)
    /// Covers: relation-geq-to-leq, relation-leq-to-geq
    FlipComparison,

    /// Negate comparison: (a < b) ↔ !(a >= b)
    /// Covers: relation-leq-to-not-gth, relation-geq-to-not-lth, relation-neq-to-not-equ, etc.
    NegateComparison,

    /// Expand comparison with equality: (a <= b) ↔ ((a < b) || (a == b))
    /// Covers: relation-leq-to-lth-and-equ, relation-geq-to-gth-and-equ
    ExpandComparison,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Boolean logic transformations
    // ═══════════════════════════════════════════════════════════════════════════════
    /// De Morgan's laws: !(a && b) ↔ (!a || !b), !(a || b) ↔ (!a && !b)
    /// Covers: de-morgan-land-con/des, de-morgan-lor-con/des
    DeMorgan,

    /// Complement via XOR: !a ↔ (a ^ true)
    /// NOTE: Only valid for booleans (for integers, !a is bitwise NOT ≠ a ^ 1)
    ComplementXor,

    /// XOR to AND/OR expansion: (a ^ b) ↔ ((!a & b) | (a & !b))
    /// Covers: lxor-to-or-and, or-and-to-lxor
    XorToAndOr,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Modulo transformations
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Modulo by one: (a % 1) ↔ 0
    /// Covers: rem-of-one-des, rem-of-one-con
    ModOne,

    /// Bitwise AND to modulo: (a & 1) ↔ (a % 2)
    /// Covers: and-to-rem, rem-to-and
    AndToMod,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Shift transformations
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Shift by zero identity: (a << 0) → a, (a >> 0) → a
    ShiftZero,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Obfuscation (inject balanced operations)
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Inject add/sub pair: a → ((a + r) - r) or a → ((a - r) + r)
    /// Covers: add-sub-random-value
    /// NOTE: Only for Field types (integer overflow breaks equivalence)
    InjectAddSub,
    InjectSubAdd,

    /// Inject mul/div pair: a → ((a * r) / r)
    /// NOTE: Only for Field types (integer overflow breaks equivalence)
    InjectMulDiv,

    /// Inject xor pair: a → ((a ^ r) ^ r)
    /// Covers: inv-xor-rev (0 → r ^ r)
    InjectXorXor,

    /// Inject div pair: 1 → (r / r)
    /// Covers: one-div
    InjectDivDiv,

    /// Inject OR identity: a → (a | 0)
    /// Covers: zero-or
    InjectOrZero,

    /// Inject AND identity: a → (a & a) for integers
    /// Covers: idem-and injection
    InjectAndSelf,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Simplification / strength reduction
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Double to multiply by two: (a + a) ↔ (a * 2)
    DoubleMulTwo,

    /// Multiply by -1 to negation: (a * -1) ↔ (-a)
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

// Commutative operators
const COMMUTATIVE_OPS: &[Operator] = &[
    Operator::Add,
    Operator::Mul,
    Operator::And,
    Operator::Or,
    Operator::Xor,
    Operator::Equal,
    Operator::NotEqual,
];

// Associative operators
const ASSOCIATIVE_OPS: &[Operator] =
    &[Operator::Add, Operator::Mul, Operator::And, Operator::Or, Operator::Xor];

pub const RULES: &[Rule] = &[
    // ─────────────────────────────────────────────────────────────────────────────
    // Structural
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::SwapOperands { ops: COMMUTATIVE_OPS }),
    Rule::new(RuleKind::Associate { ops: ASSOCIATIVE_OPS }),
    Rule::new(RuleKind::AssociateSub),
    Rule::new(RuleKind::AssociateDiv),
    Rule::new(RuleKind::DivCommute),
    Rule::new(RuleKind::Distribute { outer: Operator::Mul, inner: Operator::Add }),
    Rule::new(RuleKind::Distribute { outer: Operator::Mul, inner: Operator::Sub }),
    Rule::new(RuleKind::Distribute { outer: Operator::And, inner: Operator::Or }),
    Rule::new(RuleKind::Distribute { outer: Operator::Or, inner: Operator::And }),
    // ─────────────────────────────────────────────────────────────────────────────
    // Identity
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::Identity { op: Operator::Add, identity_right: true }),
    Rule::new(RuleKind::Identity { op: Operator::Sub, identity_right: true }),
    Rule::new(RuleKind::Identity { op: Operator::Mul, identity_right: true }),
    Rule::new(RuleKind::Identity { op: Operator::Div, identity_right: true }),
    Rule::new(RuleKind::Identity { op: Operator::Xor, identity_right: true }),
    Rule::new(RuleKind::Identity { op: Operator::Or, identity_right: true }),
    Rule::new(RuleKind::Identity { op: Operator::And, identity_right: true }),
    Rule::new(RuleKind::Identity { op: Operator::Shl, identity_right: true }),
    Rule::new(RuleKind::Identity { op: Operator::Shr, identity_right: true }),
    // ─────────────────────────────────────────────────────────────────────────────
    // Absorb
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::Absorb { op: Operator::Mul }),
    Rule::new(RuleKind::Absorb { op: Operator::And }),
    Rule::new(RuleKind::Absorb { op: Operator::Or }),
    // ─────────────────────────────────────────────────────────────────────────────
    // Self-inverse
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::SelfInverse { op: Operator::Sub }),
    Rule::new(RuleKind::SelfInverse { op: Operator::Xor }),
    Rule::new(RuleKind::SelfInverse { op: Operator::Div }),
    // ─────────────────────────────────────────────────────────────────────────────
    // Idempotent
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::Idempotent { op: Operator::And }),
    Rule::new(RuleKind::Idempotent { op: Operator::Or }),
    // ─────────────────────────────────────────────────────────────────────────────
    // Unary
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::DoubleUnary { op: Operator::Neg }),
    Rule::new(RuleKind::DoubleUnary { op: Operator::Not }),
    Rule::new(RuleKind::AddNegSub),
    Rule::new(RuleKind::NegZeroSub),
    // ─────────────────────────────────────────────────────────────────────────────
    // Comparison
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::FlipComparison),
    Rule::new(RuleKind::NegateComparison),
    Rule::new(RuleKind::ExpandComparison),
    // ─────────────────────────────────────────────────────────────────────────────
    // Boolean logic
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::DeMorgan),
    Rule::new(RuleKind::ComplementXor),
    Rule::new(RuleKind::XorToAndOr),
    // ─────────────────────────────────────────────────────────────────────────────
    // Modulo
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::ModOne),
    Rule::new(RuleKind::AndToMod),
    // ─────────────────────────────────────────────────────────────────────────────
    // Shift
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::ShiftZero),
    // ─────────────────────────────────────────────────────────────────────────────
    // Obfuscation (inject balanced operations)
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::InjectAddSub),
    Rule::new(RuleKind::InjectSubAdd),
    Rule::new(RuleKind::InjectMulDiv),
    Rule::new(RuleKind::InjectXorXor),
    Rule::new(RuleKind::InjectDivDiv),
    Rule::new(RuleKind::InjectOrZero),
    Rule::new(RuleKind::InjectAndSelf),
    // ─────────────────────────────────────────────────────────────────────────────
    // Simplification
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::DoubleMulTwo),
    Rule::new(RuleKind::MulNegOneNeg),
];
