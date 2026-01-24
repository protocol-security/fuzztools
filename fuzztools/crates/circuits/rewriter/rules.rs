/// Marker trait for rules - not used but kept for potential future extensions
#[derive(Debug, Clone)]
pub struct Rule {
    pub kind: RuleKind,
}

#[derive(Debug, Clone, Copy)]
pub enum RuleKind {
    // ═══════════════════════════════════════════════════════════════════════════════
    // Structural transformations
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Swap operands for commutative operators: (a op b) ↔ (b op a)
    /// Covers: Add, Mul, And, Or, Xor, Equal, NotEqual
    SwapOperands,

    /// Re-associate operators: ((a op b) op c) ↔ (a op (b op c))
    /// Covers: Add, Mul, And, Or, Xor
    Associate,

    /// Subtraction associativity: ((a - b) - c) ↔ (a - (b + c))
    AssociateSub,

    /// Division associativity: ((a / b) * c) ↔ (a * (c / b))
    /// NOTE: Only valid for Field types (integer division has different semantics)
    AssociateDiv,

    /// Division commutativity: (a / b) → ((1 / b) * a)
    /// NOTE: Only valid for Field types (integer division truncates 1/b to 0)
    DivCommute,

    /// Distribute: (a + b) * c ↔ (a * c) + (b * c)
    DistributeMulAdd,
    /// Distribute: (a - b) * c ↔ (a * c) - (b * c)
    DistributeMulSub,
    /// Distribute: (a | b) & c ↔ (a & c) | (b & c)
    DistributeAndOr,
    /// Distribute: (a & b) | c ↔ (a | c) & (b | c)
    DistributeOrAnd,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Identity and absorbing element rules
    // ═══════════════════════════════════════════════════════════════════════════════
    /// (a + 0) ↔ a
    IdentityAdd,
    /// (a - 0) ↔ a
    IdentitySub,
    /// (a * 1) ↔ a
    IdentityMul,
    /// (a / 1) ↔ a
    IdentityDiv,
    /// (a ^ 0) ↔ a
    IdentityXor,
    /// (a | 0) ↔ a
    IdentityOr,
    /// (a & 1) ↔ a (only valid for booleans)
    IdentityAnd,
    /// (a << 0) ↔ a
    IdentityShl,
    /// (a >> 0) ↔ a
    IdentityShr,

    /// (a * 0) → 0
    AbsorbMul,
    /// (a & 0) → 0
    AbsorbAnd,
    /// (a | true) → true (only valid for booleans)
    AbsorbOr,

    /// (a - a) → 0
    SelfInverseSub,
    /// (a ^ a) → 0
    SelfInverseXor,
    /// (a / a) → 1 (only for non-zero a)
    SelfInverseDiv,

    /// (a & a) ↔ a
    IdempotentAnd,
    /// (a | a) ↔ a
    IdempotentOr,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Unary and negation transformations
    // ═══════════════════════════════════════════════════════════════════════════════
    /// --a ↔ a
    DoubleNeg,
    /// !!a ↔ a
    DoubleNot,

    /// (a - b) ↔ (a + (-b))
    AddNegSub,

    /// (-a) ↔ (0 - a)
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

impl RuleKind {
    pub fn name(&self) -> &'static str {
        match self {
            Self::SwapOperands => "SwapOperands",
            Self::Associate => "Associate",
            Self::AssociateSub => "AssociateSub",
            Self::AssociateDiv => "AssociateDiv",
            Self::DivCommute => "DivCommute",
            Self::DistributeMulAdd => "DistributeMulAdd",
            Self::DistributeMulSub => "DistributeMulSub",
            Self::DistributeAndOr => "DistributeAndOr",
            Self::DistributeOrAnd => "DistributeOrAnd",
            Self::IdentityAdd => "IdentityAdd",
            Self::IdentitySub => "IdentitySub",
            Self::IdentityMul => "IdentityMul",
            Self::IdentityDiv => "IdentityDiv",
            Self::IdentityXor => "IdentityXor",
            Self::IdentityOr => "IdentityOr",
            Self::IdentityAnd => "IdentityAnd",
            Self::IdentityShl => "IdentityShl",
            Self::IdentityShr => "IdentityShr",
            Self::AbsorbMul => "AbsorbMul",
            Self::AbsorbAnd => "AbsorbAnd",
            Self::AbsorbOr => "AbsorbOr",
            Self::SelfInverseSub => "SelfInverseSub",
            Self::SelfInverseXor => "SelfInverseXor",
            Self::SelfInverseDiv => "SelfInverseDiv",
            Self::IdempotentAnd => "IdempotentAnd",
            Self::IdempotentOr => "IdempotentOr",
            Self::DoubleNeg => "DoubleNeg",
            Self::DoubleNot => "DoubleNot",
            Self::AddNegSub => "AddNegSub",
            Self::NegZeroSub => "NegZeroSub",
            Self::FlipComparison => "FlipComparison",
            Self::NegateComparison => "NegateComparison",
            Self::ExpandComparison => "ExpandComparison",
            Self::DeMorgan => "DeMorgan",
            Self::ComplementXor => "ComplementXor",
            Self::XorToAndOr => "XorToAndOr",
            Self::ModOne => "ModOne",
            Self::AndToMod => "AndToMod",
            Self::ShiftZero => "ShiftZero",
            Self::InjectAddSub => "InjectAddSub",
            Self::InjectSubAdd => "InjectSubAdd",
            Self::InjectMulDiv => "InjectMulDiv",
            Self::InjectXorXor => "InjectXorXor",
            Self::InjectDivDiv => "InjectDivDiv",
            Self::InjectOrZero => "InjectOrZero",
            Self::InjectAndSelf => "InjectAndSelf",
            Self::DoubleMulTwo => "DoubleMulTwo",
            Self::MulNegOneNeg => "MulNegOneNeg",
        }
    }
}

pub const RULES: &[Rule] = &[
    // ─────────────────────────────────────────────────────────────────────────────
    // Structural
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::SwapOperands),
    Rule::new(RuleKind::Associate),
    Rule::new(RuleKind::AssociateSub),
    Rule::new(RuleKind::AssociateDiv),
    Rule::new(RuleKind::DivCommute),
    Rule::new(RuleKind::DistributeMulAdd),
    Rule::new(RuleKind::DistributeMulSub),
    Rule::new(RuleKind::DistributeAndOr),
    Rule::new(RuleKind::DistributeOrAnd),
    // ─────────────────────────────────────────────────────────────────────────────
    // Identity
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::IdentityAdd),
    Rule::new(RuleKind::IdentitySub),
    Rule::new(RuleKind::IdentityMul),
    Rule::new(RuleKind::IdentityDiv),
    Rule::new(RuleKind::IdentityXor),
    Rule::new(RuleKind::IdentityOr),
    Rule::new(RuleKind::IdentityAnd),
    Rule::new(RuleKind::IdentityShl),
    Rule::new(RuleKind::IdentityShr),
    // ─────────────────────────────────────────────────────────────────────────────
    // Absorb
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::AbsorbMul),
    Rule::new(RuleKind::AbsorbAnd),
    Rule::new(RuleKind::AbsorbOr),
    // ─────────────────────────────────────────────────────────────────────────────
    // Self-inverse
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::SelfInverseSub),
    Rule::new(RuleKind::SelfInverseXor),
    Rule::new(RuleKind::SelfInverseDiv),
    // ─────────────────────────────────────────────────────────────────────────────
    // Idempotent
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::IdempotentAnd),
    Rule::new(RuleKind::IdempotentOr),
    // ─────────────────────────────────────────────────────────────────────────────
    // Unary
    // ─────────────────────────────────────────────────────────────────────────────
    Rule::new(RuleKind::DoubleNeg),
    Rule::new(RuleKind::DoubleNot),
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
