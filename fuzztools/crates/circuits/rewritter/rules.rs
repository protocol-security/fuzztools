use strum::EnumIter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumIter)]
pub enum Rule {
    // ═══════════════════════════════════════════════════════════════════════════════
    // Structural
    // ═══════════════════════════════════════════════════════════════════════════════
    /// (a op b) <-> (b op a)
    SwapOperands,
    /// ((a op b) op c) <-> (a op (b op c))
    Associate,
    /// ((a - b) - c) <-> (a - (b + c))
    AssociateSub,
    /// ((a / b) * c) <-> (a * (c / b))
    AssociateDiv,
    /// (a / b) <-> ((1 / b) * a)
    DivCommute,
    /// (a + b) * c <-> (a * c) + (b * c)
    DistributeMulAdd,
    /// (a - b) * c <-> (a * c) - (b * c)
    DistributeMulSub,
    /// (a | b) & c <-> (a & c) | (b & c)
    DistributeAndOr,
    /// (a & b) | c <-> (a | c) & (b | c)
    DistributeOrAnd,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Identity and absorbing
    // ═══════════════════════════════════════════════════════════════════════════════
    /// (a + 0) <-> a
    IdentityAdd,
    /// (a - 0) <-> a
    IdentitySub,
    /// (a * 1) <-> a
    IdentityMul,
    /// (a / 1) <-> a
    IdentityDiv,
    /// (a ^ 0) <-> a
    IdentityXor,
    /// (a | 0) <-> a
    IdentityOr,
    /// (a & 1) <-> a (only valid for booleans)
    IdentityAnd,
    /// (a << 0) <-> a
    IdentityShl,
    /// (a >> 0) <-> a
    IdentityShr,

    /// (a * 0) <-> 0
    AbsorbMul,
    /// (a & 0) <-> 0
    AbsorbAnd,
    /// (a | true) <-> true (only valid for booleans)
    AbsorbOr,

    /// (a - a) <-> 0
    SelfInverseSub,
    /// (a ^ a) <-> 0
    SelfInverseXor,
    /// (a / a) <-> 1 (only for non-zero a)
    SelfInverseDiv,

    /// (a & a) <-> a
    IdempotentAnd,
    /// (a | a) <-> a
    IdempotentOr,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Unary
    // ═══════════════════════════════════════════════════════════════════════════════
    /// --a <-> a
    DoubleNeg,
    /// !!a <-> a
    DoubleNot,
    /// (a - b) <-> (a + (-b))
    AddNegSub,
    /// (-a) <-> (0 - a)
    NegZeroSub,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Comparison
    // ═══════════════════════════════════════════════════════════════════════════════
    /// (a < b) <-> (b > a)
    FlipComparison,
    /// (a < b) <-> !(a >= b)
    NegateComparison,
    /// (a <= b) <-> ((a < b) || (a == b))
    ExpandComparison,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Boolean logic
    // ═══════════════════════════════════════════════════════════════════════════════
    /// !(a && b) <-> (!a || !b), !(a || b) <-> (!a && !b)
    DeMorgan,
    /// !a <-> (a ^ true) (only for booleans)
    ComplementXor,
    /// (a ^ b) <-> ((!a & b) | (a & !b))
    XorToAndOr,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Modulo
    // ═══════════════════════════════════════════════════════════════════════════════
    /// (a % 1) <-> 0
    ModOne,
    /// (a & 1) <-> (a % 2)
    AndToMod,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Shift
    // ═══════════════════════════════════════════════════════════════════════════════
    /// (a << 0) <-> a, (a >> 0) <-> a
    ShiftZero,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Injections
    // ═══════════════════════════════════════════════════════════════════════════════
    /// a <-> ((a + r) - r) or a <-> ((a - r) + r) (only for fields)
    InjectAddSub,
    /// a <-> ((a - r) + r) (only for fields)
    InjectSubAdd,
    /// a <-> ((a * r) / r) (only for fields)
    InjectMulDiv,
    /// a <-> ((a ^ r) ^ r)
    InjectXorXor,
    /// 1 <-> (r / r)
    InjectDivDiv,
    /// a <-> (a | 0)
    InjectOrZero,
    /// a <-> (a & a)
    InjectAndSelf,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Simplification
    // ═══════════════════════════════════════════════════════════════════════════════
    /// (a + a) <-> (a * 2)
    DoubleMulTwo,
    /// (a * -1) <-> (-a) (only for field and signed integers)
    MulNegOneNeg,
}
