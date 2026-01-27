use strum::EnumIter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumIter)]
pub enum Rule {
    // ═══════════════════════════════════════════════════════════════════════════════
    // Structural transformations
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
    // Identity and absorbing element rules
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
    // Unary and negation transformations
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
    // Comparison transformations
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Flip comparison by swapping operands: (a < b) <-> (b > a)
    FlipComparison,
    /// Negate comparison: (a < b) <-> !(a >= b)
    NegateComparison,
    /// Expand comparison with equality: (a <= b) <-> ((a < b) || (a == b))
    ExpandComparison,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Boolean logic transformations
    // ═══════════════════════════════════════════════════════════════════════════════
    /// De Morgan's laws: !(a && b) <-> (!a || !b), !(a || b) <-> (!a && !b)
    DeMorgan,
    /// Complement via XOR: !a <-> (a ^ true)
    /// NOTE: Only valid for booleans (for integers, !a is bitwise NOT ≠ a ^ 1)
    ComplementXor,
    /// XOR to AND/OR expansion: (a ^ b) <-> ((!a & b) | (a & !b))
    XorToAndOr,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Modulo transformations
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Modulo by one: (a % 1) <-> 0
    ModOne,
    /// Bitwise AND to modulo: (a & 1) <-> (a % 2)
    AndToMod,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Shift transformations
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Shift by zero identity: (a << 0) <-> a, (a >> 0) <-> a
    ShiftZero,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Obfuscation (inject balanced operations)
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Inject add/sub pair: a <-> ((a + r) - r) or a <-> ((a - r) + r)
    /// NOTE: Only for Field types (integer overflow breaks equivalence)
    InjectAddSub,
    /// Inject sub/add pair: a <-> ((a - r) + r)
    /// NOTE: Only for Field types (integer overflow breaks equivalence)
    InjectSubAdd,
    /// Inject mul/div pair: a <-> ((a * r) / r)
    /// NOTE: Only for Field types (integer overflow breaks equivalence)
    InjectMulDiv,
    /// Inject xor pair: a <-> ((a ^ r) ^ r)
    InjectXorXor,
    /// Inject div pair: 1 <-> (r / r)
    InjectDivDiv,
    /// Inject OR identity: a <-> (a | 0)
    InjectOrZero,
    /// Inject AND identity: a <-> (a & a) for integers
    InjectAndSelf,

    // ═══════════════════════════════════════════════════════════════════════════════
    // Simplification / strength reduction
    // ═══════════════════════════════════════════════════════════════════════════════
    /// Double to multiply by two: (a + a) <-> (a * 2)
    DoubleMulTwo,
    /// Multiply by -1 to negation: (a * -1) <-> (-a)
    MulNegOneNeg,
}
