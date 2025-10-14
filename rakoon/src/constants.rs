use clap::ValueEnum;

/// The header displayed when running the tool
pub const HEADER: &str = "
╭─────────────────────────────────────────────────────────────────╮
│                            RAKOON                               │
│          Transaction Fuzzer for the Ethereum Protocol           │
│                                                                 │
│                        Author: nethoxa                          │
│                       Twitter: @nethoxa                         │
│                                                                 │
╰─────────────────────────────────────────────────────────────────╯
";

pub const RED: &str = "\x1b[31m";
pub const GREEN: &str = "\x1b[32m";
pub const RESET: &str = "\x1b[0m";

/// Bob's private key constant, used for signing auths in EIP-7702 transactions
pub const AUTH_PRIVATE_KEY: &str =
    "0x8c04e41e317a7cf0cf4c2f7431d0a890a950f352df41ff6d053698df61a73bba";

#[derive(Clone, ValueEnum, Debug, Copy, PartialEq, Eq)]
/// Used as a CLI flag to select the transaction type to fuzz
pub enum TransactionType {
    Legacy,
    Al,
    Eip1559,
    Eip7702,
}
