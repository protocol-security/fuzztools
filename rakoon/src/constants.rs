pub(crate) const RED: &str = "\x1b[31m";
pub(crate) const GREEN: &str = "\x1b[32m";
pub(crate) const RESET: &str = "\x1b[0m";
pub(crate) const CLEAR_SCREEN: &str = "\x1b[2J\x1b[H";

/// The header displayed when running this fuzzer.
pub(crate) const HEADER: &str = "
╭─────────────────────────────────────────────────────────────────╮
│                            RAKOON                               │
│          Transaction Fuzzer for the Ethereum Protocol           │
│                                                                 │
│                        Author: nethoxa                          │
│                       Twitter: @nethoxa                         │
│                                                                 │
╰─────────────────────────────────────────────────────────────────╯
";

/// Bob's private key constant, used for signing auths in EIP-7702 transactions.
pub(crate) const AUTH_PRIVATE_KEY: &str =
    "0x8c04e41e317a7cf0cf4c2f7431d0a890a950f352df41ff6d053698df61a73bba";
