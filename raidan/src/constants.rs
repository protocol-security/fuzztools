use clap::ValueEnum;

/// The header displayed when running the tool
pub(crate) const HEADER: &str = "
╭─────────────────────────────────────────────────────────────────╮
│                            RAIDAN                               │
│         State Transition Fuzzer for the eth2 Protocol           │
│                                                                 │
│                Authors: nethoxa and 0xMushow                    │
│               Twitter: @nethoxa and @0xMushow                   │
│                                                                 │
╰─────────────────────────────────────────────────────────────────╯
";

pub(crate) const RED: &str = "\x1b[31m";
pub(crate) const GREEN: &str = "\x1b[32m";
pub(crate) const RESET: &str = "\x1b[0m";

#[derive(Clone, ValueEnum, Debug, Copy, PartialEq, Eq)]
pub(crate) enum Method {
    ProcessSlot,
    ProcessEpoch,
}
