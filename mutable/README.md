# Mutable

This folder holds the code for a macro that recursively implements the `random` method for its fields. In short, creates random instances of said struct, without the boilerplate of implementing all the logic by hand.

## Getting started

1. Clone the `fuzztools` repository:

```sh
git clone git@github.com:protocol-security/fuzztools.git
cd fuzztools
```

2. Create a new binary crate within the workspace and add `fuzztools` as a dependency:

```sh
cargo new --bin <NAME>
cargo add fuzztools --package <NAME>
cd <NAME>
```

3. Create your struct and `#[derive(Mutable)]` on top and you are ready to go:

```rs
#[derive(Mutable)]
struct Payload {
    a: u64,
    b: u64
}

fn main() {
    let mut base = Payload {
        a: 3,
        b: 5
    };
    let mut random = SmallRng::seed_from_u64(0);

    loop {
        base.mutate(&mut random);

        // Check your target condition or send the payload
        if base.a + base.b == 5 {
            panic!("POC");
        }
    }
}
```