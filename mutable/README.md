# Mutable

This package contains a macro that implements automatically the `Mutable` trait for a given struct, if and only if the inner fields do implement it. That way, it is as easy as doing `#[derive(Mutable)]` to be able to do structure-aware fuzzing (see [rakoon](../rakoon/README.md) as an example).

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