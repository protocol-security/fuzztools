# Fuzzers

This repo holds my fuzzing toolkit of stuff im using along my fuzzers. If you want to add another one, you can reuse stuff inside `fuzztools` as follows:

1. First clone this repo

```sh
git clone https://github.com/nethoxa/fuzzers.git
cd fuzzers
```

2. Create a new cargo project

```sh
cargo new --bin <NAME>
cargo add fuzztools --package <NAME>
cd <NAME>
```

3. Now, you only need to add under the `main.rs` the usual `loop` and put the code of your fuzzer in there. For example, if we were to fuzz the next `struct`

```rs
struct Payload {
    a: u64,
    b: u64
}

fn main() {
    let mut base = Payload {
        a: 3,
        b: 5
    };

    loop {
        if StdRng::random_bool(0.5) {
            base.a.mutate();
        } else {
            base.b.mutate();
        }

        if base.a + base.b == 5 {
            panic!("POC");
        }
    }
}
``` 

You only need to implement the % where you call mutate on each and either check the needed condition or send a payload. For example, check the implementation of [rakoon](./rakoon/), where it makes use of it extensively. If you wanna load the Cargo documentation, run

```sh
cargo doc --no-deps --workspace --open 
```