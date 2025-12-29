#[cfg(test)]
mod tests {
    use std::fs;
    use std::process::Command;
    use rand::Rng;
    use crate::circuits::{ast::{scope::Scope, types::{StringType, Struct, StructField, Type, Visibility}}, context::Context, generators::types::TypeLocation};

    fn create_scope(random: &mut impl Rng, ctx: &Context) -> Scope {
        let mut scope = Scope::new();
        let mut structs: Vec<Struct> = Vec::new();
        for i in 0..5 {
            let s = Struct::random(random, &ctx, &scope, format!("s{}", i));
            structs.push(s);
        }
        scope.structs = structs;
        scope
    }

    fn compile_noir_code(code: &str, test_name: &str) {
        let tmp_dir = std::env::current_dir().unwrap().join(format!("../tmp"));

        if tmp_dir.join(test_name).exists() {
            fs::remove_dir_all(&tmp_dir.join(test_name)).unwrap();
        }

        // Create new nargo project in the current directory
        let output = Command::new("nargo")
            .args(["new", test_name])
            .current_dir(&tmp_dir)
            .output()
            .expect("Failed to execute nargo new");

        if !output.status.success() {
            panic!(
                "nargo new failed:\n{}",
                String::from_utf8_lossy(&output.stderr)
            );
        }

        // Write code to src/main.nr
        let main_nr = tmp_dir.join(format!("{}/src/main.nr", test_name));
        fs::write(&main_nr, code).unwrap();

        // Compile
        let output = Command::new("nargo")
            .args(["compile"])
            .current_dir(&tmp_dir.join(test_name))
            .output()
            .expect("Failed to execute nargo compile");

        if !output.status.success() {
            panic!(
                "nargo compile failed:\n{}{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
        }

        // Clean up on success
        fs::remove_dir_all(&tmp_dir.join(test_name)).unwrap();
    }

    #[test]
    fn test_default_type_location() {
        let random = &mut rand::rng();
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let scope = create_scope(random, &ctx);

        let mut code = String::new();

        for s in scope.structs.iter() {
            code.push_str(&format!("{}", s));
        }

        code.push_str("fn main() {\n");

        for i in 0..50 {
            let ty = Type::random(random, &ctx, &scope, TypeLocation::Default);
            code.push_str(&format!("    let v{}: {} = {};\n", i, ty, ty.random_value(random, &ctx)));
        }

        code.push_str("}\n");

        compile_noir_code(&code, "test_default_type_location");
    }

    #[test]
    fn test_nested_type_location() {
        let random = &mut rand::rng();
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let scope = create_scope(random, &ctx);

        let mut code = String::new();

        for s in scope.structs.iter() {
            code.push_str(&format!("{}", s));
        }

        code.push_str("fn main() {\n");

        for i in 0..50 {
            let ty = Type::random(random, &ctx, &scope, TypeLocation::Nested);
            code.push_str(&format!("    let v{}: [{}; 2] = [{}, {}];\n", i, ty, ty.random_value(random, &ctx), ty.random_value(random, &ctx)));
        }

        code.push_str("}\n");

        compile_noir_code(&code, "test_nested_type_location");
    }

    #[test]
    fn test_tuple_nested_type_location() {
        let random = &mut rand::rng();
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let scope = create_scope(random, &ctx);

        let mut code = String::new();

        for s in scope.structs.iter() {
            code.push_str(&format!("{}", s));
        }

        code.push_str("fn main() {\n");

        for i in 0..50 {
            let ty = Type::random(random, &ctx, &scope, TypeLocation::TupleNested);
            code.push_str(&format!("    let v{}: ({}, {}) = ({}, {});\n", i, ty, ty, ty.random_value(random, &ctx), ty.random_value(random, &ctx)));
        }

        code.push_str("}\n");

        compile_noir_code(&code, "test_tuple_nested_type_location");
    }

    #[test]
    fn test_main_type_location() {
        let random = &mut rand::rng();
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let scope = create_scope(random, &ctx);

        let mut code = String::new();

        for s in scope.structs.iter() {
            code.push_str(&format!("{}", s));
        }

        let inputs = (0..50).map(|i| {
            let ty = Type::random(random, &ctx, &scope, TypeLocation::Main);
            format!("p{}: {}", i, ty)
        }).collect::<Vec<String>>().join(", ");

        code.push_str(&format!("fn main({}) {{}}", inputs));

        compile_noir_code(&code, "test_main_type_location");
    }

    #[test]
    fn test_main_type_location_with_single_invalid_struct() {
        let random = &mut rand::rng();
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();

        let s = Struct {
            name: "s0".to_string(),
            fields: vec![StructField {
                name: "field0".to_string(),
                ty: Box::new(Type::String(StringType { size: 0, is_raw: false })),
                visibility: Visibility::Public,
            }],
        };
        let mut scope = Scope::new();
        scope.structs = vec![s];

        let mut code = String::new();

        for s in scope.structs.iter() {
            code.push_str(&format!("{}", s));
        }

        let inputs = (0..50).map(|i| {
            let ty = Type::random(random, &ctx, &scope, TypeLocation::Main);
            format!("p{}: {}", i, ty)
        }).collect::<Vec<String>>().join(", ");

        code.push_str(&format!("fn main({}) {{}}", inputs));

        compile_noir_code(&code, "test_main_type_location_with_single_invalid_struct");
    }
}
