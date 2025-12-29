#[cfg(test)]
mod tests {
    use crate::circuits::{
        ast::{
            forest::Forest,
            nodes::{Node, NodeKind},
            types::{StringType, Struct, StructField, Type, Visibility},
        },
        context::Context,
        generators::types::TypeLocation,
        rewriter::Rewriter,
        scope::{Scope, Variable},
    };
    use rand::Rng;
    use std::{fs, process::Command};

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
            panic!("nargo new failed:\n{}", String::from_utf8_lossy(&output.stderr));
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

        let mut out = String::new();

        for s in scope.structs.iter() {
            out.push_str(&format!("{}", s));
        }

        out.push_str("fn main() {\n");

        for i in 0..50 {
            let ty = Type::random(random, &ctx, &scope, TypeLocation::Default);
            out.push_str(&format!("    let v{}: {} = {};\n", i, ty, ty.random_value(random, &ctx)));
        }

        out.push_str("}\n");

        compile_noir_code(&out, "test_default_type_location");
    }

    #[test]
    fn test_nested_type_location() {
        let random = &mut rand::rng();
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let scope = create_scope(random, &ctx);

        let mut out = String::new();

        for s in scope.structs.iter() {
            out.push_str(&format!("{}", s));
        }

        out.push_str("fn main() {\n");

        for i in 0..50 {
            let ty = Type::random(random, &ctx, &scope, TypeLocation::Nested);
            out.push_str(&format!(
                "    let v{}: [{}; 2] = [{}, {}];\n",
                i,
                ty,
                ty.random_value(random, &ctx),
                ty.random_value(random, &ctx)
            ));
        }

        out.push_str("}\n");

        compile_noir_code(&out, "test_nested_type_location");
    }

    #[test]
    fn test_tuple_nested_type_location() {
        let random = &mut rand::rng();
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let scope = create_scope(random, &ctx);

        let mut out = String::new();

        for s in scope.structs.iter() {
            out.push_str(&format!("{}", s));
        }

        out.push_str("fn main() {\n");

        for i in 0..50 {
            let ty = Type::random(random, &ctx, &scope, TypeLocation::TupleNested);
            out.push_str(&format!(
                "    let v{}: ({}, {}) = ({}, {});\n",
                i,
                ty,
                ty,
                ty.random_value(random, &ctx),
                ty.random_value(random, &ctx)
            ));
        }

        out.push_str("}\n");

        compile_noir_code(&out, "test_tuple_nested_type_location");
    }

    #[test]
    fn test_main_type_location() {
        let random = &mut rand::rng();
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let scope = create_scope(random, &ctx);

        let mut out = String::new();

        for s in scope.structs.iter() {
            out.push_str(&format!("{}", s));
        }

        let inputs = (0..50)
            .map(|i| {
                let ty = Type::random(random, &ctx, &scope, TypeLocation::Main);
                format!("p{}: {}", i, ty)
            })
            .collect::<Vec<String>>()
            .join(", ");

        out.push_str(&format!("fn main({}) {{}}", inputs));

        compile_noir_code(&out, "test_main_type_location");
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

        let mut out = String::new();

        for s in scope.structs.iter() {
            out.push_str(&format!("{}", s));
        }

        let inputs = (0..50)
            .map(|i| {
                let ty = Type::random(random, &ctx, &scope, TypeLocation::Main);
                format!("p{}: {}", i, ty)
            })
            .collect::<Vec<String>>()
            .join(", ");

        out.push_str(&format!("fn main({}) {{}}", inputs));

        compile_noir_code(&out, "test_main_type_location_with_single_invalid_struct");
    }

    #[test]
    fn test_random_forest() {
        let random = &mut rand::rng();
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let scope = create_scope(random, &ctx);

        let mut forest = Forest::default();
        forest.random(random, &ctx, &scope);
        forest.save_as_dot(&std::env::current_dir().unwrap().join("test_random_forest.dot"));
    }

    #[test]
    fn test_forest_with_inputs_and_subforest() {
        let random = &mut rand::rng();
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let scope = create_scope(random, &ctx);

        // 1. Create input nodes
        let input_count = 5;
        let inputs: Vec<(String, Type)> = (0..input_count)
            .map(|i| {
                let ty = Type::random(random, &ctx, &scope, TypeLocation::Main);
                (format!("input_{}", i), ty)
            })
            .collect();

        // 2. Create parent forest with those inputs
        let mut parent_forest = Forest::default();

        // Add inputs to the forest and register them
        for (name, ty) in &inputs {
            let idx = parent_forest.input(name.clone(), ty.clone());
            parent_forest.register(idx, NodeKind::Input, ty, None);
        }

        // Generate random expressions on top of inputs
        parent_forest.random(random, &ctx, &scope);

        // 3. Collect all variables defined in parent forest into a scope
        let parent_variables: Vec<Variable> = parent_forest
            .nodes
            .get(&NodeKind::Variable)
            .map(|vars| {
                vars.iter()
                    .filter_map(|&idx| match &parent_forest.graph[idx] {
                        Node::Variable { name, ty, .. } => {
                            Some(Variable { name: name.clone(), ty: ty.clone(), mutable: false })
                        }
                        _ => None,
                    })
                    .collect()
            })
            .unwrap_or_default();

        // 4. Create sub-forest with: original inputs + parent's variables as inputs
        let mut sub_forest = Forest::default();
        sub_forest.var_counter = parent_forest.var_counter; // Continue variable naming

        // Add original inputs
        for (name, ty) in &inputs {
            let idx = sub_forest.input(name.clone(), ty.clone());
            sub_forest.register(idx, NodeKind::Input, ty, None);
        }

        // Add parent's variables as inputs to sub-forest
        for var in &parent_variables {
            let idx = sub_forest.input(var.name.clone(), var.ty.clone());
            sub_forest.register(idx, NodeKind::Input, &var.ty, None);
        }

        // Generate sub-forest expressions
        sub_forest.random(random, &ctx, &scope);

        // Save both for inspection
        parent_forest.save_as_dot(&std::env::current_dir().unwrap().join("test_parent_forest.dot"));
        sub_forest.save_as_dot(&std::env::current_dir().unwrap().join("test_sub_forest.dot"));

        // Assertions
        assert!(parent_forest.nodes.get(&NodeKind::Input).map_or(0, |v| v.len()) == input_count);

        let sub_input_count = sub_forest.nodes.get(&NodeKind::Input).map_or(0, |v| v.len());
        assert_eq!(sub_input_count, input_count + parent_variables.len());

        println!("Parent forest: {} inputs, {} variables", input_count, parent_variables.len());
        println!("Sub forest: {} inputs (original + inherited)", sub_input_count);
    }

    #[test]
    fn test_rewriter_before_after() {
        let random = &mut rand::rng();
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let scope = create_scope(random, &ctx);

        // Create and populate a forest
        let mut forest = Forest::default();
        forest.random(random, &ctx, &scope);

        // Clone it before applying any rule
        let before = forest.clone();

        // Apply one random equivalence rule
        let rewriter = Rewriter::new();
        rewriter.apply_random(random, &mut forest);

        // Save both for comparison
        let base = std::env::current_dir().unwrap();
        before.save_as_dot(&base.join("test_rewriter_before.dot"));
        forest.save_as_dot(&base.join("test_rewriter_after.dot"));

        println!("Before: {} nodes", before.graph.node_count());
        println!("After:  {} nodes", forest.graph.node_count());
    }

    #[test]
    fn test_formatter() {
        let random = &mut rand::rng();
        let ctx: Context =
            serde_json::from_str(&fs::read_to_string("../configs/noiruzz.json").unwrap()).unwrap();
        let scope = create_scope(random, &ctx);
        let mut out = String::new();

        for s in scope.structs.iter() {
            out.push_str(&format!("{}", s));
        }

        out.push_str("fn main() {\n");

        let mut forest = Forest::default();
        forest.random(random, &ctx, &scope);

        out.push_str(&format!("{}", forest));
        out.push_str("}\n");

        compile_noir_code(&out, "test_formatter");

        forest.save_as_dot(&std::env::current_dir().unwrap().join("test_formatter.dot"));

        println!("{}", out);
    }
}
