use std::fmt::Display;
// @audit todo this all type_d on the string from the types
#[derive(Debug, Clone)]
pub(crate) enum NoirType {
    Field(Field),
    Integer(Integer), // @audit signed and unsigned, error on overflow
    Boolean(Boolean),
    String(String),
    Array(Array),
    Slice(Slice),
    Tuple(Tuple),
    Struct(Struct),
    Reference(Reference),
    //Function(Function), // @audit todo
}

impl Display for NoirType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NoirType::Field(field) => write!(f, "{}", field),
            NoirType::Integer(integer) => write!(f, "{}", integer),
            NoirType::Boolean(boolean) => write!(f, "{}", boolean),
            NoirType::String(string) => write!(f, "{}", string),
            NoirType::Array(array) => write!(f, "{}", array),
            NoirType::Slice(slice) => write!(f, "{}", slice),
            NoirType::Tuple(tuple) => write!(f, "{}", tuple),
            NoirType::Struct(struct_) => write!(f, "{}", struct_),
            NoirType::Reference(reference) => write!(f, "{}", reference),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Field {}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Field")
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Integer {
    signed: bool,
    size: u64,
}

impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.signed {
            write!(f, "i{}", self.size)
        } else {
            write!(f, "u{}", self.size)
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Boolean {}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bool")
    }
}

#[derive(Debug, Clone)]
pub(crate) struct String {
    size: u64,
}

// @audit what about scape hatchs \r, \n, \t, \0, \", \\ and raw strings r"...", r#"...", r######"..."###### as well as f"..." writing { with {{"
impl Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "str<{}>", self.size)
    }
}

// @audit you can call function in getters like
/*
let _ = array[f(x)];

fn f(x: u32) -> u32 {
    x * 2
}
*/
#[derive(Debug, Clone)]
pub(crate) struct Array {
    type_: Box<NoirType>,
    size: u64,
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}; {}]", self.type_, self.size)
    }
}

// @audit must be initialized as = &[1, 2, 3, ...] or &[0; 3]
#[derive(Debug, Clone)]
pub(crate) struct Slice {
    type_: Box<NoirType>,
}

impl Display for Slice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.type_)
    }
}

// @audit accedemos a los fields con .0, .1, .2...
#[derive(Debug, Clone)]
pub(crate) struct Tuple {
    types: Vec<Box<NoirType>>,
}

impl Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, type_) in self.types.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", type_)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone)]
pub(crate) struct StructField {
    name: String,
    type_: Box<NoirType>,
    visibility: Visibility,
}

#[derive(Debug, Clone)]
pub(crate) enum Visibility {
    Public,
    Crate,
    Private,
}

// @audit access fields via struct.field_name
#[derive(Debug, Clone)]
pub(crate) struct Struct {
    name: String,
    fields: Vec<Box<StructField>>,
}

impl Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Reference {
    type_: Box<NoirType>,
    mutable: bool,
}

impl Display for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.mutable {
            write!(f, "&mut {}", self.type_)
        } else {
            write!(f, "&{}", self.type_)
        }
    }
}