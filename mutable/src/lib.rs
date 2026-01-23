use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Type};

fn is_option(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty &&
        let Some(segment) = type_path.path.segments.last()
    {
        return segment.ident == "Option";
    }
    false
}

/// Derives the `Mutable` trait for a given struct.
#[proc_macro_derive(Mutable)]
pub fn mutable_derive(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let id = &input.ident;

    let Data::Struct(DataStruct { fields, .. }) = &input.data else {
        panic!("Mutable can only be derived for structs");
    };

    let field_count = fields.len();
    let match_arms = fields.iter().enumerate().map(|(i, field)| {
        let field_type = &field.ty;

        let field_access = match &field.ident {
            Some(name) => quote! { self.#name },
            None => {
                let index = syn::Index::from(i);
                quote! { self.#index }
            }
        };

        if is_option(field_type) {
            quote! {
                #i => match &mut #field_access {
                    Some(value) => value.mutate(random),
                    None =>random.random_bool(0.25),
                },
            }
        } else {
            quote! {
                #i => #field_access.mutate(random),
            }
        }
    });

    quote! {
        #[automatically_derived]
        impl Mutable for #id {
            fn mutate(&mut self, random: &mut impl Rng) -> bool {
                match random.random_range(0..#field_count) {
                    #(#match_arms)*
                    _ => unreachable!(),
                }
            }
        }
    }
    .into()
}
