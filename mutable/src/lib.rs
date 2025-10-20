extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Type};

/// Check if a type is `Option<T>`
fn is_option(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            return segment.ident == "Option";
        }
    }
    false
}

#[proc_macro_derive(Mutable)]
pub fn mutable_derive(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let id = &input.ident;

    match &input.data {
        Data::Struct(DataStruct { fields, .. }) => {
            let field_count = fields.len();
            let match_arms = fields.iter().enumerate().map(|(i, field)| {
                let field_type = &field.ty;

                // Handle both named fields and tuple struct fields
                let field_access = if let Some(field_name) = &field.ident {
                    // Named field: self.field_name
                    quote! { self.#field_name }
                } else {
                    // Tuple struct field: self.0, self.1, etc.
                    let index = syn::Index::from(i);
                    quote! { self.#index }
                };

                if is_option(field_type) {
                    quote! {
                        #i => {
                            if let Some(value) = #field_access.as_mut() {
                                value.mutate(random);
                            } else if random.random_bool(0.25) {
                                #field_access = Some(Default::default());
                            }
                        }
                    }
                } else {
                    quote! {
                        #i => {
                            #field_access.mutate(random);
                        }
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

                        return false;
                    }
                }
            }
        },
        _ => panic!("Mutable can only be derived for structs"),
    }
    .into()
}
