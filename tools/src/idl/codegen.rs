use convert_case::{Casing as _, Case};
use miette::bail;
use proc_macro2::Literal;
use quote::{format_ident, quote};
use syn::Ident;

use super::{EnumBodyDef, EnumCaseDef, EnumDef, InterfaceDef, MethodDef, PrimType, TypeDef, ValueType};

pub fn format_code(ts: &proc_macro2::TokenStream) -> String {
    let Ok(ast) = syn::parse_file(&ts.to_string()) else {
        panic!("unable to format: {ts}")
    };
    prettyplease::unparse(&ast)
}

pub fn generate_server(
    interface: &InterfaceDef,
) -> miette::Result<proc_macro2::TokenStream> {
    let trait_name = interface.name.value().to_case(Case::Pascal);
    let trait_name = quote::format_ident!("{trait_name}");
    let methods = interface.methods.iter().map(|(name, method)| {
        generate_server_trait_method(name, method)
    }).collect::<miette::Result<Vec<_>>>()?;
    let types = interface.types.iter().map(|(name, td)| {
        match td {
            TypeDef::Enum(e) => generate_enum(name, e),
            TypeDef::Struct(_) => unimplemented!(),
        }
    }).collect::<miette::Result<Vec<_>>>()?;
    let (op_enum_name, op_enum) = generate_server_op_enum(interface)?;
    let dispatch_cases = interface.methods.iter().map(|(name, method)| {
        let case = format_ident!("{}", name.to_case(Case::Pascal));
        let body = generate_server_method_dispatch(name, method)?;
        Ok(quote! {
            #op_enum_name::#case => {
                #body
            }
        })
    }).collect::<miette::Result<Vec<_>>>()?;
    Ok(quote::quote! {
        trait #trait_name {
            #(#methods)*
        }

        #op_enum

        impl<'a, T> idyll_runtime::Server<#op_enum_name> for (core::marker::PhantomData<#op_enum_name>, &'a mut T)
            where T: #trait_name
        {
            fn dispatch_op(&mut self, op: #op_enum_name, msg: &RecvMessage<'_>) -> Result<(), ReplyFaultReason> {
                let Ok(msg_data) = &msg.data else {
                    return Err(ReplyFaultReason::BadMessageSize);
                };
                match op {
                    #(#dispatch_cases)*
                }
            }
        }

        #(#types)*
    })
}

pub fn generate_client(
    interface: &InterfaceDef,
) -> miette::Result<proc_macro2::TokenStream> {
    let struct_name = interface.name.value().to_case(Case::Pascal);
    let struct_name = quote::format_ident!("{}", struct_name);
    let methods = interface.methods.iter().map(|(name, method)| {
        generate_client_method(name, method)
    }).collect::<miette::Result<Vec<_>>>()?;
    let types = interface.types.iter().map(|(name, td)| {
        match td {
            TypeDef::Enum(e) => generate_enum(name, e),
            TypeDef::Struct(_) => unimplemented!(),
        }
    }).collect::<miette::Result<Vec<_>>>()?;
    Ok(quote::quote! {
        pub struct #struct_name(core::cell::Cell<userlib::TaskId>);

        impl #struct_name {
            #(#methods)*
        }

        impl From<userlib::TaskId> for #struct_name {
            fn from(tid: userlib::TaskId) -> Self {
                Self(tid.into())
            }
        }

        #(#types)*
    })
}

pub fn generate_client_method(
    name: &str,
    def: &MethodDef,
) -> miette::Result<proc_macro2::TokenStream> {
    let doc = def.doc.as_ref().map(|text| {
        let text = text.value();
        quote! { #[doc = #text] }
    });
    let args = def.args.iter().map(|(name, arg)| {
        let ty = generate_type(&arg.type_)?;
        let name = format_ident!("{name}");
        Ok(quote! { #name: #ty })
    }).collect::<miette::Result<Vec<_>>>()?;
    let argtypes = def.args.values().map(|arg| {
        generate_type(&arg.type_)
    }).collect::<miette::Result<Vec<_>>>()?;
    let argnames = def.args.keys().map(|name| {
        format_ident!("{name}")
    }).collect::<Vec<_>>();
    let return_type = def.result.as_ref().map(|t| generate_type(t.value()))
        .unwrap_or_else(|| Ok(quote! { () }))?;
    let operation = def.operation;
    let syscall = if def.auto_retry {
        // We'll use the userlib-provided common routine for this, to save
        // space. This means we can only produce user errors, and the user error
        // type need not be convertible from TaskDeath.
        quote! {
            userlib::send_with_retry_on_death(
                &self.0,
                #operation,
                &argbuffer,
                &mut retbuffer,
                &mut [],
            )
        }
    } else {
        if def.result.is_none() {
            bail!(
                "operation '{name}' can fail but returns ()",
            )
        }
        quote! {
            {
                let send_result = userlib::sys_send(
                    &self.0,
                    #operation,
                    &argbuffer,
                    &mut retbuffer,
                    &mut [],
                );
                match send_result {
                    Ok(rc_and_len) => rc_and_len,
                    Err(dead) => {
                        self.0.set(
                            self.0.get().with_generation(dead.new_generation())
                        );
                        return <#return_type>::from(dead);
                    }
                }
            }
        }
    };
    let retparser = if let Some(rty) = &def.result {
        let rty = generate_type(rty.value())?;
        quote! {
            let _ = rc;
            let retbuffer = unsafe { retbuffer.get_unchecked(..len) };
            let Ok((retval, _)) = hubpack::deserialize::<#rty>(retbuffer) else {
                panic!();
            };
            retval
        }
    } else {
        // No parsing is required.
        quote! {
            let _ = (rc, len);
        }
    };
    let name = format_ident!("{name}");
    Ok(quote! {
        #doc
        pub fn #name(&self, #(#args,)*) {
            type ArgsType = (#(#argtypes,)*);
            let args: ArgsType = (#(#argnames,)*);
            let mut argbuffer = [0u8; <ArgsType as hubpack::SerializedSize>::MAX_SIZE];
            let mut retbuffer = [0u8; <#return_type as hubpack::SerializedSize>::MAX_SIZE];
            // Because all the types involved are defined in the file or are
            // from a set of core types, and we used hubpack's size estimate to
            // create the buffer, we never expect this to fail. If it does fail,
            // we will make up a length. This will likely cause the _server_ to
            // kill us due to a deserialization error. Think of it as a cheap
            // (or expensive, depending on your POV) panic.
            let n = hubpack::serialize(&mut argbuffer, &args).unwrap_or(argbuffer.len());
            let argbuffer = unsafe { argbuffer.get_unchecked_mut(..n) };
            let (rc, len) = #syscall;
            #retparser
        }
    })
}

pub fn generate_enum(
    name: &str,
    enum_case_def: &EnumDef,
) -> miette::Result<proc_macro2::TokenStream> {
    let cases = enum_case_def.cases.iter().map(|(name, def)| {
        generate_enum_case(name, def)
    }).collect::<miette::Result<Vec<_>>>()?;
    let name = format_ident!("{name}");
    Ok(quote! {
        #[derive(serde::Serialize, serde::Deserialize, hubpack::SerializedSize)]
        pub enum #name {
            #(#cases)*
        }
    })
}

pub fn generate_enum_case(
    name: &str,
    enum_case_def: &EnumCaseDef,
) -> miette::Result<proc_macro2::TokenStream> {
    let name = quote::format_ident!("{name}");
    let discriminant = enum_case_def.integer_value.map(|i| {
        let i = proc_macro2::Literal::i64_unsuffixed(i);
        quote::quote! { = #i }
    });
    let body = enum_case_def.body.as_ref().map(generate_enum_body)
        .transpose()?;
    Ok(quote::quote! {
        #name #body #discriminant,
    })
}

pub fn generate_enum_body(
    def: &EnumBodyDef,
) -> miette::Result<proc_macro2::TokenStream> {
    match def {
        EnumBodyDef::Tuple(types) => {
            let types = types.iter().map(generate_type)
                .collect::<miette::Result<Vec<_>>>()?;
            Ok(quote! { (#(#types,)*) })
        }
        EnumBodyDef::Struct(fields) => {
            let fields = fields.iter().map(|(name, fld)| {
                let ty = generate_type(&fld.type_)?;
                let name = format_ident!("{name}");
                Ok(quote! { #name: #ty, })
            }).collect::<miette::Result<Vec<_>>>()?;
            Ok(quote! { { #(#fields)* } })
        }
    }
}

pub fn generate_type(
    ty: &ValueType,
) -> miette::Result<proc_macro2::TokenStream> {
    match ty {
        ValueType::Prim(p) => match p {
            PrimType::U8 => Ok(quote! { u8 }),
            PrimType::U16 => Ok(quote! { u16 }),
            PrimType::U32 => Ok(quote! { u32 }),
            PrimType::U64 => Ok(quote! { u64 }),
            PrimType::U128 => Ok(quote! { u128 }),
            PrimType::I8 => Ok(quote! { i8 }),
            PrimType::I16 => Ok(quote! { i16 }),
            PrimType::I32 => Ok(quote! { i32 }),
            PrimType::I64 => Ok(quote! { i64 }),
            PrimType::I128 => Ok(quote! { i128 }),
            PrimType::F32 => Ok(quote! { f32 }),
            PrimType::F64 => Ok(quote! { f64 }),
            PrimType::Bool => Ok(quote! { bool }),
        },
        ValueType::Tuple(types) => {
            let parts = types.iter().map(generate_type).collect::<miette::Result<Vec<_>>>()?;
            Ok(quote! { (#(#parts,)*) })
        }
        ValueType::Array(ty, len) => {
            let ty = generate_type(ty)?;
            Ok(quote! { [#ty; #len] })
        }
        ValueType::Named { name, generics } => {
            let name = format_ident!("{name}");
            if !generics.is_empty() {
                let generics = generics.iter().map(generate_type).collect::<miette::Result<Vec<_>>>()?;
                Ok(quote! { #name<#(#generics,)*> })
            } else {
                Ok(quote! { #name })
            }
        }
    }
}

pub fn generate_server_trait_method(
    name: &str,
    def: &MethodDef,
) -> miette::Result<proc_macro2::TokenStream> {
    let doc = def.doc.as_ref().map(|text| {
        let text = text.value();
        quote! { #[doc = #text] }
    });
    let args = def.args.iter().map(|(name, arg)| {
        let ty = generate_type(&arg.type_)?;
        let name = format_ident!("{name}");
        Ok(quote! { #name: #ty })
    }).collect::<miette::Result<Vec<_>>>()?;
    let return_type = if let Some(rt) = &def.result {
        let p = generate_type(rt.value())?;
        quote! { Result<#p, userlib::ReplyFaultReason> }
    } else {
        quote! { Result<(), idyll_runtime::ReplyFaultReason> }
    };
    let name = format_ident!("{name}");
    Ok(quote! {
        #doc
        fn #name(&mut self, full_msg: &userlib::RecvMessage<'_>, #(#args,)*)
            -> #return_type;
    })
}

fn generate_server_op_enum(iface: &InterfaceDef) -> miette::Result<(Ident, proc_macro2::TokenStream)> {
    let enumname = format!("{}Operation", iface.name.value().to_case(Case::Pascal));
    let enumname = format_ident!("{enumname}");
    let cases = iface.methods.iter().map(|(name, def)| {
        let discrim = format_ident!("{}", name.to_case(Case::Pascal));
        let op = def.operation;
        quote! {
            #discrim = #op,
        }
    }).collect::<Vec<_>>();
    let match_cases = iface.methods.iter().map(|(name, def)| {
        let discrim = format_ident!("{}", name.to_case(Case::Pascal));
        let op = def.operation;
        quote! {
            #op => Ok(#enumname::#discrim),
        }
    }).collect::<Vec<_>>();
    Ok((
        enumname.clone(),
        quote! {
            #[derive(Copy, Clone, Debug)]
            #[repr(u16)]
            enum #enumname {
                #(#cases)*
            }

            impl TryFrom<u16> for #enumname {
                type Error = u16;
                fn try_from(x: u16) -> Result<Self, Self::Error> {
                    match x {
                        #(#match_cases)*
                        _ => Err(x),
                    }
                }
            }
        },
    ))
}
fn generate_server_method_dispatch(name: &str, def: &MethodDef) -> miette::Result<proc_macro2::TokenStream> {
    let method_name = format_ident!("{name}");
    let argtypes = def.args.values().map(|a| generate_type(&a.type_))
        .collect::<miette::Result<Vec<_>>>()?;

    let arg_expansion = (0..def.args.len()).map(|i| {
        let f = Literal::usize_unsuffixed(i);
        quote! { args.#f }
    }).collect::<Vec<_>>();

    let respond = if let Some(rty) = &def.result {
        let rty = generate_type(rty.value())?;
        quote! {
            let mut retbuf = [0u8; <#rty as hubpack::SerializedSize>::MAX_SIZE];
            let retlen = hubpack::serialize(&mut retbuf, &r).unwrap_or(0);
            let reply = unsafe { retbuf.get_unchecked(..retlen) };
            userlib::sys_reply(msg.sender, userlib::ResponseCode::SUCCESS, reply);
        }
    } else {
        quote! {
            let () = r;
            userlib::sys_reply(msg.sender, userlib::ResponseCode::SUCCESS, &[]);
        }
    };

    Ok(quote! {
        let args = hubpack::deserialize::<(#(#argtypes,)*)>(msg_data)
            .map_err(|_| ReplyFaultReason::BadMessageContents)?
            .0;
        let r = self.1.#method_name(msg, #(#arg_expansion),*)?;
        #respond
        Ok(())
    })
}

