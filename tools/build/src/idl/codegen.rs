use convert_case::{Casing as _, Case};
use miette::bail;
use proc_macro2::Literal;
use quote::{format_ident, quote};
use syn::Ident;

use super::{EnumBodyDef, EnumCaseDef, EnumDef, InterfaceDef, MethodDef, PrimType, StructDef, TypeDef, ValueType};

pub fn format_code(ts: &proc_macro2::TokenStream) -> String {
    let ast = match syn::parse_file(&ts.to_string()) {
        Ok(ast) => ast,
        Err(e) => panic!("unable to format ({e}): {ts}"),
    };
    prettyplease::unparse(&ast)
}

pub fn generate_server(
    interface: &InterfaceDef,
) -> miette::Result<proc_macro2::TokenStream> {
    let trait_name = interface.name.value().to_case(Case::Pascal);
    let trait_name = quote::format_ident!("{trait_name}");

    let const_name = interface.name.value().to_case(Case::ScreamingSnake);
    let const_name = quote::format_ident!("{const_name}_BUFFER_SIZE");

    let methods = interface.methods.iter().map(|(name, method)| {
        generate_server_trait_method(name, method)
    }).collect::<miette::Result<Vec<_>>>()?;
    let types = interface.types.iter().map(|(name, td)| {
        match td {
            TypeDef::Enum(e) => generate_enum(name, e),
            TypeDef::Struct(s) => generate_struct(name, s),
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

        const #const_name: usize = <#op_enum_name as idyll_runtime::ServerOp>::INCOMING_SIZE;

        impl<'a, T> idyll_runtime::Server<#op_enum_name> for (core::marker::PhantomData<#op_enum_name>, &'a mut T)
            where T: #trait_name
        {
            fn dispatch_op(&mut self, op: #op_enum_name, msg: &userlib::Message<'_>) -> Result<(), userlib::ReplyFaultReason> {
                let Ok(msg_data) = &msg.data else {
                    return Err(userlib::ReplyFaultReason::BadMessageSize);
                };
                let meta = idyll_runtime::Meta {
                    sender: msg.sender,
                    lease_count: msg.lease_count,
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
            TypeDef::Struct(s) => generate_struct(name, s),
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

    let lease_args = {
        let mut lease_args = vec![];
        for (lease_name, lease_def) in &def.leases {
            let lease_name = format_ident!("{lease_name}");
            let lease_type = generate_type(&lease_def.type_)?;
            let lease_type = if lease_def.write {
                quote! { &mut [#lease_type] }
            } else {
                quote! { & [#lease_type] }
            };
            lease_args.push(quote! {
                #lease_name: #lease_type
            });
        }
        lease_args
    };
    let args_and_leases = args.iter().chain(&lease_args)
        .collect::<Vec<_>>();

    let return_type = def.result.as_ref().map(|t| generate_type(t.value()))
        .unwrap_or_else(|| Ok(quote! { () }))?;
    let operation = *def.operation.value();
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
                &mut leases,
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
                    self.0.get(),
                    #operation,
                    &argbuffer,
                    &mut retbuffer,
                    &mut leases,
                );
                match send_result {
                    Ok(rc_and_len) => rc_and_len,
                    Err(dead) => {
                        self.0.set(
                            self.0.get().with_generation(dead.new_generation())
                        );
                        return <#return_type as idyll_runtime::FromTaskDeath>::from_task_death(dead);
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

    let leases = def.leases.iter().map(|(lease_name, lease_def)| {
        let method_name = match (lease_def.read, lease_def.write) {
            (false, false) => format_ident!("no_access"),
            (true, false) => format_ident!("read_only"),
            (true, true) => format_ident!("write_only"),
            (false, true) => format_ident!("read_write"),
        };
        let as_bytes = if lease_def.write {
            quote! { as_bytes_mut }
        } else {
            quote! { as_bytes }
        };
        let lease_arg_name = format_ident!("{lease_name}");
        quote! {
            userlib::Lease::#method_name(
                zerocopy::IntoBytes::#as_bytes(#lease_arg_name)
            )
        }
    }).collect::<Vec<_>>();

    let args_tuple = quote! { (#(#argtypes,)*) };

    let name = format_ident!("{name}");
    Ok(quote! {
        #doc
        pub fn #name(&self, #(#args_and_leases,)*) -> #return_type {
            let args: #args_tuple = (#(#argnames,)*);
            let mut argbuffer = [0u8; <#args_tuple as hubpack::SerializedSize>::MAX_SIZE];
            let mut retbuffer = [0u8; <#return_type as hubpack::SerializedSize>::MAX_SIZE];

            let mut leases = [
                #(#leases),*
            ];

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
    def: &EnumDef,
) -> miette::Result<proc_macro2::TokenStream> {
    let cases = def.cases.iter().map(|(name, def)| {
        generate_enum_case(name, def)
    }).collect::<miette::Result<Vec<_>>>()?;
    let name = format_ident!("{name}");
    let from_impl = if let Some(death_case) = &def.task_death_case {
        let death = format_ident!("{death_case}");
        Some(quote! {
            impl idyll_runtime::FromTaskDeath for #name {
                fn from_task_death(_: userlib::TaskDeath) -> Self {
                    #name::#death
                }
            }
        })
    } else {
        None
    };
    let extra_derives = def.rust_derive.iter().map(|trt| {
        format_ident!("{trt}")
    }).collect::<Vec<_>>();
    Ok(quote! {
        #[derive(serde::Serialize, serde::Deserialize, hubpack::SerializedSize #(,#extra_derives)*)]
        pub enum #name {
            #(#cases,)*
        }
        #from_impl
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
        #name #body #discriminant
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

pub fn generate_struct(
    name: &str,
    def: &StructDef,
) -> miette::Result<proc_macro2::TokenStream> {
    let fields = def.fields.iter().map(|(name, fld)| {
        let ty = generate_type(&fld.type_)?;
        let name = format_ident!("{name}");
        Ok(quote! { pub #name: #ty })
    }).collect::<miette::Result<Vec<_>>>()?;
    let extra_derives = def.rust_derive.iter().map(|trt| {
        format_ident!("{trt}")
    }).collect::<Vec<_>>();
    let name = format_ident!("{name}");
    Ok(quote! {
        #[derive(serde::Serialize, serde::Deserialize, hubpack::SerializedSize #(,#extra_derives)*)]
        pub struct #name {
            #(#fields,)*
        }
    })
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
    let mut args = def.args.iter().map(|(name, arg)| {
        let ty = generate_type(&arg.type_)?;
        let name = format_ident!("{name}");
        Ok(quote! { #name: #ty })
    }).collect::<miette::Result<Vec<_>>>()?;
    for (lease_name, lease_def) in &def.leases {
        let lease_name = format_ident!("{lease_name}");
        let ty = generate_type(&lease_def.type_)?;
        let att = match (lease_def.read, lease_def.write) {
            (false, false) => {
                // ummmm
                quote! { () }
            }
            (true, false) => quote! { idyll_runtime::Read },
            (false, true) => quote! { idyll_runtime::Write },
            (true, true) => quote! { idyll_runtime::ReadWrite },
        };
        args.push(quote! {
            #lease_name: idyll_runtime::Leased<#att, #ty>
        });
    }
    let return_type = if let Some(rt) = &def.result {
        let p = generate_type(rt.value())?;
        quote! { Result<#p, userlib::ReplyFaultReason> }
    } else {
        quote! { Result<(), idyll_runtime::ReplyFaultReason> }
    };
    let name = format_ident!("{name}");
    Ok(quote! {
        #doc
        fn #name(&mut self, meta: idyll_runtime::Meta, #(#args,)*)
            -> #return_type;
    })
}

fn generate_server_op_enum(iface: &InterfaceDef) -> miette::Result<(Ident, proc_macro2::TokenStream)> {
    let enumname = format!("{}Operation", iface.name.value().to_case(Case::Pascal));
    let enumname = format_ident!("{enumname}");
    let cases = iface.methods.iter().map(|(name, def)| {
        let discrim = format_ident!("{}", name.to_case(Case::Pascal));
        let op = *def.operation.value();
        quote! {
            #discrim = #op,
        }
    }).collect::<Vec<_>>();
    let match_cases = iface.methods.iter().map(|(name, def)| {
        let discrim = format_ident!("{}", name.to_case(Case::Pascal));
        let op = *def.operation.value();
        quote! {
            #op => Ok(#enumname::#discrim),
        }
    }).collect::<Vec<_>>();
    let incoming_sizes = iface.methods.values().map(|def| {
        let args = def.args.values().map(|arg| {
            let ty = generate_type(&arg.type_)?;
            Ok(quote::quote! {
                <#ty as hubpack::SerializedSize>::MAX_SIZE
            })
        }).collect::<miette::Result<Vec<_>>>()?;
        Ok(quote::quote! {
            #(#args)+*
        })
    }).collect::<miette::Result<Vec<_>>>()?;
    let reply_cases = iface.methods.iter().map(|(name, def)| {
        let discrim = format_ident!("{}", name.to_case(Case::Pascal));
        let ty = if let Some(rt) = &def.result {
            generate_type(rt.value())?
        } else {
            quote! { () }
        };
        Ok(quote::quote! {
            #enumname::#discrim => <#ty as hubpack::SerializedSize>::MAX_SIZE
        })
    }).collect::<miette::Result<Vec<_>>>()?;

    Ok((
        enumname.clone(),
        quote! {
            #[derive(Copy, Clone, Debug)]
            #[repr(u16)]
            enum #enumname {
                #(#cases)*
            }

            impl idyll_runtime::ServerOp for #enumname {
                const INCOMING_SIZE: usize = idyll_runtime::const_max(&[
                    #(#incoming_sizes),*
                ]);
                fn required_reply_space(&self) -> usize {
                    match self {
                        #(#reply_cases,)*
                    }
                }
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

    let mut arg_expansion = (0..def.args.len()).map(|i| {
        let f = Literal::usize_unsuffixed(i);
        quote! { args.#f }
    }).collect::<Vec<_>>();
    for i in 0..def.leases.len() {
        let n = Literal::usize_unsuffixed(i);
        arg_expansion.push(quote! { leases.#n });
    }

    let leases = def.leases.values().enumerate().map(|(i, lease)| {
        let ty = generate_type(&lease.type_)?;
        let att = match (lease.read, lease.write) {
            (false, false) => {
                // ummmm
                quote! { () }
            }
            (true, false) => quote! { idyll_runtime::Read },
            (false, true) => quote! { idyll_runtime::Write },
            (true, true) => quote! { idyll_runtime::ReadWrite },
        };
        Ok(quote! {
            idyll_runtime::Leased::<#att, #ty>::new(msg.sender, #i)
                .ok_or(userlib::ReplyFaultReason::BadLeases)?
        })
    }).collect::<miette::Result<Vec<_>>>()?;

    let leases = if leases.is_empty() {
        quote! {}
    } else {
        quote! { let leases = (#(#leases,)*); }
    };

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

    let deserialize_args = if argtypes.is_empty() {
        None
    } else {
        Some(quote! {
            let args = hubpack::deserialize::<(#(#argtypes,)*)>(msg_data)
                .map_err(|_| userlib::ReplyFaultReason::BadMessageContents)?
                .0;
        })
    };

    Ok(quote! {
        #deserialize_args
        #leases
        let r = self.1.#method_name(meta, #(#arg_expansion),*)?;
        #respond
        Ok(())
    })
}
