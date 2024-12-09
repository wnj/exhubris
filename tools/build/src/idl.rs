pub mod codegen;

use std::{collections::{BTreeMap, BTreeSet}, path::Path, sync::Arc};

use indexmap::IndexMap;
use kdl::{KdlDocument, KdlNode};
use miette::{bail, miette, LabeledSpan, NamedSource, IntoDiagnostic as _, Context};

use crate::appcfg::{add_source, get_children_named, get_unique_bool, get_unique_i64_value, get_unique_optional_string_value, get_unique_string_value, get_uniquely_named_children, no_children, required_children, Spanned};

pub fn load_interface(
    path: impl AsRef<Path>,
) -> miette::Result<InterfaceDef> {
    let path = path.as_ref();
    let doc_src = std::fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("can't read {}", path.display()))?;
    let source = Arc::new(NamedSource::new(
            path.display().to_string(),
            doc_src.clone(),
    ));
    let doc: kdl::KdlDocument = crate::appcfg::add_source(&source, || {
        Ok(doc_src.parse()?)
    })?;
    parse_interface(&source, &doc)
}

pub fn parse_interface(
    source: &Arc<NamedSource>,
    doc: &KdlDocument,
) -> miette::Result<InterfaceDef> {
    add_source(source, || parse_interface_internal(doc))
}

fn parse_interface_internal(
    doc: &KdlDocument,
) -> miette::Result<InterfaceDef> {
    let first = doc.nodes().first().ok_or_else(|| {
        miette!("alleged interface doesn't contain any nodes")
    })?;
    if first.name().value() != "interface" {
        bail!(
            labels=[LabeledSpan::at(*first.name().span(), "expected 'interface' here")],
            "this may not be an interface, first node is not 'interface'"
        );
    }

    let mut def = InterfaceDef {
        name: get_unique_string_value(doc, "interface")?,
        methods: Default::default(),
        types: Default::default(),
    };

    for (name, node) in get_uniquely_named_children(doc, "enum")? {
        let e = parse_enum(required_children(node)?)?;
        def.types.insert(name, TypeDef::Enum(e));
    }

    for (name, node) in get_uniquely_named_children(doc, "struct")? {
        let e = parse_struct(required_children(node)?)?;
        def.types.insert(name, TypeDef::Struct(e));
    }

    for (name, node) in get_uniquely_named_children(doc, "method")? {
        let m = parse_method(required_children(node)?)?;
        def.methods.insert(name, m);
    }

    let mut used_operation_numbers = BTreeMap::new();
    for method in def.methods.values() {
        let o = *method.operation.value();
        if used_operation_numbers.contains_key(&o) {
            bail!(
                labels = [
                    LabeledSpan::at(used_operation_numbers[&o], "used here"),
                    LabeledSpan::at(method.operation.span(), "also used here"),
                ],
                "interface {} contains more than one method with \
                    operation number {o}",
                    def.name.value()
            );
        }
        used_operation_numbers.insert(o, method.operation.span());
    }

    Ok(def)
}

fn parse_enum(
    doc: &KdlDocument,
) -> miette::Result<EnumDef> {

    let mut def = EnumDef::default();
    for (name, node) in get_uniquely_named_children(doc, "case")? {
        let mut case = EnumCaseDef::default();

        if node.entries().len() == 1 {
            // cool
        } else if node.entries().len() == 2 {
            let val = &node.entries()[1];
            let Some(i) = val.value().as_i64() else {
                bail!(
                    labels = [LabeledSpan::at(*val.span(), "not an integer")],
                    "enum value should be an integer"
                )
            };
            case.integer_value = Some(i);
        } else {
            bail!(
                labels = [LabeledSpan::at(*node.entries()[2].span(), "unexpected")],
                "too many arguments for enum case"
            )
        }

        if let Some(children) = node.children() {
            case.doc = get_unique_optional_string_value(children, "doc")?;

            let is_tuple = get_unique_optional_string_value(children, "struct-style")?
                .map(|s| s.value() == "tuple")
                .unwrap_or(false);

            
            if is_tuple {
                // Tuple fields don't have names.
                let mut types = vec![];
                for field in get_children_named(children, "field")? {
                    if field.entries().len() == 1 && field.entries()[0].name().is_none() {
                        let e = &field.entries()[0];
                        if let Some(s) = e.value().as_string() {
                            types.push(parse_value_type(s)?);
                        } else {
                            bail!(
                                labels=[LabeledSpan::at(*e.span(), "not a string")],
                                "tuple field should have a string (type) argument"
                            );
                        }
                    } else {
                        bail!(
                            labels=[LabeledSpan::at(*field.span(), "not one argument")],
                            "wrong number of arguments for field in tuple struct"
                        );
                    }
                }
                case.body = Some(EnumBodyDef::Tuple(types));
            } else {
                // Expect field names.
                let mut fields = IndexMap::new();
                for (name, node) in get_uniquely_named_children(children, "field")? {
                    if node.entries().len() != 2 {
                        bail!(
                            labels=[LabeledSpan::at(*node.span(), "not two arguments")],
                            "wrong number of arguments for field in named struct"
                        );
                    }
                    let e_type = &node.entries()[1];
                    if let Some(s) = e_type.value().as_string() {
                        fields.insert(name, FieldDef {
                            type_: parse_value_type(s)?,
                        });
                    } else {
                        bail!(
                            labels=[LabeledSpan::at(*e_type.span(), "not a string")],
                            "struct field should have a string (type) argument"
                        );
                    }

                }
                case.body = Some(EnumBodyDef::Struct(fields));
            }
        }

        def.cases.insert(name, case);
    }

    for derive_child in get_children_named(doc, "rust-derive")? {
        no_children(derive_child)?;
        for e in derive_child.entries() {
            if e.name().is_some() {
                bail!(
                    labels=[LabeledSpan::at(*e.span(), "has a property name")],
                    "rust-derive arguments should be unnamed strings"
                );
            }
            let Some(s) = e.value().as_string() else {
                bail!(
                    labels=[LabeledSpan::at(*e.span(), "not a string")],
                    "rust-derive arguments should be unnamed strings"
                );
            };
            def.rust_derive.insert(s.to_string());
        }
    }

    if let Some(death_case) = get_unique_optional_string_value(doc, "on-task-death")? {
        if !def.cases.contains_key(death_case.value()) {
            bail!(
                labels=[LabeledSpan::at(death_case.span(), "not recognized as a case name")],
                "on-task-death names a nonexistent case"
            )
        }
        def.task_death_case = Some(death_case.into_value());
    }
    Ok(def)
}

fn parse_struct(
    doc: &KdlDocument,
) -> miette::Result<StructDef> {

    let mut def = StructDef::default();
    for (name, node) in get_uniquely_named_children(doc, "field")? {
        if node.entries().len() != 2 {
            bail!(
                labels=[LabeledSpan::at(*node.span(), "not two arguments")],
                "wrong number of arguments for field in named struct"
            );
        }
        let e_type = &node.entries()[1];
        if let Some(s) = e_type.value().as_string() {
            def.fields.insert(name, FieldDef {
                type_: parse_value_type(s)?,
            });
        } else {
            bail!(
                labels=[LabeledSpan::at(*e_type.span(), "not a string")],
                "struct field should have a string (type) argument"
            );
        }
    }
    for derive_child in get_children_named(doc, "rust-derive")? {
        no_children(derive_child)?;
        for e in derive_child.entries() {
            if e.name().is_some() {
                bail!(
                    labels=[LabeledSpan::at(*e.span(), "has a property name")],
                    "rust-derive arguments should be unnamed strings"
                );
            }
            let Some(s) = e.value().as_string() else {
                bail!(
                    labels=[LabeledSpan::at(*e.span(), "not a string")],
                    "rust-derive arguments should be unnamed strings"
                );
            };
            def.rust_derive.insert(s.to_string());
        }
    }

    Ok(def)
}

fn parse_method(
    doc: &KdlDocument,
) -> miette::Result<MethodDef> {

    let docstr = get_unique_optional_string_value(doc, "doc")?;

    let operation = get_unique_i64_value(doc, "operation")?;
    let operation = operation.try_map_with_span(|i, span| {
        u16::try_from(i).map_err(|_| {
            miette!(
                labels=[LabeledSpan::at(span, "out of range")],
                "operation must fit in a 16-bit unsigned integer"
            )
        })
    })?;

    let args = get_uniquely_named_children(doc, "arg")?
        .into_iter()
        .map(|(name, node)| {
            parse_arg(node)
                .map(|a| (name, a))
        })
        .collect::<miette::Result<IndexMap<String, ArgDef>>>()?;

    let leases = get_uniquely_named_children(doc, "lease")?
        .into_iter()
        .map(|(name, node)| {
            parse_lease(node)
                .map(|a| (name, a))
        })
        .collect::<miette::Result<IndexMap<String, _>>>()?;

    let auto_retry = get_unique_bool(doc, "auto-retry")?;

    let result = get_unique_optional_string_value(doc, "result")?
        .map(|s| {
            parse_value_type(s.value())
                .map(|t| Spanned::new(t, s.span()))
        }).transpose()?;
        

    Ok(MethodDef {
        doc: docstr,
        operation,
        args,
        leases,
        result,
        auto_retry,
    })
}

fn parse_arg(node: &KdlNode) -> miette::Result<ArgDef> {
    // Simple case?
    let e = node.entries();
    if e.len() == 2 && e[0].name().is_none() && e[1].name().is_none() {
        no_children(node)?;
        let Some(typename) = e[1].value().as_string() else {
            bail!(
                labels=[LabeledSpan::at(*e[1].span(), "not a string")],
                "argument type should be a string"
            )
        };
        Ok(ArgDef {
            type_: parse_value_type(typename)?,
        })
    } else {
        bail!(
            labels=[LabeledSpan::at(*node.span(), "wrong argument count")],
            "arg should have two arguments, name and type"
        )
    }
}

fn parse_lease(node: &KdlNode) -> miette::Result<LeaseDef> {
    // Simple case?
    let e = node.entries();
    if e.len() == 2 && e[0].name().is_none() && e[1].name().is_none() {
        let Some(typename) = e[1].value().as_string() else {
            bail!(
                labels=[LabeledSpan::at(*e[1].span(), "not a string")],
                "argument type should be a string"
            )
        };
        let children = required_children(node)?;
        let read = get_unique_bool(children, "read")?;
        let write = get_unique_bool(children, "write")?;

        Ok(LeaseDef {
            type_: parse_value_type(typename)?,
            read,
            write,
        })
    } else {
        bail!(
            labels=[LabeledSpan::at(*node.span(), "wrong argument count")],
            "arg should have two arguments, name and type"
        )
    }
}

fn parse_value_type(s: &str) -> miette::Result<ValueType> {
    // Handle the primitive cases
    let s = s.trim();
    match s {
        "u8" => Ok(ValueType::Prim(PrimType::U8)),
        "u16" => Ok(ValueType::Prim(PrimType::U16)),
        "u32" => Ok(ValueType::Prim(PrimType::U32)),
        "u64" => Ok(ValueType::Prim(PrimType::U64)),
        "u128" => Ok(ValueType::Prim(PrimType::U128)),

        "i8" => Ok(ValueType::Prim(PrimType::I8)),
        "i16" => Ok(ValueType::Prim(PrimType::I16)),
        "i32" => Ok(ValueType::Prim(PrimType::I32)),
        "i64" => Ok(ValueType::Prim(PrimType::I64)),
        "i128" => Ok(ValueType::Prim(PrimType::I128)),

        "f32" => Ok(ValueType::Prim(PrimType::F32)),
        "f64" => Ok(ValueType::Prim(PrimType::F64)),

        "bool" => Ok(ValueType::Prim(PrimType::Bool)),

        _ if s.starts_with('[') => {
            if !s.ends_with(']') {
                bail!("malformed array/slice type: {s}");
            }
            let middle = s.strip_prefix('[').unwrap().strip_suffix(']').unwrap();
            if let Some((element, count)) = middle.rsplit_once(';') {
                let count = count.trim().parse().into_diagnostic()?;
                let element = Box::new(parse_value_type(element)?);
                Ok(ValueType::Array(element, count))
            } else {
                bail!("array type missing semicolon: {s}");
            }
        }

        _ if s.starts_with('(') => {
            unimplemented!("tuple types: {s:?}");
        }

        _ => {
            let (basename, generics) = s.split_once('<').unwrap_or((s, ""));
            let generics = generics.strip_suffix('>').unwrap_or(generics).trim();
            if generics.contains('<') {
                unimplemented!("the proper type parser is not done yet");
            }

            let generics = if !generics.is_empty() {
                generics.split(',')
                    .map(parse_value_type)
                    .collect::<miette::Result<Vec<_>>>()?
            } else {
                vec![]
            };

            let name = basename.to_string();
            Ok(ValueType::Named {
                name,
                generics,
            })
        }
    }
}

#[derive(Clone, Debug)]
pub struct InterfaceDef {
    pub name: Spanned<String>,
    pub methods: IndexMap<String, MethodDef>,
    pub types: IndexMap<String, TypeDef>,
}

#[derive(Clone, Debug)]
pub struct MethodDef {
    pub doc: Option<Spanned<String>>,
    pub operation: Spanned<u16>,
    pub args: IndexMap<String, ArgDef>,
    pub leases: IndexMap<String, LeaseDef>,
    pub result: Option<Spanned<ValueType>>,
    pub auto_retry: bool,
}

#[derive(Clone, Debug)]
pub enum TypeDef {
    Enum(EnumDef),
    Struct(StructDef),
}

#[derive(Clone, Debug, Default)]
pub struct EnumDef {
    pub rust_derive: BTreeSet<String>,
    pub cases: IndexMap<String, EnumCaseDef>,
    pub task_death_case: Option<String>,
}

#[derive(Clone, Debug, Default)]
pub struct EnumCaseDef {
    pub doc: Option<Spanned<String>>,
    pub integer_value: Option<i64>,
    pub body: Option<EnumBodyDef>,
}

#[derive(Clone, Debug)]
pub enum EnumBodyDef {
    Tuple(Vec<ValueType>),
    Struct(IndexMap<String, FieldDef>),
}

#[derive(Clone, Debug, Default)]
pub struct StructDef {
    pub rust_derive: BTreeSet<String>,
    pub fields: IndexMap<String, FieldDef>,
}

#[derive(Clone, Debug)]
pub struct FieldDef {
    pub type_: ValueType,
}

#[derive(Clone, Debug)]
pub struct ArgDef {
    pub type_: ValueType,
}

#[derive(Clone, Debug)]
pub struct LeaseDef {
    pub type_: ValueType,
    pub read: bool,
    pub write: bool,
}

#[derive(Clone, Debug)]
pub enum ValueTypeOrSlice {
    Val(ValueType),
    Slice {
        element: ValueType,
    },
}

#[derive(Clone, Debug)]
pub enum ValueType {
    Prim(PrimType),
    Tuple(Vec<ValueType>),
    Array(Box<ValueType>, u32),
    Named {
        name: String,
        generics: Vec<ValueType>,
    },
}

#[derive(Copy, Clone, Debug)]
pub enum PrimType {
    U8,
    U16,
    U32,
    U64,
    U128,

    I8,
    I16,
    I32,
    I64,
    I128,

    F32,
    F64,

    Bool,
}
