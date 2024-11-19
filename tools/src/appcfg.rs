use std::{collections::{BTreeMap, BTreeSet}, fmt::Display, path::{Path, PathBuf}, sync::Arc};

use cargo_metadata::Package;
use indexmap::IndexMap;
use kdl::{KdlDocument, KdlNode, KdlValue};
use miette::{bail, diagnostic, miette, Context, IntoDiagnostic as _, LabeledSpan, NamedSource, SourceSpan};

use crate::BuildEnv;

#[derive(Clone, Debug)]
pub struct AppDef {
    /// Source code of the root app def file, for citations.
    pub source: Arc<NamedSource>,

    /// Name of app, for naming images and such.
    pub name: Spanned<String>,

    pub board: BoardDef,

    pub tasks: IndexMap<String, TaskDef>,

    pub kernel: KernelDef,
}

#[derive(Clone, Debug)]
pub struct BoardDef {
    /// Source code containing the board def, for citations; this may be the file
    /// itself, or may be a file into which it has been inlined.
    pub source: Arc<NamedSource>,

    /// Name of board, for matching in conditional compilation.
    pub name: Spanned<String>,

    pub chip: ChipDef,
}

#[derive(Clone, Debug)]
pub struct ChipDef {
    /// Source code containing the chip def, for citations; this may be the file
    /// itself, or may be a file into which it has been inlined.
    pub source: Arc<NamedSource>,

    /// Name of chip model, for matching in conditional compilation.
    pub name: Spanned<String>,

    /// Target triple for cross compilation.
    pub target_triple: Spanned<String>,

    /// Size of vector table in bytes.
    pub vector_table_size: Spanned<u64>,

    /// Allocatable memory regions.
    pub memory: IndexMap<String, Spanned<RegionDef>>,
}

/// Definition of a region of address space.
#[derive(Clone, Debug)]
pub struct RegionDef {
    /// Base address of the region.
    pub base: Spanned<u64>,
    /// Size of the region in bytes.
    pub size: Spanned<u64>,
}

#[derive(Clone, Debug)]
pub struct TaskDef {
    /// Name of the task, which is technically redundant since it's also a map
    /// key, but, this makes it easier to include in messages.
    pub name: String,

    /// Explicit stack size for task.
    pub stack_size: Spanned<u64>,

    /// Priority of the task (lower numbers are more important).
    pub priority: Spanned<u8>,

    pub package_source: Spanned<PackageSource>,

    pub cargo_features: BTreeMap<String, SourceSpan>,
    pub default_features: bool,
    pub toolchain: Option<Spanned<String>>,
    pub target: Option<Spanned<String>>,
}

#[derive(Clone, Debug)]
pub struct KernelDef {
    pub stack_size: Spanned<u64>,

    pub package_source: Spanned<PackageSource>,

    pub cargo_features: BTreeMap<String, SourceSpan>,
    pub default_features: bool,
    pub toolchain: Option<Spanned<String>>,
    pub target: Option<Spanned<String>>,
}

#[derive(Clone, Debug)]
pub enum PackageSource {
    WorkspaceCrate {
        name: String,
    },
    GitCrate {
        repo: String,
        name: String,
        rev: String,
    },
}

#[derive(Copy, Clone, Debug)]
pub struct Spanned<T> {
    value: T,
    span: SourceSpan,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: SourceSpan) -> Self {
        Self { value, span }
    }

    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn value_mut(&mut self) -> &mut T {
        &mut self.value
    }

    pub fn into_value(self) -> T {
        self.value
    }

    pub fn span(&self) -> SourceSpan {
        self.span
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }

    pub fn try_map_with_span<U, E>(self, f: impl FnOnce(T, SourceSpan) -> Result<U, E>) -> Result<Spanned<U>, E> {
        let Self { value, span } = self;
        Ok(Spanned {
            value: f(value, span)?,
            span,
        })
    }
}

pub fn parse_source_str<R>(
    path: &str,
    src: &str,
    body: impl FnOnce(Arc<NamedSource>, KdlDocument) -> miette::Result<R>,
) -> miette::Result<R> {
    let source = Arc::new(NamedSource::new(
            path,
            src.to_string(),
    ));
    let doc: kdl::KdlDocument = add_source(&source, || {
        Ok(src.parse()?)
    }).wrap_err("config does not appear to be valid KDL")?;
    body(source, doc)
}


pub fn parse_app_str(
    path: &str,
    src: &str,
    ctx: &dyn LoadContext,
) -> miette::Result<AppDef> {
    parse_source_str(path, src, |source, doc| {
        parse_app(source, &doc, ctx)
    })
}

pub fn parse_board_str(
    path: &str,
    src: &str,
    ctx: &dyn LoadContext,
) -> miette::Result<BoardDef> {
    parse_source_str(path, src, |source, doc| {
        parse_board(source, &doc, ctx)
    })
}

pub fn parse_app(
    source: Arc<NamedSource>,
    doc: &KdlDocument,
    ctx: &dyn LoadContext,
) -> miette::Result<AppDef> {
    add_source(&source, || {
        let name = get_unique_string_value(doc, "name")?;

        let board = get_unique_child_or_include(
            &source,
            doc,
            "board",
            Namespace::Board,
            ctx,
            |s, d| parse_board(s, &d, ctx),
        )?;

        let tasks = get_uniquely_named_children(doc, "task")?
            .into_iter()
            .map(|(name, tasknode)| {
                let taskdoc = required_children(tasknode)?;
                let t = parse_task(source.clone(), taskdoc, &name)?;
                Ok((name, t))
            })
            .collect::<miette::Result<IndexMap<_, _>>>()?;

        let kernel_node = get_unique_child(doc, "kernel")?;
        let kernel = parse_kernel(
            source.clone(),
            required_children(kernel_node)?,
        )?;


        Ok(AppDef {
            source: source.clone(),
            name,
            board,
            tasks,
            kernel,
        })
    })
}

pub fn get_unique_child_or_include<T>(
    source: &Arc<NamedSource>,
    doc: &KdlDocument,
    name: &str,
    ns: Namespace,
    ctx: &dyn LoadContext,
    proc: impl FnOnce(Arc<NamedSource>, KdlDocument) -> miette::Result<T>,
) -> miette::Result<T> {
    let child = get_unique_child(doc, name)?;
    if child.children().is_some() {
        no_arguments(child)?;
        proc(source.clone(), required_children(child)?.clone())
    } else {
        let ident = get_unique_string_value(doc, name)?;
        let (docsrc, doc) = ctx.get_resource(ns, ident.value())?;
        proc(docsrc, doc)
            .wrap_err_with(|| {
                miette!(
                    labels = [LabeledSpan::at(ident.span(), ident.value())],
                    "can't parse {ns} included from {}",
                    source.name(),
                ).with_source_code(source.clone())
            })
    }
}

pub fn parse_board(
    source: Arc<NamedSource>,
    doc: &KdlDocument,
    ctx: &dyn LoadContext,
) -> miette::Result<BoardDef> {
    add_source(&source, || {
        let name = get_unique_string_value(doc, "name")?;

        let chip = get_unique_child_or_include(
            &source,
            doc,
            "chip",
            Namespace::Chip,
            ctx,
            |s, d| parse_chip(s, &d),
        ).wrap_err_with(|| "can't process chip for board")?;

        Ok(BoardDef {
            source: source.clone(),
            name,
            chip,
        })
    })
}

pub fn parse_chip(
    source: Arc<NamedSource>,
    doc: &KdlDocument,
) -> miette::Result<ChipDef> {
    add_source(&source, || {
        let name = get_unique_string_value(doc, "name")?;
        let target_triple = get_unique_string_value(doc, "target-triple")?;
        let vector_table_size = get_unique_i64_value(doc, "vector-table-size")?;
        let vector_table_size = vector_table_size.try_map_with_span(|i, span| {
            u64::try_from(i)
                .map_err(|_| {
                    miette!(
                        labels = [LabeledSpan::at(span, "not a u64")],
                        "vector table for chip '{}' must be an unsigned integer",
                        name.value()
                    )
                })
        })?;

        let memory = get_unique_child(doc, "memory")?;
        no_arguments(memory)?;
        let memory = get_uniquely_named_children(required_children(memory)?, "region")?
            .into_iter()
            .map(|(key, node)| parse_region(node).map(|r| (key, r)))
            .collect::<miette::Result<IndexMap<_, _>>>()?;

        Ok(ChipDef {
            source: source.clone(),
            name,
            target_triple,
            vector_table_size,
            memory,
        })
    })
}

pub fn parse_task(
    source: Arc<NamedSource>,
    doc: &KdlDocument,
    name: &str,
) -> miette::Result<TaskDef> {
    add_source(&source, || {
        let stack_size = get_unique_i64_value(doc, "stack-size")?;
        let stack_size = stack_size.try_map_with_span(|i, span| {
            u64::try_from(i)
                .map_err(|_| {
                    miette!(
                        labels = [LabeledSpan::at(span, "not a u64")],
                        "stack size for task '{name}' must be an unsigned integer"
                    )
                })
        })?;
       
        let priority = get_unique_i64_value(doc, "priority")?;
        let priority = priority.try_map_with_span(|i, span| {
            u8::try_from(i)
                .map_err(|_| {
                    miette!(
                        labels = [LabeledSpan::at(span, "not a u8")],
                        "priority for task '{name}' must be an unsigned 8-bit integer"
                    )
                })
        })?;

        let package_source = {
            let workspace_crate = get_unique_optional_string_value(doc, "workspace-crate")?;
            let git = get_unique_optional_child(doc, "git-crate")?;

            match (workspace_crate, git) {
                (Some(name), None) => name.map(|n| PackageSource::WorkspaceCrate { name: n }),
                (None, Some(gitatts)) => {
                    let c = required_children(gitatts)?;
                    let repo = get_unique_string_value(c, "repo")?.into_value();
                    let name = get_unique_string_value(c, "package")?.into_value();
                    let rev = get_unique_string_value(c, "rev")?.into_value();
                    Spanned::new(
                        PackageSource::GitCrate { repo, name, rev, },
                        *gitatts.span(),
                    )
                }
                (Some(a), Some(b)) => bail!(
                    labels=[
                        LabeledSpan::at(a.span(), "here"),
                        LabeledSpan::at(*b.span(), "also here"),
                    ],
                    "too many crate specs for task",
                ),
                (None, None) => bail!(
                    "missing crate spec for task",
                ),
            }
        };

        let cargo_features = get_unique_optional_string_array(doc, "features")?;

        let mut unique_features = BTreeMap::new();
        let mut duplicate_features = vec![];
        for f in cargo_features {
            if unique_features.insert(f.value().clone(), f.span()).is_some() {
                duplicate_features.push(f);
            }
        }
        if !duplicate_features.is_empty() {
            let labels = duplicate_features.into_iter().map(|f| LabeledSpan::at(f.span(), "duplicate")).collect::<Vec<_>>();
            bail!(
                labels = labels,
                "Cargo features must not be repeated"
            );
        }

        let default_features = !get_unique_bool(doc, "no-default-features")?;

        let toolchain = get_unique_optional_string_value(doc, "toolchain")?;
        let target = get_unique_optional_string_value(doc, "target")?;
        Ok(TaskDef {
            name: name.to_string(),
            stack_size,
            package_source,
            priority,
            cargo_features: unique_features.into_iter().collect(),
            default_features,
            toolchain,
            target,
        })
    })
}

pub fn parse_kernel(
    source: Arc<NamedSource>,
    doc: &KdlDocument,
) -> miette::Result<KernelDef> {
    add_source(&source, || {
        let stack_size = get_unique_i64_value(doc, "stack-size")?;
        let stack_size = stack_size.try_map_with_span(|i, span| {
            u64::try_from(i)
                .map_err(|_| {
                    miette!(
                        labels = [LabeledSpan::at(span, "not a u64")],
                        "stack size for kernel must be an unsigned integer"
                    )
                })
        })?;
       
        let package_source = {
            let workspace_crate = get_unique_optional_string_value(doc, "workspace-crate")?;
            let git = get_unique_optional_child(doc, "git-crate")?;

            match (workspace_crate, git) {
                (Some(name), None) => name.map(|n| PackageSource::WorkspaceCrate { name: n }),
                (None, Some(gitatts)) => todo!(),
                (Some(a), Some(b)) => bail!(
                    labels=[
                        LabeledSpan::at(a.span(), "here"),
                        LabeledSpan::at(*b.span(), "also here"),
                    ],
                    "too many crate specs for kernel",
                ),
                (None, None) => bail!(
                    "missing crate spec for kernel",
                ),
            }
        };

        let cargo_features = get_unique_optional_string_array(doc, "features")?;

        let mut unique_features = BTreeMap::new();
        let mut duplicate_features = vec![];
        for f in cargo_features {
            if unique_features.insert(f.value().clone(), f.span()).is_some() {
                duplicate_features.push(f);
            }
        }
        if !duplicate_features.is_empty() {
            let labels = duplicate_features.into_iter().map(|f| LabeledSpan::at(f.span(), "duplicate")).collect::<Vec<_>>();
            bail!(
                labels = labels,
                "Cargo features must not be repeated"
            );
        }

        let default_features = !get_unique_bool(doc, "no-default-features")?;
        let toolchain = get_unique_optional_string_value(doc, "toolchain")?;
        let target = get_unique_optional_string_value(doc, "target")?;

        Ok(KernelDef {
            stack_size,
            package_source,
            cargo_features: unique_features.into_iter().collect(),
            default_features,
            toolchain,
            target,
        })
    })
}

fn parse_region(node: &KdlNode) -> miette::Result<Spanned<RegionDef>> {
    let doc = node.children().ok_or_else(|| {
        miette!(
            labels = [LabeledSpan::at(*node.span(), "missing body")],
            "region node must have a body"
        )
    })?;
    let base = get_unique_i64_value(doc, "base")?
        .try_map_with_span(|v, span| {
            u64::try_from(v).into_diagnostic().wrap_err_with(|| {
                miette!(
                    labels = [LabeledSpan::at(span, "not an unsigned integer")],
                    "base address must fit in a u64"
                )
            })
        })?;
    let size = get_unique_i64_value(doc, "size")?
        .try_map_with_span(|v, span| {
            u64::try_from(v).into_diagnostic().wrap_err_with(|| {
                miette!(
                    labels = [LabeledSpan::at(span, "not an unsigned integer")],
                    "size must fit in a u64"
                )
            })
        })?;

    Ok(Spanned::new(RegionDef { base, size }, *node.span()))
}


pub fn add_source<T>(source: &Arc<NamedSource>, body: impl FnOnce() -> miette::Result<T>) -> miette::Result<T> {
    body().map_err(|e| {
        if e.source().is_none() {
            e.with_source_code(source.clone())
        } else {
            e
        }
    })
}

fn no_arguments(node: &KdlNode) -> miette::Result<()> {
    if node.entries().is_empty() {
        Ok(())
    } else {
        Err(miette!(
            labels = [LabeledSpan::at(*node.span(), "has args/properties")],
            "node must not have arguments or properties"
        ))
    }
}

fn required_children(node: &KdlNode) -> miette::Result<&KdlDocument> {
    node.children().ok_or_else(|| {
        miette!(
            labels = [LabeledSpan::at(*node.span(), "missing body")],
            "{} node must have a body",
            node.name()
        )
    })
}

fn get_unique_i64_value(doc: &KdlDocument, name: &str) -> miette::Result<Spanned<i64>> {
    get_unique_value(doc, name)?
        .try_map_with_span(|v, span| {
            v.as_i64().ok_or_else(|| {
                miette!(
                    labels = [LabeledSpan::at(span, "not an integer")],
                    "value for {name} must be an integer"
                )
            })
        })
}

fn get_unique_string_value<'d>(doc: &'d KdlDocument, name: &str) -> miette::Result<Spanned<String>> {
    get_unique_value(doc, name)?
        .try_map_with_span(|v, span| {
            v.as_string().ok_or_else(|| {
                miette!(
                    labels = [LabeledSpan::at(span, "not a string")],
                    "value for {name} must be a string"
                )
            }).map(|s| s.to_string())
        })
}

/*
fn get_unique_optional_string_value<'d>(doc: &'d KdlDocument, name: &str) -> miette::Result<Option<(&'d str, SourceSpan)>> {
    let Some((v, span)) = get_unique_optional_value(doc, name)? else {
        return Ok(None)
    };
    let s = v.as_string().ok_or_else(|| {
        miette!(
            labels = [LabeledSpan::at(span, "not a string")],
            "value for {name} must be a string"
        )
    })?;
    Ok(Some((s, span)))
}
*/

fn get_unique_value<'d>(doc: &'d KdlDocument, name: &str) -> miette::Result<Spanned<&'d KdlValue>> {
    get_unique_optional_value(doc, name)?
        .ok_or_else(|| {
            miette!(
                labels = [LabeledSpan::at(*doc.span(), "not found in this")],
                "required node '{name}' is missing",
            )
        })
}

fn get_unique_optional_value<'d>(doc: &'d KdlDocument, name: &str) -> miette::Result<Option<Spanned<&'d KdlValue>>> {
    let Some(node) = get_unique_optional_child(doc, name)? else {
        return Ok(None);
    };

    match node.entries().len() {
        0 => {
            Err(miette!(
                labels = [LabeledSpan::at(*node.span(), "node without value")],
                "the {name} node must have a value"
            ))
        }
        1 => {
            let entry = node.entries().first().unwrap();
            if entry.name().is_some() {
                return Err(miette!(
                    labels = [LabeledSpan::at(*entry.span(), "property, not argument")],
                    "value for {name} should be an argument, not a property"
                ));
            }
            Ok(Some(Spanned::new(entry.value(), *entry.span())))
        }
        n => {
            Err(miette!(
                labels = [LabeledSpan::at(*node.span(), "node with too many values")],
                "the {name} node must have one value, not {n}"
            ))
        }
    }
}

fn get_unique_optional_string_value(doc: &KdlDocument, name: &str) -> miette::Result<Option<Spanned<String>>> {
    let Some(value) = get_unique_optional_value(doc, name)? else {
        return Ok(None);
    };
    Ok(Some(value.try_map_with_span(|v, span| v.as_string().ok_or_else(|| {
        miette!(
            labels = [LabeledSpan::at(span, "not a string")],
            "value for {name} must be a string"
        )
    }))?.map(|s| s.to_string())))
}


fn get_unique_optional_string_array(doc: &KdlDocument, name: &str) -> miette::Result<Vec<Spanned<String>>> {
    let Some(child) = get_unique_optional_child(doc, name)? else {
        return Ok(vec![]);
    };

    let mut strings = vec![];
    if let Some(body) = child.children() {
        for node in body.nodes() {
            if node.name().value() != "-" {
                bail!(
                    labels = [LabeledSpan::at(*node.name().span(), "not a dash")],
                    "elements of this array must all start with a dash (-)"
                );
            }
            if let Some(ec) = node.children() {
                bail!(
                    labels = [LabeledSpan::at(*ec.span(), "unexpected body")],
                    "array element must be a simple string and not have a body"
                );
            }
            match node.entries().len() {
                0 => {
                    bail!(
                        labels = [LabeledSpan::at(*node.span(), "missing string")],
                        "each array element must be a dash followed by a string"
                    );
                }
                1 => {
                    let ent = node.entries().first().unwrap();
                    if ent.name().is_some() {
                        bail!(
                            labels = [LabeledSpan::at(*ent.span(), "has a name")],
                            help = "to include an equals sign, quote the string",
                            "string required, not a property"
                        );
                    }
                    if let Some(s) = ent.value().as_string() {
                        strings.push(Spanned::new(s.to_string(), *ent.span()));
                    } else {
                        bail!(
                            labels = [LabeledSpan::at(*ent.span(), "not a string")],
                            help = "to make this a string, quote it",
                            "string required"
                        );
                    }
                }
                n => {
                    bail!(
                        labels = [LabeledSpan::at(*node.span(), format!("this is {n} arguments"))],
                        "each array element must be a dash followed by a single string"
                    );
                }
            }
        }
    } else {
        for entry in child.entries() {
            if entry.name().is_some() {
                bail!(
                    labels=[LabeledSpan::at(*entry.span(), "this is a property")],
                    "expected simple arguments, not properties"
                );
            }
            let s = entry.value().as_string().ok_or_else(|| miette!(
                    labels=[LabeledSpan::at(*entry.span(), "not a string")],
                    "arguments to node '{name}' need to be strings"
            ))?;
            strings.push(Spanned::new(s.to_string(), *entry.span()));
        }
    }
    Ok(strings)
}

fn get_unique_bool<'d>(doc: &'d KdlDocument, name: &str) -> miette::Result<bool> {
    let mut found = vec![];
    for node in doc.nodes() {
        if node.name().value() == name {
            found.push(node);
        }
    }

    if found.is_empty() {
        Ok(false)
    } else if found.len() > 1 {
        let mut labels = vec![];
        let first = found.remove(0);
        labels.push(LabeledSpan::at(*first.span(), "found here 👍"));
        for dupe in found {
            labels.push(LabeledSpan::at(*dupe.span(), "also found here 😕"));
        }
        Err(diagnostic!(
            labels = labels,
            "expected {name} to appear at most once",
        ).into())
    } else {
        no_arguments(&found[0])?;
        Ok(true)
    }
}

fn get_unique_optional_child<'d>(doc: &'d KdlDocument, name: &str) -> miette::Result<Option<&'d KdlNode>> {
    let mut found = vec![];
    for node in doc.nodes() {
        if node.name().value() == name {
            found.push(node);
        }
    }

    if found.is_empty() {
        Ok(None)
    } else if found.len() > 1 {
        let mut labels = vec![];
        let first = found.remove(0);
        labels.push(LabeledSpan::at(*first.span(), "found here 👍"));
        for dupe in found {
            labels.push(LabeledSpan::at(*dupe.span(), "also found here 😕"));
        }
        Err(diagnostic!(
            labels = labels,
            "expected one {name}, not many",
        ).into())
    } else {
        Ok(Some(found[0]))
    }
}

fn get_unique_child<'d>(doc: &'d KdlDocument, name: &str) -> miette::Result<&'d KdlNode> {
    get_unique_optional_child(doc, name)?
        .ok_or_else(|| {
            miette!(
                labels = [LabeledSpan::at(*doc.span(), "not found in this")],
                "required node '{name}' is missing",
            )
        })
}

fn get_uniquely_named_children<'d>(doc: &'d KdlDocument, name: &str) -> miette::Result<IndexMap<String, &'d KdlNode>> {
    let mut found = vec![];
    for node in doc.nodes() {
        if node.name().value() == name {
            found.push(node);
        }
    }

    let mut by_name = IndexMap::new();
    for node in found {
        let argval = node.get(0).ok_or_else(|| {
            miette!(
                labels = [LabeledSpan::at(*node.span(), "should have a string argument")],
                "{name} missing name argument"
            )
        })?;
        let nodename = argval.value().as_string().ok_or_else(|| {
            miette!(
                labels = [LabeledSpan::at(*argval.span(), "should be a string")],
                "{name} name argument isn't a string"
            )
        })?;
        match by_name.entry(nodename.to_string()) {
            indexmap::map::Entry::Vacant(v) => {
                // yay
                v.insert(node);
            }
            indexmap::map::Entry::Occupied(e) => {
                return Err(miette!(
                    labels = [
                        LabeledSpan::at(*e.get().span(), "previously found here"),
                        LabeledSpan::at(*node.span(), "later found here"),
                    ],
                    "duplicate {name} named {nodename}"
                ));
            }
        }
    }

    Ok(by_name)
}

pub trait LoadContext {
    fn get_resource(&self, ns: Namespace, identifier: &str) -> miette::Result<(Arc<NamedSource>, KdlDocument)>;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Namespace {
    Chip,
    Board,
}

impl Display for Namespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Namespace::Chip => write!(f, "chip"),
            Namespace::Board => write!(f, "board"),
        }
    }
}

pub struct SelfContained;

impl LoadContext for SelfContained {
    fn get_resource(&self, _: Namespace, _: &str) -> miette::Result<(Arc<NamedSource>, KdlDocument)> {
        bail!("includes not supported in this context")
    }
}

pub struct FsContext {
    root: PathBuf,
}

impl FsContext {
    pub fn from_root(root: impl AsRef<Path>) -> Self {
        Self {
            root: PathBuf::from(root.as_ref()),
        }
    }
}

impl LoadContext for FsContext {
    fn get_resource(&self, ns: Namespace, identifier: &str) -> miette::Result<(Arc<NamedSource>, KdlDocument)> {
        let mut path = self.root.join(match ns {
            Namespace::Chip => "chips",
            Namespace::Board => "boards",
        });
        path.push(identifier);
        path.set_extension("kdl");
        let src = std::fs::read_to_string(&path)
            .into_diagnostic()
            .wrap_err_with(|| format!("can't load {ns} file: {}", path.display()))?;
        let named_src = Arc::new(NamedSource::new(
                path.display().to_string(),
                src.clone(),
        ));
        let doc = add_source(&named_src, || Ok(src.parse()?))?;
        Ok((named_src, doc))
    }
}

// checks beyond parsing:
// - is toolchain, like, installed? (record build env)
// - is target triple supported by toolchain?
// - are all workspace-crate references valid? (record package metadata)
//      - are features valid? (from metadata)
// - are task definition files available for all tasks? (parse them)

#[derive(Clone, Debug)]
pub struct CheckedCfg {
    /// Information about the build toolchain.
    pub build_env: BuildEnv,
    /// The target libdir for the target triple. We don't really _use_ this, but
    /// it proves that the target is installed!
    pub target_libdir: PathBuf,
    // Metadata on every package referenced from the config.
    pub package_metadata: IndexMap<String, Package>,
}

#[derive(Clone, Debug)]
pub struct BuildPlan {
    pub method: BuildMethod,
    /// Name of package from Cargo's perspective.
    pub package_name: String,
    /// Name of bin target within the package. In most cases, this is the same
    /// as the package name.
    pub bin_name: String,
    /// Target triple to build for.
    pub target_triple: String,
    /// Toolchain to use, if overridden.
    pub toolchain_override: Option<String>,
    /// Cargo features to enable. This list is not comprehensive if the crate
    /// lists default features.
    pub cargo_features: BTreeSet<String>,
    /// Whether to enable default features, in addition to the `cargo_features`
    /// list.
    pub default_features: bool,
    /// Environment to set for Cargo, to smuggle information through to the
    /// crate's build scripts.
    pub smuggled_env: BTreeMap<String, String>,
    /// Any extra rustflags.
    pub rustflags: String,
}

#[derive(Clone, Debug)]
pub enum BuildMethod {
    CargoWorkspaceBuild,
    CargoInstallGit {
        repo: String,
        rev: String,
    },
}

pub struct BuildPlans {
    pub tasks: IndexMap<String, BuildPlan>,
    pub kernel: BuildPlan,
}

pub fn plan_build(
    app: &AppDef,
) -> miette::Result<BuildPlans> {
    let meta = cargo_metadata::MetadataCommand::new();
    let meta = meta.exec().into_diagnostic()?;

    // Index all bin targets found by (package, name).
    let bin = "bin".to_string();
    let binmap = meta.workspace_packages()
        .into_iter()
        .flat_map(|pack| {
            pack.targets.iter()
                .filter(|t| t.kind.contains(&bin))
                .map(move |t| ((&pack.name, &t.name), pack))
        })
        .collect::<BTreeMap<_, _>>();

    // Attempt to locate the cargo package for each task.
    let mut task_plans = IndexMap::new();
    for (name, task) in &app.tasks {
        let target_triple = task.target.as_ref()
            .unwrap_or(&app.board.chip.target_triple);
        let plan = match task.package_source.value() {
            PackageSource::WorkspaceCrate { name } => {
                let package = binmap.get(&(name, name))
                    .ok_or_else(|| {
                        miette!(
                            labels = [LabeledSpan::at(
                                task.package_source.span(),
                                "requested here"
                            )],
                            "can't find package/binary '{name}'"
                        )
                    })?;
                let missing_features = task.cargo_features.iter()
                    .filter(|(f, _)| !package.features.contains_key(*f))
                    .map(|(_, s)| LabeledSpan::at(*s, "unknown feature"))
                    .collect::<Vec<_>>();
                if !missing_features.is_empty() {
                    return Err(miette!(
                            labels = missing_features,
                            "task {name} refers to undefined Cargo features"
                    ).with_source_code(app.source.clone()));
                }

                BuildPlan {
                    method: BuildMethod::CargoWorkspaceBuild,
                    package_name: package.name.clone(),
                    bin_name: package.name.clone(),
                    target_triple: target_triple.value().clone(),
                    toolchain_override: task.toolchain.as_ref().map(|s| s.value().clone()),
                    cargo_features: task.cargo_features.keys().cloned().collect(),
                    default_features: task.default_features,
                    smuggled_env: Default::default(), // TODO
                    rustflags: Default::default(), // TODO
                }
            }
            PackageSource::GitCrate { repo, name, rev } => {
                BuildPlan {
                    method: BuildMethod::CargoInstallGit {
                        repo: repo.clone(),
                        rev: rev.clone(),
                    },
                    package_name: name.clone(),
                    bin_name: name.clone(),
                    target_triple: target_triple.value().clone(),
                    toolchain_override: task.toolchain.as_ref().map(|s| s.value().clone()),
                    cargo_features: task.cargo_features.keys().cloned().collect(),
                    default_features: task.default_features,
                    smuggled_env: Default::default(), // TODO
                    rustflags: Default::default(), // TODO
                }
            }
        };
        task_plans.insert(name.clone(), plan);
    }
    // aaaand the kernel
    let kernel = {
        let target_triple = app.kernel.target.as_ref()
            .unwrap_or(&app.board.chip.target_triple);
        match app.kernel.package_source.value() {
            PackageSource::WorkspaceCrate { name } => {
                let package = binmap.get(&(name, name))
                    .ok_or_else(|| {
                        miette!(
                            labels = [LabeledSpan::at(
                                app.kernel.package_source.span(),
                                "requested here"
                            )],
                            "can't find package/binary '{name}'"
                        )
                    })?;
                let missing_features = app.kernel.cargo_features.iter()
                    .filter(|(f, _)| !package.features.contains_key(*f))
                    .map(|(_, s)| LabeledSpan::at(*s, "unknown feature"))
                    .collect::<Vec<_>>();
                if !missing_features.is_empty() {
                    return Err(miette!(
                            labels = missing_features,
                            "kernel refers to undefined Cargo features"
                    ).with_source_code(app.source.clone()));
                }

                BuildPlan {
                    method: BuildMethod::CargoWorkspaceBuild,
                    package_name: package.name.clone(),
                    bin_name: package.name.clone(),
                    target_triple: target_triple.value().clone(),
                    toolchain_override: app.kernel.toolchain.as_ref().map(|s| s.value().clone()),
                    cargo_features: app.kernel.cargo_features.keys().cloned().collect(),
                    default_features: app.kernel.default_features,
                    smuggled_env: Default::default(), // TODO
                    rustflags: Default::default(), // TODO
                }
            }
            PackageSource::GitCrate { repo, name, rev } => {
                BuildPlan {
                    method: BuildMethod::CargoInstallGit {
                        repo: repo.clone(),
                        rev: rev.clone(),
                    },
                    package_name: name.clone(),
                    bin_name: name.clone(),
                    toolchain_override: app.kernel.toolchain.as_ref().map(|s| s.value().clone()),
                    target_triple: target_triple.value().clone(),
                    cargo_features: app.kernel.cargo_features.keys().cloned().collect(),
                    default_features: app.kernel.default_features,
                    smuggled_env: Default::default(), // TODO
                    rustflags: Default::default(), // TODO
                }
            }
        }
    };
    
    Ok(BuildPlans {
        tasks: task_plans,
        kernel,
    })
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;

    #[derive(Default)]
    struct FakeContext {
        boards: BTreeMap<String, (Arc<NamedSource>, KdlDocument)>,
        chips: BTreeMap<String, (Arc<NamedSource>, KdlDocument)>,
    }

    impl FakeContext {
        fn add_board(&mut self, ident: &str, path: &str, text: &str) {
            let nsrc = Arc::new(NamedSource::new(path, text.to_string()));
            let doc = text.parse().expect("fixture failed to parse");
            self.boards.insert(ident.to_string(), (nsrc, doc));
        }
        fn add_chip(&mut self, ident: &str, path: &str, text: &str) {
            let nsrc = Arc::new(NamedSource::new(path, text.to_string()));
            let doc = text.parse().expect("fixture failed to parse");
            self.chips.insert(ident.to_string(), (nsrc, doc));
        }
    }

    impl LoadContext for FakeContext {
        fn get_resource(&self, ns: Namespace, identifier: &str) -> miette::Result<(Arc<NamedSource>, KdlDocument)> {
            let map = match ns {
                Namespace::Chip => &self.chips,
                Namespace::Board => &self.boards,
            };
            map.get(identifier)
                .ok_or_else(|| miette!("can't find {ns} named '{identifier}'"))
                .cloned()
        }
    }

    #[test]
    #[should_panic(expected = "does not appear to be valid KDL")]
    fn app_not_even_kdl() {
        let appcfg = "LOL I AM THE LIZARD KING";
        parse_app_str("<input>", appcfg, &SelfContained).unwrap();
    }

    #[test]
    #[should_panic]
    fn not_an_app_cfg() {
        let appcfg = r#"
            hello
            i-am "technically" { valid "kdl"; }
            "#;
        parse_app_str("<input>", appcfg, &SelfContained).unwrap();
    }

    fn assert_error_is_decent(e: miette::Report, substring: &str) {
        println!("{e:?}");
        assert!(e.source_code().is_some(), "error is missing source code!");
        assert!(e.labels().into_iter().flatten().count() != 0, "error has no labels!");

        let msg = e.to_string();
        assert!(msg.contains(substring),
            "substring '{substring}' not present in message: {msg}");
    }

    #[test]
    fn app_missing_name() {
        let appcfg = "
            board {
            }
            ";
        match parse_app_str("<input>", appcfg, &SelfContained) {
            Ok(_) => panic!("should not have parsed"),
            Err(e) => assert_error_is_decent(e, "'name' is missing"),
        }
    }

    #[test]
    fn app_missing_board() {
        let appcfg = r#"
            name "charles"
            "#;
        match parse_app_str("<input>", appcfg, &SelfContained) {
            Ok(_) => panic!("should not have parsed"),
            Err(e) => assert_error_is_decent(e, "'board' is missing"),
        }
    }

    #[test]
    fn app_simple_successful() {
        let appcfg = r#"
            name "charles"
            board {
                name "film (canada)"
                chip {
                    name "potato"
                    target-triple "i4004-none-gnueabi"
                    memory {
                    }
                }
            }
            "#;
        let cfg = parse_app_str("<input>", appcfg, &SelfContained).expect("should have parsed");
        assert_eq!(cfg.name.value(), "charles");
        assert_eq!(cfg.board.name.value(), "film (canada)");
        assert_eq!(cfg.board.chip.name.value(), "potato");
        assert_eq!(cfg.board.chip.target_triple.value(), "i4004-none-gnueabi");
        assert!(cfg.board.chip.memory.is_empty());
    }

    #[test]
    fn app_board_include() {
        let mut ctx = FakeContext::default();
        ctx.add_board("film-board-of-canada", "fake-board.kdl", r#"
            name "film (canada)"
            chip {
                name "potato"
                target-triple "i4004-none-gnueabi"
                memory {
                }
            }
            "#);
        let appcfg = r#"
            name "charles"
            board "film-board-of-canada"
            "#;
        let cfg = parse_app_str("<input>", appcfg, &ctx).expect("should have parsed");
        assert_eq!(cfg.name.value(), "charles");
        assert_eq!(cfg.board.name.value(), "film (canada)");
        assert_eq!(cfg.board.chip.name.value(), "potato");
        assert_eq!(cfg.board.chip.target_triple.value(), "i4004-none-gnueabi");
        assert!(cfg.board.chip.memory.is_empty());
    }

    #[test]
    fn board_chip_include() {
        let mut ctx = FakeContext::default();
        ctx.add_chip("potato", "lol-im-a-potato.kdl", r#"
            name "potato"
            target-triple "i4004-none-gnueabi"
            memory {
            }
            "#);
        let boardcfg = r#"
            name "film (canada)"
            chip "potato"
            "#;
        let cfg = parse_board_str("<input>", boardcfg, &ctx).expect("should have parsed");
        assert_eq!(cfg.name.value(), "film (canada)");
        assert_eq!(cfg.chip.name.value(), "potato");
        assert_eq!(cfg.chip.target_triple.value(), "i4004-none-gnueabi");
        assert!(cfg.chip.memory.is_empty());
    }
}
