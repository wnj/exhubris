//! Parses an embedding of JSON into KDL that is derived from the official JiK
//! embedding, with some tweaks.
//!
//! You do not need to understand JiK to use this embedding, the rules are laid
//! out below.
//!
//! # Why JSON?
//!
//! This is specifically intended for encoding application-specified
//! configuration data into KDL files. It defines a mapping from KDL to JSON,
//! specifically, because
//!
//! - JSON's object model is very simple and a good match for KDL. (By contrast,
//!   RON is difficult to express in KDL due to missing syntax.)
//!
//! - The JiK embedding existed as a starting point.
//!
//! - The `serde_json::from_value` function can take the JSON `Value` produced
//!   by these functions and _turn it into any deserializable Rust type,_ which
//!   means you don't have to actually deal with JSON if you don't want to.
//!
//! - The shape of the data can be checked using `jsonschema`.
//!
//! - Because JSON Schema is defined in terms of a JSON embedding, this also
//!   lets you encode JSON Schemas in KDL.
//!
//! # Embedding basics
//!
//! Without additional context, any top-level KDL document is assumed to
//! represent a JSON object. Each node in the document is treated as a property
//! of the object, with the node's name as the property name, and the node's
//! first argument as the property value.
//!
//! All "scalar" literals appearing as values --- integers, booleans, floats,
//! and the special value `null` --- are transferred directly to their JSON
//! equivalent.
//!
//! To represent an object as a property value, add a body to the node using
//! curly braces (`{}`). Its contents are then parsed using the same rules.
//!
//! Using the rules described so far, this KDL document...
//!
//! ```kdl
//! sandwiches 3
//! orders {
//!     bob "ham"
//!     claire "avocado"
//! }
//! ```
//!
//! ...is equivalent to this JSON:
//!
//! ```json
//! {
//!     "sandwiches": 3,
//!     "orders": {
//!         "bob": "ham",
//!         "claire": "avocado"
//!     }
//! }
//! ```
//!
//! # Arrays
//!
//! A document in which all node names are dashes (`-`) is treated as an array.
//! This KDL document:
//!
//! ```kdl
//! - 1
//! - 2
//! - 3
//! ```
//!
//! is equivalent to the JSON `[1, 2, 3]`.
//!
//! # Compact arrays
//!
//! If a node has multiple arguments, its arguments will be collected into an
//! array. As a result, these two KDL documents express the same JSON structure:
//!
//! ```kdl
//! orders {
//!     - "ham"
//!     - "avocado"
//! }
//! ```
//!
//! ```kdl
//! orders "ham" "avocado"
//! ```
//!
//! A node with a compact array value _must not_ have a curly-brace body. This
//! is treated as ambiguous and rejected.
//!
//! An empty array (a node with no arguments) cannot be written this way; we
//! assume it's an error. Use empty curly braces with the `(array)` annotation
//! described below.
//!
//! Currently, a single-element array cannot be written as a compact array. This
//! will likely change.
//!
//! # Compact objects
//!
//! If a node has properties instead of objects, the properties will be
//! collected into an object. As a result, these two KDL documents express the
//! same JSON structure:
//!
//! ```kdl
//! orders {
//!     bob "ham"
//!     claire "avocado"
//! }
//! ```
//!
//! ```kdl
//! orders bob="ham" claire="avocado"
//! ```
//!
//! A node with a compact object value _must not_ have a curly-brace body. This
//! is treated as ambiguous and rejected.
//!
//! An empty object cannot be written this way. Use empty curly braces.
//!
//! # Ambiguous object/arrays
//!
//! There are several ways to write a value that make it ambiguous whether you
//! intended it to be an object or an array. All of these ambiguous cases are
//! rejected by the parser. In some cases, there is a trick to override the
//! decision.
//!
//! First: curly brace bodies with a mix of `-` and other node names.
//!
//! ```kdl
//! // This is not accepted
//! orders {
//!     - "ham"
//!     claire "avocado"
//! }
//! ```
//!
//! If you need to use a literal `-` as an object property name, wrap it in
//! quotes. This version of the document is accepted:
//!
//! ```kdl
//! orders {
//!     // No, really, dash is the property name!
//!     "-" "ham"
//!     claire "avocado"
//! }
//! ```
//!
//! Second: compact arrays or objects with a mix of arguments and properties.
//! For example:
//!
//! ```kdl
//! // Ambiguous mix of arguments and properties
//! orders "ham" claire="avocado"
//! ```
//!
//! Either add a name to the nameless arguments (making them properties) or
//! remove the property names.
//!
//! Finally: because arrays are detected based on their contents, an empty
//! braced body is by default an object.
//!
//! ```kdl
//! this-is-an-object {}
//! ```
//!
//! To express an empty array, use the `(array)` type annotation:
//!
//! ```kdl
//! (array)this-is-an-array {}
//! ```
//!
//! The `(array)` annotation can be used on non-empty arrays to force
//! interpretation of the contents. This isn't generally useful, but who knows.
//!
//! There is an equivalent `(object)` annotation for symmetry that ensures that
//! the value of a node is treated as an object. There is no syntactic need to
//! use it, but you might find it clearer in some cases.
//!
//! # Differences from JiK
//!
//! As far as I can tell, JiK hasn't been implemented, so these differences are
//! theoretical.
//!
//! - Node names are always treated as meaningful and not ignored, because that
//!   seemed like a good way to accidentally mask typos.
//!
//! - A top-level KDL document can only encode an array or object (as a direct
//!   result of the first point). This is intended specifically for embedding
//!   JSON trees _within a larger KDL document,_ so this isn't a practical
//!   limitation (the JSON is not the top-level document in that case).
//!
//! - Mixed arguments/properties/bodies are treated as mistakes. You can either
//!   write a compact array/object, or use a body, but not both. (This
//!   eliminates some weird corner cases in combinations.)
//!
//! - The `(array)` annotation can only be used to force interpretation of a
//!   curly-braced body. It cannot be used to cause a single value to be
//!   interpreted as an array. This will probably become more lenient in the
//!   future.
//!
//! - A bare `-` as an object property is always treated as a mistake, even
//!   with the `(object)` annotation on its parent. To use `-` as an object
//!   property, it must always be quoted. (This makes it easier to see that
//!   you're in an object while skimming.)
//!
//! - The `(object)` annotation doesn't really do anything except assert that
//!   the value is an object. It doesn't change how node names are treated.
//!
//! - Probably other things.

use kdl::{KdlDocument, KdlNode, KdlValue};
use miette::{bail, IntoDiagnostic as _, LabeledSpan};
use serde::de::DeserializeOwned;
use serde_json::{Map, Value};

/// Deserializes the contents of a KDL document to a `T` by going first to JSON,
/// then through `serde`.
///
/// Because a KDL _document_ in this embedding always represents either an
/// object or an array, `T` should be deserializable from one of those two
/// structures.
///
/// Note that a KDL document in this embedding _cannot_ represent an empty
/// array. So using this to directly deserialize things like `Vec` is probably
/// unwise.
pub fn deserialize_doc<T>(doc: &KdlDocument) -> miette::Result<T>
    where T: DeserializeOwned,
{
    serde_json::from_value(parse_doc(doc)?).into_diagnostic()
}

/// Deserializes the arguments/properties/body of a KDL node to a `T` by going
/// first to JSON, then through `serde`.
///
/// The node name is ignored, though its type annotation (if any) is respected.
/// This is intended for deserializing types from within a larger, non-JSON KDL
/// document.
pub fn deserialize_node_contents<T>(node: &KdlNode) -> miette::Result<T>
    where T: DeserializeOwned,
{
    serde_json::from_value(parse_node_contents(node)?).into_diagnostic()
}


/// Interprets the nodes in `doc` as the contents of a JSON object or array.
///
/// If `doc` is empty, it's assumed to be an object. This is because a
/// _document_ can't bear a type annotation in KDL. If you have a `KdlNode`
/// (which _can_ have a type annotation), call `parse_as_value` instead.
///
/// Otherwise, its contents are interpreted according to the embedding rules
/// described in the module docs.
pub fn parse_doc(doc: &KdlDocument) -> miette::Result<Value> {
    // First, check if it's an array, with an override to cause an empty
    // document to be parsed as an object.
    if !doc.nodes().is_empty() && doc.nodes().iter().all(is_array_element) {
        parse_as_array(doc).map(Value::Array)
    } else if
        let Some(surprise) = doc.nodes().iter().find(|&n| is_array_element(n))
    {
        bail!(
            labels=[LabeledSpan::at(
                *surprise.name().span(),
                "this looks like an array element",
            )],
            help="to use a literal - as an object property, quote it: \"-\"",
            "it's ambiguous whether this is an object or an array"
        )
    } else {
        parse_as_object(doc).map(Value::Object)
    }
}

fn is_array_element(node: &KdlNode) -> bool {
    if let Some(repr) = node.name().repr() {
        if repr != "-" {
            // This is a quoted identifier
            return false;
        }
    }
    node.name().value() == "-"
}

fn parse_as_object(doc: &KdlDocument) -> miette::Result<Map<String, Value>> {
    doc.nodes().iter().map(|n| {
        Ok((n.name().value().to_string(), parse_node_contents(n)?))
    }).collect()
}

fn parse_as_array(doc: &KdlDocument) -> miette::Result<Vec<Value>> {
    // We've already checked the names of all nodes, so we just need to collect
    // their values.
    doc.nodes().iter().map(parse_node_contents).collect()
}

/// Interprets the argument(s) or body of a single node as a JSON value. The
/// node's name is ignored, but its type annotation is respected.
///
/// For embedding a JSON sub-tree inside a more general KDL document, this is
/// the entry point you probably want to use. It ensures that the top-level JSON
/// object can be a scalar or an empty array.
///
/// The node is interpreted according to the embedding rules described in the
/// module docs.
pub fn parse_node_contents(node: &KdlNode) -> miette::Result<Value> {
    if let Some(children) = node.children() {
        if let Some(first) = node.entries().first() {
            bail!(
                labels = [
                    LabeledSpan::at(*first.span(), "argument"),
                    LabeledSpan::at(*children.span(), "children"),
                ],
                "node may have children ({{...}}) or arguments but not both"
            )
        }
        if let Some(t) = node.ty() {
            match t.value() {
                "array" => {
                    if let Some(bad) = children.nodes().iter()
                        .find(|n| !is_array_element(n))
                    {
                        bail!(
                            labels = [
                                LabeledSpan::at(*t.span(), "asserted to be an array"),
                                LabeledSpan::at(*bad.span(), "not an array element"),
                            ],
                            "ambiguous explicit array"
                        )
                    }
                    parse_as_array(children).map(Value::from)
                }
                "object" => {
                    if let Some(bad) = children.nodes().iter()
                        .find(|n| is_array_element(n))
                    {
                        bail!(
                            labels = [
                                LabeledSpan::at(*t.span(), "asserted to be an object"),
                                LabeledSpan::at(*bad.span(), "looks like an array element"),
                            ],
                            help = "to use a literal - as an object key, \
                                        quote it: \"-\"",
                            "ambiguous explicit object"
                        )
                    }
                    parse_as_object(children).map(Value::from)
                }
                _ => {
                    bail!(
                        labels=[LabeledSpan::at(*t.span(), "not recognized")],
                        help = "did you mean (array) or (object)?",
                        "unexpected type annotation"
                    )
                }
            }
        } else {
            parse_doc(children)
        }
    } else if node.entries().iter().all(|e| e.name().is_some()) {
        // inline map
        let m: Map<_, _> = node.entries().iter().map(|e| {
            (e.name().unwrap().value().to_string(), convert_value(e.value()))
        }).collect();
        Ok(m.into())
    } else if let Some(surprise) = node.entries().iter()
        .find(|n| n.name().is_some())
    {
        // Because the check above failed, there must also be at least one entry
        // that is _not_ a property, so this unwrap will succeed.
        let other = node.entries().iter().find(|n| n.name().is_none()).unwrap();
        bail!(
            labels = [
                LabeledSpan::at(*surprise.span(), "this has a property name"),
                LabeledSpan::at(*other.span(), "this does not"),
            ],
            help = "either give all arguments property names (map) or \
                    remove them all (array)",
            "it's ambiguous whether this is an object or an array"
        )
    } else if node.entries().len() > 1 {
        // inline array
        let v: Vec<_> = node.entries().iter()
            .map(|e| convert_value(e.value()))
            .collect();
        Ok(v.into())
    } else {
        // single value
        Ok(convert_value(node.entries()[0].value()))
    }
}

fn convert_value(value: &KdlValue) -> Value {
    match value.clone() {
        KdlValue::RawString(s) | KdlValue::String(s) => s.into(),

        KdlValue::Base2(i) | KdlValue::Base8(i) | KdlValue::Base10(i)
            | KdlValue::Base16(i) => i.into(),

        KdlValue::Base10Float(f) => f.into(),
        KdlValue::Bool(b) => b.into(),
        KdlValue::Null => Value::Null,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    macro_rules! assert_parse_eq {
        ($docstr:literal, $($jsoninp:tt)+) => {
            {
                let docstr = $docstr;
                let parsed = parse_doc(&docstr.parse().unwrap())
                    .map_err(|e| e.with_source_code(docstr)).unwrap();
                assert_eq!(
                    parsed,
                    json!($($jsoninp)+),
                )
            }
        }
    }

    #[test]
    fn empty_document() {
        assert_parse_eq!("", {});
    }

    #[test]
    fn basic_array() {
        assert_parse_eq!("
            - 0
            - 1
            - 2
        ", json!([0,1,2]));
    }

    #[test]
    fn basic_map() {
        assert_parse_eq!(r#"
            dogs 3
            cats "allergic"
        "#, json!({"dogs":3, "cats":"allergic"}));
    }

    #[test]
    fn inner_map() {
        assert_parse_eq!(
            r#"
                - {
                    inner-1 42
                    inner-2 "omglol"
                }
                - true
            "#,
            json!([
                {
                    "inner-1": 42,
                    "inner-2": "omglol"
                },
                true
            ])
        );
    }

    #[test]
    fn inner_array() {
        assert_parse_eq!(
            r#"
                outer-key {
                    - 42
                    - "omglol"
                }
                pants true
            "#,
            json!({
                "outer-key": [
                    42,
                    "omglol"
                ],
                "pants": true
            })
        );
    }

    #[test]
    fn dash_as_object_property() {
        assert_parse_eq!(
            r#"
                outer-key 42
                "-" "dash"
                pants true
            "#,
            json!({
                "outer-key": 42,
                "-": "dash",
                "pants": true
            })
        );
    }

    #[test]
    fn empty_object_value() {
        assert_parse_eq!(
            r#"
                outer-key {}
                pants true
            "#,
            json!({
                "outer-key": {},
                "pants": true
            })
        );
    }

    #[test]
    fn empty_array_value() {
        assert_parse_eq!(
            r#"
                (array)outer-key {}
                pants true
            "#,
            json!({
                "outer-key": [],
                "pants": true
            })
        );
    }

    #[test]
    fn inline_array() {
        assert_parse_eq!(
            r#"
                outer-key 1 2 3 4 5
            "#,
            json!({
                "outer-key": [1, 2, 3, 4, 5],
            })
        );
    }

    #[test]
    fn inline_object() {
        assert_parse_eq!(
            r#"
                outer-key x=1 y=2
            "#,
            json!({
                "outer-key": {"x":1, "y":2}
            })
        );
    }
}
