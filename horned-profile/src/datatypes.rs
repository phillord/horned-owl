//! XSD datatype legality shared between `el` and `ql`.
//!
//! `el::is_el_data_range`'s and `ql::is_ql_data_range`'s allowlists turned
//! out empirically identical (see each module's own doc comments for how
//! they arrived at the same 13-item set independently, via separate
//! `robot validate-profile` probing passes). Factored out into one shared
//! list once the duplication was noticed, so the two can't silently drift
//! apart if either profile's list is ever revised.

use horned_owl::vocab;

pub(crate) const EL_QL_DATATYPE_ALLOWLIST: [&str; 13] = [
    "http://www.w3.org/2001/XMLSchema#string",
    "http://www.w3.org/2001/XMLSchema#normalizedString",
    "http://www.w3.org/2001/XMLSchema#token",
    "http://www.w3.org/2001/XMLSchema#Name",
    "http://www.w3.org/2001/XMLSchema#NCName",
    "http://www.w3.org/2001/XMLSchema#NMTOKEN",
    "http://www.w3.org/2001/XMLSchema#hexBinary",
    "http://www.w3.org/2001/XMLSchema#base64Binary",
    "http://www.w3.org/2001/XMLSchema#integer",
    "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",
    "http://www.w3.org/2001/XMLSchema#decimal",
    "http://www.w3.org/2001/XMLSchema#dateTime",
    "http://www.w3.org/2001/XMLSchema#anyURI",
];

/// Returns `true` if `iri` is a legal EL/QL datatype.
///
/// An XSD datatype is EL/QL-legal only if it's in
/// [`EL_QL_DATATYPE_ALLOWLIST`]; a non-XSD datatype (`owl:real`/
/// `owl:rational`, `rdf:PlainLiteral`/`XMLLiteral`, ...) defaults to legal,
/// matching this crate's existing treatment of those as always-usable
/// built-ins.
pub(crate) fn is_el_or_ql_datatype(iri: &str) -> bool {
    if vocab::is_xsd_datatype(iri) {
        EL_QL_DATATYPE_ALLOWLIST.contains(&iri)
    } else {
        true
    }
}
