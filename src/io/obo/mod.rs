//! OBO flat-file format 1.4 I/O.
//!
//! First-class support for the [OBO flat-file
//! format](https://owlcollab.github.io/oboformat/doc/obo-syntax.html) 1.4,
//! implementing the OBO ↔ OWL 2 mapping (issue
//! [#181](https://github.com/phillord/horned-owl/issues/181)).
//!
//! The reader parses with a hand-written nom parser, following the vendored `fastobo-syntax` grammar, and maps to
//! horned-owl components; the writer renders the OBO-expressible fragment back,
//! giving read/write round-trip. Behaviour is pinned to the OWL-API `oboformat`
//! mapping as an oracle.
#[doc(hidden)]
pub mod common;
pub mod reader;
pub mod writer;
pub use reader::read;
pub use writer::{
    OBOStreamWriterConfiguration, StreamMode, write, write_stream, write_stream_chunked,
    write_stream_with_config,
};

#[cfg(test)]
mod oracle;
