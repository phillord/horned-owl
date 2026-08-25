//! OWL Manchester Syntax I/O.
pub mod reader;
pub mod writer;
pub use reader::{parse_class_expression, read};
pub use writer::{AsManchester, Manchester, write};
