//! OWL Manchester Syntax I/O.
pub mod reader;
pub mod writer;
pub use reader::{parse_class_expression, read, read_with_build};
pub use writer::{AsManchester, Manchester, write};
