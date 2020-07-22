//! Errors
#[derive(Debug, Fail)]
pub enum CommandError {
    #[fail(display = "An argument that was expected is missing")]
    MissingArgument,
}
