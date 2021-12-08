use rio_xml::RdfXmlError;
use thiserror::Error;

// Errors
#[derive(Debug, Error)]
pub enum CommandError {
    #[error("An argument that was expected is missing")]
    MissingArgument,

    #[error("Oops")]
    Underlying(#[source] Box<dyn std::error::Error>)
}


pub fn underlying<E: std::error::Error +'static>(error:E) -> CommandError{
    CommandError::Underlying(Box::new(error))
}

impl From<RdfXmlError> for CommandError{
    fn from(e: RdfXmlError) -> Self {
        Self::Underlying(Box::new(e))
    }
}
