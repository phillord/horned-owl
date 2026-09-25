//! # Horned-OWL
//!
//! Horned-OWL provides an interface to parse, generate and handle ontologies
//! written using the [Web Ontology Language (OWL)](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/).

//! Unlike a simple classification taxonomy, OWL is highly expressive and maps
//! to a formal semantics which makes the ontology open to computational
//! reasoning.
//!
//! The aim of the library is to provide a representation of OWL that
//! can be used to manipulate OWL ontologies.
//!
//! The focus of this library is on performance, compared to the [OWL
//! API](https://github.com/owlcs/owlapi), thereby allowing large
//! scale, bulk manipulation of ontologies that currently requires
//! specialized machinery.
//!
//! # Author
//!
//! This library is written by Phillip Lord <phillip.lord@newcastle.ac.uk>
//!
//! # Status
//!
//! The core data model of the library now provides a complete
//! implementation of the OWL2 DL specification. It appears to be
//! highly performant, being between 1 and 2 orders of magnitude
//! faster than the OWL API for some tasks.
//extern crate curie;
//extern crate enum_meta;

//#[macro_use]
extern crate indexmap;
extern crate log;
extern crate quick_xml;

pub mod adaptor;
pub mod curie;
pub mod error;
pub mod io;
pub mod model;
pub mod normalize;
pub mod ontology;
pub mod resolve;
pub mod visitor;
pub mod vocab;

/// The version of this horned-owl library crate, baked in at compile
/// time. Exposed so consumers (notably the `horned-bin` CLIs) can report
/// exactly which horned-owl source a binary was compiled from.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// `Instant` that also works on wasm (where the std clock would trap). Used for
/// the optional perf timing in the RDF reader / SetOntology build so those paths
/// don't abort the wasm module merely by reading the clock. Three backends:
///   - wasm32 (browser via JS, or wasip1 via WASI): `web-time`;
///   - wasm64-unknown-unknown (wasmtime reactor: no WASI/JS, std clock traps): a
///     clock imported from the metering host (`host.now_nanos`);
///   - everything else (native): `std::time`.
pub(crate) mod time {
    #[cfg(target_arch = "wasm64")]
    pub use self::host_clock::Instant;
    #[cfg(target_arch = "wasm32")]
    pub use web_time::Instant;
    #[cfg(not(target_family = "wasm"))]
    pub use std::time::Instant;

    /// A monotonic `Instant` for the wasm64 reactor, read from a host import
    /// (`host.now_nanos() -> i64`, monotonic nanoseconds). The wasmtime host
    /// supplies it; see semantic-mcp `engine.rs` (`make_linker`).
    #[cfg(target_arch = "wasm64")]
    mod host_clock {
        pub use std::time::Duration;

        #[link(wasm_import_module = "host")]
        unsafe extern "C" {
            fn now_nanos() -> i64;
        }

        #[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
        pub struct Instant(i64);

        impl Instant {
            pub fn now() -> Self {
                Instant(unsafe { now_nanos() })
            }
            pub fn elapsed(&self) -> Duration {
                let now = unsafe { now_nanos() };
                Duration::from_nanos(now.saturating_sub(self.0).max(0) as u64)
            }
            pub fn duration_since(&self, earlier: Instant) -> Duration {
                Duration::from_nanos(self.0.saturating_sub(earlier.0).max(0) as u64)
            }
        }

        // Match `std::time::Instant`: `later - earlier` yields the elapsed `Duration`.
        impl std::ops::Sub for Instant {
            type Output = Duration;
            fn sub(self, earlier: Instant) -> Duration {
                self.duration_since(earlier)
            }
        }
    }
}
