use criterion::criterion_main;

mod io;
mod iteration;
mod model;

use crate::io::io;
use crate::iteration::iteration;
use crate::model::model;

criterion_main!(model, io, iteration);
