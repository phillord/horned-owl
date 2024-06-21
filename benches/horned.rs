use criterion::criterion_main;

mod io;
mod model;

use crate::io::io;
use crate::model::model;

criterion_main!(model, io);
