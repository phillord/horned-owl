use criterion::criterion_main;

mod io_read;
mod io_write;
mod iteration;
mod model;

use crate::io_read::io_read;
use crate::io_write::io_write;
use crate::iteration::iteration;
use crate::model::model;

criterion_main!(model, io_read, io_write, iteration);
