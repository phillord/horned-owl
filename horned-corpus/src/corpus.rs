use anyhow::Context;
use std::io::Read;
use std::path::{Path, PathBuf};

pub fn entries(dir: &Path) -> anyhow::Result<Vec<PathBuf>> {
    let mut v: Vec<PathBuf> = std::fs::read_dir(dir)?
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.is_file())
        .collect();
    v.sort();
    Ok(v)
}

pub fn read_bytes(path: &Path) -> anyhow::Result<Vec<u8>> {
    let raw = std::fs::read(path).with_context(|| format!("read {path:?}"))?;
    if raw.len() >= 2 && raw[0] == 0x1f && raw[1] == 0x8b {
        let mut out = Vec::new();
        flate2::read::GzDecoder::new(&raw[..]).read_to_end(&mut out)?;
        Ok(out)
    } else {
        Ok(raw)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    #[test]
    fn reads_plain_and_gzip() {
        let dir = std::env::temp_dir().join(format!("hrt-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("a.ofn"), b"hello").unwrap();
        let mut enc = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
        enc.write_all(b"world").unwrap();
        std::fs::write(dir.join("b.ofn.gz"), enc.finish().unwrap()).unwrap();
        assert_eq!(entries(&dir).unwrap().len(), 2);
        assert_eq!(read_bytes(&dir.join("a.ofn")).unwrap(), b"hello");
        assert_eq!(read_bytes(&dir.join("b.ofn.gz")).unwrap(), b"world");
    }
}
