use super::*;
use oxrdf::{BlankNodeRef, LiteralRef, Quad, QuadRef, NamedNodeRef, NamedOrBlankNodeRef, TermRef, TripleRef};
use oxrdfio::WriterQuadSerializer;


impl<'a, A: AsRef<str>> From<&'a PNamedNode<A>> for NamedNodeRef<'a> {
    fn from(arnn: &'a PNamedNode<A>) -> Self {
        NamedNodeRef::new_unchecked(arnn.iri.as_ref())
    }
}

impl From<NamedNodeRef<'_>> for PNamedNode<String> {
    fn from(nn: NamedNodeRef<'_>) -> Self {
        let iri: String = nn.as_str().to_string();
        PNamedNode::new(iri)
    }
}

impl<'a, A: AsRef<str>> From<&'a PBlankNode<A>> for BlankNodeRef<'a> {
    fn from(arbn: &'a PBlankNode<A>) -> Self {
        // new_unchecked?
        BlankNodeRef::new(arbn.id.as_ref()).unwrap()
    }
}

impl From<BlankNodeRef<'_>> for PBlankNode<String> {
    fn from(bn: BlankNodeRef<'_>) -> Self {
        PBlankNode {
            id: bn.as_str().to_string(),
        }
    }
}

impl<'a, A: AsRef<str>> From<&'a PLiteral<A>> for LiteralRef<'a> {
    fn from(l: &'a PLiteral<A>) -> Self {
        match l {
            PLiteral::Simple { value } => LiteralRef::new_simple_literal(value.as_ref()),
            PLiteral::LanguageTaggedString { value, language } => {
                LiteralRef::new_language_tagged_literal_unchecked(value.as_ref(), language.as_ref())
            }
            PLiteral::Typed { value, datatype } => {
                LiteralRef::new_typed_literal(value.as_ref(), datatype)
            }
        }
    }
}

impl From<LiteralRef<'_>> for PLiteral<String> {
    fn from(l: LiteralRef<'_>) -> Self {
        if let Some(lang) = l.language() {
            return PLiteral::LanguageTaggedString {
                value: l.value().to_string(),
                language: lang.to_string()
            }
        }

        if l.datatype().as_str() == "http://www.w3.org/2001/XMLSchema#string" {
            return PLiteral::Simple {
                value: l.value().to_string()
            }
        }

        return PLiteral::Typed {
            value: l.value().to_string(),
            datatype: l.datatype().into()
        }
    }
}

impl<'a, A: AsRef<str>> From<&'a PNamedOrBlankNode<A>> for NamedOrBlankNodeRef<'a> {
    fn from(anbn: &'a PNamedOrBlankNode<A>) -> Self {
        match anbn {
            PNamedOrBlankNode::NamedNode(nn) => NamedOrBlankNodeRef::NamedNode(nn.into()),
            PNamedOrBlankNode::BlankNode(bn) => NamedOrBlankNodeRef::BlankNode(bn.into()),
        }
    }
}

impl From<NamedOrBlankNodeRef<'_>> for PNamedOrBlankNode<String> {
    fn from(nbn: NamedOrBlankNodeRef<'_>) -> Self {
        match nbn {
            NamedOrBlankNodeRef::NamedNode(nn) => PNamedOrBlankNode::NamedNode(nn.into()),
            NamedOrBlankNodeRef::BlankNode(bn) => PNamedOrBlankNode::BlankNode(bn.into()),
        }
    }
}

impl<'a, A: AsRef<str>> From<&'a PTerm<A>> for TermRef<'a> {
    fn from(t: &'a PTerm<A>) -> Self {
        match t {
            PTerm::NamedNode(nn) => TermRef::NamedNode(nn.into()),
            PTerm::BlankNode(bn) => TermRef::BlankNode(bn.into()),
            PTerm::Literal(l) => TermRef::Literal(l.into()),
        }
    }
}

impl From<TermRef<'_>> for PTerm<String> {
    fn from(t: TermRef<'_>) -> Self {
        match t {
            TermRef::NamedNode(nn) => PTerm::NamedNode(nn.into()),
            TermRef::BlankNode(bn) => PTerm::BlankNode(bn.into()),
            TermRef::Literal(l) => PTerm::Literal(l.into()),
        }
    }
}

impl<'a, A: AsRef<str>> From<&'a PTriple<A>> for TripleRef<'a> {
    fn from(t: &'a PTriple<A>) -> Self {
        TripleRef {
            subject: (&t.subject).into(),
            predicate: (&t.predicate).into(),
            object: (&t.object).into(),
        }
    }
}

impl From<TripleRef<'_>> for PTriple<String> {
    fn from(t: TripleRef<'_>) -> Self {
        PTriple {
            subject: t.subject.into(),
            predicate: t.predicate.into(),
            object: t.object.into(),
        }
    }
}

impl From<QuadRef<'_>> for PTriple<String> {
    fn from(q: QuadRef<'_>) -> Self {
        let t:TripleRef<'_> = q.into();
        t.into()
    }
}

impl From<Quad> for PTriple<String> {
    fn from(q: Quad) -> Self {
        q.as_ref().into()
    }
}

pub struct WriterQuadSerializerAdaptor<W: Write> {
    writer: WriterQuadSerializer<W>
}

impl<W:Write> WriterQuadSerializerAdaptor<W> {
    pub fn new(writer: WriterQuadSerializer<W>) -> WriterQuadSerializerAdaptor<W>{
        Self{writer}
    }
}

impl<A: AsRef<str>, W: Write> RdfFormatter<A, W> for WriterQuadSerializerAdaptor<W> {
    fn format(&mut self, triple: PTriple<A>) -> Result<(), io::Error> {
        self.writer.serialize_triple(&triple)
    }

    fn finish(self) -> Result<W, io::Error> {
        self.writer.finish()
    }
}

#[cfg(test)]
mod test {
    use oxrdfio::{RdfParser, RdfSerializer};

    use crate::{PTriple, RdfFormatter, ox::WriterQuadSerializerAdaptor};

    fn nt_roundtrip(nt: &str) {
        let source: Vec<PTriple<String>> = RdfParser::from_format(oxrdfio::RdfFormat::NTriples)
            .for_reader(nt.as_bytes())
            .map(Result::unwrap)
            .map(Into::into)
            .collect();

        let sink = vec![];
        let mut f = WriterQuadSerializerAdaptor::new(
            RdfSerializer::from_format(oxrdfio::RdfFormat::NTriples).for_writer(sink)
        );

        for t in source {
            f.format(t).unwrap()
        }

        let w:Vec<u8> = <WriterQuadSerializerAdaptor<Vec<u8>> as RdfFormatter<String, Vec<u8>>>::finish(f).unwrap();
        let s = String::from_utf8(w).unwrap();

        assert_eq!(s, nt);
    }

    #[test]
    fn nt_single_triple() {
        nt_roundtrip(
            r###"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF1.1 XML Syntax" .
"###,
        )
    }
}
