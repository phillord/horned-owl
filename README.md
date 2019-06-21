Horned OWL
==========

Horned-OWL is a library for manipulating OWL (Ontology Web Language)
data. While there are a number of libraries that manipulate this form
of data such as the [OWL API](https://github.com/owlcs/owlapi),
they can be quite slow. Horned-OWL is aimed at allowing ontologies
with millions of terms.

The library now implements all of OWL2, and we are working on further
parser functionality. We are testing it with real world tasks, such as
parsing the Gene Ontology, which is does in 2-3 seconds which is 20-40
times faster than the OWL API.
