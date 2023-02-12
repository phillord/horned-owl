# Intro

Here we describe different methods for supporting SWRLRules and
potentially also regularizing support for `OntologyID` alongside
`Axiom`.

The complexity of the OWL data model makes it unlikely I can implement
these alongside each other to compare, because it's too much work.

The OWL API uses "solution 3" -- it just considers a `SWRLRule` as an
axiom. It doesn't do the same with `OntologyID`.


# Problem

Horned-OWL does not support SWRL rules at the moment, meaning that it
fails to parse quite a few ontologies. While SWRL rules are not part
of OWL but an extension to it, that they are used means that people
will expect them to be supported.

## Related Problem

The data model of Horned-OWL means, that we cannot represent an
Ontology as a iterator. This is because we have followed the spec
literally, meaning an ontology is a set of Axioms and an Ontology ID.

This means we have to write From/Into conversion methods which do most
of the work with an iterator and transfer the Ontology ID independently.
,
# Still Unclear

I haven't read the SWRL spec yet. While it is not as complicated as
OWL per se, neither is it simple. The implementation will take a bit
of effort.


# Solution 1

SWRL is not part of OWL, so the obvious solution is to add a new
library, defining a `SWRLOntology` which would look something like

```
pub trait SWRLOntology<A> {
   pub fn ontology(&self) -> &Ontology;
}
```

As with the current `Ontology`, we do not define the actual important
bit of the class (i.e. the ability to produce an
`Iterator<item=Axiom>`). In this case, it would also have to produce
an `Iterator<Item=SWRLRule>`.

## Pro
,
Fits with the spec.

## Cons

This doesn't combine well with any other extensions; although there
probably are not going to be any.

It complicates the data model further, in that an ontology is not a
set of axioms, set of SWRL rules and an ontology ID.


## Solution 2

We simply support SWRLRules like we do `OntologyID`

```
pub trait Ontology<A> {
    // Existing methods
    fn swrl_rules(&self) -> &Set<SWRLRule>;
    fn mut_swrl_rules(&self) -> &mut Set<SWRLRule>;
}
```

## Cons

We have to change every ontology implementation. And the
implementation will become more complex.

And the interface as given, with a `Set<SWRLRule>` doesn't fit well
with the current interface which does not define a collection type.

# Solution 3

We bend the spec a bit and call a `SWRLRule` an axiom type. If we
wanted to be honest, we could rename `Axiom` to be
`Component`. `Component` could support additional methods `is_id`,
`is_axiom`, `is_swrl_rule`.

## Pro

All existing ontologies would more or less just work.

As we have already bent truth, We could rethink the treatment of
`OntologyID` also, and add that as an `AxiomType`. This would make an
Ontology entirely representable by an `Iterator`.

`IndexedOntology` could drop all the `OntologyID` support.

There would be a clean indicator at compile time where `SWRLRule`
needed to be supported.

## Con

All the complexity of `SWRL` will go into the `model` namespace. It
won't be possible to have a `OWL` model without it.

Accessing `OntologyID` would become linear (for `SetOntology` but not
`AxiomMapped`).


# Solution 4

We could turn an ontology into a `Iterator<Item=Component>` where

```
pub Enum Component {
    AnnotatedAxiom(ax), SWRLRule(rl), OntologyID(id)
}
```

## Pro

Most natural fit to the spec

Most existing ontologies could use their current implementation to
support both the `SWRLRule` and `OntologyID`, as with Solution 3


## Con

An ontology would become a `Iterator<Item=Component>` which means
accessing the `Axiom` would require a separate (potentially failing)
step to unpack `Component` to `Axiom`.



# Current Situation

I am inclined toward Solution 3 and which would include pulling
`OntologyID` in as an axiom at the same time.
