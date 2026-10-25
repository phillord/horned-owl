//! A compact set of annotations.

use std::borrow::Borrow;
use std::collections::BTreeSet;
use std::fmt;

use super::Annotation;

/// A set of annotations, stored as a sorted boxed slice.
///
/// Unlike a `BTreeSet`, it allocates nothing when empty and exactly its size
/// otherwise. Inserting and removing are linear in its size.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AnnotationSet<A>(Box<[Annotation<A>]>);

impl<A> Default for AnnotationSet<A> {
    fn default() -> Self {
        AnnotationSet(Box::new([]))
    }
}

impl<A: Ord> AnnotationSet<A> {
    /// An empty set.
    pub fn new() -> Self {
        Self::default()
    }

    /// The number of annotations.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether the set has no annotation.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// The annotations in ascending order.
    pub fn iter(&self) -> std::slice::Iter<'_, Annotation<A>> {
        self.0.iter()
    }

    /// The annotations as a sorted slice.
    pub fn as_slice(&self) -> &[Annotation<A>] {
        &self.0
    }

    /// Whether the set holds `value`.
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        Annotation<A>: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        self.find(value).is_ok()
    }

    /// The annotation of the set equal to `value`.
    pub fn get<Q>(&self, value: &Q) -> Option<&Annotation<A>>
    where
        Annotation<A>: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        self.find(value).ok().map(|i| &self.0[i])
    }

    /// The smallest annotation.
    pub fn first(&self) -> Option<&Annotation<A>> {
        self.0.first()
    }

    /// The largest annotation.
    pub fn last(&self) -> Option<&Annotation<A>> {
        self.0.last()
    }

    /// Adds `value`; `false` if the set held it already.
    pub fn insert(&mut self, value: Annotation<A>) -> bool {
        match self.find(&value) {
            Ok(_) => false,
            Err(at) => {
                let mut v = std::mem::take(&mut self.0).into_vec();
                v.insert(at, value);
                self.0 = v.into_boxed_slice();
                true
            }
        }
    }

    /// Adds `value`, replacing an equal annotation, which is returned.
    pub fn replace(&mut self, value: Annotation<A>) -> Option<Annotation<A>> {
        match self.find(&value) {
            Ok(at) => Some(std::mem::replace(&mut self.0[at], value)),
            Err(_) => {
                self.insert(value);
                None
            }
        }
    }

    /// Removes `value`; `false` if the set did not hold it.
    pub fn remove<Q>(&mut self, value: &Q) -> bool
    where
        Annotation<A>: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        self.take(value).is_some()
    }

    /// Removes and returns the annotation equal to `value`.
    pub fn take<Q>(&mut self, value: &Q) -> Option<Annotation<A>>
    where
        Annotation<A>: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        let at = self.find(value).ok()?;
        let mut v = std::mem::take(&mut self.0).into_vec();
        let taken = v.remove(at);
        self.0 = v.into_boxed_slice();
        Some(taken)
    }

    /// Removes and returns the smallest annotation.
    pub fn pop_first(&mut self) -> Option<Annotation<A>> {
        if self.0.is_empty() {
            return None;
        }
        let mut v = std::mem::take(&mut self.0).into_vec();
        let first = v.remove(0);
        self.0 = v.into_boxed_slice();
        Some(first)
    }

    /// Keeps the annotations for which `keep` is true.
    pub fn retain(&mut self, keep: impl FnMut(&Annotation<A>) -> bool) {
        let mut v = std::mem::take(&mut self.0).into_vec();
        v.retain(keep);
        self.0 = v.into_boxed_slice();
    }

    /// Removes all annotations.
    pub fn clear(&mut self) {
        self.0 = Box::new([]);
    }

    /// Moves all annotations of `other` into the set.
    pub fn append(&mut self, other: &mut Self) {
        let other = std::mem::take(other);
        self.extend(other);
    }

    /// Whether all annotations of the set are in `other`.
    pub fn is_subset(&self, other: &Self) -> bool {
        self.iter().all(|a| other.contains(a))
    }

    /// The annotations of the set that are not in `other`, in ascending order.
    pub fn difference<'a>(
        &'a self,
        other: &'a Self,
    ) -> impl Iterator<Item = &'a Annotation<A>> + 'a {
        self.iter().filter(move |a| !other.contains(*a))
    }

    /// The annotations in the set or in `other`, in ascending order.
    pub fn union<'a>(&'a self, other: &'a Self) -> impl Iterator<Item = &'a Annotation<A>> + 'a {
        let mut all: Vec<&Annotation<A>> = self.iter().chain(other.iter()).collect();
        all.sort();
        all.dedup();
        all.into_iter()
    }

    fn find<Q>(&self, value: &Q) -> Result<usize, usize>
    where
        Annotation<A>: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        self.0.binary_search_by(|a| a.borrow().cmp(value))
    }

    fn from_vec(mut v: Vec<Annotation<A>>) -> Self {
        v.sort();
        v.dedup();
        AnnotationSet(v.into_boxed_slice())
    }
}

impl<A: fmt::Debug> fmt::Debug for AnnotationSet<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.0.iter()).finish()
    }
}

impl<A: Ord> FromIterator<Annotation<A>> for AnnotationSet<A> {
    fn from_iter<I: IntoIterator<Item = Annotation<A>>>(iter: I) -> Self {
        Self::from_vec(iter.into_iter().collect())
    }
}

impl<A: Ord> Extend<Annotation<A>> for AnnotationSet<A> {
    fn extend<I: IntoIterator<Item = Annotation<A>>>(&mut self, iter: I) {
        let mut v = std::mem::take(&mut self.0).into_vec();
        v.extend(iter);
        *self = Self::from_vec(v);
    }
}

impl<'a, A: Ord + Clone + 'a> Extend<&'a Annotation<A>> for AnnotationSet<A> {
    fn extend<I: IntoIterator<Item = &'a Annotation<A>>>(&mut self, iter: I) {
        self.extend(iter.into_iter().cloned());
    }
}

impl<A> IntoIterator for AnnotationSet<A> {
    type Item = Annotation<A>;
    type IntoIter = std::vec::IntoIter<Annotation<A>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_vec().into_iter()
    }
}

impl<'a, A> IntoIterator for &'a AnnotationSet<A> {
    type Item = &'a Annotation<A>;
    type IntoIter = std::slice::Iter<'a, Annotation<A>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<A: Ord> From<BTreeSet<Annotation<A>>> for AnnotationSet<A> {
    fn from(set: BTreeSet<Annotation<A>>) -> Self {
        AnnotationSet(set.into_iter().collect::<Vec<_>>().into_boxed_slice())
    }
}

impl<A: Ord> From<AnnotationSet<A>> for BTreeSet<Annotation<A>> {
    fn from(set: AnnotationSet<A>) -> Self {
        set.into_iter().collect()
    }
}

impl<A: Ord> From<Vec<Annotation<A>>> for AnnotationSet<A> {
    fn from(v: Vec<Annotation<A>>) -> Self {
        Self::from_vec(v)
    }
}

impl<A: Ord, const N: usize> From<[Annotation<A>; N]> for AnnotationSet<A> {
    fn from(a: [Annotation<A>; N]) -> Self {
        Self::from_vec(a.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{AnnotationValue, Build, Literal, RcStr};

    fn ann(b: &Build<RcStr>, n: u32) -> Annotation<RcStr> {
        Annotation {
            ap: b.annotation_property(format!("http://e/p{}", n % 3)),
            av: AnnotationValue::Literal(Literal::Simple {
                literal: n.to_string(),
            }),
            ann: AnnotationSet::new(),
        }
    }

    #[test]
    fn behaves_as_btreeset() {
        let b = Build::new_rc();
        let mut set = AnnotationSet::new();
        let mut reference = BTreeSet::new();
        for n in [5, 3, 9, 3, 1, 7, 5, 0] {
            assert_eq!(set.insert(ann(&b, n)), reference.insert(ann(&b, n)));
        }
        for n in [3, 4, 0] {
            assert_eq!(set.remove(&ann(&b, n)), reference.remove(&ann(&b, n)));
        }
        assert_eq!(set.len(), reference.len());
        assert!(set.iter().eq(reference.iter()));
        assert!(set.contains(&ann(&b, 9)) && !set.contains(&ann(&b, 3)));
        let collected: AnnotationSet<RcStr> = reference.iter().cloned().rev().collect();
        assert_eq!(collected, set);
        assert_eq!(BTreeSet::from(set.clone()), reference);
        assert_eq!(format!("{set:?}"), format!("{reference:?}"));
    }

    #[test]
    fn orders_as_btreeset() {
        let b = Build::new_rc();
        let sets: Vec<Vec<u32>> = vec![vec![], vec![1], vec![1, 2], vec![2], vec![0, 9]];
        for x in &sets {
            for y in &sets {
                let (sx, sy): (AnnotationSet<RcStr>, AnnotationSet<RcStr>) = (
                    x.iter().map(|n| ann(&b, *n)).collect(),
                    y.iter().map(|n| ann(&b, *n)).collect(),
                );
                let (bx, by): (BTreeSet<_>, BTreeSet<_>) = (
                    x.iter().map(|n| ann(&b, *n)).collect(),
                    y.iter().map(|n| ann(&b, *n)).collect(),
                );
                assert_eq!(sx.cmp(&sy), bx.cmp(&by), "{x:?} {y:?}");
            }
        }
    }

    #[test]
    fn is_compact() {
        assert_eq!(
            std::mem::size_of::<AnnotationSet<RcStr>>(),
            2 * std::mem::size_of::<usize>()
        );
        let b = Build::new_rc();
        let set: AnnotationSet<RcStr> = [ann(&b, 1)].into();
        assert_eq!(set.as_slice().len(), 1);
    }
}
