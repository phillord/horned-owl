// use std::{borrow::Borrow, rc::Rc, sync::Arc};

// fn copy<R:Borrow<str>, S:Borrow<str> + From<R>>(r:R) -> S {
//     r.into()
// }

// fn main(){
//     let r:Arc<str> = "hello".into();
//     let s:Rc<str> = copy(r);

//     dbg!(s);
// }


use std::{borrow::{Borrow}, collections::BTreeSet, rc::Rc, sync::Arc, cell::RefCell};

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct IRI<A: Borrow<str>>(A);

impl<A: Borrow<str>> Borrow<str> for IRI<A> {
    fn borrow(&self) -> &str {
        self.0.borrow()
    }
}


#[derive(Debug, Default)]
pub struct Build<A>(
    RefCell<BTreeSet<IRI<A>>>
)
where A: Borrow<str>;


fn add_to_build<A>(b: &mut Build<A>, iri:IRI<A>) -> bool
where A: Borrow<str> + Ord {
    b.0.borrow_mut().insert(iri)
}

impl<A: Borrow<str> + Clone + Ord + From<String>> Build<A> {
    fn iri<S: Borrow<str> + Clone>(&mut self, s: S) -> IRI<A>{
        let mut cache = self.0.borrow_mut();
        if let Some(iri) = cache.get(s.borrow()) {
            iri.clone()
        }
        else {
            let st = s.borrow().to_string();
            let iri = IRI(A::from(st));
            cache.insert(iri.clone());
            iri
        }
    }
}


fn main() {
    // We have ditched the "Rc" from build since we never used it -- I
    // actually have used a &Build where it might be relevant.
    let mut b_rc = Build(RefCell::new(BTreeSet::new()));
    let mut b_arc = Build(RefCell::new(BTreeSet::new()));

    let s = "hello".to_string();

    // We can add to build
    add_to_build(&mut b_rc, IRI(Rc::from(&s[..])));
    add_to_build(&mut b_arc, IRI(Arc::from(&s[..])));

    dbg!(&b_rc);
    dbg!(&b_arc);

    let _iri = b_rc.iri("hello");
    let _iri = b_rc.iri("goodbye");
    let _iri = b_arc.iri("arc goodbye");

    dbg!(&b_rc);

    // We don't need an Rc at all if we don't want
    let mut b_ref = Build(RefCell::new(BTreeSet::new()));
    add_to_build(&mut b_ref, IRI(&s[..]));


    // IRI is now threadable by choice
    let i_rc = IRI(Rc::from("hello"));
    let _i_rc2 = i_rc.clone();
    let i_arc = IRI(Arc::from("hello"));
    let i_arc2 = i_arc.clone();

    let thread1 = std::thread::spawn(move || {
        for _ in 0..5 {
            dbg!("thread1", &i_arc);
        }
    });

    let thread2 = std::thread::spawn(move || {
        for _ in 0..5 {
            dbg!("thread2", &i_arc2);
        }
    });

    thread1.join().unwrap();
    thread2.join().unwrap();
}
