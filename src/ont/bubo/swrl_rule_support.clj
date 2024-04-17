(clojure.core/defn with-df [f]
  (f (owl-data-factory)))

(clojure.core/defn swrl-rule-set [head-set body-set]
  (.applyChange
   (owl-ontology-manager)
   (org.semanticweb.owlapi.model.AddAxiom.
    o
    (with-df
      #(.getSWRLRule %
        head-set
        body-set)))))

(clojure.core/defn swrl-rule [head body]
  (swrl-rule-set #{head} #{body}))



;; OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
;;         // Let's create an ontology and name it
;;         // "http://www.co-ode.org/ontologies/testont.owl" We need to set up a
;;         // mapping which points to a concrete file where the ontology will be
;;         // stored. (It's good practice to do this even if we don't intend to
;;         // save the ontology).
;;         IRI ontologyIRI = IRI.create("http://www.co-ode.org/ontologies/", "testont.owl");
;;         // Create a document IRI which can be resolved to point to where our
;;         // ontology will be saved.
;;         IRI documentIRI = IRI.create("file:/tmp/", "SWRLTest.owl");
;;         // Set up a mapping, which maps the ontology to the document IRI
;;         SimpleIRIMapper mapper = new SimpleIRIMapper(ontologyIRI, documentIRI);
;;         manager.getIRIMappers().add(mapper);
;;         // Now create the ontology - we use the ontology IRI (not the physical
;;         // IRI)
;;         OWLOntology ontology = manager.createOntology(ontologyIRI);
;;         OWLDataFactory factory = manager.getOWLDataFactory();
;;         // Get hold of references to class A and class B. Note that the ontology
;;         // does not contain class A or classB, we simply get referenc/mes to
;;         // objects from a data factory that represent class A and class B
;;         OWLClass classA = factory.getOWLClass(ontologyIRI + "#", "A");
;;         OWLClass classB = factory.getOWLClass(ontologyIRI + "#", "B");
;;         SWRLVariable var = factory.getSWRLVariable(ontologyIRI + "#", "x");
;;         SWRLRule rule =
;;             factory.getSWRLRule(Collections.singleton(factory.getSWRLClassAtom(classA, var)),
;;                 Collections.singleton(factory.getSWRLClassAtom(classB, var)));
;;         manager.applyChange(new AddAxiom(ontology, rule));
;;         OWLObjectProperty objProp = factory.getOWLObjectProperty(ontologyIRI + "#", "propA");
;;         OWLObjectProperty propB = factory.getOWLObjectProperty(ontologyIRI + "#", "propB");
;;         SWRLObjectPropertyAtom propAtom = factory.getSWRLObjectPropertyAtom(objProp, var, var);
;;         SWRLObjectPropertyAtom propAtom2 = factory.getSWRLObjectPropertyAtom(propB, var, var);
;;         Set<SWRLAtom> antecedent = new HashSet<SWRLAtom>();
;;         antecedent.add(propAtom);
;;         antecedent.add(propAtom2);
;;         SWRLRule rule2 = factory.getSWRLRule(antecedent, Collections.singleton(propAtom));
;;         manager.applyChange(new AddAxiom(ontology, rule2));
;;         // Now save the ontology. The ontology will be saved to the location
;;         // where we loaded it from, in the default ontology format
;;         manager.saveOntology(ontology);


