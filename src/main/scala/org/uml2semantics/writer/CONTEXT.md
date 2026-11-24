# Context: writer

Mapping the cached UML model to semantic-web artifacts. `UML2OWLWriter` (working) produces an OWL 2 ontology in RDF/XML via the OWL API; `UML2SHACLWriter` (stub, in development) will produce a SHACL shapes graph. Writers receive snapshots (`Set[UMLClass]`, `Set[UMLAttribute]`, `Set[UMLEnumeration]`) taken from the caches by `translate`.

## Glossary — the UML→OWL mapping

- **Class translation** — a UML class becomes a declared `owl:Class`, annotated with `rdfs:label` (the class label, falling back to the IRI string when the label is empty) and `rdfs:comment` for its definition.
- **Attribute translation** — depends on the attribute type:
  - *supported data type* → an `owl:DatatypeProperty` with the owning class as domain and the datatype as range;
  - *class or curie type* → an `owl:ObjectProperty` with the owning class as domain and the type class as range;
  - *untyped* → an `owl:ObjectProperty` with a domain but no range;
  - *enumeration type* → an `owl:ObjectProperty` with the owning class as domain and the enumeration's class as range.
  Properties get the same label/definition annotations as classes.
- **Enumeration translation** — a UML enumeration becomes a declared `owl:Class`, labelled and commented like a class and made equivalent to the `ObjectOneOf` of its values. Each *enumeration value* becomes a declared `owl:NamedIndividual` with a class assertion to that class, a label, and its definition as `rdfs:comment`. The values are declared pairwise different in one `DifferentIndividuals` axiom, since OWL makes no unique name assumption. The class assertion is entailed by the equivalence but is stated, so it is visible without a reasoner.
  - *no values* → the class only. An empty one-of is not valid OWL 2 and would assert that the class is empty, which a model still being drafted does not mean.
  - *one value* → the equivalence, but no `DifferentIndividuals`, which needs at least two.
  - *individuals rather than literals* — the alternative, a datatype defined as a one-of of strings, could not carry a curie, a label or a definition.
- **Multiplicity translation** — a subclass-of-restriction axiom on the domain class:
  - `[n,n]`, n ≠ 0 → exact cardinality n;
  - `[m,n]`, m > 0 → intersection of min m and max n;
  - `[0,n]` → max n only;
  - `[n,*]`, n > 0 → min n only;
  - `[0,*]` and `[*,*]` → no restriction;
  - `[*,n]` → unreachable. The model rejects an unbounded min and substitutes `[0,*]`, so the writer only ever sees usable multiplicities; the branch survives because the compiler cannot prove that. It reports rather than ignoring: an impossible state that silently weakened the output would be invisible, which is the failure mode the diagnostics exist to prevent. Validating in the model rather than here is deliberate — the model is where the modeller's own input is, so the diagnostic can name the file and row.
  Object properties restrict over `owl:Thing`; data properties restrict over the range datatype.
- **Generalization set translation** — every child gets a `SubClassOf(child, parent)` axiom, plus per constraint combination:
  - *complete disjoint* → `DisjointUnion(parent, children)`;
  - *complete overlapping* → `EquivalentClasses(parent, UnionOf(children))`;
  - *incomplete disjoint* → `DisjointClasses(children)`;
  - *incomplete overlapping* → subclass axioms only.
- **Default disjointness assumption** — **classes with no parents and no shared descendants, together with enumerations that share no value, are declared mutually disjoint**, in one global `DisjointClasses` axiom. This is a deliberate modeling decision: UML classes are taken to denote disjoint sets unless the hierarchy says otherwise. `--no-global-disjointness` switches the assumption off entirely, for models that never intended it.
  - *No parents* — a class with a parent is left out. Disjointness within a hierarchy is the generalization set's business, and a class is never disjoint from its own superclass. Having *children* does not exclude a class: that says nothing about how it relates to classes outside its own hierarchy.
  - *No shared descendants* — two classes declared disjoint cannot have a common subclass, or that subclass is unsatisfiable, and two roots can share one. Where a class has more than one root among its ancestors, all of those roots are dropped, reported as `GlobalDisjointnessRelaxed`. Descendants rather than children, because a root can contradict another through a grandchild. All of them rather than the one that would suffice, because which to keep would be arbitrary. Deep single inheritance is untouched — a chain has exactly one root, and one class cannot contradict itself.
  - *Enumerations* — the class of every enumeration is a root: an enumeration has no parents, a value is never an instance of a UML class, and a value belongs to one enumeration. The exception is two enumerations given the same curie for a value. They share that individual, so declaring them disjoint would make the ontology inconsistent; all enumerations sharing a value are dropped from the axiom, reported as `GlobalDisjointnessRelaxed`. Name-only values cannot collide, being qualified by their enumeration. Because the one axiom spans classes and enumerations, it is written after both have been translated.
- **Error accumulation** — failed axiom additions are recorded instead of aborting, so a dropped axiom still leaves the rest of the ontology written. `generateOWL` returns `Seq[Diagnostic]`: a failed axiom becomes an `AxiomAdditionFailed` error, and a failure to write the file becomes `OntologySaveFailed`. See the reader context for what a diagnostic is.
- **Output serialization** — RDF/XML (`RDFXMLDocumentFormat`), written to the file given by `-o`, with the ontology IRI from `-i`.

## Invariants and assumptions

- The writer is a pure consumer: it never mutates the model caches, and all identity/IRI decisions were made in the model context — the writer just calls `getIRI`/`getLabel`.
- Expected outputs for manual testing live in `src/test/resources/`; `runTest.sh` exercises an XMI-to-OWL translation end to end.
- Disjoint-classes and equivalent-to-union axioms require at least two classes; smaller sets are logged as warnings and skipped.
