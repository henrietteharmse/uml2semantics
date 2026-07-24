# Context: writer

Mapping the cached UML model to semantic-web artifacts. `UML2OWLWriter` (working) produces an OWL 2 ontology in RDF/XML via the OWL API; `UML2SHACLWriter` (stub, in development) will produce a SHACL shapes graph. Writers receive snapshots (`Set[UMLClass]`, `Set[UMLAttribute]`) taken from the caches by `Main`.

## Glossary — the UML→OWL mapping

- **Class translation** — a UML class becomes a declared `owl:Class`, annotated with `rdfs:label` (the class label, falling back to the IRI string when the label is empty) and `rdfs:comment` for its definition.
- **Attribute translation** — depends on the attribute type:
  - *supported data type* → an `owl:DatatypeProperty` with the owning class as domain and the datatype as range;
  - *class or curie type* → an `owl:ObjectProperty` with the owning class as domain and the type class as range;
  - *untyped* → an `owl:ObjectProperty` with a domain but no range.
  Properties get the same label/definition annotations as classes.
- **Multiplicity translation** — a subclass-of-restriction axiom on the domain class:
  - `[n,n]`, n ≠ 0 → exact cardinality n;
  - `[m,n]`, m > 0 → intersection of min m and max n;
  - `[0,n]` → max n only;
  - `[n,*]`, n > 0 → min n only;
  - `[0,*]` and `[*,*]` → no restriction;
  - `[*,n]` → a multiplicity error (unbounded min is meaningless).
  Object properties restrict over `owl:Thing`; data properties restrict over the range datatype.
- **Generalization set translation** — every child gets a `SubClassOf(child, parent)` axiom, plus per constraint combination:
  - *complete disjoint* → `DisjointUnion(parent, children)`;
  - *complete overlapping* → `EquivalentClasses(parent, UnionOf(children))`;
  - *incomplete disjoint* → `DisjointClasses(children)`;
  - *incomplete overlapping* → subclass axioms only.
- **Default disjointness assumption** — classes that participate in no generalization set are declared mutually disjoint in one global `DisjointClasses` axiom. This is a deliberate modeling decision: UML classes are taken to denote disjoint sets unless a generalization set says otherwise.
- **Error accumulation** — failed axiom additions append messages to an error list instead of aborting; `generateOWL` returns `Either[String, ListBuffer[String]]` — `Left` for a storage (save) failure, `Right` carrying accumulated per-axiom errors.
- **Output serialization** — RDF/XML (`RDFXMLDocumentFormat`), written to the file given by `-o`, with the ontology IRI from `-i`.

## Invariants and assumptions

- The writer is a pure consumer: it never mutates the model caches, and all identity/IRI decisions were made in the model context — the writer just calls `getIRI`/`getLabel`.
- Expected outputs for manual testing live in `src/test/resources/`; `runTest.sh` exercises an XMI-to-OWL conversion end to end.
- Disjoint-classes and equivalent-to-union axioms require at least two classes; smaller sets are logged as warnings and skipped.
