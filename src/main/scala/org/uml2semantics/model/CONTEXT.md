# Context: model

The internal vocabulary for describing a UML class diagram, independent of input format (TSV/XMI) and output format (OWL/SHACL). Everything here lives in `org.uml2semantics.model` and its `cache` subpackage. Readers construct these objects through builders; writers consume them.

## Glossary

- **Class identity** (`UMLClassIdentity`) — how a class is known: an optional *name*, an optional *curie*, plus the *ontology prefix* used to mint an IRI when only a name is present. At least one of name or curie must be present; having both is the fully *enriched* form.
- **Name** (`UMLClassName`, `UMLAttributeName`) — a plain UML identifier. Must not contain a colon and must not be blank. A name mints its IRI under the ontology prefix.
- **Curie** (`Curie`) — a compact URI of the form `prefixName:reference` (W3C CURIE spec). Only valid when its prefix name is a configured *prefix namespace*. A curie resolves to an IRI by prefix expansion, bypassing ontology-prefix minting — this is how classes and attributes are mapped onto external vocabularies (e.g. `schema:Person`).
- **Name-or-curie rule** — an identifier string containing `:` is interpreted as a curie; otherwise as a name (`withNameOrCurie`). This is why names may not contain colons.
- **Enrichment** — a later input source supplying the missing half of an identity (e.g. class `Person` later gains curie `schema:Person`). The caches merge the old entry into the enriched one field-by-field: non-empty older values (definition, children, attribute type, multiplicity) are kept when the newer build leaves them empty, and attributes still pointing at the old class identity are migrated (`AttributeBuilderCache.updateClassIdentity`). Enrichment is the mechanism behind the `--overrides` precedence between TSV and XMI.
- **Ontology prefix** — the `PrefixNamespace` of the ontology being generated (CLI flag `-p`). Used to mint IRIs for name-only identities.
- **Prefix namespace** (`PrefixNamespace`) — a prefix name bound to a prefix IRI, registered in a global prefix map. `xsd`, `rdf`, `rdfs`, `owl` are predefined; others come from CLI flags `-p` and `-x`.
- **IRI minting** — for a class name: `<ontology-prefix-iri>/Name` (no extra `/` if the prefix IRI already ends in `/` or `#`). For an attribute name: `<ontology-prefix-iri>/ClassLabel#attributeName` — the class label as path segment, `#` as fragment separator. Curies expand directly and take precedence.
- **Labeled IRI** (`LabeledIRI`) — anything carrying both an IRI and a human-readable label. The label prefers the name over the curie.
- **Attribute identity** (`UMLAttributeIdentity`) — an attribute's name/curie scoped by the identity of its owning class. Attributes are always class-scoped; there is no global attribute namespace.
- **Attribute type** (`UMLAttributeType`) — one of: a *supported data type* (`UMLXMLDataType`), a class in the diagram (`UMLClassType`), or a curie denoting an external class (`CurieBasedUMLClassAttributeType`). A type string resolves in this order: supported-datatype curie → already-cached class → curie → a new placeholder class built on the fly.
- **Supported data type** (`SupportedDataType`) — the closed enum of XSD/RDF/RDFS datatypes admissible as attribute types, keyed by their curie literal (e.g. `xsd:string`, `rdf:langString`).
- **Cardinality** (`UMLCardinality`) — a non-negative integer or the unbounded marker `*` (`UMLInfinite`). A blank cardinality is *unspecified* and defaults to 1, per the UML spec. An unparsable cardinality string also defaults to 1, but is logged as a warning so the modeller can find the typo.
- **Multiplicity** (`UMLMultiplicity`) — a min and max cardinality pair, default `[1,1]`. Invariant: max ≥ min, where `*` is greater than any integer, and an unbounded min is never admissible. The constructor is private, so `apply` is the only way to build one and the invariant cannot be sidestepped — a case class would otherwise hand out a synthesized constructor that bypasses the check.
- **Unusable multiplicity** — a pair that breaks the invariant, either as `[*, n]` or as a max below its min. It costs the attribute its *cardinality restriction*, not its existence: the pair falls back to `[0,*]`, which asserts nothing about cardinality, and the attribute keeps its type, label and definition. Reported as `UnboundedMinMultiplicity` or `MaxBelowMin`. Repairing to the nearest valid pair is deliberately rejected — that would assert a constraint the modeller never wrote, and a wrong exact cardinality can make otherwise sound instance data inconsistent, whereas asserting nothing is merely weaker than intended.
- **Generalization set** (`UMLGeneralizationSet`) — the set of child classes sharing one parent, qualified by a *covering constraint* and a *disjoint constraint*.
- **Covering constraint** — `Complete` (the children exhaust the parent) or `Incomplete`.
- **Disjoint constraint** — `Disjoint` (children are pairwise disjoint) or `Overlapping`.
- **Builder-and-cache pattern** — model objects are constructed through builders, and every `build` writes both the object and its builder into a global mutable cache (`ClassBuilderCache`, `ClassIdentityBuilderCache`, `AttributeBuilderCache`, `AttributeIdentityBuilderCache`). A later source can fetch the builder, extend it, and rebuild — this is what makes enrichment work. The caches, not reader return values, are the hand-off point to the writer.

## Invariants and assumptions

- A class belongs to at most one generalization set — a simplifying assumption; UML allows several (`@Todo` in `ClassBuilder`).
- Generalization set names are not yet supported.
- Every identity must have a name or a curie; builders throw `IllegalArgumentException` otherwise.
- The default covering/disjoint constraints are {incomplete, overlapping} everywhere (`UMLGeneralizationSet`, `GeneralizationSetBuilder`, and the XMI reader's internal representation), matching the UML spec defaults of `isCovering = false` and `isDisjoint = false`.
