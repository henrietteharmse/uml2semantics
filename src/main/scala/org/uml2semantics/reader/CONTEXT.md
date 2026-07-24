# Context: reader

Turning input representations of a UML class diagram into cached model objects. A reader never returns data — it populates the global builder caches in the model context as a side effect, and `Main` decides which readers run and in what order.

## Glossary

- **Reader** (`UMLClassDiagramReader`) — the contract `parseUMLClassDiagram(input: InputParameters): Unit`: parse one input format and populate the model caches. The two implementations are `TSVReader` and `XMIReader`.
- **TSV input** — tab-separated files with fixed, named header columns. The *classes file* (`-c`) has columns Name, Curie, Definition, Parents; the *attributes file* (`-a`) has Class, Curie, Name, ClassEnumOrPrimitiveType, MinMultiplicity, MaxMultiplicity, Definition. A cell may be empty; Name and Curie are the two halves of an identity (see *enrichment* in the model context).
- **Parents column** — `|`-separated list of parent identifiers (names or curies) in the classes TSV. Each listed parent acquires the row's class as a child in its generalization set.
- **ClassEnumOrPrimitiveType** — the attribute-type column; resolved via the model's attribute-type resolution order (datatype curie, known class, curie, placeholder class).
- **Enumerations input** — CLI flags (`-e`, `-n`) and header enums (`EnumerationsHeader`, `EnumerationValuesHeader`) exist, but enumeration parsing is not implemented yet.
- **XMI input** — an Enterprise Architect XMI export (`-m`), parsed with DOM + XPath. Classes come from `packagedElement[@type='uml:Class']`; class definitions, attributes (with type, bounds, and documentation) come from EA's `<element>` extension section; covering/disjointness comes from the `generalizationSet` node's `isCovering`/`isDisjoint` attributes.
- **Two-pass parent population** — pass one reads all classes/rows, building each class and collecting a parent→children map; pass two (`populateParentsWithTheirChildren`, in `ReaderHelper` for TSV and inlined in `XMIReader`) fetches each parent's cached builder, attaches its generalization set, and rebuilds it. A parent referenced but never declared as a class is built on the fly.
- **Generalization set merge rule** (XMI) — generalizations pointing at the same parent are merged into one generalization set only if their covering/disjoint flags agree; a mismatch is an error.
- **Override precedence** (`--overrides`, `Overrides` enum) — when both TSV and XMI are provided, the overriding source is parsed *last*, so its values win during cache enrichment while empty fields inherit from the earlier source. Default: TSV overrides XMI.
- **Row-level error handling** — a row or class that fails to parse is logged and skipped; parsing continues. There is no fail-fast mode.

## Invariants and assumptions

- Readers communicate exclusively through the model caches; order of reader invocation is therefore semantically significant.
- The TSV format has no columns for covering/disjoint constraints — generalization sets from TSV always get the UML spec defaults {incomplete, overlapping}. Only XMI input can express `Complete`/`Disjoint` variations.
- XMI parsing depends on Enterprise Architect's export dialect (the `<element>` extension section); XMI from other tools is untested.
- `parseClassAttributes` in `TSVReader` is marked `@TODO: Implement this method and test` — it has an implementation but has not been verified.
