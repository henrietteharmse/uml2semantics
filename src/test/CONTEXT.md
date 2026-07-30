# Context: test

The vocabulary of the automated test suite. Tests are the one place that spans all three production contexts — they drive a reader, the caches, and a writer as a single pipeline — so their own terms live here rather than in any one of them.

## Glossary

- **Case** — a fixture isolating exactly one behaviour: the smallest input that provokes it, plus the single *golden* it must produce. Lives in `src/test/resources/cases/<input-format>/<group>/`, with every file prefixed by the case name.
- **Scenario** — a realistic model expressed in one or more input encodings, with one or more expected outputs, under `src/test/resources/scenarios/<name>/`. `employer` is one model with five XMI variants sharing a single TSV. *Avoid* using this word for a case: a scenario proves that many behaviours compose, a case proves that one behaviour is right.
- **Group** — a cluster of cases covering one axis of behaviour (identity and IRI minting, generalization, global disjointness, attribute type resolution, multiplicity, prefixes, CLI plumbing). One munit suite per group.
- **Golden** — the expected ontology for a case, written in OWL Functional Syntax and hand-authored by transcribing the mapping rules in the writer context *before* the code is run. *Avoid*: snapshot, baseline, recorded output — a golden is predicted, not captured, which is what makes it able to fail.
- **Matrix** — the enumeration of cases per input format that defines what "exhaustive" means for that format. A behaviour reachable from an input format but absent from the matrix is a coverage hole, and is recorded as such rather than left implicit.
- **Diagnostic assertion** — a case's claim about the severity and code of the diagnostics a malformed input produces. Cases assert severity and code, never message wording, so messages stay free to improve.

## Invariants and assumptions

- Comparison is semantic, not textual: a golden is loaded through the OWL API and compared as an axiom set, so serialization, prefix choices and axiom order are all irrelevant.
- A case asserts the *complete* axiom set, not a subset. Over-generation is a failure mode that matters here, because global disjointness adds axioms nobody asked for.
- Suites run sequentially (`Test / parallelExecution := false`), because the model caches and the prefix map are global mutable singletons. A shared base suite owns the reset, so a cache added later is reset everywhere at once.
- The four generalization-set constraint combinations are absent from the TSV matrix by construction, not by omission: TSV has no covering or disjointness columns, so TSV generalization sets are always {incomplete, overlapping}.

## The TSV matrix

Seven groups, one suite each, under `org.uml2semantics.cases.tsv`: identity and IRI minting,
generalization, global disjointness, attribute type resolution, multiplicity, prefixes, and CLI
plumbing. Together they are exhaustive for what TSV can express.

Behaviours deliberately absent, and why:

- The four generalization-set constraint combinations — TSV has no covering or disjointness
  columns, so only XMI can reach them.
- The writer's `[*,n]` branch — the model rejects an unbounded min and substitutes `[0,*]`, so the
  writer never sees one. The `UnboundedMinMultiplicity` diagnostic itself *is* covered, from the
  model.

## Flagged ambiguities

- "Integration test" covered both the whole-model end-to-end check and the per-behaviour checks. Resolved: the per-behaviour fixtures are **cases**, the whole-model ones are **scenarios**; both drive the same public entry point, so both are integration tests in the loose sense.
