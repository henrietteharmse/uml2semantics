# Context: test

The vocabulary of the automated test suite. Tests are the one place that spans all three production contexts — they drive a reader, the caches, and a writer as a single pipeline — so their own terms live here rather than in any one of them.

## Glossary

- **Case** — a fixture isolating exactly one behaviour: the smallest input that provokes it, plus the single *golden* it must produce. Lives in `src/test/resources/cases/<input-format>/<group>/`, with every file prefixed by the case name. A case may combine formats: `runCase` passes whichever of the case's TSV files and XMI file (`<case>.xml`) exist, and the case lives under the format whose reader it is about.
- **Scenario** — a realistic model expressed in one or more input encodings, with one or more expected outputs, under `src/test/resources/scenarios/<name>/`. `employer-generalization` is one model with five XMI variants and one set of TSV files. *Avoid* using this word for a case: a scenario proves that many behaviours compose, a case proves that one behaviour is right.
- **Expected ontology** — what one input of a scenario translates to. For the export `input/xmi/sparx/<model>.xml` it is `scenarios/<name>/expected/<model>.owl`; for the TSV files under `input/tsv` it is `scenarios/<name>/expected/tsv/<name>.owl`. Unlike a golden it is captured, not predicted: it is written by the tool and then reviewed. `ScenarioSuite` finds the expected ontologies and the TSV inputs on disk, so an expected ontology cannot be added without being checked, and TSV files cannot be added without one.
- **Group** — a cluster of cases covering one axis of behaviour (identity and IRI minting, generalization, global disjointness, attribute type resolution, multiplicity, prefixes, enumerations, CLI plumbing). One munit suite per group.
- **Model assertion** — a case's claim about what the reader left in the model caches, made in addition to its golden. Used by the enumeration groups (through the shared `EnumerationAssertions`) for what the ontology cannot show: which half of an identity a row supplied, or that two rows became one value, once only the resulting IRI and label are written.
- **Golden** — the expected ontology for a case, written in OWL Functional Syntax and hand-authored by transcribing the mapping rules in the writer context *before* the code is run. *Avoid*: snapshot, baseline, recorded output — a golden is predicted, not captured, which is what makes it able to fail.
- **Matrix** — the enumeration of cases per input format that defines what "exhaustive" means for that format. A behaviour reachable from an input format but absent from the matrix is a coverage hole, and is recorded as such rather than left implicit.
- **Diagnostic assertion** — a case's claim about the severity and code of the diagnostics a malformed input produces. Cases assert severity and code, never message wording, so messages stay free to improve.

## Invariants and assumptions

- Comparison is semantic, not textual: a golden is loaded through the OWL API and compared as an axiom set, so serialization, prefix choices and axiom order are all irrelevant.
- A case asserts the *complete* axiom set, not a subset. Over-generation is a failure mode that matters here, because global disjointness adds axioms nobody asked for.
- Suites run sequentially (`Test / parallelExecution := false`), because the model caches and the prefix map are global mutable singletons. A shared base suite owns the reset, so a cache added later is reset everywhere at once.
- The four generalization-set constraint combinations are absent from the TSV matrix by construction, not by omission: TSV has no covering or disjointness columns, so TSV generalization sets are always {incomplete, overlapping}.

## The TSV matrix

Eight groups, one suite each, under `org.uml2semantics.cases.tsv`: identity and IRI minting,
generalization, global disjointness, attribute type resolution, multiplicity, prefixes,
enumerations, and CLI plumbing. Together they are exhaustive for what TSV can express.

The enumeration group follows `docs/specs/tsv-enumerations.md` rule by rule for reading, and the
*enumeration translation* in the writer context for its goldens. It also carries the enumeration
side of IRI minting and of global disjointness, rather than spreading enumerations over the
prefixes and disjointness groups.

Behaviours deliberately absent, and why:

- The four generalization-set constraint combinations — TSV has no covering or disjointness
  columns, so only XMI can reach them.
- Enriching the values of two partial enumerations when a later row ties them together — the only
  row that can tie the halves is in the enumerations file, which is read before any value exists.
  The cache merges values regardless, for the day a second source can supply enumerations.
- The writer's `[*,n]` branch — the model rejects an unbounded min and substitutes `[0,*]`, so the
  writer never sees one. The `UnboundedMinMultiplicity` diagnostic itself *is* covered, from the
  model.

## The XMI matrix

Three groups so far, one suite each, under `org.uml2semantics.cases.xmi`: enumerations,
associations and overrides. The first two follow their specifications rule by rule,
`docs/specs/xmi-enumerations.md` and `docs/specs/xmi-associations.md`, including the rules for
combining XMI with TSV. The override group has no specification: it pins what the TSV classes file does to a
class the XMI file already has, how the children the two sources give a class combine, and that
a generalization and the range of a property follow a class or an enumeration that a TSV file
gives a curie. These are the *class merge* and *reference follows identity* of the model context.

A fixture is a hand-written file in the shape of an Enterprise Architect export, trimmed to what
the reader looks at, with a comment naming the rule it isolates. It is not an export: a case has to
be small enough to read, and an export of one class is several hundred lines.

The translation of an enumeration is pinned by the TSV enumeration group; the XMI goldens show that
an enumeration read from XMI comes out the same. An association becomes an attribute and nothing
else, so its golden is the one the equivalent row of a TSV attributes file would have.

The association suite also reads the real exports under `scenarios` and asserts, on the model, the
attributes the specification says their associations become. Those are model assertions and not
goldens, so they do not depend on a scenario's expected files.

Behaviours deliberately absent, and why:

- Classes, generalizations, generalization sets and attributes read from XMI have no cases of
  their own yet. The override group reads one class with a definition and two children, and the
  scenarios cover the rest. This is a coverage hole, not a decision.
- The real-world export (`cases/xmi/EA-export-xmi-2.1.mod.xml`, 26 MB) is not read by any test. Its
  numbers in the specifications come from running the tool on it by hand.
- A supporting attribute supplied by TSV under `--overrides XMI`. The rule is stated in the
  associations specification but no case isolates it yet, so this is a coverage hole as well.

## The scenarios

One suite, `org.uml2semantics.scenarios.ScenarioSuite`, with one test per expected ontology. Each
translates one input of a scenario, expects a run without diagnostics, and compares the result with
the expected ontology as an axiom set. The arguments are the ones the expected ontologies were
written with:

- An export is translated with the ontology IRI `https://uml2semantics.com/examples/employer` and
  the default ontology prefix, which is what `runTest.sh` uses.
- The TSV files are translated with the ontology IRI `http://uml2semantics.org/examples/employer/v.0.1`
  and the ontology prefix `emp:http://uml2semantics.org/examples/employer#`, which is what the
  README uses for TSV. The prefix is needed: the TSV files of `employer-generalization` use `emp`
  in curies.

A scenario's export and its TSV files are not the same model, so their expected ontologies are not
compared with each other. In `simple-employer`, for one, the export names an association
`isEmployedBy` and the TSV names the attribute `employedBy`.

When a test fails after a deliberate change to the translation, the expected ontology is written
again by the tool with those arguments and the difference is reviewed. A failure nobody intended is
a regression.

## Flagged ambiguities

- "Integration test" covered both the whole-model end-to-end check and the per-behaviour checks. Resolved: the per-behaviour fixtures are **cases**, the whole-model ones are **scenarios**; both drive the same public entry point, so both are integration tests in the loose sense.
