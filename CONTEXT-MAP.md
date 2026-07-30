# Context Map

Three production contexts mirror the package structure under
`src/main/scala/org/uml2semantics/`; a fourth covers the test suite, which
spans all three by driving reader, caches and writer as one pipeline.

| Context | CONTEXT.md                                           | Scope |
| ------- | ---------------------------------------------------- | ----- |
| model   | `src/main/scala/org/uml2semantics/model/CONTEXT.md`  | UML domain model and builder caches: `UMLClass`, `UMLAttribute`, generalization sets, prefixes, datatypes |
| reader  | `src/main/scala/org/uml2semantics/reader/CONTEXT.md` | Input parsing: TSV and XMI readers that populate the caches |
| writer  | `src/main/scala/org/uml2semantics/writer/CONTEXT.md` | Output generation: OWL 2 writer, SHACL writer (in development) |
| test    | `src/test/CONTEXT.md`                                | Test vocabulary: cases, scenarios, goldens, the per-format coverage matrix |

System-wide ADRs live in `docs/adr/`; context-specific ADRs live in each
context's `docs/adr/` directory.
