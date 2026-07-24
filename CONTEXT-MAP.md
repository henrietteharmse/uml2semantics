# Context Map

This repo is split into three contexts, mirroring the package structure
under `src/main/scala/org/uml2semantics/`.

| Context | CONTEXT.md                                           | Scope |
| ------- | ---------------------------------------------------- | ----- |
| model   | `src/main/scala/org/uml2semantics/model/CONTEXT.md`  | UML domain model and builder caches: `UMLClass`, `UMLAttribute`, generalization sets, prefixes, datatypes |
| reader  | `src/main/scala/org/uml2semantics/reader/CONTEXT.md` | Input parsing: TSV and XMI readers that populate the caches |
| writer  | `src/main/scala/org/uml2semantics/writer/CONTEXT.md` | Output generation: OWL 2 writer, SHACL writer (in development) |

System-wide ADRs live in `docs/adr/`; context-specific ADRs live in each
context's `docs/adr/` directory.
