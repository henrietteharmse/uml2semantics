# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

uml2semantics converts UML class diagrams into OWL 2 ontologies. It accepts input as TSV files (for classes, attributes, enumerations) or XMI files (from tools like Enterprise Architect), and outputs OWL ontologies in RDF/XML format. The project assumes conceptual UML models (no interfaces, access modifiers, or operations).

## Build & Run Commands

- **Compile**: `sbt clean compile`
- **Build assembly JAR**: `sbt assembly` (outputs `target/scala-3.6.3/uml2semantics.jar`)
- **Run**: `java -jar target/scala-3.6.3/uml2semantics.jar [options]`
- **Manual test**: `./runTest.sh` (runs a sample XMI-to-OWL conversion)

There is no automated test suite (no ScalaTest/specs2). Testing is done manually via `runTest.sh` and comparing output OWL files against expected outputs in `src/test/resources/`.

## Tech Stack

- Scala 3.6.3, SBT 1.10.7
- OWL API 5.5.1 for ontology generation
- Apache Jena 5.3.0 for RDF support
- scopt for CLI argument parsing
- scala-csv for TSV reading
- Requires Java 11+

## Architecture

The pipeline follows a **Read → Cache → Write** pattern:

### Entry Point
`Main.scala` — parses CLI args via scopt, routes to appropriate reader(s), then invokes the OWL writer. Entry point is `@main def uml2owl`.

### Readers (`reader/`)
- **TSVReader** — reads tab-separated files for classes, attributes, enumerations
- **XMIReader** — parses XMI/XML using DOM + XPath (handles Enterprise Architect exports)
- **ReaderHelper** — shared logic for populating parent-child (generalization) relationships
- When both TSV and XMI are provided, the `--overrides` flag controls processing order (which source takes precedence)

### Model & Caches (`model/`, `model/cache/`)
Readers populate global mutable caches rather than returning data:
- **ClassBuilderCache / ClassIdentityBuilderCache** — stores UML classes
- **AttributeBuilderCache / AttributeIdentityBuilderCache** — stores UML attributes
- Key model types: `UMLClass`, `UMLAttribute`, `LabeledIRI`, `PrefixNamespace`, `SupportedDataType`
- `UMLClass` supports generalization sets with covering (complete/incomplete) and disjointness constraints

### Writer (`writer/`)
- **UML2OWLWriter** — reads from caches, generates OWL 2 ontology using OWL API. Handles all four generalization set constraint combinations (complete/incomplete × disjoint/overlapping).
- **UML2SHACLWriter** — SHACL output (in development)

### Utilities
- `inline/Code.scala` — debug/error logging with source location info via `sourcecode` library
- `Input.scala` — `InputParameters` case class and `Overrides` enum

## Key CLI Parameters

| Flag | Purpose |
|------|---------|
| `-c` | TSV classes file |
| `-a` | TSV attributes file |
| `-e` | TSV enumerations file |
| `-n` | TSV enumeration values file |
| `-m` | XMI input file |
| `-o` | Output OWL file (required) |
| `-i` | Ontology IRI (required) |
| `-p` | Ontology prefix as `name:iri` |
| `-x` | Additional prefixes (comma-separated CURIEs) |
| `--overrides` | `XMI` or `TSV` — which source overrides when both provided |
