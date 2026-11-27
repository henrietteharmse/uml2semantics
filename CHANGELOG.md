# Changelog

All notable changes to uml2semantics are recorded here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and versions follow
[Semantic Versioning](https://semver.org/).

Releases are cut by automation. A pull request that bumps the version in
`build.sbt` and adds the matching section below becomes a GitHub release when it
is merged into `main`. The heading format is

    ## [MAJOR.MINOR.PATCH] - YYYY-MM-DD - Release title

The release title is optional and becomes the name of the GitHub release. The
section body becomes the release notes, followed by the list of merged pull
requests that GitHub generates.

## [Unreleased]

### Added

- Continuous integration on GitHub Actions: every pull request and push to
  `main` runs the test suite and builds the assembly jar.
- Automated releases: merging a version bump into `main` tags the commit and
  publishes a GitHub release carrying `uml2semantics.jar`, a versioned copy and
  a `SHA256SUMS` file.

## [0.0.5] - 2026-11-26 - Support for enumerations in both TSV and XMI formats

### Added

- Enumerations are read from both TSV and XMI input and translated to a class
  equivalent to the one-of of its values.

## [0.0.4] - 2026-02-28 - XMI support for classes and attributes

### Added

- XMI input alongside TSV, tested against Enterprise Architect 17.1 exports.
- Generalization sets with overlapping/disjoint and incomplete/complete
  settings.

## [0.0.3] - 2023-11-22

### Added

- Enumerations.

## [0.0.2] - 2023-08-28

### Changed

- Relaxed attribute types. Previously an attribute type had to be an XML
  Schema datatype or a UML class; it may now be a CURIE in a configured prefix,
  for example `foaf:Document` with `-x "foaf:http://xmlns.com/foaf/0.1/"`.
- An attribute no longer needs a type, as for the DCAT v2 `dct:relation`
  attribute of `dcat:Relationship`.

## [0.0.1] - 2023-07-24

First release of uml2semantics.

[Unreleased]: https://github.com/henrietteharmse/uml2semantics/compare/v0.0.5...HEAD
[0.0.5]: https://github.com/henrietteharmse/uml2semantics/compare/v0.0.4...v0.0.5
[0.0.4]: https://github.com/henrietteharmse/uml2semantics/compare/v0.0.3...v0.0.4
[0.0.3]: https://github.com/henrietteharmse/uml2semantics/compare/v0.0.2...v0.0.3
[0.0.2]: https://github.com/henrietteharmse/uml2semantics/compare/v0.0.1...v0.0.2
[0.0.1]: https://github.com/henrietteharmse/uml2semantics/releases/tag/v0.0.1
