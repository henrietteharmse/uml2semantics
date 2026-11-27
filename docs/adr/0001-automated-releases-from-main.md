# ADR 0001: Automated releases from `main`, archived on Zenodo

- Status: accepted
- Date: 2026-11-27
- Scope: system-wide

## Context

Until v0.0.5 every release was made by hand: tag a commit, run `sbt assembly`
locally, create a GitHub release and upload `uml2semantics.jar`. Five releases
were made this way between July 2023 and November 2026. The process had three
weaknesses.

1. Nothing ran the tests before a release. The repository had no continuous
   integration at all.
2. The version in `build.sbt` drifted from the tags: it still read `v0.0.2`
   when `v0.0.5` was released, because nothing connected the two.
3. Release notes were written in different styles from one release to the next.

A fourth need was new: releases should be archived on [Zenodo](https://zenodo.org)
so that each one has a citable DOI.

## Decision

A release is **declared by a pull request and performed by automation** when
that pull request merges into `main`.

### Version source of truth

`ThisBuild / version` in `build.sbt` is the single source of truth, written as
`MAJOR.MINOR.PATCH` without a `v` prefix. The git tag is `v` plus that version.
Versions follow Semantic Versioning; while the major version is `0`, a minor
bump marks new behaviour and a patch bump marks a fix.

### Release pull request

To release, a pull request

- sets the new version in `build.sbt`, and
- moves the `[Unreleased]` notes in `CHANGELOG.md` under a heading of the form
  `## [MAJOR.MINOR.PATCH] - YYYY-MM-DD - Release title`.

The `.github/scripts/release-metadata.sh` script reads both files and fails
when they disagree. The CI workflow runs it on every pull request, so a release
pull request cannot merge half-finished.

### Release workflow

`.github/workflows/release.yml` runs on every push to `main`. It asks the same
script whether a tag exists for the declared version.

- Tag exists: nothing happens. Ordinary merges are not releases.
- No tag: the tests run, the assembly jar is built, and a GitHub release is
  published carrying `uml2semantics.jar`, `uml2semantics-<version>.jar` and
  `SHA256SUMS`. Its title is the CHANGELOG heading's title and its body is the
  CHANGELOG section followed by GitHub's generated list of merged pull requests.
  Creating the release creates the tag at the merge commit.

The asset name `uml2semantics.jar` is kept unchanged because the README's
download link points at `releases/latest`.

The workflow never commits back to `main`. The release date is whatever the
pull request author writes in the CHANGELOG heading.

### Zenodo

Zenodo archives releases through its GitHub integration, which the maintainer
enables once for the repository on zenodo.org. From then on every published
GitHub release is archived automatically: Zenodo mints a DOI for the version
and a concept DOI shared by all versions. Record metadata comes from
`CITATION.cff`. No Zenodo token is stored in the repository.

## Consequences

- Tests gate every release, and the jar that is published is the jar that was
  tested.
- `build.sbt` can no longer drift from the tags, because the tag is derived
  from it.
- A release costs one pull request that edits two files. There is no tagging,
  no local build and no manual upload.
- Zenodo archives the source snapshot of the release, not the jar. The jar is
  reachable from the GitHub release that the Zenodo record links to. Should the
  binary itself need to be in the Zenodo record, a workflow step uploading it
  through the Zenodo REST API with a token stored as a repository secret would
  be the extension point.
- Releases are created with the workflow's `GITHUB_TOKEN`. Such releases fire
  repository webhooks, which is what Zenodo relies on, but do not trigger other
  GitHub Actions workflows. Any future automation that should run after a
  release therefore belongs in `release.yml` itself.
- The first release under this scheme is `0.1.0`, since `v0.0.5` added new
  behaviour and continuing at `0.0.x` would make the version meaningless.

## Alternatives considered

- **Release on every merge with an automatic patch bump.** Rejected: a
  documentation-only merge would become a release, and the version would stop
  carrying information. `v0.0.4` fixed an example image.
- **Tag-driven releases with the version derived from git** (sbt-dynver,
  sbt-ci-release). Rejected: it still requires a manual tagging step, which is
  what the automation set out to remove, and `build.sbt` would no longer state
  the version.
- **Conventional-commit tooling** (semantic-release, release-please). Rejected
  for a solo-maintained project with a handful of releases a year: it imposes a
  commit message convention and adds a bot that opens pull requests, for little
  gain over editing two files.
- **Zenodo REST API upload from the workflow.** Deferred, see Consequences. It
  would put the jar in the Zenodo record at the cost of a secret to manage and
  more workflow code.
