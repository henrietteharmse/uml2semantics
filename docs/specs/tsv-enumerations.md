# TSV specification: enumerations and enumeration values

**Status: draft, not implemented.** The CLI already accepts `-e` and `-n`, but today both files are
ignored with an `EnumerationInputIgnored` warning. This document fixes the input format first, so
the model, reader and writer work can be driven from it. It covers *what the modeller writes*. How
an enumeration becomes OWL is the next step and is deliberately left out, apart from the points
listed under [Deferred to the translation step](#deferred-to-the-translation-step).

Terms used here (*name*, *curie*, *identity*, *enrichment*, *built on the fly*, *diagnostic*) are
the ones defined in the model and reader `CONTEXT.md` files.

## The two files

A UML enumeration is described by two TSV files, in the same way a class is described by a classes
file and an attributes file.

| Flag | File | One row per |
|------|------|-------------|
| `-e`, `--enumerations` | enumerations file | enumeration |
| `-n`, `--enumeration-values` | enumeration values file | value of an enumeration (a UML *enumeration literal*) |

Both files follow the conventions of the existing TSV input:

- The first line is a header. Columns are found by header name, so their order is free.
- Every column listed below must be present in the header, even when all of its cells are empty.
  Columns not listed are ignored.
- A cell may be empty unless a rule below says otherwise.

## Enumerations file (`-e`)

| Column | Meaning |
|--------|---------|
| `Name` | The UML name of the enumeration. Must not contain a colon. |
| `Curie` | A curie for the enumeration, when it should map onto an existing IRI instead of one minted under the ontology prefix. Its prefix must be configured with `-p` or `-x`. |
| `Definition` | Free text describing the enumeration. |

`Name` and `Curie` are the two halves of the enumeration's identity. At least one must be given.

Example:

| Name            | Curie | Definition                                  |
|-----------------|-------|---------------------------------------------|
| PaymentSchedule |       | The frequency at which an employee is paid. |

## Enumeration values file (`-n`)

| Column | Meaning |
|--------|---------|
| `Enumeration` | The enumeration the value belongs to, given as its name or its curie. Required. |
| `Name` | The UML name of the value. Must not contain a colon. |
| `Curie` | A curie for the value, when it should map onto an existing IRI. Its prefix must be configured with `-p` or `-x`. |
| `Definition` | Free text describing the value. |

`Name` and `Curie` are the two halves of the value's identity. At least one must be given.

Example:

| Enumeration     | Name    | Curie | Definition                                    |
|-----------------|---------|-------|-----------------------------------------------|
| PaymentSchedule | Daily   |       | The employee is paid at the end of each day.  |
| PaymentSchedule | Weekly  |       | The employee is paid at the end of each week. |
| PaymentSchedule | Monthly |       | The employee is paid on the last day of the month. |

## Using an enumeration as an attribute type

No new column is needed. The existing `ClassEnumOrPrimitiveType` column of the attributes file
takes the name or curie of an enumeration:

| Class        | Curie | Name            | ClassEnumOrPrimitiveType | MinMultiplicity | MaxMultiplicity | Definition |
|--------------|-------|-----------------|--------------------------|-----------------|-----------------|------------|
| Renumeration |       | paymentSchedule | PaymentSchedule          | 1               | 1               |            |

The enumeration and its values in these examples are those of `SimpleEmployerWithEnumExample`.

## Rules

### Identity

1. **Name-or-curie rule.** The `Enumeration` cell of a value row is read like the `Class` cell of an
   attribute row: a string containing `:` is a curie, anything else is a name.
2. **Either half refers to the same enumeration.** When an enumeration has both a name and a curie,
   value rows and attribute types may use either one. In the example below all three values belong
   to one enumeration:

   | Curie              | Name            | Definition                       |
   |--------------------|-----------------|----------------------------------|
   | emp:SalarySchedule | PaymentSchedule | Defines the frequency of payments |

   | Enumeration        | Curie       | Name   | Definition |
   |--------------------|-------------|--------|------------|
   | emp:SalarySchedule | emp:Daily   |        |            |
   | PaymentSchedule    |             | Weekly |            |
   | emp:SalarySchedule | emp:Monthly |        |            |

3. **A value is scoped by its enumeration.** A value name only has to be unique within its own
   enumeration. `Other` in `PaymentSchedule` and `Other` in `ContractType` are two different values.
   This mirrors attributes, which are scoped by their class.
4. **Repeated rows are enrichment, not duplicates.** Two rows for the same enumeration, or for the
   same value of the same enumeration, are merged exactly as repeated class rows are: a later row
   may supply the missing half of the identity or a definition. A row carrying both `Name` and
   `Curie` is what ties the two halves together.
5. **Enumerations and classes share one namespace.** A name or curie may identify a class or an
   enumeration, never both. This follows UML, where both are classifiers in the same package.
   Whichever use is read second is rejected. In practice an enumeration can appear neither in the
   `Parents` column of the classes file nor in the `Class` column of the attributes file.

### Completeness

6. **Enumeration built on the fly.** A value row whose `Enumeration` was never declared in the
   enumerations file still creates that enumeration, with no definition. This also covers supplying
   `-n` without `-e`. It mirrors a class named only as an attribute's owner. The enumeration is
   built after the value row succeeds, so a rejected row leaves no enumeration behind.
7. **Enumeration without values.** An enumeration with no value rows is accepted and reported as a
   warning. The tool cannot tell an omission from a model that is still being drafted, which is the
   same reasoning as for a blank attribute type. This also covers supplying `-e` without `-n`.
8. **An enumeration must be declared to be an enumeration.** A `ClassEnumOrPrimitiveType` cell that
   names something found in neither enumeration file is treated as a class, as it is today. The
   tool has no way to guess that an undeclared type was meant as an enumeration.

### Reading order

9. **Enumerations are read before attributes.** The files are read in the order classes,
   enumerations, enumeration values, attributes, whatever order the flags were given in. An
   attribute type then resolves as: supported datatype curie, known class, known enumeration, curie,
   placeholder class.
10. **Row order carries no meaning.** UML orders enumeration literals. This format does not record
    that order, and value rows of different enumerations may be interleaved.

### Not expressible

11. A value is a name with an optional definition. There is no column for an underlying code or
    initial value.
12. Enumerations own no attributes. An attribute row whose `Class` is an enumeration is rejected
    under rule 5.

## Diagnostics

Rows are handled as elsewhere in the TSV reader: a row that cannot be honoured is reported and
skipped, and reading continues. Existing codes are reused where the fault is the same. The two
codes marked *new* are proposals.

| Situation | Severity | Code |
|-----------|----------|------|
| Neither `Name` nor `Curie` given, for an enumeration or a value | Error | `BlankIdentity` |
| `Enumeration` cell of a value row is blank | Error | `BlankIdentity` |
| A `Name` contains a colon | Error | `NameContainsColon` |
| A curie uses a prefix not configured with `-p` or `-x` | Error | `UnresolvableCurie` |
| A name or curie is used for both a class and an enumeration (rule 5) | Error | `ClassifierKindConflict` (*new*) |
| An enumeration has no values (rule 7) | Warning | `EnumerationWithoutValues` (*new*) |
| A required column is missing from the header | Error on every row | `RowRejected` |

`EnumerationInputIgnored` is retired once the files are read.

## Deferred to the translation step

These are consequences of the format that the model and writer work must settle. They are listed so
that nothing here is mistaken for a decision already made.

- The OWL shape of an enumeration and of its values, and of an attribute typed by an enumeration.
- The IRI minted for a name-only value. Rule 3 implies it must be qualified by its enumeration, as
  an attribute IRI is qualified by its class.
- What an enumeration without values translates to.
- Whether enumerations take part in the default disjointness assumption.
- Reading enumerations from XMI, and how `--overrides` applies to them.
