# TSV specification: enumerations and enumeration values

**Status: implemented.** The TSV reader reads both files into the model as specified here, and the
OWL writer translates the result. This document covers *what the modeller writes*.
[Translation to OWL](#translation-to-owl) summarises what it becomes; the mapping itself is defined
in the writer `CONTEXT.md`.

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
   same reasoning as for a blank attribute type. This also covers supplying `-e` without `-n`. It
   is judged once every input has been read, so an enumeration that gets its values from an XMI
   file read in the same run is not reported.
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
codes marked *new* were added for enumerations.

| Situation | Severity | Code |
|-----------|----------|------|
| Neither `Name` nor `Curie` given, for an enumeration or a value | Error | `BlankIdentity` |
| `Enumeration` cell of a value row is blank | Error | `BlankIdentity` |
| A `Name` contains a colon | Error | `NameContainsColon` |
| A curie uses a prefix not configured with `-p` or `-x` | Error | `UnresolvableCurie` |
| A name or curie is used for both a class and an enumeration (rule 5) | Error | `ClassifierKindConflict` (*new*) |
| An enumeration has no values (rule 7) | Warning | `EnumerationWithoutValues` (*new*) |
| Two values of one enumeration would get the same IRI | Error | `EnumerationValueIRIConflict` |
| A required column is missing from the header | Error on every row | `RowRejected` |

A name with a colon or a curie with an unconfigured prefix is an error even when the other half of
the identity is fine. For a curie the classes file and the attributes file do the same: a curie with
an unconfigured prefix rejects the row even when the row also has a name.

`EnumerationInputIgnored` is retired: the files are read and translated.

## Translation to OWL

For the `PaymentSchedule` example above, in Manchester syntax:

```
Class: PaymentSchedule
  EquivalentTo: {Daily, Weekly, Monthly}
Individual: Daily
  Types: PaymentSchedule
Individual: Weekly
  Types: PaymentSchedule
Individual: Monthly
  Types: PaymentSchedule
DifferentIndividuals: Daily, Weekly, Monthly

ObjectProperty: paymentSchedule
  Domain: Renumeration
  Range: PaymentSchedule
```

- **Enumeration.** A class, with its IRI minted as a class IRI is, labelled and commented like a
  class, and equivalent to the one-of of its values.
- **Value.** A named individual of that class, with a label and, if it has a definition, a comment.
  The values of an enumeration are declared pairwise different.
- **IRI of a value.** A curie expands directly. A name-only value is minted under the ontology
  prefix and qualified by the label of its enumeration, as an attribute is by its class:
  `PaymentSchedule#Daily`. This is what makes rule 3 hold in the ontology. For an enumeration known
  only by a curie the label is the curie text, so that text appears in the IRI.
- **Value names that are not safe in an IRI.** A character of a value name is kept in the IRI when
  it is a letter or a digit, in any script, or one of `-`, `.`, `_` and `~`. Every other character
  becomes `_`, so the value `Hp 0` of `Signal` gets `Signal#Hp_0`. The label is the name as given.
  Two values of one enumeration may not end up with the same IRI, as `Hp 0` and `Hp_0` would; the
  row read second is rejected. These are rules 16 to 20 of
  [`xmi-enumerations.md`](xmi-enumerations.md), where the reason for them is given.
- **Attribute typed by an enumeration.** An object property ranging over the enumeration's class,
  with its multiplicity translated as for any object property.
- **Enumeration without values.** A class and nothing more. It is not made equivalent to an empty
  one-of, which is not valid OWL 2 and would say the class is empty.
- **Enumeration with one value.** The one-of is written; the different-individuals axiom is not,
  since it needs at least two.
- **Global disjointness.** Enumeration classes join the global disjointness axiom with the root
  classes, under the same `--no-global-disjointness` switch. Two enumerations given the same curie
  for a value share that individual, and declaring them disjoint would make the ontology
  inconsistent. Both are left out of the axiom, reported as `GlobalDisjointnessRelaxed`.

## Reading enumerations from XMI

Enumerations are also read from XMI, and `--overrides` applies to them as it does to classes. Both
are specified in [`xmi-enumerations.md`](xmi-enumerations.md).
