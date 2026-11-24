# XMI specification: enumerations

**Status: implemented.** The XMI reader reads enumerations and their values into the model as
specified here. What it did before is kept under
[Before this was implemented](#before-this-was-implemented).

This document covers *what is read from an XMI file* and what it becomes in the model. The
translation of the model to OWL is unchanged and is the one described in
[`tsv-enumerations.md`](tsv-enumerations.md) and the writer `CONTEXT.md`. Wherever a rule here has
a counterpart in the TSV specification it says so, because the aim is that an enumeration means the
same thing whichever input it came from.

Terms used here (*name*, *curie*, *identity*, *enrichment*, *built on the fly*, *diagnostic*) are
the ones defined in the model and reader `CONTEXT.md` files.

## The two examples

Both are Enterprise Architect exports of XMI 2.1.

| | Initial example | Real-world example |
|---|---|---|
| File | `src/test/resources/scenarios/simple-employer/input/xmi/sparx/SimpleEmployerWithEnumExample.xml` | `src/test/resources/cases/xmi/EA-export-xmi-2.1.mod.xml` |
| Size | 40 KB | 26 MB |
| Classes | 4 | 2000 |
| Enumerations | 1 (`PaymentSchedule`) | 203, spread over 23 packages |
| Values | 3 (`Daily`, `Weekly`, `Monthly`) | 1619, from 0 to 166 per enumeration |
| Enumerations with a definition | 0 | 9 |
| Values with a definition | 0 | 0 |
| Attributes typed by an enumeration | 0 | 200, on 200 classes, over 195 enumerations |
| Enumerations nothing refers to | 0 | 8 |
| Enumerations without values | 0 | 1 (`Enumeration1`) |
| Classes that specialise an enumeration | 0 | 7 |
| Associations ending at an enumeration | 1, named `paymentSchedule` | 0 |

The initial example uses its enumeration through an association only: `paymentSchedule`, drawn
from `Renumeration` to `PaymentSchedule`. `Renumeration` has no attributes.

The real-world example is the PlanPro interface schema, a model built for XML Schema generation.
That shows in its conventions: an enumeration `ENUMBandbreite` is wrapped by a class
`TCBandbreite` whose single attribute `Wert` is typed by the enumeration.

## What is read

An enumeration and its values are read from the standard UML part of the file. Their definitions
are read from Enterprise Architect's extension part, as class and attribute definitions are today.

| Model | Read from |
|-------|-----------|
| Enumeration | every `packagedElement` with `xmi:type="uml:Enumeration"`, at any package depth |
| Name or curie of the enumeration | its `name` attribute |
| Value | every `ownedLiteral` child of that element with `xmi:type="uml:EnumerationLiteral"` |
| Name or curie of the value | the `name` attribute of the `ownedLiteral` |
| Definition of the enumeration | `properties/@documentation` of the extension `element` with the same `xmi:idref` and `xmi:type="uml:Enumeration"` |
| Definition of the value | `documentation/@value` of the extension `attribute` whose `xmi:idref` is the literal's `xmi:id` |

In the initial example:

```xml
<packagedElement xmi:type="uml:Enumeration" xmi:id="EAID_67A26A2B_…" name="PaymentSchedule" visibility="public">
  <ownedLiteral xmi:type="uml:EnumerationLiteral" xmi:id="EAID_ED73F133_…" name="Daily" visibility="public"/>
  <ownedLiteral xmi:type="uml:EnumerationLiteral" xmi:id="EAID_A8FB95BC_…" name="Weekly" visibility="public"/>
  <ownedLiteral xmi:type="uml:EnumerationLiteral" xmi:id="EAID_57A830FA_…" name="Monthly" visibility="public"/>
</packagedElement>
```

Because the structure comes from the standard part, an XMI file from another tool yields its
enumerations and values too, only without definitions. That is untested.

## Using an enumeration as an attribute type

Nothing new is read. The XMI reader already takes the type of an attribute as a name, from
`properties/@type` of the attribute in the extension part, and resolves it through the model's
attribute-type resolution: supported datatype curie, known class, known enumeration, curie,
placeholder class. Because enumerations are read before attributes (rule 9), a type name that is
the name of an enumeration resolves to that enumeration.

The initial example has no such attribute; its enumeration is used through an association
(rule 14). In the real-world example:

```xml
<attribute xmi:idref="EAID_F7EA2AC4_…" name="Wert" scope="Private">
  <properties type="ENUMAussenelementansteuerung_Art" …/>
  <bounds lower="1" upper="1"/>
</attribute>
```

The standard part also gives the type, as a reference by `xmi:id`. In all 200 cases of the
real-world example the name in the extension part is the name of the enumeration that reference
points at, so resolving by name loses nothing there.

## Rules

### Identity

1. **Name-or-curie rule.** The `name` of an enumeration or of a literal is read as the `name` of an
   XMI class is: a string containing `:` is a curie, whose prefix must be configured with `-p` or
   `-x`, and anything else is a name. An XMI file can therefore supply only one half of an
   identity. The other half comes from TSV; see [Combining XMI with TSV](#combining-xmi-with-tsv).
2. **The package is not part of the identity.** Enumerations are read from every package, and two
   enumerations with the same name in different packages are one enumeration holding the values of
   both, without a diagnostic. This is how same-named XMI classes behave today. It does not occur
   in either example: the 203 enumeration names of the real-world example are all different.
3. **A value is scoped by its enumeration.** Same as TSV rule 3. The real-world example relies on
   it: 103 of its value names occur in more than one enumeration, and `sonstige` occurs in 120.
   The initial example has one enumeration and does not show it.
4. **A literal name repeated within one enumeration is one value.** Same as TSV rule 4. It does not
   occur in either example.
5. **Enumerations and classes share one namespace.** Same as TSV rule 5: a name or curie identifies
   a class or an enumeration, never both, and whichever use is read second is rejected. Given the
   reading order of rule 9, an enumeration whose name a class already holds is rejected together
   with its values. Neither example has such a clash.

### Completeness

6. **Enumeration without values.** Accepted and reported as a warning, as in TSV rule 7.
   `Enumeration1` in the real-world example is the one instance.
7. **An enumeration nothing refers to is still read.** It is translated whether or not an attribute
   is typed by it. This covers 8 enumerations in the real-world example.
8. **A type name that matches no enumeration is treated as a class**, as it is today and as in TSV
   rule 8.

### Reading order

9. **Classes, then enumerations with their values, then attributes.** This is the order of TSV
   rule 9. The XMI reader used to read each class together with its attributes; that is now two
   passes with the enumerations between them. Document order carries no meaning.
10. **The order of literals is not kept.** Same as TSV rule 10. Enterprise Architect records a
    position for each literal; it is not read.

### Not read

11. **A value is a name with an optional definition.** The initial value of a literal, its
    stereotype, its visibility and Enterprise Architect's `IsLiteral` and `union` flags are
    ignored. Neither example gives a literal an initial value. The `enum` stereotype is not a
    reliable marker in any case: the initial example puts it on all 3 literals, the real-world
    example on 42 of 1619.
12. **Enumerations own no attributes.** An `ownedAttribute` of an enumeration is rejected, as an
    attribute row whose `Class` is an enumeration is under TSV rule 12. It does not occur in either
    example.
13. **Enumerations take no part in generalization.** A generalization whose parent or child is an
    enumeration is rejected under rule 5; the class or enumeration at the other end is kept. The
    real-world example has 7, each a class stereotyped `XSDsimpleType` specialising an enumeration,
    such as `TLinksRechts` under `ENUMLinksRechts`. Specialising an enumeration is left to a later
    iteration.
14. **An association ending at an enumeration is not read here.** How associations are read is
    specified in [`xmi-associations.md`](xmi-associations.md). One thing is fixed here: an
    association ending at an enumeration gives an attribute typed by that enumeration. The initial
    example depends on it. Its association `paymentSchedule` from `Renumeration` to
    `PaymentSchedule`, with `1` at the `PaymentSchedule` end, is the only use of the enumeration,
    and gives the attribute `paymentSchedule` of `Renumeration` with `[1,1]`.
15. **Nested enumerations are not read.** An enumeration owned by a class instead of a package is
    exported as a `nestedClassifier`. It does not occur in either example.

## The IRI of a value

The real-world example is the reason for this section. 223 of its 1619 value names are not plain
identifiers, and a name such as `Hp 0` or `PZB/LZB` written into an IRI as it stands gives an IRI
that is invalid or misleading. Class, attribute and enumeration names in that file are all plain
identifiers, so values are the one place it happens.

16. **Unsafe characters in a value name become `_` in its IRI.** A character is kept when it is a
    letter or a digit, in any script, or one of `-`, `.`, `_` and `~`. Every other character is
    replaced by `_`, one for one. The rest of the IRI is minted as before: under the ontology
    prefix, qualified by the label of the enumeration.
17. **The label is the name as given.** Only the IRI changes. The value keeps its name in the
    model, and that name is its `rdfs:label`.
18. **A curie is not altered.** A value given by a curie expands to exactly the IRI the modeller
    wrote.
19. **Two values of one enumeration may not end up with the same IRI.** `Hp 0` and `Hp_0` in the
    same enumeration would. The one read second is rejected. Values of different enumerations
    cannot clash, because the enumeration qualifies the IRI.
20. **The rule belongs to the model, not to the XMI reader.** A value name read from the values
    file of the TSV input is minted the same way, and the paragraph on the IRI of a value in
    [`tsv-enumerations.md`](tsv-enumerations.md) says so. Names of classes, attributes and
    enumerations are minted as written.

With an ontology prefix that ends in `#`:

| Name | IRI ends in | Label |
|------|-------------|-------|
| `Daily` | `PaymentSchedule/Daily` | `Daily` |
| `Hp 0` | `ENUMFT_Hinweis_Funktion/Hp_0` | `Hp 0` |
| `Sh 2/Sh 0` | `ENUMFT_Hinweis_Funktion/Sh_2_Sh_0` | `Sh 2/Sh 0` |
| `PZB/LZB` | `ENUMNID_STM/PZB_LZB` | `PZB/LZB` |
| `Präzisionshalt hochpräzise` | `ENUMDP_ATO/Präzisionshalt_hochpräzise` | `Präzisionshalt hochpräzise` |
| `AE-E` | `ENUMDP_Typ_ESG/AE-E` | `AE-E` |
| `12` | `ENUMFue_Schaltfall/12` | `12` |

In the real-world example the characters replaced are the space (184 occurrences), `/` (6), `(`
and `)` (3 each) and `+` (2). The hyphen (96), the full stop (3), umlauts and `ß` are kept. 130
value names change, and no two values of one enumeration end up with the same IRI.

## Diagnostics

An XMI diagnostic carries the file name and no row, as XMI diagnostics do today, so the message
names the enumeration and, where there is one, the value. An enumeration or value that cannot be
honoured is reported and skipped, and reading continues. One code is new.

| Situation | Severity | Code |
|-----------|----------|------|
| An enumeration or a literal has no `name`, or a blank one | Error | `BlankIdentity` |
| A `name` reads as a curie whose prefix is not configured with `-p` or `-x` | Error | `UnresolvableCurie` |
| An enumeration has the name of a class (rule 5) | Error | `ClassifierKindConflict` |
| An enumeration owns an attribute (rule 12) | Error | `ClassifierKindConflict` |
| A generalization has an enumeration at either end (rule 13) | Error | `ClassifierKindConflict` |
| An enumeration has no values (rule 6) | Warning | `EnumerationWithoutValues` |
| Two values of one enumeration would get the same IRI (rule 19) | Error | `EnumerationValueIRIConflict` (*new*) |

An enumeration that is rejected takes its values with it. A value that is rejected leaves its
enumeration and its other values in place.

`EnumerationWithoutValues` is reported once per enumeration, after every input has been read. When
XMI and TSV are combined, an enumeration declared in the XMI file and given its values in the
values file has values, and is not reported. For that reason the check is no longer made by a
reader but by the translation, once every reader has run.

A class that cannot be read is now reported and skipped as well, where it used to stop the reader.
That is what lets an XMI class with the name of an enumeration read earlier from TSV be turned
away, as described under [Combining XMI with TSV](#combining-xmi-with-tsv).

## Combining XMI with TSV

`--overrides` works for enumerations as it does for classes: the overriding source is read last,
and *enumeration enrichment* merges what it supplies into what the first source left in the cache.
TSV overrides XMI by default.

- **Giving a curie to an enumeration drawn by name.** A row of the enumerations file carrying both
  `Name` and `Curie` ties the two halves together, as in TSV rule 2. A row of the values file
  carrying both does the same for a value. This is the way to map `PaymentSchedule` onto an
  existing IRI without renaming it in the diagram.
- **Definitions.** The overriding source wins where it gives a definition. Where it gives none, the
  definition of the other source is kept.
- **Values are combined.** A value given by either source belongs to the enumeration. Neither
  source can remove a value the other one gave.
- **The shared namespace holds across sources.** Rule 5 follows the order in which the sources are
  read. With TSV overriding, a row of the classes file, a `Parents` cell or the `Class` cell of an
  attribute row that names an enumeration read from XMI is rejected. With XMI overriding, an
  enumeration in the XMI file is rejected when TSV already made a class of that name. That includes
  a placeholder class, so an attributes file that uses `PaymentSchedule` as a type without an
  enumerations file turns the XMI enumeration away. In the same way a class in the XMI file is
  rejected when TSV already made an enumeration of that name.
- **An attributes file can use an XMI enumeration.** With TSV overriding, a
  `ClassEnumOrPrimitiveType` cell naming an enumeration read from XMI resolves to it, with no
  enumerations file needed.

## Translation to OWL

Unchanged. For the initial example, with `-p emp:http://uml2semantics.org/examples/employer#`,
`PaymentSchedule` becomes:

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

DisjointClasses: Person, Renumeration, PaymentSchedule
```

The value IRIs are `…employer#PaymentSchedule/Daily` and so on.

That is all this specification gives for the initial example. The property that uses the
enumeration comes from the association `paymentSchedule`, and so from
[`xmi-associations.md`](xmi-associations.md):

```
ObjectProperty: paymentSchedule
  Domain: Renumeration
  Range: PaymentSchedule
Class: Renumeration
  SubClassOf: paymentSchedule exactly 1 owl:Thing
```

For the real-world example the result is 203 enumeration classes and 1619 individuals, 9 of the
classes with a comment, and 200 object properties named `Wert`, each ranging over an enumeration
class with exact cardinality 1. 130 of the individuals have an IRI that differs from their label
under rule 16. The diagnostics that concern enumerations are one
`EnumerationWithoutValues` warning and 7 `ClassifierKindConflict` errors. A run of the
implementation gave exactly these numbers, in about six seconds.

## Before this was implemented

Recorded by running the tool as it was before any of this was implemented.

- **Initial example.** The ontology has the classes `Employee`, `Employer`, `Person` and
  `Renumeration`, the three datatype properties of `Person`, and nothing else. `PaymentSchedule`
  and its values are absent, and so is every association. No diagnostics.
- **Real-world example.** It could not be translated, for two reasons.
  - *A class that specialises an enumeration stops the reader.* A generalization looks its parent
    up among classes only, so a parent that is an enumeration comes back with a blank name, and
    the reader stops with an uncaught `BlankIdentity` exception before any ontology is written. A
    copy of an earlier version of the initial example, given one such generalization, reproduced
    this. The real-world
    example has 7, so rule 13 is needed before it can be read at all.
  - *Reading does not finish.* A run of the tool on the file was stopped after 25 minutes, still
    reading classes and before it reached the first failure above. The reader searched the whole
    document once or more for every class, generalization and attribute, and this file has 2000
    classes and 3384 attributes. That is not about enumerations, but it bounded how they could be
    read: a search of the whole document for each of the 1619 literals, to find its definition,
    would have added to the same cost. The reader now walks the document once and looks
    everything up in what it found, for classes, generalizations and attributes as well.
- **Other fixtures.** The five XMI variants under `scenarios/employer-generalization` have the
  classes `Person`, `Employee` and `Employer` and no enumeration, so reading enumerations from
  XMI does not change what they give.

## Decisions

Taken on 2026-11-24, on questions the first draft of this specification left open.

1. **The initial example.** Its enumeration is used through the named association
   `paymentSchedule` from `Renumeration` to `PaymentSchedule`, and not through an attribute. The
   example therefore shows its enumeration in use only once associations are read too.
2. **A class that specialises an enumeration** is rejected for now (rule 13) and will be dealt
   with in a later iteration. The 7 cases in the real-world example are an XML Schema idiom for a
   simple type restricted to enumerated values.
3. **Value names that are not safe in an IRI** have their unsafe characters replaced by `_` in the
   IRI, and keep the name as given for the label (rules 16 to 20).
4. **Definitions wrapped in JSON** are read as they stand, as class definitions are today. The 9
   enumeration definitions of the real-world example read
   `{"Documentation":"VNB: Versorgungsnetzbetreiber; NEA: Netzersatzaggregat"}`, a convention of
   that model which 136 of its classes share.
5. **Same-named enumerations in different packages** are merged without a diagnostic (rule 2), as
   classes are. The real-world example has 30 class names that occur more than once and no such
   enumeration names.

## Still open

Both are implemented as proposed, and are still to be confirmed.

1. **Which characters count as unsafe.** Rule 16 keeps letters of any script, so umlauts and `ß`
   stay in the IRI, which an IRI allows. Restricting the IRI to ASCII letters would change 166
   value names of the real-world example instead of 130, still with no two values sharing an IRI.
2. **Rule 19 and its code are additions.** The decision on unsafe names did not cover two names
   that become the same IRI. Rejecting the second with `EnumerationValueIRIConflict` is a
   proposal. It does not arise in either example.
