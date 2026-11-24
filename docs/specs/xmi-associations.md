# XMI specification: associations

**Status: proposed.** Nothing in this document is implemented.

An attribute and an association translate to the same OWL axioms: a property with a domain, a
range and a cardinality restriction. That is why an association adds nothing new to the model. It
becomes an *attribute*, exactly the one a modeller would write as a row of the TSV attributes
file, and the translation of an attribute to OWL is unchanged.

Today the XMI reader does not look at the association lines of a diagram. An association reaches
the ontology only when a *supporting attribute* states it. One that has no supporting attribute
is lost, and nothing is reported; see [What the examples do today](#what-the-examples-do-today).
This document specifies reading the association itself, so that it no longer depends on a
supporting attribute.

It is the companion of [`xmi-enumerations.md`](xmi-enumerations.md).

Terms used here (*name*, *curie*, *identity*, *enrichment*, *diagnostic*) are the ones defined in
the model and reader `CONTEXT.md` files. One term is new:

- **Supporting attribute**: an attribute of the class at one end of an association, typed by the
  classifier at the other end. It states what the association states. In the older employer
  exports `Renumeration` is joined to `PaymentSchedule` by an unnamed line and also has the
  attribute `paymentSchedule` of type `PaymentSchedule`.

## The examples

All are Enterprise Architect exports of XMI 2.1.

| | Initial example | Real-world example |
|---|---|---|
| File | `src/test/resources/scenarios/simple-employer/input/xmi/sparx/SimpleEmployerWithEnumExample.xml` | `src/test/resources/cases/xmi/EA-export-xmi-2.1.mod.xml` |
| Associations | 4 | 55 |
| With a name | 4 | 0 |
| With a role name on either end | 0 | 0 |
| Unnamed, with a supporting attribute | 0 | 0 |
| Unnamed, without a supporting attribute | 0 | 55 |
| Ending at an enumeration | 1 | 0 |
| Direction as recorded | `Unspecified` | `Source -> Destination` |
| Aggregations, compositions, association classes | 0 | 0 |

The four associations of the initial example, as drawn:

| Name | Source | Source multiplicity | Target | Target multiplicity |
|------|--------|---------------------|--------|---------------------|
| `isEmployedBy` | `Employee` | | `Employer` | `1` |
| `employs` | `Employer` | | `Employee` | `1..*` |
| `renumeration` | `Employee` | `1` | `Renumeration` | `1` |
| `paymentSchedule` | `Renumeration` | `1` | `PaymentSchedule`, an enumeration | `1` |

The 55 associations of the real-world example each join a class to a class named `…_Choice_n`, an
XML Schema idiom. None has a name, a role name, a multiplicity or a supporting attribute.

Other files in the repository, used below because they are drawn differently:

- **`SimpleEmployerExample.xml`**, beside the initial example: `isEmployedBy` from `Employee` to
  `Employer` with `1` at `Employer`, and `employs` from `Employer` to `Employee` with `1..*` at
  `Employee`.
- **The employer generalization scenario**: the five files under
  `scenarios/employer-generalization/input/xmi/sparx`. Each has `isEmployedBy` from `Employee` to
  `Employer` and `employs` from `Employer` to `Employee` with `1..*` at `Employee`. `isEmployedBy`
  has `1` at `Employer` in four of them and no multiplicity in
  `WithGeneralizationSet-CompleteOverlapping.xml`. None has an enumeration or an unnamed
  association.
- **The older employer exports**: three files under `examples/xmi/sparx` (`Employer.xml`,
  `Employer-WithoutGeneralizationSet.xml`, `Employer-WithGeneralizationSet-CompleteOverlapping.xml`)
  and `scenarios/employer-generalization/source-models/Employer.xml`. They have `renumeration`
  from `Employee` to `Renumeration` with `1` at both ends, `employes` from `Employee` to
  `Employer` with `1..*` at `Employee`, `employedBy` from `Employer` to `Employee` with `1` at
  `Employer`, and an unnamed line from `Renumeration` to `PaymentSchedule` with its supporting
  attribute.
- **`examples/xmi/sparx/resource-metadata-schema.xml`**: 12 associations, all unnamed. 9 have a
  supporting attribute, such as `dcat:service` of `dcat:Catalog` for the line to
  `dcat:DataService`. 3 have none.

## What is read

An association is read from Enterprise Architect's extension part, as attributes are today. The
standard UML part also describes the association, but has no notion of the source and target on
which the direction rule rests.

| Model | Read from |
|-------|-----------|
| Association | every `connector` under `connectors` whose `properties/@ea_type` is `Association` |
| Name or curie of the association | the `name` attribute of the `connector` |
| Source and target classifier | `source/model/@name` and `target/model/@name`, with the kind in `model/@type` |
| Role name of an end | `source/role/@name` and `target/role/@name` |
| Multiplicity of an end | `source/type/@multiplicity` and `target/type/@multiplicity` |
| Definition of the association | `documentation/@value` of the `connector` |
| Definition of a role | `source/documentation/@value` and `target/documentation/@value` |

In the initial example:

```xml
<connector xmi:idref="EAID_EB6B0559_…" name="employs">
  <source xmi:idref="EAID_EA60BA14_…">
    <model ea_localid="5" type="Class" name="Employer"/>
    <role visibility="Public" targetScope="instance"/>
    <type aggregation="none" containment="Unspecified"/>
  </source>
  <target xmi:idref="EAID_A8606A15_…">
    <model ea_localid="7" type="Class" name="Employee"/>
    <role visibility="Public" targetScope="instance"/>
    <type multiplicity="1..*" aggregation="none" containment="Unspecified"/>
  </target>
  <properties ea_type="Association" direction="Unspecified"/>
</connector>
```

A file exported without the extension part yields no associations, as it yields no attributes
today. Four of the employer files under `examples/xmi/sparx` are such exports.

## Rules

### Name and direction

1. **An association belongs to its source.** The source and target are the ends as Enterprise
   Architect records them, which is the direction in which the line was drawn. The association
   name becomes an attribute of the source class. Its type is the target classifier and its
   multiplicity is that of the target end, or `1` when the target end has none (rule 14).

   | Association | Attribute | Of class | Type | Multiplicity |
   |-------------|-----------|----------|------|--------------|
   | `employs`, drawn from `Employer` to `Employee`, `1..*` at `Employee` | `employs` | `Employer` | `Employee` | `[1,*]` |

2. **Where the multiplicity sits does not change the direction.** An association named `owns`
   drawn from `Person` to `Car` with `0..1` at the `Person` end is `Person owns Car`. The `0..1`
   says that a car has at most one owner, which is a statement about the opposite direction
   (rule 5). The attribute `owns` of `Person` gets `[1,1]`, since the `Car` end has no
   multiplicity. The older employer exports are drawn against this rule; see
   [Associations to redraw](#associations-to-redraw).
3. **A role name on the target end takes precedence over the association name.** It names the
   same attribute: of the source class, typed by the target, with the target end's multiplicity.
   The association name is then not used.
4. **A role name on the source end gives a second attribute, in the opposite direction**: of the
   target class, typed by the source classifier, with the source end's multiplicity. The
   association name is never used in this direction.
5. **A source end without a role name is not read.** Its multiplicity constrains the opposite
   direction, for which there is no name.
6. **Name-or-curie rule.** An association name or a role name is read as an attribute name is: a
   string containing `:` is a curie, whose prefix must be configured with `-p` or `-x`, and
   anything else is a name.
7. **Navigability is not read.** Neither the `direction` of the connector nor the `isNavigable`
   flag of an end changes anything.

### Unnamed associations

8. **An association with no name and no role name gives no attribute.** There is nothing to name
   one with.
9. **With a supporting attribute, nothing is lost and nothing is reported.** The attribute already
   states the association. This covers the line from `Renumeration` to `PaymentSchedule` in the
   older employer exports, and 9 of the 12 associations of `resource-metadata-schema.xml`.
10. **Without a supporting attribute, it is reported and nothing is written for it.** No attribute
    is made, so the ontology gets no property and no restriction for the association: there is no
    name to mint a property IRI from. The classes at its two ends are read as usual. The
    association is reported as `UnnamedAssociation`, because something in the diagram does not
    reach the ontology. This covers all 55 associations of the real-world example and 3 of
    `resource-metadata-schema.xml`.

### What an association becomes

11. **An attribute, and nothing else.** The attribute an association gives is the one a row of the
    TSV attributes file would give.
12. **The owner is a class.** An attribute that would belong to an enumeration is rejected, since
    enumerations own no attributes. That is an association drawn *from* an enumeration, or a role
    name on the source end of an association drawn *to* one.
13. **The type is a class or an enumeration.** An association ending at an enumeration gives an
    attribute typed by that enumeration, as `xmi-enumerations.md` rule 14 requires;
    `paymentSchedule` in the initial example is one. An end that is
    neither a class nor an enumeration makes the association unreadable; it is reported and
    skipped.
14. **Multiplicity.** `n` is `[n,n]`, `m..n` is `[m,n]`, and `*` is `[0,*]`. An end with no
    multiplicity is taken to be `1`, so the attribute gets `[1,1]`. This is the UML default and
    what a blank multiplicity gives in TSV. It is not reported.
15. **Definition.** An attribute named by the association name takes the definition of the
    association. An attribute named by a role name takes the definition of that role. No example
    has either.
16. **An association from a class to itself** is read like any other, giving an attribute of the
    class typed by the class.
17. **A named association and an attribute of the same name are one attribute.** An attribute is
    identified by its class and its name, so an association named `paymentSchedule` from
    `Renumeration` and an owned attribute `paymentSchedule` of `Renumeration` are the same
    attribute, as two rows of the attributes file for it would be. The one read later replaces
    the earlier one, as it does for two such rows today. This is not reported.

### Reading order

18. **Classes, enumerations, attributes, then associations.** Every classifier an association can
    refer to has then been read, supporting attributes are known when rule 9 is applied, and
    rule 17 lets the association have the last word over an attribute of the same name.

### Not read

19. **Aggregations and compositions.** Enterprise Architect exports them as connectors of another
    type. Which way their name reads has not been looked at, and no example has one.
20. **Association classes.** No example has one.
21. **Everything else about an end**: ordering, uniqueness, changeability, constraints and
    qualifiers.

## Diagnostics

An XMI diagnostic carries the file name and no row, so the message names the association, or its
two ends when it has no name. One code is new.

| Situation | Severity | Code |
|-----------|----------|------|
| An unnamed association has no supporting attribute (rule 10) | Error | `UnnamedAssociation` (*new*) |
| A name reads as a curie whose prefix is not configured with `-p` or `-x` | Error | `UnresolvableCurie` |
| An attribute would belong to an enumeration (rule 12) | Error | `ClassifierKindConflict` |
| An end is neither a class nor an enumeration (rule 13) | Error | `RowRejected` |
| A multiplicity cannot be parsed | Warning | `UnparsableCardinality` |
| The maximum of a multiplicity is below its minimum | Error | `MaxBelowMin` |
| The minimum of a multiplicity is `*` | Error | `UnboundedMinMultiplicity` |

The last three are the existing codes for an attribute's multiplicity, with their existing
severity and effect.

## Combining XMI with TSV

Because an association becomes an attribute, `--overrides` applies to it as it does to any
attribute read from XMI. A row of the attributes file for the same class and name enriches or
overrides what the association gave. A row carrying both `Name` and `Curie` is the way to map the
association onto an existing property without renaming it in the diagram.

## What the initial example becomes

The four associations give these attributes under rule 1:

| Class | Name | ClassEnumOrPrimitiveType | MinMultiplicity | MaxMultiplicity |
|-------|------|--------------------------|-----------------|-----------------|
| Employee | isEmployedBy | Employer | 1 | 1 |
| Employer | employs | Employee | 1 | * |
| Employee | renumeration | Renumeration | 1 | 1 |
| Renumeration | paymentSchedule | PaymentSchedule | 1 | 1 |

`scenarios/simple-employer/input/tsv/Attributes.tsv` has the second row. It has the first under
the older name `employedBy`, which read together with this file would be a second attribute
beside `isEmployedBy`. It has no rows for the last two, and no `Renumeration` class.

In OWL:

```
ObjectProperty: isEmployedBy
  Domain: Employee
  Range: Employer
ObjectProperty: employs
  Domain: Employer
  Range: Employee
ObjectProperty: renumeration
  Domain: Employee
  Range: Renumeration
ObjectProperty: paymentSchedule
  Domain: Renumeration
  Range: PaymentSchedule

Class: Employee
  SubClassOf: isEmployedBy exactly 1 owl:Thing, renumeration exactly 1 owl:Thing
Class: Employer
  SubClassOf: employs min 1 owl:Thing
Class: Renumeration
  SubClassOf: paymentSchedule exactly 1 owl:Thing
```

`isEmployedBy` and `employs` are not declared inverses of each other. They are two associations,
and nothing in the diagram relates them.

For the real-world example the result is no attributes and 55 `UnnamedAssociation` errors.

## What the employer generalization scenario becomes

All five files give the same two attributes:

| Class | Name | ClassEnumOrPrimitiveType | MinMultiplicity | MaxMultiplicity |
|-------|------|--------------------------|-----------------|-----------------|
| Employee | isEmployedBy | Employer | 1 | 1 |
| Employer | employs | Employee | 1 | * |

In four files the `[1,1]` of `isEmployedBy` is the `1` drawn at the `Employer` end. In
`WithGeneralizationSet-CompleteOverlapping.xml` that end has no multiplicity and the `[1,1]` is
the default of rule 14.

## What the examples do today

Recorded by running the current tool.

- **An association with a supporting attribute reaches the ontology through the attribute.**
  `examples/xmi/sparx/Employer-WithoutGeneralizationSet.xml` gives the object property
  `paymentSchedule` of `Renumeration`.
- **A named association without a supporting attribute is lost.** The same run has none of
  `renumeration`, `employes` and `employedBy`. The initial example gives its four classes and the
  three datatype properties of `Person`, and none of its four associations. No diagnostic says
  so in either run.
- **A role name does not change that.** A copy of an earlier version of the initial example, with
  the role name `staff` added by hand to the `Employee` end of `employs`, gave no property
  `staff`. The reader takes attributes only from the `attributes` of a class in the extension
  part, and a role name is not among them. No file in the repository has a role name, so this
  could not be tried on a real export.

## Associations to redraw

Rule 1 reads an association the wrong way round when its line was drawn from the class the name
reads towards. It gives no diagnostic: the result is a valid association that says something
other than what was meant.

The initial example, `SimpleEmployerExample.xml` and the five files of the employer
generalization scenario were redrawn on 2026-11-24, and all of their named associations now read
as meant. Two remain, both in the older employer exports, which no test reads:

| Association | Drawn | Rule 1 gives | Meant |
|-------------|-------|--------------|-------|
| `employes` | `Employee` to `Employer` | `Employee employes Employer [1,1]` | `Employer employes Employee [1,*]` |
| `employedBy` | `Employer` to `Employee` | `Employer employedBy Employee [1,1]` | `Employee employedBy Employer [1,1]` |

In both the multiplicity sits on the source end, so it is not read either (rule 5), and the
attribute gets the default `[1,1]`. For `employes` that also loses the `1..*`. To be read as
meant, each is redrawn from the class that owns it, or its name is moved from the line to a role
name on the end it reads towards (rule 4).

## Decisions

Taken on 2026-11-24.

1. **An association belongs to its source.** The examples disagree on which way an association
   name reads. Three rules were considered: source to target with role names taking precedence,
   the end that carries the multiplicity, and role names only. Source to target was chosen
   (rules 1 to 4), and confirmed after a count showed that the multiplicity rule would have read
   every named association as it was then drawn. The diagrams are redrawn to fit the rule
   instead; see [Associations to redraw](#associations-to-redraw).
2. **A supporting attribute stands for its association.** An attribute and an association
   translate to the same axioms, so an association stated by an attribute is not lost. Only an
   association with neither a name, a role name nor a supporting attribute is (rules 8 to 10).
3. **A target end without a multiplicity is assumed to be `1`** (rule 14).

## Still open

1. **What counts as a supporting attribute.** The definition used here is an attribute of the
   class at either end, typed by the classifier at the other end, whatever its name and
   multiplicity.
2. **Whether an unnamed association without a supporting attribute is an error.** Rule 10 reports
   it as an error, because something in the diagram does not reach the ontology. The real-world
   example would report 55. A warning is the alternative.
3. **Expected files.** `scenarios/employer-generalization/expected` is empty. The expected
   ontologies for its five files are still to be written, and include the two attributes in
   [What the employer generalization scenario becomes](#what-the-employer-generalization-scenario-becomes).
