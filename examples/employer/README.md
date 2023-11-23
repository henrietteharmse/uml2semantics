# Instruction to generate the Employer ontology
```

java -jar $uml2semantics/target/scala-3.2.2/uml2semantics.jar \ 
-c "$uml2semantics/examples/employer/Employer - Classes.tsv" \
-a "$uml2semantics/examples/employer/Employer - Attributes.tsv" \
-e "$uml2semantics/examples/employer/Employer - Enumerations.tsv" \
-n "$uml2semantics/examples/employer/Employer - EnumerationNamedValues.tsv" \
-o "$uml2semantics/examples/employer/employer.rdf" \
-p "emp:http://uml2semantics.org/examples/employer#" \
-i "http://uml2semantics.org/examples/employer/v.0.1" 

```
