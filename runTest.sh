#!/usr/bin/env bash

java -jar ./target/scala-3.2.2/uml2semantics.jar \
-c "./src/test/resources/Employer - Classes.tsv" \
-a "./src/test/resources/Employer - Attributes.tsv" \
-e "./src/test/resources/Employer - Enumerations.tsv" \
-n "./src/test/resources/Employer - EnumerationNamedValues.tsv" \
-o "./src/test/resources/employer.rdf" \
-p "emp:http://uml2semantics.org/examples/employer#" \
-i "http://uml2semantics.org/examples/employer/v.0.1"