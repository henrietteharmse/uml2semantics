#!/usr/bin/env bash

#java -jar ./target/scala-3.3.0/uml2semantics.jar \
#-c "./src/test/resources/Employer - Classes.tsv" \
#-a "./src/test/resources/Employer - Attributes.tsv" \
#-e "./src/test/resources/Employer - Enumerations.tsv" \
#-n "./src/test/resources/Employer - EnumerationNamedValues.tsv" \
#-o "./src/test/resources/employer.owl" \
#-p "emp:http://uml2semantics.org/examples/employer#" \
#-i "http://uml2semantics.org/examples/employer/v.0.1"


java -jar ./target/scala-3.3.0/uml2semantics.jar \
-m "./examples/xmi/sparx/Employer.xml" \
-o "./src/test/resources/employer.owl" \
-p "emp:http://uml2semantics.org/examples/employer#" \
-i "http://uml2semantics.org/examples/employer/v.0.1"