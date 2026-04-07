#!/usr/bin/env bash

#java -jar ./target/scala-3.3.0/uml2semantics.jar \
#-c "./src/test/resources/scenarios/employer/input/tsv/Classes.tsv" \
#-a "./src/test/resources/scenarios/employer/input/tsv/Attributes.tsv" \
#-e "./src/test/resources/scenarios/employer/input/tsv/Enumerations.tsv" \
#-n "./src/test/resources/scenarios/employer/input/tsv/EnumerationNamedValues.tsv" \
#-o "./test-output/employer/employer.owl" \
#-p "emp:http://uml2semantics.org/examples/employer#" \
#-i "http://uml2semantics.org/examples/employer/v.0.1"

# Define the base directory
BASE_DIR=$(dirname "$0")
echo "BASE_DIR: $BASE_DIR"

mkdir -p "$BASE_DIR/test-output/employer"

java -jar "$BASE_DIR/target/scala-3.6.3/uml2semantics.jar" \
-m "$BASE_DIR/src/test/resources/scenarios/employer/input/xmi/sparx/WithoutGeneralizationSet.xml" \
-i "https://uml2semantics.com/examples/employer" \
-x "foaf:http://xmlns.com/foaf/0.1/,vcard:http://www.w3.org/2006/vcard/ns#,dct:http://purl.org/dc/terms/,dctype:http://purl.org/dc/dcmitype/,dct:http://purl.org/dc/terms/,prov:http://www.w3.org/ns/prov#,skos:http://www.w3.org/2004/02/skos/core#,sdo:http://schema.org/" \
-o "$BASE_DIR/test-output/employer/WithoutGeneralizationSet.owl"