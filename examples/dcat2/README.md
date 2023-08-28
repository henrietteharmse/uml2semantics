# Instruction to generate the DCAT v2 ontology

```

java -jar $uml2semantics/target/scala-3.2.2/uml2semantics.jar \ 
-c "$uml2semantics/examples/dcat2/DCATv2 - Classes.tsv" \
-a "$uml2semantics/examples/dcat2/DCATv2 - Attributes.tsv" \
-o "$uml2semantics/examples/dcat2/dcat-v2.rdf" \
-p "dcat:http://www.w3.org/ns/dcat#" \
-i "http://www.w3.org/ns/dcat#v2" \
-x "foaf:http://xmlns.com/foaf/0.1/,vcard:http://www.w3.org/2006/vcard/ns#,dct:http://purl.org/dc/terms/,dctype:http://purl.org/dc/dcmitype/,dct:http://purl.org/dc/terms/,prov:http://www.w3.org/ns/prov#,skos:http://www.w3.org/2004/02/skos/core#,sdo:http://schema.org/"

```

Note that using option `-x` we specify all prefixes that are used in the UML class diagram. 