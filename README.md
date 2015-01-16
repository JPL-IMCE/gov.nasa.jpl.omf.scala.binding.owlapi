# OWL API binding for OMF (Ontological Modeling Framework)

The OWL API binding for OMF implements the OMF Core functional API using the OWL API. This particular binding captures the conventions and restrictions on the use of OWL2-DL adopted for the development of JPL's Integrated Model-Centric Engineering (IMCE) ontologies. OWL2-DL ontologies that conform to these conventions and restrictions enjoy several useful properties, including:

- standard syntax for creating, editing, saving ontologies with tools that conform to the W3C's OWL Functional Syntax and RDF/XML Mapping specifications

- standard semantics for reasoning ontologies with tools that conform to the W3C's OWL Functional Syntax for OWL-DL ontologies and the direct and RDF-based mapping semantics specifications

## Dependencies

- See 'OMF Scala Core' (and its dependencies)

## Building the OWL API Scala binding for OMF Scala

### Updating license header

```
sbt formatLicenseHeaders
```

### Building with SBT

```
sbt owlapiLibs/pack
sbt publishLocal
```

### Building with Eclipse

The Eclipse `.classpath` file refers to the OMF Scala Core project as "gov.nasa.omf.scala.core".
Make sure that this project is imported in the Eclipse workspace.

Build the Scala project as usual with Eclipse

### Unit Tests

Running unit tests from SBT:

```
sbt test
```

Running unit tests from the Scala IDE in Eclipse:

In the Scala perspective, use the Run or Debug menus to launch the configurations included in the `launchers/` folder.

 


