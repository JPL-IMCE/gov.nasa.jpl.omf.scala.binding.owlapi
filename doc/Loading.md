- Loading OMF Terminology Graphs from OWL:

gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIStoreOps#loadTerminologyGraph
gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFGraphStore#loadTerminologyGraph
gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader#loadTerminologyGraph
gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader#loadTerminologyGraphs
gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader#loadTerminologyGraphLayer
gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader#loadTerminologyGraphFromOntologyDocument
gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFGraphStore#convertTerminologyGraphFromOntologyDocument
gov.nasa.jpl.omf.scala.binding.owlapi.types.ImmutableModelTerminologyGraphResolver#resolve (Line 48)
gov.nasa.jpl.omf.scala.binding.owlapi.types.ImmutableModelTerminologyGraphResolver#resolve (line 101)

val tboxG: MutableModelTerminologyGraph = ...

For OMF terms/axioms, calls:

val <x>M = tboxG.create<X>(...)
OWLAPIOMFGraphStore.register<X>Instance(tboxG, <x>M)

