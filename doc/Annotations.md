# Annotations

In general OML Annoations correspond to OWL Annotations except for `rdfs:label` as described below:

  | OWL Annotation Subject | OML Annotation Property |
  |---------|----------------------------|
  | OWL Class corresponding to an OML reification | `oml:hasReificationLabel` |
  | OWL ObjectProperty corresponding to an OML reified property | `oml:hasPropertyLabel` |
  | OWL ObjectProperty corresponding to an OML inverse reified property | `oml:hasInverseLabel` |
  | Other OWL logical element corresponding to an OML logical element | `rdfs:label` |

This table is used for both parsing OWL => OML where `rdfs:label` maps as described above
and for serializing OML => OWL where `oml:hasReificationLabel`, `oml:hasPropertyLabel` and `oml:hasInverseLabel`
map to `rdfs:label` as described above.