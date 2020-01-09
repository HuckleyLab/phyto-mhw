# README for contraction

Data on whether or not there has been a contraction at range edges for marine and terrestrial species

## range_contraction.csv
- Reference: citation to paper providing the data
- Species: genus and species
- Taxonomic group: broad taxonomic grouping
- Latitude or elevation: was the range edge measured in terms of latitude or elevation?
- Warm edge: paper's conclusion about warm range edge change through time (expansion, stable, or contraction)
- Local extinction: was there a local extirpation at the warm range edge (contraction) or did the species persist (stable or expansion)
- Latitude: latitude of the warm range edge at the beginning of the study
- Habitat: marine or terrestrial
- Geographic region: broad geographic region
- DateStart: year of first survey of the warm range edge
- DateEnd: year of the last survey
- Method: whether the range edge was surveyed twice or more than twice through time
- Mobility: mode of movement (sessile, crawl, or more mobile)
- Include: whether or not the data point meets our inclusion criteria (TRUE/FALSE)
- Include.reason: rationale for Include choice
- Notes: other notes about the data point

## taxonomy_by_hand.csv
Hand-entered taxonomic classification information for species that couldn't be found by [local_extirpation_make_taxonomy.r](scripts/local_extirpation_make_taxonomy.r)
- Species3: species and genus to allow merging with other taxonomic data in local_extirpation_make_taxonomy.r (also includes group names)
- kingdom: taxonomc classification
- subkingdom: taxonomc classification
- infrakingdom: taxonomc classification
- superphylum: taxonomc classification
- phylum: taxonomc classification
- subphylum: taxonomc classification
- infraphylum: taxonomc classification
- superclass: taxonomc classification
- class: taxonomc classification
- subclass: taxonomc classification
- infraclass: taxonomc classification
- superorder: taxonomc classification
- order: taxonomc classification
- suborder: taxonomc classification
- infraorder: taxonomc classification
- superfamily: taxonomc classification
- family: taxonomc classification
- subfamily: taxonomc classification
- tribe: taxonomc classification
- genus: taxonomc classification
- subgenus: taxonomc classification
- species: species and genus (also includes group names)

## WoS_2017-08-28.csv
Web of Science literature search conducted 28 August 2017 for Topic=(global warm* OR climate change) AND Topic=(extinction* OR contraction* OR range shift*) AND Year: 2016-2017
