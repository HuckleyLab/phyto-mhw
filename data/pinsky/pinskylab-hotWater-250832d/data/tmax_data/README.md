# README for tmax_data

## dataset_1_ARRs.tsv
- Genus: taxonomic classification
- Species: taxonomic classification
- ctmax_ARR_GS: acclimation response ratio (ARR) for critical thermal maximum (CTmax) from Gunderson & Stillman 2015 Proceedings B doi: 10.1098/rspb.2015.0401

## dataset_1_hotwater.csv
- Genus: taxonomic classification
- Species: taxonomic classification
- tmax_metric: type of upper thermal limit (lethal or critical)
- tmax_acc: acclimation temperature in °C
- tmax_acc_days: duration of acclimation in days
- tmax_acc_type: whether acclimation was in the field or in a lab
- tmax_col_time_if_field_fresh: season of collection, if acclimated in the field
- tmax: upper thermal limit in °C
- lat: latitude of collection
- lon: longitude of collection
- no_NicheMapR_data: reason for lack of NicheMapR climate data (tend to be remote islands or high latitudes)
- altitude: altitude of collection in meters
- Phylum: taxonomic classification
- Class: taxonomic classification
- Order: taxonomic classification
- Family: taxonomic classification
- habitat: habitat of collection (marine or terrestrial)
- citation: name of the data source
- citation_details_or_paper_in_which_data_were_compiled: longer citation, or name of the compilation paper from which data were pulled
- notes.2017: other notes

## dataset_1_traits.csv
- Genus: taxonomic classification
- Species: taxonomic classification
- Phylum: taxonomic classification
- Class: taxonomic classification
- Order: taxonomic classification
- Family: taxonomic classification
- Realm: primary realm of the species (marine, freshwater, or terrestrial)
- Realm_detail: details about the habitat use for more complicated cases (amphidromous, anadromous, brackish, catadromous, freshwater, marine)
- Realm_source: source of information about realm (WoRMS=World Register of Marine Species; Fishbase; Sealifebase)
- thermy: ectotherm or endotherm
- demers_pelag: for marine species, whether demersal or pelagic
- demers_pelag_fb: detail on marine habitat use using categories from Fishbase (http://www.fishbase.org)
- demers_pelag_source: source of the demers_pelag_fb
- mobility: mode of animal mobility for marine species (sessile, crawl, or swim)
- mobility_source: source of data for mobility column
- weight: weight in grams
- weight_source: source for the weight column
- length: length in cm
- lengthType: type of length measurement. Codes are from Fishbase and Sealifebase (TL=total length; CW=carapace width; WD=width; etc.)
- length_source: source of length data
- fishbase_sci: scientific name as recorded in Fishbase or Sealifebase