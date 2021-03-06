# prep

Scripts for data preparation. 


## Dependencies 

The scripts rely on some pre-existing files under the `data` folder. Some of
these files are simply downloaded (e.g. from orpha.net or from obofoundry.org). 
Other files must be processed manually, e.g. converting from obo format to
flat tables. A complete set of data files in the correct format are available
in a data snapshot at zenodo ([dataset 4916171](https://zenodo.org/record/4916171))

Several scripts require access to executables, which are assumed to be present 
under the repository root. 

- `crossmap` - executable for [crossmap](https://github.com/tkonopka/crossmap). 
  Crossmap is a program for creating, managing, and querying desktop-scale
  knowledge-bases.

- `crossprep` - executable for crossprep, which is a utility distributed 
  alongside [crossmap](https://github.com/tkonopka/crossmap). Crossprep is a 
  utility that prepares data files for use with crossmap.
  
- `node2vec` - executable for node2vec, distributed under 
  [snap](https://github.com/snap-stanford/snap). Node2vec is a utility to 
  generate embeddings for graphs.

- `obotools` - executable for obotools, which is distributed under 
  [phenoscoring](https://github.com/tkonopka/phenoscoring). Obotools is a
  utility for parsing ontology obo files.

- `phenoprep` - executable for phenoprep, which is distributed under 
  [phenoscoring](https://github.com/tkonopka/phenoscoring). Phenoprep is a 
  utility that prepares data files for use with phenoscoring.

- `phenoscoring` - executable for 
  [phenoscoring](https://github.com/tkonopka/phenoscoring). Phenoscoring
  is a program that creates and manages databases of model-disease 
  associations.

An 'executable' here means a script that launches the corresponding software, 
i.e. internally handles and resolves the location of the software. An example 
executable script for crossmap might be as follows:

```
#!/bin/bash -l
python3 /path/to/crossmap/crossmap.py $@
```


## Scripts

The scripts in the folder automate some data processing steps. Some steps depend
on files generated in previous steps.

- `prep-orphanet.bash` - converts data in orphanet's xml format for use with 
  crossmap.

- `prep-crossmap-search-mp.bash` - builds a crossmap instance for hp-mp 
  translation and runs searches to produce hp-mp translation tables.

- `prep-crossmap-search-hp.bash` - builds a crossmap instance for hp ontology
  and runs searches using hp terms.

- `prep-crossmap-mp.R` - a follow-up script after `prep-crossmap.bash` that 
  converts hp-mp translations into a format for phenoscoring.

- `prep-impc.R` - downloads data about mouse models from IMPC solr servers.
  Prepares data for use with phenoscoring and crossmap.
  
- `prep-phenoscoring.bash` - builds databases using `phenoscoring` which 
  produce vector representations based on complete phenotypes.

- `prep-phenoscoring.R` - uses phenoscoring dbs to create search-like results
  that map diseases to models

- `prep-crossmap-disease.R` - use conversion of hp-mp translations to prepare
  disease data for crossmap.

- `prep-crossmap-search-models.bash` - builds a crossmap instance with mouse
  models and runs searches to get nearest neighbors.
  
- `prep_node2vec.R` - creates data files suitable for processing with node2vec

- `prep_node2vec.bash` - create node2vec embeddings

