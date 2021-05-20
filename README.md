# mouse-embeddings

This repository holds an analysis of phenotypes associated with mouse models. 

A snapshot of the repository is available at zenodo ([TO DO](to do)). 
The snapshot includes raw data files, processed data files, computed results, 
generated figures, and source code. The github repository holds only the 
source code, without data files.


## Setup

Directories in the github repository:

- `prep` - directory with scripts that process raw data files and perform
 some one-time computations, e.g. database builds and database queries.
- `R` - functions used during generation of vignettes
- `vignettes` - code for Rmarkdown vignettes, and associated helper files.

Directories not included in the github repository, but included in the
data snapshot (and required for running the code from scratch):

- `data` - directory with data files pertaining to ontologies, mouse models,
  and diseases.
- `results` - directory with flat data files generated from `data`.
- `cache` - directory with processed data files using in R vignettes.

The `vignettes` folder contains two Rmarkdown files and several helper files. 
To compile the vignettes into pdf documents, launch R, install the required
packages (see `vignettes/config.R`) and render the Rmarkdown files.

```
render("ME_Figures.Rmd")
render("ME_Supplementary.Rmd")
```

