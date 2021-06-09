# mouse-embeddings

This repository holds an analysis of phenotypes associated with mouse models. 

The repository describes analyses of large datasets that are not included
on github. A bundle that includes the datasets and a copy of the analysis
code is available on 
zenodo ([dataset 4916172](https://zenodo.org/record/4916172)).
The bundle includes raw data files, processed data files, computed results, 
figures, and source code.  


## Setup

Directories in the github repository:

- `prep` - directory with scripts that process raw data files and perform
 one-time computations, e.g. database builds and database queries.
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

