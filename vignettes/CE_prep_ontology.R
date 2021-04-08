# preparation of data structures with ontology information


# ############################################################################
# MP phenotype information

# files with parsed ontology names and ancestors
mp_names_file <- file.path(data.dir, "obo", "mp-names.tsv.gz")
mp_ancestors_file <- file.path(data.dir, "obo", "mp-ancestors.tsv.gz")
mp_raw_file <- file.path(data.dir, "crossmap", "mp-parents.yaml.gz")


if (!assignc("mp_info")) {
  mp_info <- list(names=fread(mp_names_file),
                  ancestors=fread(mp_ancestors_file))
  mp_info$ancestors <- mp_info$ancestors[,
    list(ancestor=c(id, unlist(strsplit(ancestors, ";")))),
    by="id"]
  savec(mp_info)
}

if (!assignc("mp_raw")) {
  mp_raw <- read_yaml(mp_raw_file)
}
