# preparation of data structures for diseases


# ############################################################################
# disease information

# files with disease definitions
disease_raw_file <- file.path(data.dir, "orphanet", "orphanet.yaml.gz")
gene_gene_raw_file <- file.path(data.dir, "impc", "solr-gene-gene-raw.tsv.gz")
gene_raw_file <- file.path(data.dir, "impc", "solr-gene-raw.tsv.gz")


if (!assignc("gene_raw")) {
  gene_raw <- fread(gene_raw_file)[, c("gene_symbol", "gene_id")]
}


if (!assignc("disease_raw")) {
  disease_raw <- read_yaml(disease_raw_file)
  savec(disease_raw)
}
if (!assignc("disease_info")) {
  gene_gene <- fread(gene_gene_raw_file)
  setnames(gene_gene, c("hgnc_gene_id", "gene_id"), c("hgnc_id", "marker_id"))
  disease_info <- rbindlist(lapply(disease_raw, function(x) {
    hgnc_ids <- unique(x$metadata$hgnc_ids)
    marker_ids <- unique(gene_gene[hgnc_id %in% hgnc_ids]$marker_id)
    data.table(disease_id=x$metadata$id,
               disease_name=x$title,
               disease_hgnc_ids=paste(hgnc_ids, collapse=","),
               disease_marker_ids=paste(marker_ids, collapse=","),
               disease_num_phenotypes=length(x$metadata$phenotype_id),
               disease_num_hgnc_genes=length(hgnc_ids),
               disease_num_mgi_genes=length(marker_ids))
  }))
  savec(disease_info)
}


# create mappings from disease to mp phenotypes
if (!assignc("disease_phenotypes")) {
  make_disease_phenotypes <- function(translation_method) {
    oomap <- fread(hp_mp_translations_files[[translation_method]])
    setnames(oomap, c("term1", "term2"), c("hp", "mp"))
    result <- rbindlist(lapply(disease_raw, function(x) {
      x_hp <- x$metadata$phenotype_ids
      if (length(x_hp)==0) return(NULL)
      data.table(id=x$metadata$id, phenotype=oomap[hp %in% x_hp]$mp)
    }))
    unique(result[order(id, phenotype)])
  }
  disease_phenotypes <- lapply(translation_methods, make_disease_phenotypes)
  savec(disease_phenotypes)
}


# create simple mappings from disease id to marker ids
if (!assignc("disease_genes")) {
  uncomma <- function(x) { unlist(strsplit(x, ",")) }
  disease_genes <- disease_info[, list(marker_id=uncomma(disease_marker_ids)),
                                  by="disease_id"]
  setnames(disease_genes, "disease_id", "id")
  savec(disease_genes)
}

