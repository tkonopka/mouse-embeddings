#!/bin/bash
# this script relies on an executable crossprep in the repo root
#
# usage: from the scripts directory, execute without any additional parameters

cd ../data/orphanet
../../crossprep orphanet --orphanet_genes en_product6.xml \
                         --orphanet_phenotypes en_product4.xml \
                         --orphanet_nomenclature en_product1.xml \
                         --name orphanet

