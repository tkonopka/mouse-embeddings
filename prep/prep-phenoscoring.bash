#

cd ../data/phenoscoring


# process MGI models to get phenotype priors
if [ ! -f "phenoprep-mgi-priors.tsv.gz" ]
then
  ../../phenoprep MGI --input MGI_GenePheno_wheader.rpt.gz \
                      --tpr 0.8 --fpr 0.05 \
                      --obo mp.obo \
                      --priors marker \
                      --output phenoprep-mgi
fi


# make an owlsim translation from hp to mp terms
if [ ! -f "hp-mp-owlsim-oomap.tsv.gz" ]
then
  ../../phenoprep oomap --input ../owltools/owltools-cache-hp-mp.txt.gz \
                        --obo mp.obo \
                        --output hp-mp-owlsim
fi


# prepare disease representations
for OOMETHOD in owlsim crossmap
do
  if [ ! -f "phenoprep-orphanet-$OOMETHOD-priors.tsv.gz" ]
  then
    # build disease definitions
    ../../phenoprep phenotypetab --input phenotype_annotation_orphanet.tab.gz \
                                 --tpr 0.8 --fpr 0.05 \
                                 --obo mp.obo \
                                 --oomap hp-mp-$OOMETHOD-oomap.tsv.gz \
                                 --output phenoprep-orphanet-$OOMETHOD
  fi
done


# build the database and update with mouse models
for OOMETHOD in owlsim crossmap
do
  if [ ! -f "phenoscoring-orphanet-$OOMETHOD.sqlite" ]
  then
    ../../phenoscoring build --db phenoscoring-orphanet-$OOMETHOD.sqlite \
                             --obo mp.obo \
                             --phenotype_frequencies phenoprep-mgi-priors.tsv.gz \
                             --reference_phenotype phenoprep-orphanet-$OOMETHOD-phenotypes.tsv.gz \
                             --prior 0.0000001 \
                             --cores 4
    ../../phenoscoring update --db phenoscoring-orphanet-$OOMETHOD.sqlite \
                              --obo mp.obo \
                              --model_descriptions ../impc/mouse-model-descriptions.tsv.gz \
                              --model_phenotypes ../impc/mouse-model-phenotypes.tsv.gz \
                              --min_enrichment 100 --fp_penalty 1 \
                              --cores 4 --partition_size 800
   ../../phenoscoring export --db phenoscoring-orphanet-$OOMETHOD.sqlite \
                             --table model_score | gzip > \
                             phenoscoring-orphanet-$OOMETHOD-model-scores.tsv.gz
  fi
done


# write model representations
for OOMETHOD in owlsim crossmap
do
  if [ ! -f "phenoscoring-orphanet-$OOMETHOD-references_data.tsv.gz" ]
  then
    ../../phenoscoring representations --db phenoscoring-orphanet-$OOMETHOD.sqlite \
                                       --obo mp.obo --partition_size 8000
  fi
done
