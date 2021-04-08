# create node2vec embeddings with different settings

cd ../results/

# build embeddings for ontologies and mouse models
for DATASET in mp-ontology mouse-model-concise
do

  cat ../data/node2vec/$DATASET-nodes.txt | gzip > $DATASET-nodes.tsv.gz

  for DIM in 2 4
  do
    OUTFILE=$DATASET-node2vec-d$DIM.tsv
    if [ ! -f "$OUTFILE.gz" ]
    then
      ../node2vec -i:../data/node2vec/$DATASET-edges.txt \
                  -o:$OUTFILE \
                  -d:$DIM
      gzip $OUTFILE
    fi
  done

done

