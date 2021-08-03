# searches against hp terms (e.g. for hp ontology embedding)
# output files are search-(db dataset)--data-(query dataset).tsv.gz


cd ../data/crossmap

# build an instance with ontology definitions
../../crossmap build --config config-hp.yaml

# perform search with many neighbors (for ontology embedding)
for N in 15
do
  OUTSEARCH="search-hp--data-hp-n$N-diff0.tsv.gz"
  if [ ! -f "$OUTSEARCH" ]
  then
    ../../crossmap search --config config-hp.yaml \
                          --dataset hp \
                          --data hp-parents.yaml.gz --n $N --tsv \
                          | gzip > $OUTSEARCH
  fi
done
