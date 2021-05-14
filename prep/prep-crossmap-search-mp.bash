# searches against mp terms (e.g. for ontology translation)
# output files are search-(db dataset)--data-(query dataset).tsv.gz


cd ../data/crossmap

# build an instance with ontology definitions
../../crossmap build --config config-mp.yaml
../../crossmap add --config config-mp.yaml \
                   --dataset manual --data hpmp-manual.yaml

# perform search for 1-to-1 ontology translation (with and without diffusion)
for N in 1
do
  for DIFF in 0 1
  do
    OUTSEARCH="search-mp--data-hp-n$N-diff$DIFF.tsv.gz"
    if [ ! -f "$OUTSEARCH" ]
    then
      ../../crossmap search --config config-mp.yaml \
                            --dataset mp \
                            --data hp-parents.yaml.gz --n $N --tsv \
                            --diffusion "{\"mp\":0.$DIFF,\"manual\":$DIFF.0}" \
                            | gzip > $OUTSEARCH
    fi
  done
done


# perform search with many neighbors (for ontology embedding)
for N in 15
do
  OUTSEARCH="search-mp--data-mp-n$N-diff0.tsv.gz"
  if [ ! -f "$OUTSEARCH" ]
  then
    ../../crossmap search --config config-mp.yaml \
                          --dataset mp \
                          --data mp-parents.yaml.gz --n $N --tsv \
                          | gzip > $OUTSEARCH
  fi
done
