# searches against mouse models
# output files are search-(db dataset)--data-(query dataset).tsv.gz

cd ../data/crossmap
cp ../impc/mouse-model-concise.yaml.gz .
cp ../impc/mouse-model-complete.yaml.gz .

# use feature weighting from a crossmap instance build with the mp ontology
if [ ! -f "crossmap-mp-feature-map.tsv.gz" ]
then
  cp crossmap-mp/crossmap-mp-feature-map.tsv .
  gzip crossmap-mp-feature-map.tsv
fi


# build an instance with ontology definitions
../../crossmap build --config config-mouse-models.yaml


# perform search among mouse models
for TYPE in concise complete
do
  for DIFF in 0
  do
    OUTSEARCH="search-models-$TYPE--data-models-$TYPE-diff$DIFF.tsv.gz"
    if [ ! -f "$OUTSEARCH" ]
    then
      ../../crossmap search --config config-mouse-models.yaml \
                            --dataset $TYPE \
                            --data mouse-model-$TYPE.yaml.gz \
                            --n 15 --tsv \
                            --diffusion "{\"mp\":0.$DIFF}" \
                            | gzip > $OUTSEARCH
    fi
  done
done


for TYPE in concise complete
do
  for TRANSLATION in owlsim crossmap
  do
    for DISEASE in orphanet
    do
      for DIFF in 0
      do
        OUTSEARCH="search-models-$TYPE--data-$DISEASE-$TRANSLATION-diff$DIFF.tsv.gz"
        if [ ! -f "$OUTSEARCH" ]
        then
          ../../crossmap search --config config-mouse-models.yaml \
                                --dataset $TYPE \
                                --data $DISEASE-$TRANSLATION.yaml.gz \
                                --n 15 --tsv \
                                --diffusion "{\"mp\":0.$DIFF}" \
                                | gzip > $OUTSEARCH
        fi
      done
    done
  done
done

