#!/bin/bash

awk -F',' 'NR>1 {print $2}' "taxonomy.csv" > "taxon_list.txt"

while read -r TAXON; do
    FILENAME="Zerofilled_By_Species/${TAXON}.csv"
    rm -f "${FILENAME}"
    echo "checklist_id,scientific_name,breeding_code,breeding_category,behavior_code,age_sex,observation_count,species_observed\n" > "${FILENAME}"
done < "taxon_list.txt"

mlr --csv --from zerofilled_observations.csv put -q 'tee > "Zerofilled_By_Species/".$scientific_name.".csv", $*'

IFS=","
for FNAME in Zerofilled_By_Species/*; do
  NEWFNAME=$(echo "${FNAME}" | tr ' ' '_')
  mv "${FNAME}" "${NEWFNAME}"
done
