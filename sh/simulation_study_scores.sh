#!/bin/bash
unset R_HOME

set -e

echo ""
echo "######## Collating scores ############"
echo ""

echo ""
echo "The code will collate and print the scores for the simulation study computed by running sh/simulation_study.sh. "
echo ""

echo ""
echo "The code will work with only one replicated estimate, but we recommend running sh/simulation_study.sh for multiple experiments. "
echo ""

echo ""
echo "Have you ran sh/simulation_study.sh at least once? (y/n)"
echo ""
read bool

if ! [[ $bool == "y" ||  $bool == "Y" || $bool == "n" ||  $bool == "N" ]]; then
    echo "Please re-run and type y or n"
    exit 1
fi

if [[ $bool == "y" ||  $bool == "Y" ]]; then
	Rscript src/Simulation_study/collate_results.R;

fi
	

if [[ $bool == "n" ||  $bool == "N" ]]; then
    echo "Then you should probably do that!"
    exit 1
fi
echo ""
echo "######## Everything finished! ############"
echo ""