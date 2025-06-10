#!/bin/bash
unset R_HOME

set -e

echo ""
echo "######## Collating scores ############"
echo ""

echo ""
echo "The code will collate and print the test scores for the fitted models computed by running sh/application.sh. "
echo ""

echo ""
echo "The code will work with only one bootstrap sample estimate (for each model), but we recommend running sh/application.sh for multiple samples. "
echo ""

echo ""
echo "Have you ran sh/application.sh at least once? (y/n)"
echo ""
read bool

if ! [[ $bool == "y" ||  $bool == "Y" || $bool == "n" ||  $bool == "N" ]]; then
    echo "Please re-run and type y or n"
    exit 1
fi

if [[ $bool == "y" ||  $bool == "Y" ]]; then
	Rscript src/Application/collate_p0_results.R;
	Rscript src/Application/collate_bGEV_results.R 

fi
	

if [[ $bool == "n" ||  $bool == "N" ]]; then
    echo "Then you should probably do that!"
    exit 1
fi
echo ""
echo "######## Everything finished! ############"
echo ""