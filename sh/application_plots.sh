#!/bin/bash
unset R_HOME

set -e

echo ""
echo "######## Creating plots ############"
echo ""

echo ""
echo "The code will create the plots provide in the paper. Most of these will be estimates related to the fitted models computed by running sh/application.sh. "
echo ""

echo ""
echo "The code will work with only one bootstrap sample estimate (for each model), but we recommend running sh/application.sh for multiple samples (or, as in the paper, all 200). "
echo ""

echo ""
echo "The code will take a couple of hours to run and requires >20G RAM."
echo ""

echo ""
echo "Proceed? (y/n)"
echo ""
read bool
if ! [[ $bool == "y" ||  $bool == "Y"  ]]; then
    echo "Please re-run and type y or n"
    exit 1
fi

if [[ $bool == "n" ||  $bool == "N" ]]; then
exit 1
fi


echo ""
echo "Have you ran sh/application.sh at least once? (y/n)"
echo ""
read bool

if ! [[ $bool == "y" ||  $bool == "Y" || $bool == "n" ||  $bool == "N" ]]; then
    echo "Please re-run and type y or n"
    exit 1
fi

if [[ $bool == "y" ||  $bool == "Y" ]]; then
	Rscript src/Application/make_plots.R;

fi
	

if [[ $bool == "n" ||  $bool == "N" ]]; then
    echo "Then you should probably do that!"
    exit 1
fi
echo ""
echo "######## Everything finished! ############"
echo ""