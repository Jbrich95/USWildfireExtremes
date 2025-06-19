#!/bin/bash
unset R_HOME

set -e

echo ""
echo "######## Setting up ############"
echo ""

echo ""
echo "The code will produce one replicated experiment of the simulation study of your choosing."
echo ""

	
echo ""
echo "Do you wish to run a replicate of study B2 for parameter estimation?> (y/n)"
echo ""
read bool

if ! [[ $bool == "y" ||  $bool == "Y" || $bool == "n" ||  $bool == "N" ]]; then
    echo "Please re-run and type y or n"
    exit 1
fi

if [[ $bool == "y" ||  $bool == "Y" ]]; then
echo ""
echo "Please enter the replicate ID that you wish to perform (1-100)."
echo ""
read rep_id

echo ""
echo "Please enter the sample size n."
echo ""
read n

Rscript src/Simulation_study/B2_parameter_estimation.R $rep_id $n
fi

echo ""
echo "Do you wish to run a replicate of study B3.1 for response distribution misspecification? (y/n)"
echo ""
read bool

if ! [[ $bool == "y" ||  $bool == "Y" || $bool == "n" ||  $bool == "N" ]]; then
    echo "Please re-run and type y or n"
    exit 1
fi

if [[ $bool == "y" ||  $bool == "Y" ]]; then
echo ""
echo "Please enter the replicate ID that you wish to perform (1-100)."
echo ""
read rep_id

echo ""
echo "Please enter the sample size n."
echo ""
read n

echo ""
echo "Please enter the case ID (1=log-normal, 2=GPD)"
echo ""
read case_id

Rscript src/Simulation_study/B3_1_misspecified_response.R $rep_id $n $case_id
fi


echo ""
echo "######## Everything finished! ############"
echo ""