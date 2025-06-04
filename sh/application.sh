#!/bin/bash
unset R_HOME

set -e

echo ""
echo "######## Setting up ############"
echo ""

echo ""
echo "The code will produce one bootstrap sample and compute parameter estimates for all models consider in the application. If running application.sh for the first time, you must set ID=1 next."
echo ""

echo ""
echo "Please enter the ID of the bootstrap you wish to perform (1-200)."
echo ""
read boot_id

echo ""
echo "Do you wish to generate the indices to determine the training/validation/test datasets echo? (y/n)"
echo "NOTE THAT THIS CAN BE QUITE COMPUTATIONALLY INTENSIVE AND REQUIRES >20G of RAM, and so are available in the repository."
echo ""
read bool

if ! [[ $bool == "y" ||  $bool == "Y" || $bool == "n" ||  $bool == "N" ]]; then
    echo "Please re-run and type y or n"
    exit 1
fi

if [[ $bool == "y" ||  $bool == "Y" ]]; then
	Rscript src/Application/make_validation_test_indices.R $boot_id
fi
	
echo ""
echo "Do you wish to train the probability of fire, p_0(s,t), models> (y/n)"
echo ""
read bool

if ! [[ $bool == "y" ||  $bool == "Y" || $bool == "n" ||  $bool == "N" ]]; then
    echo "Please re-run and type y or n"
    exit 1
fi

if [[ $bool == "y" ||  $bool == "Y" ]]; then
	echo ""
	echo "Training the probability of fire, p_0(s,t), models"
	echo ""

	echo ""
	echo "linear p_0(s,t) model"
	echo ""
	Rscript src/Application/bootstrap_p0_linear_estimate.R $boot_id

	echo ""
	echo "GAM p_0(s,t) model"
	echo ""
	Rscript src/Application/bootstrap_p0_GAM_estimate.R $boot_id

	echo ""
	echo "local PINN p_0(s,t) model"
	echo ""
	Rscript src/Application/bootstrap_p0_local_PINN_estimate.R $boot_id

	echo ""
	echo "global PINN p_0(s,t) model"
	echo ""
	Rscript src/Application/bootstrap_p0_local_PINN_estimate.R $boot_id

	echo ""
	echo "CNN p_0(s,t) model"
	echo ""
	Rscript src/Application/bootstrap_p0_NN_estimate.R $boot_id

	echo ""
	echo "All p_0(s,t) models trained"
	echo ""
fi

echo ""
echo "Do you wish to train the burnt area, Y(s,t)| Y(s,t) >0, models> (y/n)"
echo ""
read bool

if ! [[ $bool == "y" ||  $bool == "Y" || $bool == "n" ||  $bool == "N" ]]; then
    echo "Please re-run and type y or n"
    exit 1
fi

if [[ $bool == "y" ||  $bool == "Y" ]]; then
	echo ""
	echo "Training the threshold u(s,t) CNN model"
	echo ""

	Rscript src/Application/bootstrap_threshold_estimate.R $boot_id
fi

echo ""
echo "######## Everything finished! ############"
echo ""