# Source code for "Regression modelling of spatiotemporal extreme U.S. wildfires via partially-interpretable neural networks"

This repository contains code for reproducing the results in Regression modelling of spatiotemporal extreme U.S. wildfires via partially-interpretable neural networks" [(Richards and Huser, 2022)](https://arxiv.org/abs/2208.07581).

The methodology described in the manuscript has also been incorporated into the R package [pinnEV](https://github.com/Jbrich95/pinnEV).

## Repository structure

We first briefly describe the repository structure, although an understanding of this structure is not needed to reproduce the results. The repository is organised into folders containing source code (`src`), intermediate objects generated from the source code (`intermediates`), figures (`img`), and controlling shell scripts that execute the source code (`sh`). Each folder is further divided into the following tree structure, where each branch is associated with one component of the manuscript:

```bash
├── Application         (Section 4)
├── Simulation          (Appendix B)
```

## Instructions

First, download this repository and navigate to its top-level directory within the terminal.

### Software dependencies

Before installing the software dependencies, users may wish to setup a [conda](https://docs.conda.io/projects/conda/en/latest/user-guide/install/linux.html) environment, so that the dependencies of this repository do not affect the user's current installation. To create a conda environment, run the following command in terminal:

```
conda create -n USWildfiresExtremes -c conda-forge r-base python=3.10.17
```

Then activate the conda environment with:

```
conda activate USWildfiresExtremes
```

The above conda environment installs R and Python 3 automatically. Once both R and Python are setup, install the R package dependencies (given in `dependencies.txt`) by running the following command from the top-level of the repository:

```
Rscript dependencies_install.R; Rscript keras_check.R 
```
This will also install the Python dependencies, via the R package `reticulate`.

Note that Linux systems have several system dependencies that may need to be installed before installing `devtools` (e.g., `fontconfig1`, `harfbuzz`, and `fribidi`). If you run into problems when installing `devtools`, try installing it manually with  `conda` using the command (other R packages can also be installed this way):

```
conda install -c conda-forge r-devtools
```

or by manually installing the system dependencies before installing `devtools` manually in R:

```
sudo apt -y install libfontconfig1-dev libharfbuzz-dev libfribidi-dev
```


### Hardware requirements

The code will run on CPUs. No GPU acceleration is required.

### Reproducing the results

The replication script is `sh/all.sh`, invoked using `bash sh/all.sh` from the top level of this repository. For all studies, the replication script will automatically train the neural estimators, generate estimates from both the neural and likelihood-based estimators, and populate the `img` folder with the figures and results of the manuscript.

The nature of our experiments means that the run time for reproducing the results of the manuscript is substantial (1-2 days in total). When running the replication script, the user will be prompted with an option to quickly establish that the code is working by using a small number of parameter configurations and epochs. Our envisioned workflow is to establish that the code is working with this "quick" option, clear the populated folders by entering `bash sh/clear.sh`, and then run the code in full (possibly over the weekend). **NB:** under this "quick" option, very few training samples and epochs are used when training the GNN, and the results produced will therefore not be meaningful and should not be interpreted.  

Note that the replication script is clearly presented and commented; hence, one may easily "comment out" sections to produce a subset of the results. (Comments in `.sh` files are made with `#`.) 

