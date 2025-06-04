# Source code for "Regression modelling of spatiotemporal extreme U.S. wildfires via partially-interpretable neural networks"

This repository contains code for reproducing the results in Regression modelling of spatiotemporal extreme U.S. wildfires via partially-interpretable neural networks" [(Richards and Huser, 2022)](https://arxiv.org/abs/2208.07581).

The methodology described in the manuscript has also been incorporated into the R package [pinnEV](https://github.com/Jbrich95/pinnEV).

## Repository structure

We first briefly describe the repository structure. The repository is organised into folders containing source code (`src`), intermediate objects generated from the source code (`intermediates`), figures (`img`), and controlling shell scripts that execute the source code (`sh`). Each folder is further divided into the following tree structure, where each branch is associated with one component of the manuscript:

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

The code will run on CPUs. No GPU acceleration is required. However, please note that simulation of the space-time Gaussian process to create the training/validation/test sets for the application requires >16Gb of RAM. The indices used in the paper are provided in the repository: intermediates/indices/.

### Reproducing the results

Replication scripts are included in shell scripts (in sh/) and should be invoked from the top of the directory. For example, one can replicate the application by invoking bash sh/application.sh. Given that 
