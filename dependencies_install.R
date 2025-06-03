# ---- Helper functions for this script ----

## Facilitates user input regardless of how this script was invoked
user_decision <- function(prompt, allowed_answers = c("y", "n")) {
  
  if (interactive()) {
    answer <- readline(paste0(prompt, "\n"))
  } else {
    cat(prompt)
    answer <- readLines("stdin", n = 1)
  }
  
  answer <- tolower(answer)
  
  if (!(answer %in% allowed_answers)) {
    tmp <- paste(allowed_answers, collapse = " or ")
    cat(paste0("Please enter ", tmp, ".\n"))
    answer <- user_decision(prompt, allowed_answers = allowed_answers)
  }
  
  return(answer)
}

# Installs the dependencies
install_dependencies <- function(install_exact_versions) {
  
  x <- read.table("dependencies.txt", header = FALSE)
  pkg_versions <- setNames(as.character(x[, 2]), x[, 1])
  rm(x)
  
  
  # ---- CRAN packages ----
  
  ## Find the packages not yet installed and add them to the list
  installed_idx <- names(pkg_versions) %in% rownames(installed.packages())
  new_packages  <- names(pkg_versions)[!(installed_idx)]
  
  if (exists("install_exact_versions") && install_exact_versions) {
    ## Find the packages that are installed, but not the correct version
    installed_pkg_versions <- sapply(names(pkg_versions)[installed_idx], function(pkg) as.character(packageVersion(pkg)))
    idx          <- installed_pkg_versions != pkg_versions[installed_idx]
    already_installed_pkgs_different_versions <- names(installed_pkg_versions)[idx]
  }
  
  ## Now install the new packages: Here, we always install the correct
  ## package version (no reason not to)
  if(length(new_packages)) {
    cat("\nPackage dependencies are being installed automatically using dependencies_install.R\n")
    for (pkg in new_packages) {
      devtools::install_version(pkg, version = pkg_versions[pkg],
                                repos = CRANMIRROR, upgrade = "never",
                                dependencies = TRUE)
    }
  }
  
  # Change the already installed packages to the correct versions IF we have been told to do so
  if(exists("install_exact_versions") && install_exact_versions && length(already_installed_pkgs_different_versions)) {
    for (pkg in already_installed_pkgs_different_versions) {
      devtools::install_version(pkg, version = pkg_versions[pkg],
                                repos = CRANMIRROR, upgrade = "never",
                                dependencies = TRUE)
    }
  }
  
  
}


# ---- Install dependencies ----

install_depends <- user_decision("Do you want to automatically install package dependencies? (y/n)")

if (install_depends == "y") {
  
  install_exact_versions <- user_decision("Do you want to ensure that all package versions are as given in dependencies.txt (this option is recommended)? (y/n)")
  install_exact_versions <- install_exact_versions == "y" # Convert to Boolean
  
  
  CRANMIRROR <- "https://cran.ma.imperial.ac.uk"
  
  ## devtools is required for installing the other packages, so it has to be installed here
  if (!("devtools" %in% rownames(installed.packages()))) {
    cat("Installing the package 'devtools'...\n")
    install.packages("devtools", repos = CRANMIRROR, dependencies = TRUE)
  }
  if (!("devtools" %in% rownames(installed.packages()))) stop("\nThe package 'devtools' failed to install, please install it manually. \n Note that on Linux systems there are several system dependencies that may need to be installed before installing devtools (e.g., fontconfig1, harfbuzz, and fribidi). Try using the following command before installing devtools: \n sudo apt -y install libfontconfig1-dev libharfbuzz-dev libfribidi-dev\n On mac)S, you may need to install libgit2: \n brew install libgit2\n 
 ")
  
  
  if (install_exact_versions) {
    cat("When changing the packages to the versions specified in dependencies.txt, please use your discretion when answering the question “Which would you like to update?”.  Updating all packages (i.e., option 3) may cause errors.\n")
  }
  
  
  install_dependencies(install_exact_versions)
}




# Installs tensorflow and Keras via R-reticulate

packages = c("keras", "tensorflow", "reticulate")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# TF version 2.15.0
tf_version = "2.15.0"
reticulate::use_condaenv("USWildfiresExtremes", required = T)
keras::install_keras(method = "conda",
                     envname = "USWildfiresExtremes",
                     version = tf_version) #Install version of tensorflow in virtual environment



