
<!-- README.md is generated from README.Rmd. Please edit that file -->
gamar
=====

The package `gamar` provides an interface to the GAMA simulation platform ([gama-platform.org](gama-platform.org)). It loads gaml files defining models and provides facilities to define plans of experiments. It also call the GAMA engine to run defined plans of experiments. The simulation results can also be loaded into R for subsequent analyses.

Installation
------------

Before installing `gamar` you need to install the package `devtools` if not already installed on your system:

``` r
> if (!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
```

Once `devtools` is installed on your system, you can install `gamar` directly from GitHub:

``` r
> devtools::install_github("choisy/gamar")
```

Usage
-----

The package `gamar` contains 6 functions:

-   `defpath` to define the path to the GAMA executable
-   `examples` to manipulate examples from the built-in library
-   `list_gaml` to list the paths to the gaml models files in a hierarchy
-   `list_experiment` to list experiments of a gaml model file
-   `experiment` to create an object of class `experiment`
-   `load_experiment` to load an experiment of a model

See articles and vignettes for more details.
