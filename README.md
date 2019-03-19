[![Build Status](https://travis-ci.com/2DegreesInvesting/pacta.svg?token=2zLsWyJspq4x2F3gWvyf&branch=master)](https://travis-ci.com/2DegreesInvesting/pacta)
[![codecov](https://codecov.io/gh/2DegreesInvesting/pacta/branch/master/graphs/badge.svg)](https://codecov.io/gh/2DegreesInvesting/pacta)


# pacta

R package to perform Paris Agreement Capital Transition Assessment for corporate lending portfolio including matching and analysis module

## Purpose

Package includes useful functions and procedures which can be used by any user. Purpose of package is to implement support for standard workflow operartions. Users may use own workflows (different data formats, different match rules, etc) while still using standard building blocks.

All package makes available set of functions which are pure in their behaviour - so they fully depend on input and produce output only according to input. No IO operations are done inside functions. All file access for reads and writes are performed script level and not part of this package.

## Simple package installation 

Install R first from [cran](https://cran.r-project.org). Please install R into local folder. Due to many files R works slowly when it is installed on network drive.

Enter R envinronment, install additional packages this one

```R
install.packages("devtools")
devtools::install_github(repo = "2DegreesInvesting/pacta")
```

## Installation of sources 

Please note that `devtools` and `roxygen2` packages is required. This is easiest way to build packages in R. Please open R session and run following command

```R
install.packages(c("devtools", "roxygen2"))
```

After that you can clone git repository into your working folder

```sh
git clone https://github.com/2DegreesInvesting/pacta.git
```

Now everything is ready, you can run RStudio and open project - there is a Rproj file in the root folder.

```sh
./pacta.Rproj
```

After project is open it needs to be built. Run `[Cmd/Ctrl]`+`[Shift]`+`B` to build project. R might request you to install additional packages used, please refer to `Imports:` section of package [DESCRIPTION](/DESCRIPTION).

## Making changes 

Please refer to [CONTRIBUTING](/CONTRIBUTING.md) document that describes workflow used for changes. Few important rules worth mentioning again:

* all changes are done via branches, no changes in `master` please;
* first write test then write functionality - take care of automated testing;
* always biuld - `[Cmd/Ctrl]`+`[Shift]`+`B` - for documentation update;
* always test - `[Cmd/Ctrl]`+`[Shift]`+`T` - to make sure regression tests are passing;
* always check - `[Cmd/Ctrl]`+`[Shift]`+`E` - to make sure package passes integration tests and additional checks. 

Please keep Warnings and Errors on **zero level**.

## Documentation

Please activate documentation generation during builds - go to Rstudio menu `Tools` -> `Project options...` -> `Build Tools` -> activate flag `Generate documentation with Roxygen`, theck click on `Configure` button and mark `Automatically roxygenize when running` / `Build & Reload`
