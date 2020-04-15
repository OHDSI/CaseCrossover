CaseCrossover
=============

[![Build Status](https://travis-ci.org/OHDSI/CaseCrossover.svg?branch=master)](https://travis-ci.org/OHDSI/CaseCrossover)
[![codecov.io](https://codecov.io/github/OHDSI/CaseCrossover/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/CaseCrossover?branch=master)

CaseControl is part of the [OHDSI Methods Library](https://ohdsi.github.io/MethodsLibrary).

Introduction
============
CaseCrossover is an R package for performing case-crossover and case-time-control analyses in an observational database in the OMOP Common Data Model.

Features
========
- Extracts the necessary data from a database in OMOP Common Data Model format
- Nesting in a cohort of interest (e.g. people with a particular prior condition)
- Ability to specify multiple control windows
- Ability to adjust for time-trends in exposure (case-time-control design)
- Fitting outcome models using conditional logistic regression

Technology
==========
CaseCrossover is an R package.

System Requirements
===================
Requires R. Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in CaseCrossover require Java.

Installation
============
1. Make sure your R environment is properly configured. This means that Java must be installed, and on Windows RTools must be installed. See [these instructions](https://ohdsi.github.io/MethodsLibrary/rSetup.html) for how to configure your R environment.
3. In R, use the following commands to download and install CaseCrossover:

  ```r
  install.packages("drat")
  drat::addRepo("OHDSI")
  install.packages("CaseCrossover")
  ```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/CaseCrossover).

PDF versions of the documentation are also available:
* Vignette: [Single studies using the CaseCrossover package](https://raw.githubusercontent.com/OHDSI/CaseCrossover/master/inst/doc/SingleStudies.pdf)
* Vignette: [Running multiple analyses at once using the CaseCrossover package](https://raw.githubusercontent.com/OHDSI/CaseCrossover/master/inst/doc/MultipleAnalyses.pdf)
* Package manual: [CaseCrossover.pdf](https://raw.githubusercontent.com/OHDSI/CaseCrossover/master/extras/CaseCrossover.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/CaseCrossover/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/MethodsLibrary/contribute.html) how you can contribute to this package.

License
=======
CaseCrossover is licensed under Apache License 2.0

Development
===========
CaseCrossover is being developed in R Studio.

### Development status

Beta
