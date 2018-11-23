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
- Fitting outcome models using conditional logisitc regression

Technology
==========
CaseCrossover is an R package.

System Requirements
===================
Requires R (version 3.2.2 or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in CaseCrossover require Java.

Installation
============
1. On Windows, make sure [RTools](http://cran.r-project.org/bin/windows/Rtools/) is installed.
2. The DatabaseConnector and SqlRender packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>.
3. In R, use the following commands to download and install CaseCrossover:

  ```r
  install.packages("drat")
  drat::addRepo("OHDSI")
  install.packages("CaseCrossover")
  ```

User Documentation
==================
* Vignette: [Single studies using the CaseCrossover package](https://raw.githubusercontent.com/OHDSI/CaseCrossover/master/inst/doc/SingleStudies.pdf)
* Vignette: [Running multiple analyses at once using the CaseCrossover package](https://raw.githubusercontent.com/OHDSI/CaseCrossover/master/inst/doc/MultipleAnalyses.pdf)
* Package manual: [CaseCrossover.pdf](https://raw.githubusercontent.com/OHDSI/CaseCrossover/master/extras/CaseCrossover.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/CaseCrossover/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
CaseCrossover is licensed under Apache License 2.0

Development
===========
CaseCrossover is being developed in R Studio.

### Development status

Beta
