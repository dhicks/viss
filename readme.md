---
title: 'Data and analysis code for "Developing Measures of Public Perceptions of Values in Science"'
authors: "D.J. Hicks, Emilio J.C. Lobato, Cosmo E. Campbell, Joseph Dad"
---

Data for study 1 and 3 are available in `data/01` and `data/03`, respectively. Those data files were cleaned and anonymized using the scripts `scripts/01/01_clean.R` and `scripts/03/01_clean.R`. 

To reproduce the analysis, run the scripts in numerical order in `scripts/01` and then `scripts/03`.  `renv` was initially used to maintain package versions, but in some cases analysis at later steps of the project required using more recent package versions, and the `renv` lockfile was not maintained.  We have not confirmed that the analysis is fully reproducible starting from scratch. 