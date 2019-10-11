# Automating Dynamic Consent: Dataset

This repository contains the dataset and code for the paper: **Automating dynamic consent decisions for the processing of social media data in health research**, which is to appear in the Journal of Empirical Research on Human Research Ethics.


### Instructions

*analysis.R* is the main R file. Use this to run the code. There may be some issues due to packages being updated since the code was written, which I will do my best to return to and fix. Note that I believe that some functions used do not support R's set.seed functionality, resulting in different train/test/validation sets as those used for the analysis in the paper.

Alternatively, the *RData* file contains the global environment for the data as it was executed for the paper. This contains the same results as those presented in the paper (i.e. this contains the train/test/validation sets as reported).

More information coming soon.
