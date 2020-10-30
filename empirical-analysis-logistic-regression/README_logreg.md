# Bunce-2020-xcultural-competence/empirical-analysis-logistic-regression
files relating to the manuscript:

Bunce JA (2020). Cultural Diversity in Unequal Societies Sustained Through Cross-Cultural Competence.

The original preprint is on SocArXiv [here](https://osf.io/preprints/socarxiv/bwtvu/)

This is the analysis of the empirical data using logistic regression, corresponding to Appendix Figures S1, S3, and S4.


Steps to reproduce the analysis in this paper:

1) Create a project folder on your machine. Name it whatever you want.

2) Inside this project folder, put the file ``RunAll.R``

3) Also inside the project folder, create three sub-folders named (exactly) ``Code``, ``Plots``, and ``Data``

4) Inside the ``Data`` folder, put the file ``Manu_perceptions_28aug20.csv``

5) Inside the ``Code`` folder, put all the other files.

6) Open the file ``RunAll.R``. Inside it, you can set the path to your project folder. Then run its parts in order in R.

Figures in the manuscript and appendix will appear in the ``Plots`` folder.

It can take several hours to run both models in this analysis to convergence. However, you can get pretty good estimates by using two mcmc chains of 1000 samples each, which should only take a few minutes. ``RunAll.R`` lets you modify the number of mcmc chains and samples. 

