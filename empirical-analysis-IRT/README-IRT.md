# Bunce-2020-xcultural-competence/empirical-analysis-IRT
files relating to the manuscript:

Bunce JA (2020). Sustaining Cultural Diversity Through Cross-Cultural Competence.

The original preprint is on SocArXiv [here](https://osf.io/preprints/socarxiv/bwtvu/)


Steps to reproduce the analysis in this paper:

1) Create a project folder on your machine. Name it whatever you want.

2) Inside this project folder, put the file ``RunAll_IRT.R``

3) Also inside the project folder, create three sub-folders named (exactly) ``Code``, ``Plots``, and ``Data``

4) Inside the ``Data`` folder, put the file ``Manu_perceptions_11sep18.csv``

5) Inside the ``Code`` folder, put all the other files.

6) Open the file ``RunAll_IRT.R``. Inside it, you can set the path to your project folder. Then run its parts in order in R.

Figures in the manuscript and appendix will appear in the ``Plots`` folder.

It can take several hours to run all three models in this analysis to convergence. However, you can get pretty good estimates by using two mcmc chains of 1000 samples each, which should only take a few minutes. ``RunAll_IRT.R`` lets you modify the number of mcmc chains and samples. 

