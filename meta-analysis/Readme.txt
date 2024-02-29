Data and code for Nature manuscript titled “Environmental drivers of increased ecosystem respiration 
in a warming tundra”

Corresponding author Dr. Sybryn Maes – sybryn.maes@gmail.com 


Zenodo contains all collected data and the outputs on https://zenodo.org/doi/10.5281/zenodo.10572479. 

The paragraph names (df_1a through df_4b) refer to scripts. The *.csv files can be found in the
Zenodo repository.

df_0
-Study design Figure 1 and Extended Fig. 1 from main text

df_1a
-Effect size calculations of response (ER)
-Links to df_1.csv file with raw flux and environmental data
-Only the experiments that state ‘Open Access’ in the excel file Authors_Datasets (sheet 2). 
For experiments stating ‘Available Upon Request’, you need to contact the authors for the -raw flux data.

df_1b
-Effect size calculations of environmental drivers
-Links to df_1.csv file with raw flux data data (see above) and Dataset_ID.csv 
(this file includes all dataset IDs to merge the drivers into one dataframe)

df_2a-f
-Meta-analysis (2a) and meta-regression models (2b-f) (ER, N=136)
-Links to df_2.csv file with effect size data and context-dependencies and Forestplot_horiz_weights_fig.csv 
(this file includes the mean pooled Hedges SMD as well as the individual dataset Hedges SMD to plot figure 2)
-Contains code for Figs. 2-4 and Extended Figs 2-3

df_3
-Meta-regression for experimental warming duration
-Contains code for Fig. 5

df_4a           
-Effect size calculations of autotrophic-heterotrophic respiration partitioning (Ra, Rh, N=9)
-Links to df_3.csv file with raw partitioning data of subset experiments (output file df_4.csv)

df_4b           
-Sub-meta-analysis models (ER, Ra, Rh)
-Links to df_4.csv (input file)

NOTES
·       All additional input files for the meta-analysis R-scripts are included within the folders. 
·       ER, Ra, Rh = ecosystem, autotrophic, and heterotrophic respiration
·       N = sample size (number of datasets)


