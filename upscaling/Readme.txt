This is the code repository for the article XXX

INTRODUCTION

All code is collected in this directory. It does not form a single script or program, rather, 
it is a collection of steps. While this is suboptimal, the computational requirements of the
steps are very different, and as the analysis was not intended to be a part of a single, complete 
model, it was efficient to perform the steps in different environments. The environment for
running each step is described in the comments of each code file. This split between environments,
made it inconvenient to script the whole process. Furthermore, the environments used by others that
intend to reproduce the analysis will almost inevitably be different, rendering such a script 
less than ideal.

The scripts assume they are started to run in the directory where the data files are stored.
Those that take a significant amount of time use data split into chunks, typically 40. 
Modify for your needs and computing resources. The scripts for splitting the data are provided.

The step by step preparation and analysis leaves behind a large number of intermediate files. 
The code does not remove the files so the analysis steps can be run multiple times and the
intermediate results are visible. The files do consume a lot of space, so you may want to 
remove them afterwards. 

ANALYSIS STEPS

Steps in order of execution. Refer to the comments in each of the code sections for
details. For methodology, please refer to the article and its suppelement. The numbered steps 
here do not refer to the graphical presentation of the method structure, but rather to the 
division between code files.



1. Retrieve the required data. How to do this depends on the available software. 
ISRIC prodides a number of APIs for data retrieval. The API descriptions and instructions can 
be found here: https://www.isric.org/explore/soilgrids (see under section DATA ACCESS). The data 
is located here: https://files.isric.org/soilgrids/latest/data/ but simply retrieving
all of it is very resource consuming. Please use the recommended methods if possible, for
example WCS can automatically retrieve a subset of the data. The rest of the code assumes
a full coverage map that was locally available, but it is cropped to the study area
early in the process, in step 3.

The required data are:
soc, soil organic carbon content in the fine earth fraction
bdod, bulk density of the fine earth fraction
nitrogen, total nitrogen 

2. Generate study area masks. The code is in 2_mask_for_study_area.R.

3. Generate bootstrap sampling from the observed soil parameters.

4. Produce the baseline and modelled respiration. The code is in 4_tabulatedResPlotting.R. 
Additionally, the script produces tabulated results and figures Fig 6, SI Fig 6, and SI Fig 7

5. Aggregate the soilgrid data to the required resolution, here 1x1 km.
The code is in the files 5a_aggregate_bdod.R, 5b_aggregate_n.R, and 5c_aggregate_soc.R. 

6. Prepare data for analysis. The script 6_prepareData.R finds where the mineral layer
begins, calculates the weighted means of the soil variables over the layer and projects them in crs3995,
finally storing the data for further processing. Please see details on running the steps in the script,
the process takes a lot of memory.

7. The Monte Carlo simulation of the soil parameters in 7_monteCarloSoil.R. The nitrogen and soil organic carbon
data from previous steps is read in, and n=100 new distributions are simulated for nitrogen and CN (refer
to method in article).

8. Run the model predictions for each of the 100 distributions of N and CN. The data is split into slices using 
8a_slice_data.R to enable parallel processing. The predict code is in 8b_predict.R, which is run using slurm and the
configuration script 8c_startmod.sh. The actual command and contents of the configuration script depend on the
cluster used. The code can also be run without clustering with minimal modifications, but it will take long.

9. Combine the resulting distributions using 9a_mixture_dists.R. This is also parallelized, and the sample 
configuration script is in 9b_startmix.sh.

10. Finally, combine the slices and recreate the results data in raster representation using 10_recreate_rasters.R.

