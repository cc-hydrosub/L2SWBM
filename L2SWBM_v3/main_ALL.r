####################################################################################################
####################################################################################################
####################################################################################################
### L2SWBM Operational Script
### Joeseph Smith
### University of Michigan, Cooperative Institute for Great Lakes Research
### National Oceanic and Atmospheric Administration - Great Lakes Environmental Research Laboratory
### Ann Arbor, MI, USA
####################################################################################################
####################################################################################################
####################################################################################################

library(rjags)

##### SET WORKING DIRECTORY

rootDir = "D:/L2SWBM_v3"
#rootDir = "C:/Users/joexi/Desktop/L2SWBM_v3"
setwd(rootDir);

### GET FUNCTIONS, GLOBAL (GREAT LAKES WIDE) VARIABLES AND CONFIGURATION
source('configEtc.r');

### GET DATA
source('data_ALL.r');

source('proc_prior_ALL.r');

### CREATE RESULTS DIRECTORY
if(!file.exists(paste("results", sep=''))){
	dir.create(paste("results", sep=''));
}

### CREATE MODEL DIRECTORY
if(!file.exists(paste("results/", modelName, sep=''))){
	dir.create(paste("results/", modelName, sep=''));
}
setwd(paste(rootDir,"/results/", modelName, sep=''))

write.table(config, paste(modelName,'_config.csv', sep=''), sep=',', row.names=TRUE, col.names=c("Config", "Value"))

### PREVIEW DATA
source('../../priorPlotter.r')

source('../../tsPlotter_Preview_ALL.r')

### WRITE THE MODEL

source('../../proc_model_write_ALL.r');

### RUN THE MODEL

source('../../proc_model_run_ALL.r');

### SAVE DATA

save.image(paste(modelName,'.RData', sep=''));

### PLOTS

source('../../tsPlotter_ALL.r');

if(checkModel){
	source('../../statsGen.r');
}

source('../../dataGen_ALL.r');
