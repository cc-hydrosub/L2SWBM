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

setwd("D:/L2SWBM_Ops");

### GET FUNCTIONS, GLOBAL (GREAT LAKES WIDE) VARIABLES AND CONFIGURATION
source('configEtc.r');

### GET DATA
source('data_ALL.r');

### PREVIEW DATA
#source('tsPlotter_Preview_ALL.r')

source('proc_ALL.r');

save.image(paste('L2SWBM_ALL_',rollPeriod,'_',
	as.numeric(biasOutflows),
	as.numeric(incProcError),
	as.numeric(checkModel),
	as.numeric(dHPrecDefined),
'_',startAnalysisYear,'_',startAnalysisMonth,'_',endAnalysisYear,'_',endAnalysisMonth,'_',iters,'_',modelSuffix,'.RData',sep=''));

source('tsPlotter_ALL.r');
if(checkModel){
	source('statsGen.r');
}
source('dataGen_ALL.r');
