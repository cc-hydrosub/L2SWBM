# FUNCTIONS, GLOBAL (GREAT LAKES WIDE) VARIABLES AND CONFIGURATION

config = read.csv('config.csv', skip=0, row.names=1)

modelName = as.character(config['Model Name',1]);

startAnalysis = as.numeric(unlist(strsplit(as.character(
	config['Analysis Start',1]), ' ', fixed=TRUE)));

startAnalysisYear = startAnalysis[1]
startAnalysisMonth = startAnalysis[2]

endAnalysis = as.numeric(unlist(strsplit(as.character(
	config['Analysis End',1]), ' ', fixed=TRUE)));

endAnalysisYear = endAnalysis[1]
endAnalysisMonth = endAnalysis[2]

priorPrecipStart = as.numeric(unlist(strsplit(as.character(
	config['Prior Precip Start',1]), ' ', fixed=TRUE)));
priorPrecipEnd = as.numeric(unlist(strsplit(as.character(
	config['Prior Precip End',1]), ' ', fixed=TRUE)));

priorEvapStart = as.numeric(unlist(strsplit(as.character(
	config['Prior Evap Start',1]), ' ', fixed=TRUE)));
priorEvapEnd = as.numeric(unlist(strsplit(as.character(
	config['Prior Evap End',1]), ' ', fixed=TRUE)));

priorRunoffStart = as.numeric(unlist(strsplit(as.character(
	config['Prior Runoff Start',1]), ' ', fixed=TRUE)));
priorRunoffEnd = as.numeric(unlist(strsplit(as.character(
	config['Prior Runoff End',1]), ' ', fixed=TRUE)));

priorOutflowStart = as.numeric(unlist(strsplit(as.character(
	config['Prior Outflow Start',1]), ' ', fixed=TRUE)));
priorOutflowEnd = as.numeric(unlist(strsplit(as.character(
	config['Prior Outflow End',1]), ' ', fixed=TRUE)));

priorDiversionStart = as.numeric(unlist(strsplit(as.character(
	config['Prior Diversion Start',1]), ' ', fixed=TRUE)));
priorDiversionEnd = as.numeric(unlist(strsplit(as.character(
	config['Prior Diversion End',1]), ' ', fixed=TRUE)));

priorNBSStart = as.numeric(unlist(strsplit(as.character(
	config['Prior NBS Start',1]), ' ', fixed=TRUE)));
priorNBSEnd = as.numeric(unlist(strsplit(as.character(
	config['Prior NBS End',1]), ' ', fixed=TRUE)));

priorPrecipColumn = as.integer(as.vector(config['Prior Precip Column',1]))
priorEvapColumn = as.integer(as.vector(config['Prior Evap Column',1]))
priorRunoffColumn = as.integer(as.vector(config['Prior Runoff Column',1]))
priorOutflowColumn = as.integer(as.vector(config['Prior Outflow Column',1]))
priorDiversionColumn = as.integer(as.vector(config['Prior Diversion Column',1]))
priorNBSColumn = as.integer(as.vector(config['Prior NBS Column',1]))
residNBSColumn = as.integer(as.vector(config['Residual NBS Column',1]))

evapPriorPrecMod = as.numeric(as.vector(config['Evap Prior Precision Mod',1]))
outflowPriorPrecMod = as.numeric(as.vector(config['Outflow Prior Precision Mod',1]))
diversionPriorPrecMod = as.numeric(as.vector(config['Diversion Prior Precision Mod',1]))
nbsPriorPrecMod = as.numeric(as.vector(config['NBS Prior Precision Mod',1]))

biasOutflows = as.logical(config['Outflow Bias',1])
flowUncertaintyInPercent = as.logical(config['Flow Uncertainty in Percent',1])

marysUncertainty = as.numeric(as.vector(config['St. Marys Uncertainty',1]))
ollUncertainty = as.numeric(as.vector(config['Ogoki Long-Lac Uncertainty',1]))
clairRiverUncertainty = as.numeric(as.vector(config['St. Clair River Uncertainty',1]))
chicagoUncertainty = as.numeric(as.vector(config['Chicago Uncertainty',1]))
detroitUncertainty = as.numeric(as.vector(config['Detroit Uncertainty',1]))
niagaraUncertainty = as.numeric(as.vector(config['Niagara Uncertainty',1]))
wellandUncertainty = as.numeric(as.vector(config['Welland Uncertainty',1]))
lawrenceUncertainty = as.numeric(as.vector(config['St. Lawrence Uncertainty',1]))

lakesPrecip = unlist(strsplit(as.character(config['Lakes Precip',1]), ' ', fixed=TRUE));
lakesEvap = unlist(strsplit(as.character(config['Lakes Evap',1]), ' ', fixed=TRUE));
lakesRunoff = unlist(strsplit(as.character(config['Lakes Runoff',1]), ' ', fixed=TRUE));
lakesOutflow = unlist(strsplit(as.character(config['Lakes Outflow',1]), ' ', fixed=TRUE));
lakesDiversion = unlist(strsplit(as.character(config['Lakes Diversion',1]), ' ', fixed=TRUE));
lakesNBS = unlist(strsplit(as.character(config['Lakes NBS',1]), ' ', fixed=TRUE));

precipInput = as.numeric(unlist(strsplit(as.character(
	config['Precip Inputs',1]), ' ', fixed=TRUE)));
evapInput = as.numeric(unlist(strsplit(as.character(
	config['Evap Inputs',1]), ' ', fixed=TRUE)));
runoffInput = as.numeric(unlist(strsplit(as.character(
	config['Runoff Inputs',1]), ' ', fixed=TRUE)));
outflowInput = as.numeric(unlist(strsplit(as.character(
	config['Outflow Inputs',1]), ' ', fixed=TRUE)));
diversionInput = as.numeric(unlist(strsplit(as.character(
	config['Diversion Inputs',1]), ' ', fixed=TRUE)));
nbsInput = as.numeric(unlist(strsplit(as.character(
	config['NBS Inputs',1]), ' ', fixed=TRUE)));

pObsPriorMeanBias = as.numeric(unlist(strsplit(as.character(
	config['Precip Obs Prior Mean Bias',1]), ' ', fixed=TRUE)))[as.logical(precipInput)];
pObsPriorBiasDev = as.numeric(unlist(strsplit(as.character(
	config['Precip Obs Prior Bias Std. Dev',1]), ' ', fixed=TRUE)))[as.logical(precipInput)];
pObsPriorBiasDev[pObsPriorBiasDev <= 0] = 10

eObsPriorMeanBias = as.numeric(unlist(strsplit(as.character(
	config['Evap Obs Prior Mean Bias',1]), ' ', fixed=TRUE)))[as.logical(evapInput)];
eObsPriorBiasDev = as.numeric(unlist(strsplit(as.character(
	config['Evap Obs Prior Bias Std. Dev',1]), ' ', fixed=TRUE)))[as.logical(evapInput)];
eObsPriorBiasDev[eObsPriorBiasDev <= 0] = 10

rObsPriorMeanBias = as.numeric(unlist(strsplit(as.character(
	config['Runoff Obs Prior Mean Bias',1]), ' ', fixed=TRUE)))[as.logical(runoffInput)];
rObsPriorBiasDev = as.numeric(unlist(strsplit(as.character(
	config['Runoff Obs Prior Bias Std. Dev',1]), ' ', fixed=TRUE)))[as.logical(runoffInput)];
rObsPriorBiasDev[rObsPriorBiasDev <= 0] = 10

oObsPriorMeanBias = as.numeric(unlist(strsplit(as.character(
	config['Outflow Obs Prior Mean Bias',1]), ' ', fixed=TRUE)))[as.logical(outflowInput)];
oObsPriorBiasDev = as.numeric(unlist(strsplit(as.character(
	config['Outflow Obs Prior Bias Std. Dev',1]), ' ', fixed=TRUE)))[as.logical(outflowInput)];
oObsPriorBiasDev[oObsPriorBiasDev <= 0] = 200

dObsPriorMeanBias = as.numeric(unlist(strsplit(as.character(
	config['Diversion Obs Prior Mean Bias',1]), ' ', fixed=TRUE)))[as.logical(diversionInput)];
dObsPriorBiasDev = as.numeric(unlist(strsplit(as.character(
	config['Diversion Obs Prior Bias Std. Dev',1]), ' ', fixed=TRUE)))[as.logical(diversionInput)];
dObsPriorBiasDev[dObsPriorBiasDev <= 0] = 10

nObsPriorMeanBias = as.numeric(unlist(strsplit(as.character(
	config['NBS Obs Prior Mean Bias',1]), ' ', fixed=TRUE)))[as.logical(nbsInput)];
nObsPriorBiasDev = as.numeric(unlist(strsplit(as.character(
	config['NBS Obs Prior Bias Std. Dev',1]), ' ', fixed=TRUE)))[as.logical(nbsInput)];
nObsPriorBiasDev[nObsPriorBiasDev <= 0] = 30

pObsSD = as.numeric(unlist(strsplit(as.character(
	config['Precip Obs Std. Dev',1]), ' ', fixed=TRUE)))[as.logical(precipInput)];
eObsSD = as.numeric(unlist(strsplit(as.character(
	config['Evap Obs Std. Dev',1]), ' ', fixed=TRUE)))[as.logical(evapInput)];
rObsSD = as.numeric(unlist(strsplit(as.character(
	config['Runoff Obs Std. Dev',1]), ' ', fixed=TRUE)))[as.logical(runoffInput)];
oObsSD = as.numeric(unlist(strsplit(as.character(
	config['Outflow Obs Std. Dev',1]), ' ', fixed=TRUE)))[as.logical(outflowInput)];
dObsSD = as.numeric(unlist(strsplit(as.character(
	config['Diversion Obs Std. Dev',1]), ' ', fixed=TRUE)))[as.logical(diversionInput)];
nObsSD = as.numeric(unlist(strsplit(as.character(
	config['NBS Obs Std. Dev',1]), ' ', fixed=TRUE)))[as.logical(nbsInput)];

incProcError = as.logical(config['Balance Process Error',1])
superiorProcErrorPriorMean = as.numeric(as.vector(config['Superior Process Error Prior Mean',1]))
superiorProcErrorPriorSD = as.numeric(as.vector(config['Superior Process Error Prior Std. Dev',1]))
miHuronProcErrorPriorMean = as.numeric(as.vector(config['MiHuron Process Error Prior Mean',1]))
miHuronProcErrorPriorSD = as.numeric(as.vector(config['MiHuron Process Error Prior Std. Dev',1]))
clairProcErrorPriorMean = as.numeric(as.vector(config['St. Clair Process Error Prior Mean',1]))
clairProcErrorPriorSD = as.numeric(as.vector(config['St. Clair Process Error Prior Std. Dev',1]))
erieProcErrorPriorMean = as.numeric(as.vector(config['Erie Process Error Prior Mean',1]))
erieProcErrorPriorSD = as.numeric(as.vector(config['Erie Process Error Prior Std. Dev',1]))
ontarioProcErrorPriorMean = as.numeric(as.vector(config['Ontario Process Error Prior Mean',1]))
ontarioProcErrorPriorSD = as.numeric(as.vector(config['Ontario Process Error Prior Std. Dev',1]))

rollPeriod = as.integer(as.vector(config['Rolling Window',1]))

dHPrecDefined = as.logical(config['Define dH Uncertainty',1])
dHPrec = as.numeric(as.vector(config['dH Uncertainty in mm',1]))
if(dHPrec < 1){
	dHPrec = 1;
}

superiorComponentWBM = as.logical(config['Superior Component WBM',1])
miHuronComponentWBM = as.logical(config['MiHuron Component WBM',1])
clairComponentWBM = as.logical(config['St. Clair Component WBM',1])
erieComponentWBM = as.logical(config['Erie Component WBM',1])
ontarioComponentWBM = as.logical(config['Ontario Component WBM',1])

clairCMS = as.logical(config['St. Clair CMS if not Component WBM',1])
if(clairComponentWBM){
	clairCMS = FALSE;
}
stcMMtoCMSFactor = as.numeric(as.vector(config['St. Clair mm to cms Factor',1]))

incNBSNode = as.logical(config['Include NBS Node if not Component WBM',1])

iters = as.integer(as.vector(config['MCMC Iterations',1]))
halfIters = iters/2;

checkModel = as.logical(config['Model Checks',1])

### GET USEFUL FUNCTIONS

source('getFunctions.r')

grid2TS = function(dat){ 
	# Assumes year is first column
	# Months are columns 2-13
	retMat = NULL;
	for(y in 1:nrow(dat)){
		for(m in 2:13){
			retMat = rbind(
				retMat,
				c(dat[y,1],m-1,dat[y,m])
			);
		}
	}
	return(retMat);
}

# Assume data are in YYYY, MM, 'data' format
getSubset = function(dat, y0, m0, y1, m1){
	retDat = NULL;
	for(y in y0:y1){
		for(mn in 1:12){
			if(y == y1 & mn > m1){
				next;
			}
			if(y == y0 & mn < m0){
				next;
			}
			rowIndex = which(dat[,1] == y & dat[,2] == mn)
			if(length(rowIndex) == 0){
				retDat = rbind(retDat, c(y, mn, rep(NA, ncol(dat)-2)))
			}
			else{
				retDat = rbind(retDat, dat[rowIndex,]);
			}
		}
	}
	return(retDat)
}

supArea = 8.1925e10;
mhgArea = 1.1685e11;
stcArea = 1.1090e9;
eriArea = 2.5404e10;
ontArea = 1.9121e10;

daysInMonths = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
daysInMonthsWithLeap = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
secondsInADay = 24*60*60;
