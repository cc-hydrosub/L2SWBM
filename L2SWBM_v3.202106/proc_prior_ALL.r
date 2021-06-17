
### FOR RESIDUAL EVAPORATION AS REQUESTED (FOR PRIORS)

superiorEvap_resid = cbind(superiorEvap_Prior[,1:2], superiorPrecip_Prior[,3] + superiorRunoff_Prior[,3] - superiorNBS_Prior[,residNBSColumn])
miHuronEvap_resid = cbind(miHuronEvap_Prior[,1:2], miHuronPrecip_Prior[,3] + miHuronRunoff_Prior[,3] - miHuronNBS_Prior[,residNBSColumn])
clairEvap_resid = cbind(clairEvap_Prior[,1:2], clairPrecip_Prior[,3] + clairRunoff_Prior[,3] - clairNBS_Prior[,residNBSColumn])
erieEvap_resid = cbind(erieEvap_Prior[,1:2], eriePrecip_Prior[,3] + erieRunoff_Prior[,3] - erieNBS_Prior[,residNBSColumn])
ontarioEvap_resid = cbind(ontarioEvap_Prior[,1:2], ontarioPrecip_Prior[,3] + ontarioRunoff_Prior[,3] - ontarioNBS_Prior[,residNBSColumn])

superiorEvap_ResidPrior = getSubset(superiorEvap_resid, priorEvapStart[1], priorEvapStart[2], priorEvapEnd[1], priorEvapEnd[2])
miHuronEvap_ResidPrior = getSubset(miHuronEvap_resid, priorEvapStart[1], priorEvapStart[2], priorEvapEnd[1], priorEvapEnd[2])
clairEvap_ResidPrior = getSubset(clairEvap_resid, priorEvapStart[1], priorEvapStart[2], priorEvapEnd[1], priorEvapEnd[2])
erieEvap_ResidPrior = getSubset(erieEvap_resid, priorEvapStart[1], priorEvapStart[2], priorEvapEnd[1], priorEvapEnd[2])
ontarioEvap_ResidPrior = getSubset(ontarioEvap_resid, priorEvapStart[1], priorEvapStart[2], priorEvapEnd[1], priorEvapEnd[2])

### ANALYSIS DATA

superiorDS_A = getSubset(superiorDS, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

superiorOutflow_A = getSubset(superiorOutflow, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
superiorDiversion_A = getSubset(superiorDiversion, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

superiorPrecip_A = getSubset(superiorPrecip, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
superiorEvap_A = getSubset(superiorEvap, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
superiorRunoff_A = getSubset(superiorRunoff, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

superiorNBS_A = getSubset(superiorNBS, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

miHuronDS_A = getSubset(miHuronDS, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

miHuronOutflow_A = getSubset(miHuronOutflow, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
miHuronDiversion_A = getSubset(miHuronDiversion, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

miHuronPrecip_A = getSubset(miHuronPrecip, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
miHuronEvap_A = getSubset(miHuronEvap, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
miHuronRunoff_A = getSubset(miHuronRunoff, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

miHuronNBS_A = getSubset(miHuronNBS, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

clairDS_A = getSubset(clairDS, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

clairPrecip_A = getSubset(clairPrecip, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
clairEvap_A = getSubset(clairEvap, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
clairRunoff_A = getSubset(clairRunoff, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

clairNBS_A = getSubset(clairNBS, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

clairOutflow_A = getSubset(clairOutflow, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

erieDS_A = getSubset(erieDS, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

erieOutflow_A = getSubset(erieOutflow, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
erieDiversion_A = getSubset(erieDiversion, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

### COME BACK HERE IF NEED BE
erieOutflow_A[,3] = erieOutflow_A[,3] - erieDiversion_A[,3]

eriePrecip_A = getSubset(eriePrecip, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
erieEvap_A = getSubset(erieEvap, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
erieRunoff_A = getSubset(erieRunoff, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

erieNBS_A = getSubset(erieNBS, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

ontarioDS_A = getSubset(ontarioDS, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

ontarioOutflow_A = getSubset(ontarioOutflow, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

ontarioPrecip_A = getSubset(ontarioPrecip, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
ontarioEvap_A = getSubset(ontarioEvap, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)
ontarioRunoff_A = getSubset(ontarioRunoff, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

ontarioNBS_A = getSubset(ontarioNBS, startAnalysisYear, startAnalysisMonth, endAnalysisYear, endAnalysisMonth)

### PRIOR DATA

superiorOutflow_Prior = getSubset(superiorOutflow_Prior, priorOutflowStart[1], priorOutflowStart[2], priorOutflowEnd[1], priorOutflowEnd[2])
superiorDiversion_Prior = getSubset(superiorDiversion, priorDiversionStart[1], priorDiversionStart[2], priorDiversionEnd[1], priorDiversionEnd[2])

superiorPrecip_Prior = getSubset(superiorPrecip_Prior, priorPrecipStart[1], priorPrecipStart[2], priorPrecipEnd[1], priorPrecipEnd[2])
superiorEvap_Prior = getSubset(superiorEvap_Prior, priorEvapStart[1], priorEvapStart[2], priorEvapEnd[1], priorEvapEnd[2])
superiorRunoff_Prior = getSubset(superiorRunoff_Prior, priorRunoffStart[1], priorRunoffStart[2], priorRunoffEnd[1], priorRunoffEnd[2])
superiorNBS_Prior = getSubset(superiorNBS_Prior, priorNBSStart[1], priorNBSStart[2], priorNBSEnd[1], priorNBSEnd[2])

miHuronOutflow_Prior = getSubset(miHuronOutflow_Prior, priorOutflowStart[1], priorOutflowStart[2], priorOutflowEnd[1], priorOutflowEnd[2])
miHuronDiversion_Prior = getSubset(miHuronDiversion, priorDiversionStart[1], priorDiversionStart[2], priorDiversionEnd[1], priorDiversionEnd[2])

miHuronPrecip_Prior = getSubset(miHuronPrecip_Prior, priorPrecipStart[1], priorPrecipStart[2], priorPrecipEnd[1], priorPrecipEnd[2])
miHuronEvap_Prior = getSubset(miHuronEvap_Prior, priorEvapStart[1], priorEvapStart[2], priorEvapEnd[1], priorEvapEnd[2])
miHuronRunoff_Prior = getSubset(miHuronRunoff_Prior, priorRunoffStart[1], priorRunoffStart[2], priorRunoffEnd[1], priorRunoffEnd[2])
miHuronNBS_Prior = getSubset(miHuronNBS_Prior, priorNBSStart[1], priorNBSStart[2], priorNBSEnd[1], priorNBSEnd[2])

clairPrecip_Prior = getSubset(clairPrecip_Prior, priorPrecipStart[1], priorPrecipStart[2], priorPrecipEnd[1], priorPrecipEnd[2])
clairEvap_Prior = getSubset(clairEvap_Prior, priorEvapStart[1], priorEvapStart[2], priorEvapEnd[1], priorEvapEnd[2])
clairRunoff_Prior = getSubset(clairRunoff_Prior, priorRunoffStart[1], priorRunoffStart[2], priorRunoffEnd[1], priorRunoffEnd[2])

clairNBS_Prior = getSubset(clairNBS, priorNBSStart[1], priorNBSStart[2], priorNBSEnd[1], priorNBSEnd[2])
clairOutflow_Prior = getSubset(clairOutflow_Prior, priorOutflowStart[1], priorOutflowStart[2], priorOutflowEnd[1], priorOutflowEnd[2])

erieOutflow_Prior = getSubset(erieOutflow_Prior, priorOutflowStart[1], priorOutflowStart[2], priorOutflowEnd[1], priorOutflowEnd[2])
erieDiversion_Prior = getSubset(erieDiversion, priorDiversionStart[1], priorDiversionStart[2], priorDiversionEnd[1], priorDiversionEnd[2])

### COME BACK HERE IF NEED BE
erieOutflow_Prior[,3] = erieOutflow_Prior[,3] - erieDiversion_Prior[,3]

eriePrecip_Prior = getSubset(eriePrecip_Prior, priorPrecipStart[1], priorPrecipStart[2], priorPrecipEnd[1], priorPrecipEnd[2])
erieEvap_Prior = getSubset(erieEvap_Prior, priorEvapStart[1], priorEvapStart[2], priorEvapEnd[1], priorEvapEnd[2])
erieRunoff_Prior = getSubset(erieRunoff_Prior, priorRunoffStart[1], priorRunoffStart[2], priorRunoffEnd[1], priorRunoffEnd[2])
erieNBS_Prior = getSubset(erieNBS_Prior, priorNBSStart[1], priorNBSStart[2], priorNBSEnd[1], priorNBSEnd[2])

ontarioOutflow_Prior = getSubset(ontarioOutflow_Prior, priorOutflowStart[1], priorOutflowStart[2], priorOutflowEnd[1], priorOutflowEnd[2])

ontarioPrecip_Prior = getSubset(ontarioPrecip_Prior, priorPrecipStart[1], priorPrecipStart[2], priorPrecipEnd[1], priorPrecipEnd[2])
ontarioEvap_Prior = getSubset(ontarioEvap_Prior, priorEvapStart[1], priorEvapStart[2], priorEvapEnd[1], priorEvapEnd[2])
ontarioRunoff_Prior = getSubset(ontarioRunoff_Prior, priorRunoffStart[1], priorRunoffStart[2], priorRunoffEnd[1], priorRunoffEnd[2])
ontarioNBS_Prior = getSubset(ontarioNBS_Prior, priorNBSStart[1], priorNBSStart[2], priorNBSEnd[1], priorNBSEnd[2])

### INSERT RESIDUAL EVAP INTO PRIOR COLUMN OF PRIOR MATRIX WHERE EVAP OBS ARE NA

superiorEvap_Prior[is.na(superiorEvap_Prior[,priorEvapColumn]),priorEvapColumn] = superiorEvap_ResidPrior[is.na(superiorEvap_Prior[,priorEvapColumn]),3]
miHuronEvap_Prior[is.na(miHuronEvap_Prior[,priorEvapColumn]),priorEvapColumn] = miHuronEvap_ResidPrior[is.na(miHuronEvap_Prior[,priorEvapColumn]),3]
erieEvap_Prior[is.na(erieEvap_Prior[,priorEvapColumn]),priorEvapColumn] = erieEvap_ResidPrior[is.na(erieEvap_Prior[,priorEvapColumn]),3]
ontarioEvap_Prior[is.na(ontarioEvap_Prior[,priorEvapColumn]),priorEvapColumn] = ontarioEvap_ResidPrior[is.na(ontarioEvap_Prior[,priorEvapColumn]),3]

clairNBS_Prior[is.na(clairNBS_Prior[,priorNBSColumn]),priorNBSColumn] = clairNBS_Prior[is.na(clairNBS_Prior[,priorNBSColumn]),residNBSColumn]


### PRIOR DISTRIBUTIONS

superiorPriorPrecipShape = miHuronPriorPrecipShape = NULL					
superiorPriorPrecipRate = miHuronPriorPrecipRate = NULL					
superiorEvapPriorMean = miHuronEvapPriorMean = NULL
superiorEvapPriorPrecision = miHuronEvapPriorPrecision = NULL
superiorRunoffLogPriorMean = miHuronRunoffLogPriorMean = NULL
superiorRunoffLogPriorPrecision = miHuronRunoffLogPriorPrecision = NULL
superiorNBSPriorMean = miHuronNBSPriorMean = NULL
superiorNBSPriorPrecision = miHuronNBSPriorPrecision = NULL
superiorOutflowPriorMean = miHuronOutflowPriorMean = NULL
superiorOutflowPriorPrecision = miHuronOutflowPriorPrecision = NULL
superiorDiversionPriorMean = miHuronDiversionPriorMean = NULL
superiorDiversionPriorPrecision = miHuronDiversionPriorPrecision = NULL

clairPriorPrecipShape = NULL					
clairPriorPrecipRate = NULL					
clairEvapPriorMean = NULL
clairEvapPriorPrecision = NULL
clairRunoffLogPriorMean = NULL
clairRunoffLogPriorPrecision = NULL
clairNBSPriorMean = NULL;
clairNBSPriorPrecision = NULL;
clairOutflowPriorMean = NULL;
clairOutflowPriorPrecision = NULL;

eriePriorPrecipShape = NULL;
eriePriorPrecipRate = NULL;
erieEvapPriorMean = NULL;
erieEvapPriorPrecision = NULL;
erieRunoffLogPriorMean = NULL;
erieRunoffLogPriorPrecision = NULL;
erieNBSPriorMean = NULL;
erieNBSPriorPrecision = NULL;
erieOutflowPriorMean = NULL;
erieOutflowPriorPrecision = NULL;
erieDiversionPriorMean = NULL
erieDiversionPriorPrecision = NULL

ontarioPriorPrecipShape = NULL;
ontarioPriorPrecipRate = NULL;
ontarioEvapPriorMean = NULL;
ontarioEvapPriorPrecision = NULL;
ontarioRunoffLogPriorMean = NULL;
ontarioRunoffLogPriorPrecision = NULL;
ontarioNBSPriorMean = NULL;
ontarioNBSPriorPrecision = NULL;
ontarioOutflowPriorMean = NULL;
ontarioOutflowPriorPrecision = NULL;

for(i in 1:12){

	## Over-lake precipitation; gamma distribution following Husak, et al (2007), IJOC
	x.bar_s = mean(superiorPrecip_Prior[superiorPrecip_Prior[,2]==i,priorPrecipColumn], na.rm=TRUE)
	theta_s = log(x.bar_s)-mean(log(superiorPrecip_Prior[superiorPrecip_Prior[,2]==i,priorPrecipColumn]), na.rm=TRUE)
	superiorPriorPrecipShape[i] = 1/(4*theta_s)*(1+sqrt(1+4*theta_s/3))
	superiorPriorPrecipRate[i] = superiorPriorPrecipShape[i]/x.bar_s

	x.bar_m = mean(miHuronPrecip_Prior[miHuronPrecip_Prior[,2]==i,priorPrecipColumn], na.rm=TRUE)
	theta_m = log(x.bar_m)-mean(log(miHuronPrecip_Prior[miHuronPrecip_Prior[,2]==i,priorPrecipColumn]), na.rm=TRUE)
	miHuronPriorPrecipShape[i] = 1/(4*theta_m)*(1+sqrt(1+4*theta_m/3))
	miHuronPriorPrecipRate[i] = miHuronPriorPrecipShape[i]/x.bar_m

	## Over-lake evaporation
	superiorEvapPriorMean[i] = mean(superiorEvap_Prior[superiorEvap_Prior[,2]==i,priorEvapColumn], na.rm=TRUE)
	superiorEvapPriorPrecision[i] =  evapPriorPrecMod*(1/var(superiorEvap_Prior[superiorEvap_Prior[,2]==i,priorEvapColumn], na.rm=TRUE))					
	miHuronEvapPriorMean[i] = mean(miHuronEvap_Prior[miHuronEvap_Prior[,2]==i,priorEvapColumn], na.rm=TRUE)
	miHuronEvapPriorPrecision[i] =  evapPriorPrecMod*(1/var(miHuronEvap_Prior[miHuronEvap_Prior[,2]==i,priorEvapColumn], na.rm=TRUE))

	## Runoff
	superiorRunoffLogPriorMean[i] = mean(log(superiorRunoff_Prior[superiorRunoff_Prior[,2]==i,priorRunoffColumn]), na.rm=TRUE)
	superiorRunoffLogPriorPrecision[i] =  (1/var(log(superiorRunoff_Prior[superiorRunoff_Prior[,2]==i,priorRunoffColumn]), na.rm=TRUE))				
	miHuronRunoffLogPriorMean[i] = mean(log(miHuronRunoff_Prior[miHuronRunoff_Prior[,2]==i,priorRunoffColumn]), na.rm=TRUE)
	miHuronRunoffLogPriorPrecision[i] =  (1/var(log(miHuronRunoff_Prior[miHuronRunoff_Prior[,2]==i,priorRunoffColumn]), na.rm=TRUE))
	
	## NBS
	superiorNBSPriorMean[i] = mean(superiorNBS_Prior[superiorNBS_Prior[,2]==i,priorNBSColumn], na.rm=TRUE)
	superiorNBSPriorPrecision[i] =  (1/var(superiorNBS_Prior[superiorNBS_Prior[,2]==i,priorNBSColumn], na.rm=TRUE))					
	miHuronNBSPriorMean[i] = mean(miHuronNBS_Prior[miHuronNBS_Prior[,2]==i,priorNBSColumn], na.rm=TRUE)
	miHuronNBSPriorPrecision[i] = (1/var(miHuronNBS_Prior[miHuronNBS_Prior[,2]==i,priorNBSColumn], na.rm=TRUE))

	## Outflow
	superiorOutflowPriorMean[i] =  mean(superiorOutflow_Prior[superiorOutflow_Prior[,2]==i,priorOutflowColumn], na.rm=TRUE)
	superiorOutflowPriorPrecision[i] = outflowPriorPrecMod*(1/var(superiorOutflow_Prior[superiorOutflow_Prior[,2]==i,priorOutflowColumn], na.rm=TRUE))					
	miHuronOutflowPriorMean[i] =  mean(miHuronOutflow_Prior[miHuronOutflow_Prior[,2]==i,priorOutflowColumn], na.rm=TRUE)
	miHuronOutflowPriorPrecision[i] = outflowPriorPrecMod*(1/var(miHuronOutflow_Prior[miHuronOutflow_Prior[,2]==i,priorOutflowColumn], na.rm=TRUE))
	
	## Diversions
	superiorDiversionPriorMean[i] =  mean(superiorDiversion_Prior[superiorDiversion_Prior[,2]==i,priorDiversionColumn], na.rm=TRUE)
	superiorDiversionPriorPrecision[i] = diversionPriorPrecMod*(1/var(superiorDiversion_Prior[superiorDiversion_Prior[,2]==i,priorDiversionColumn], na.rm=TRUE))					
	miHuronDiversionPriorMean[i] =  mean(miHuronDiversion_Prior[miHuronDiversion_Prior[,2]==i,priorDiversionColumn], na.rm=TRUE)
	miHuronDiversionPriorPrecision[i] = diversionPriorPrecMod*(1/var(miHuronDiversion_Prior[miHuronDiversion_Prior[,2]==i,priorDiversionColumn], na.rm=TRUE))	
	
	# CEO
	
	x.bar_e = mean(eriePrecip_Prior[eriePrecip_Prior[,2]==i,priorPrecipColumn], na.rm=TRUE)
	theta_e = log(x.bar_e)-mean(log(eriePrecip_Prior[eriePrecip_Prior[,2]==i,priorPrecipColumn]), na.rm=TRUE)
	eriePriorPrecipShape[i] = 1/(4*theta_e)*(1+sqrt(1+4*theta_e/3))
	eriePriorPrecipRate[i] = eriePriorPrecipShape[i]/x.bar_e

	x.bar_c = mean(clairPrecip_Prior[clairPrecip_Prior[,2]==i,priorPrecipColumn], na.rm=TRUE)
	theta_c = log(x.bar_c)-mean(log(clairPrecip_Prior[clairPrecip_Prior[,2]==i,priorPrecipColumn]), na.rm=TRUE)
	clairPriorPrecipShape[i] = 1/(4*theta_c)*(1+sqrt(1+4*theta_c/3))
	clairPriorPrecipRate[i] = clairPriorPrecipShape[i]/x.bar_c
	
	x.bar_o = mean(ontarioPrecip_Prior[ontarioPrecip_Prior[,2]==i,priorPrecipColumn], na.rm=TRUE)
	theta_o = log(x.bar_o)-mean(log(ontarioPrecip_Prior[ontarioPrecip_Prior[,2]==i,priorPrecipColumn]), na.rm=TRUE)
	ontarioPriorPrecipShape[i] = 1/(4*theta_o)*(1+sqrt(1+4*theta_o/3))
	ontarioPriorPrecipRate[i] = ontarioPriorPrecipShape[i]/x.bar_o
	
	clairEvapPriorMean[i] = mean(clairEvap_Prior[clairEvap_Prior[,2]==i,priorEvapColumn], na.rm=TRUE)
	clairEvapPriorPrecision[i] =  evapPriorPrecMod*(1/var(clairEvap_Prior[clairEvap_Prior[,2]==i,priorEvapColumn], na.rm=TRUE))		
	clairRunoffLogPriorMean[i] = mean(log(clairRunoff_Prior[clairRunoff_Prior[,2]==i,priorRunoffColumn]), na.rm=TRUE)
	clairRunoffLogPriorPrecision[i] =  (1/var(log(clairRunoff_Prior[clairRunoff_Prior[,2]==i,priorRunoffColumn]), na.rm=TRUE))	
	clairNBSPriorMean[i] =  mean(clairNBS_Prior[clairNBS_Prior[,2]==i,priorNBSColumn], na.rm=TRUE)
	clairNBSPriorPrecision[i] = (1/var(clairNBS_Prior[clairNBS_Prior[,2]==i,priorNBSColumn], na.rm=TRUE))	
	clairOutflowPriorMean[i] =  mean(clairOutflow_Prior[clairOutflow_Prior[,2]==i,priorOutflowColumn], na.rm=TRUE)
	clairOutflowPriorPrecision[i] = (1/var(clairOutflow_Prior[clairOutflow_Prior[,2]==i,priorOutflowColumn], na.rm=TRUE))	
	
	erieEvapPriorMean[i] = mean(erieEvap_Prior[erieEvap_Prior[,2]==i,priorEvapColumn], na.rm=TRUE)
	erieEvapPriorPrecision[i] =  evapPriorPrecMod*(1/var(erieEvap_Prior[erieEvap_Prior[,2]==i,priorEvapColumn], na.rm=TRUE))		
	erieRunoffLogPriorMean[i] = mean(log(erieRunoff_Prior[erieRunoff_Prior[,2]==i,priorRunoffColumn]), na.rm=TRUE)
	erieRunoffLogPriorPrecision[i] =  (1/var(log(erieRunoff_Prior[erieRunoff_Prior[,2]==i,priorRunoffColumn]), na.rm=TRUE))	
	erieNBSPriorMean[i] =  mean(erieNBS_Prior[clairNBS_Prior[,2]==i,priorNBSColumn], na.rm=TRUE)
	erieNBSPriorPrecision[i] = (1/var(erieNBS_Prior[clairNBS_Prior[,2]==i,priorNBSColumn], na.rm=TRUE))	
    erieOutflowPriorMean[i] =  mean(erieOutflow_Prior[erieOutflow_Prior[,2]==i,priorOutflowColumn], na.rm=TRUE)
	erieOutflowPriorPrecision[i] = outflowPriorPrecMod*(1/var(erieOutflow_Prior[erieOutflow_Prior[,2]==i,priorOutflowColumn], na.rm=TRUE))	
	erieDiversionPriorMean[i] =  mean(erieDiversion_Prior[erieDiversion_Prior[,2]==i,priorDiversionColumn], na.rm=TRUE)
	erieDiversionPriorPrecision[i] = (1/var(erieDiversion_Prior[erieDiversion_Prior[,2]==i,priorDiversionColumn], na.rm=TRUE))	
	
	ontarioEvapPriorMean[i] = mean(ontarioEvap_Prior[ontarioEvap_Prior[,2]==i,priorEvapColumn], na.rm=TRUE)
	ontarioEvapPriorPrecision[i] =  evapPriorPrecMod*(1/var(ontarioEvap_Prior[ontarioEvap_Prior[,2]==i,priorEvapColumn], na.rm=TRUE))		
	ontarioRunoffLogPriorMean[i] = mean(log(ontarioRunoff_Prior[ontarioRunoff_Prior[,2]==i,priorRunoffColumn]), na.rm=TRUE)
	ontarioRunoffLogPriorPrecision[i] =  (1/var(log(ontarioRunoff_Prior[ontarioRunoff_Prior[,2]==i,priorRunoffColumn]), na.rm=TRUE))	
	ontarioNBSPriorMean[i] =  mean(ontarioNBS_Prior[clairNBS_Prior[,2]==i,priorNBSColumn], na.rm=TRUE)
	ontarioNBSPriorPrecision[i] = (1/var(ontarioNBS_Prior[clairNBS_Prior[,2]==i,priorNBSColumn], na.rm=TRUE))	
    ontarioOutflowPriorMean[i] =  mean(ontarioOutflow_Prior[ontarioOutflow_Prior[,2]==i,priorOutflowColumn], na.rm=TRUE)
	ontarioOutflowPriorPrecision[i] = outflowPriorPrecMod*(1/var(ontarioOutflow_Prior[ontarioOutflow_Prior[,2]==i,priorOutflowColumn], na.rm=TRUE))	
	
}

ySuperiorOutflow1BiasPrec = 1/(rep(marysUncertainty,12)*superiorOutflowPriorMean)^2
ySuperiorOutflow2BiasPrec = 1/(rep(marysUncertainty,12)*superiorOutflowPriorMean)^2
ySuperiorDiversion1BiasPrec = 1/(rep(ollUncertainty,12)*superiorDiversionPriorMean)^2
yMiHuronOutflow1BiasPrec = 1/(rep(clairRiverUncertainty,12)*miHuronOutflowPriorMean)^2
yMiHuronOutflow2BiasPrec = 1/(rep(clairRiverUncertainty,12)*miHuronOutflowPriorMean)^2
yMiHuronDiversion1BiasPrec = 1/(rep(chicagoUncertainty,12)*miHuronDiversionPriorMean)^2
yClairOutflow1BiasPrec = 1/(rep(detroitUncertainty,12)*clairOutflowPriorMean)^2
yClairOutflow2BiasPrec = 1/(rep(detroitUncertainty,12)*clairOutflowPriorMean)^2
yErieOutflow1BiasPrec = 1/(rep(niagaraUncertainty,12)*erieOutflowPriorMean)^2
yErieDiversion1BiasPrec = 1/(rep(wellandUncertainty,12)*erieDiversionPriorMean)^2
yOntarioOutflow1BiasPrec = 1/(rep(lawrenceUncertainty,12)*ontarioOutflowPriorMean)^2

#((rep(marysUncertainty,12)*superiorOutflowPriorMean))
#((rep(ollUncertainty,12)*superiorDiversionPriorMean))
#((rep(clairRiverUncertainty,12)*miHuronOutflowPriorMean))
#((rep(chicagoUncertainty,12)*miHuronDiversionPriorMean))
#((rep(detroitUncertainty,12)*clairOutflowPriorMean))
#((rep(niagaraUncertainty,12)*erieOutflowPriorMean))
#((rep(wellandUncertainty,12)*erieDiversionPriorMean))
#((rep(lawrenceUncertainty,12)*ontarioOutflowPriorMean))