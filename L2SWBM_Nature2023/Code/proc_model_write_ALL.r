dayVector = NULL;
yearVector = NULL;
m = NULL;
for(i in startAnalysisYear:endAnalysisYear){
	for(mo in 1:12){
		if(i == startAnalysisYear & mo < startAnalysisMonth){
			next;
		}
		if(i == endAnalysisYear & mo > endAnalysisMonth){
			next;
		}
		yearVector = c(yearVector, i);
		m = c(m, mo)
		if(i %% 4 == 0){
			dayVector = c(dayVector, daysInMonthsWithLeap[mo])
		}
		else{
			dayVector = c(dayVector, daysInMonths[mo])
		}
	}
}

### MAKING SURE WE HAVE SOME SORT OF COMPLETE DIVERSION TIME SERIES

superiorDiversion_A[is.na(superiorDiversion_A[,3]),3] = superiorDiversionPriorMean[m[is.na(superiorDiversion_A[,3])]]
erieDiversion_A[is.na(erieDiversion_A[,3]),3] = erieDiversionPriorMean[m[is.na(erieDiversion_A[,3])]]

# PRIOR MONTH'S FLOW SUBSTITUTION FOR LAKE ERIE OUTFLOW DUE TO LARGE DIFFERENCE BETWEEN LOCAL TREND 
# AND HISTORICAL AVERAGE

erieOutflow_A[is.na(erieOutflow_A[,3]),3] = erieOutflow_A[which(is.na(erieOutflow_A[,3]))-1,3]

### SETTING UP THE MODEL

ySuperiorRStore = rep(NA, length(m));
yMiHuronRStore = rep(NA, length(m));
yClairRStore = rep(NA, length(m));
yErieRStore = rep(NA, length(m));
yOntarioRStore = rep(NA, length(m));
for(rp in rollPeriod:length(m)){
	ySuperiorRStore[rp] = sum(superiorDS_A[(rp-rollPeriod+1):rp,3]);
	yMiHuronRStore[rp] = sum(miHuronDS_A[(rp-rollPeriod+1):rp,3]);
	yClairRStore[rp] = sum(clairDS_A[(rp-rollPeriod+1):rp,3]);
	yErieRStore[rp] = sum(erieDS_A[(rp-rollPeriod+1):rp,3]);
	yOntarioRStore[rp] = sum(ontarioDS_A[(rp-rollPeriod+1):rp,3]);
}

yPrecip = list();
yEvap = list();
yRunoff = list();
yNBS = list();
yOutflow = list();
yDiversion = list();

yPrecipBiasMean = list();
yEvapBiasMean = list();
yRunoffBiasMean = list();
yNBSBiasMean = list();
yOutflowBiasMean = list();
yDiversionBiasMean = list();

yPrecipBiasPrec = list();
yEvapBiasPrec = list();
yRunoffBiasPrec = list();
yNBSBiasPrec = list();
yOutflowBiasPrec = list();
yDiversionBiasPrec = list();

yPrecipPrec = list();
yEvapPrec = list();
yRunoffPrec = list();
yNBSPrec = list();
yOutflowPrec = list();
yDiversionPrec = list();

########################################################
# WHERE TO PUT USER SPECIFIED BIAS STANDARD DEVIATIONS
########################################################

### PRECIP

if(ncol(superiorPrecip_A) >= 3 & superiorComponentWBM){
	for(i in 3:ncol(superiorPrecip_A)){
		yPrecip[[paste('ySuperiorPrecip',i-2,sep='')]] = superiorPrecip_A[,i]
		yPrecipBiasMean[[paste('ySuperiorPrecip',i-2,'BiasMean',sep='')]] = pObsPriorMeanBias[i-2]
		yPrecipBiasPrec[[paste('ySuperiorPrecip',i-2,'BiasPrec',sep='')]] = 1/(pObsPriorBiasDev[i-2])^2
		yPrecipPrec[[paste('ySuperiorPrecip',i-2,'Prec',sep='')]] = 1/(pObsSD[i-2])^2
	}
}
if(ncol(miHuronPrecip_A) >= 3 & miHuronComponentWBM){
	for(i in 3:ncol(miHuronPrecip_A)){
		yPrecip[[paste('yMiHuronPrecip',i-2,sep='')]] = miHuronPrecip_A[,i]
		yPrecipBiasMean[[paste('yMiHuronPrecip',i-2,'BiasMean',sep='')]] = pObsPriorMeanBias[i-2]
		yPrecipBiasPrec[[paste('yMiHuronPrecip',i-2,'BiasPrec',sep='')]] = 1/(pObsPriorBiasDev[i-2])^2
		yPrecipPrec[[paste('yMiHuronPrecip',i-2,'Prec',sep='')]] = 1/(pObsSD[i-2])^2
	}
}
if(ncol(clairPrecip_A) >= 3 & clairComponentWBM){
	for(i in 3:ncol(clairPrecip_A)){
		yPrecip[[paste('yClairPrecip',i-2,sep='')]] = clairPrecip_A[,i]
		yPrecipBiasMean[[paste('yClairPrecip',i-2,'BiasMean',sep='')]] = pObsPriorMeanBias[i-2]
		yPrecipBiasPrec[[paste('yClairPrecip',i-2,'BiasPrec',sep='')]] = 1/(pObsPriorBiasDev[i-2])^2
		yPrecipPrec[[paste('yClairPrecip',i-2,'Prec',sep='')]] = 1/(pObsSD[i-2])^2
	}
}
if(ncol(eriePrecip_A) >= 3 & erieComponentWBM){
	for(i in 3:ncol(eriePrecip_A)){
		yPrecip[[paste('yEriePrecip',i-2,sep='')]] = eriePrecip_A[,i]
		yPrecipBiasMean[[paste('yEriePrecip',i-2,'BiasMean',sep='')]] = pObsPriorMeanBias[i-2]
		yPrecipBiasPrec[[paste('yEriePrecip',i-2,'BiasPrec',sep='')]] = 1/(pObsPriorBiasDev[i-2])^2
		yPrecipPrec[[paste('yEriePrecip',i-2,'Prec',sep='')]] = 1/(pObsSD[i-2])^2
	}
}
if(ncol(ontarioPrecip_A) >= 3 & ontarioComponentWBM){	
	for(i in 3:ncol(ontarioPrecip_A)){
		yPrecip[[paste('yOntarioPrecip',i-2,sep='')]] = ontarioPrecip_A[,i]
		yPrecipBiasMean[[paste('yOntarioPrecip',i-2,'BiasMean',sep='')]] = pObsPriorMeanBias[i-2]
		yPrecipBiasPrec[[paste('yOntarioPrecip',i-2,'BiasPrec',sep='')]] = 1/(pObsPriorBiasDev[i-2])^2
		yPrecipPrec[[paste('yOntarioPrecip',i-2,'Prec',sep='')]] = 1/(pObsSD[i-2])^2
	}
}

### EVAP

if(ncol(superiorEvap_A) >= 3 & superiorComponentWBM){
	for(i in 3:ncol(superiorEvap_A)){
		yEvap[[paste('ySuperiorEvap',i-2,sep='')]] = superiorEvap_A[,i]
		yEvapBiasMean[[paste('ySuperiorEvap',i-2,'BiasMean',sep='')]] = eObsPriorMeanBias[i-2]
		yEvapBiasPrec[[paste('ySuperiorEvap',i-2,'BiasPrec',sep='')]] = 1/(eObsPriorBiasDev[i-2])^2
		yEvapPrec[[paste('ySuperiorEvap',i-2,'Prec',sep='')]] = 1/(eObsSD[i-2])^2
	}
}
if(ncol(miHuronEvap_A) >= 3 & miHuronComponentWBM){
	for(i in 3:ncol(miHuronEvap_A)){
		yEvap[[paste('yMiHuronEvap',i-2,sep='')]] = miHuronEvap_A[,i]
		yEvapBiasMean[[paste('yMiHuronEvap',i-2,'BiasMean',sep='')]] = eObsPriorMeanBias[i-2]
		yEvapBiasPrec[[paste('yMiHuronEvap',i-2,'BiasPrec',sep='')]] = 1/(eObsPriorBiasDev[i-2])^2
		yEvapPrec[[paste('yMiHuronEvap',i-2,'Prec',sep='')]] = 1/(eObsSD[i-2])^2
	}
}
if(ncol(clairEvap_A) >= 3 & clairComponentWBM){
	for(i in 3:ncol(clairEvap_A)){
		yEvap[[paste('yClairEvap',i-2,sep='')]] = clairEvap_A[,i]
		yEvapBiasMean[[paste('yClairEvap',i-2,'BiasMean',sep='')]] = eObsPriorMeanBias[i-2]
		yEvapBiasPrec[[paste('yClairEvap',i-2,'BiasPrec',sep='')]] = 1/(eObsPriorBiasDev[i-2])^2
		yEvapPrec[[paste('yClairEvap',i-2,'Prec',sep='')]] = 1/(eObsSD[i-2])^2
	}
}
if(ncol(erieEvap_A) >= 3 & erieComponentWBM){
	for(i in 3:ncol(erieEvap_A)){
		yEvap[[paste('yErieEvap',i-2,sep='')]] = erieEvap_A[,i]
		yEvapBiasMean[[paste('yErieEvap',i-2,'BiasMean',sep='')]] = eObsPriorMeanBias[i-2]
		yEvapBiasPrec[[paste('yErieEvap',i-2,'BiasPrec',sep='')]] = 1/(eObsPriorBiasDev[i-2])^2
		yEvapPrec[[paste('yErieEvap',i-2,'Prec',sep='')]] = 1/(eObsSD[i-2])^2
	}
}
if(ncol(ontarioEvap_A) >= 3 & ontarioComponentWBM){	
	for(i in 3:ncol(ontarioEvap_A)){
		yEvap[[paste('yOntarioEvap',i-2,sep='')]] = ontarioEvap_A[,i]
		yEvapBiasMean[[paste('yOntarioEvap',i-2,'BiasMean',sep='')]] = eObsPriorMeanBias[i-2]
		yEvapBiasPrec[[paste('yOntarioEvap',i-2,'BiasPrec',sep='')]] = 1/(eObsPriorBiasDev[i-2])^2
		yEvapPrec[[paste('yOntarioEvap',i-2,'Prec',sep='')]] = 1/(eObsSD[i-2])^2
	}
}

### RUNOFF

if(ncol(superiorRunoff_A) >= 3 & superiorComponentWBM){
	for(i in 3:ncol(superiorRunoff_A)){
		yRunoff[[paste('ySuperiorRunoff',i-2,sep='')]] = superiorRunoff_A[,i]
		yRunoffBiasMean[[paste('ySuperiorRunoff',i-2,'BiasMean',sep='')]] = rObsPriorMeanBias[i-2]
		yRunoffBiasPrec[[paste('ySuperiorRunoff',i-2,'BiasPrec',sep='')]] = 1/(rObsPriorBiasDev[i-2])^2
		yRunoffPrec[[paste('ySuperiorRunoff',i-2,'Prec',sep='')]] = 1/(rObsSD[i-2])^2
	}
}
if(ncol(miHuronRunoff_A) >= 3 & miHuronComponentWBM){
	for(i in 3:ncol(miHuronRunoff_A)){
		yRunoff[[paste('yMiHuronRunoff',i-2,sep='')]] = miHuronRunoff_A[,i]
		yRunoffBiasMean[[paste('yMiHuronRunoff',i-2,'BiasMean',sep='')]] = rObsPriorMeanBias[i-2]
		yRunoffBiasPrec[[paste('yMiHuronRunoff',i-2,'BiasPrec',sep='')]] = 1/(rObsPriorBiasDev[i-2])^2
		yRunoffPrec[[paste('yMiHuronRunoff',i-2,'Prec',sep='')]] = 1/(rObsSD[i-2])^2
	}
}
if(ncol(clairRunoff_A) >= 3 & clairComponentWBM){
	for(i in 3:ncol(clairRunoff_A)){
		yRunoff[[paste('yClairRunoff',i-2,sep='')]] = clairRunoff_A[,i]
		yRunoffBiasMean[[paste('yClairRunoff',i-2,'BiasMean',sep='')]] = rObsPriorMeanBias[i-2]
		yRunoffBiasPrec[[paste('yClairRunoff',i-2,'BiasPrec',sep='')]] = 1/(rObsPriorBiasDev[i-2])^2
		yRunoffPrec[[paste('yClairRunoff',i-2,'Prec',sep='')]] = 1/(rObsSD[i-2])^2
	}
}
if(ncol(erieRunoff_A) >= 3 & erieComponentWBM){
	for(i in 3:ncol(erieRunoff_A)){
		yRunoff[[paste('yErieRunoff',i-2,sep='')]] = erieRunoff_A[,i]
		yRunoffBiasMean[[paste('yErieRunoff',i-2,'BiasMean',sep='')]] = rObsPriorMeanBias[i-2]
		yRunoffBiasPrec[[paste('yErieRunoff',i-2,'BiasPrec',sep='')]] = 1/(rObsPriorBiasDev[i-2])^2
		yRunoffPrec[[paste('yErieRunoff',i-2,'Prec',sep='')]] = 1/(rObsSD[i-2])^2
	}
}
if(ncol(ontarioRunoff_A) >= 3 & ontarioComponentWBM){	
	for(i in 3:ncol(ontarioRunoff_A)){
		yRunoff[[paste('yOntarioRunoff',i-2,sep='')]] = ontarioRunoff_A[,i]
		yRunoffBiasMean[[paste('yOntarioRunoff',i-2,'BiasMean',sep='')]] = rObsPriorMeanBias[i-2]
		yRunoffBiasPrec[[paste('yOntarioRunoff',i-2,'BiasPrec',sep='')]] = 1/(rObsPriorBiasDev[i-2])^2
		yRunoffPrec[[paste('yOntarioRunoff',i-2,'Prec',sep='')]] = 1/(rObsSD[i-2])^2
	}
}

### NBS

if(ncol(superiorNBS_A) >= 3 & !superiorComponentWBM){
	for(i in 3:ncol(superiorNBS_A)){
		yNBS[[paste('ySuperiorNBS',i-2,sep='')]] = superiorNBS_A[,i]
		yNBSBiasMean[[paste('ySuperiorNBS',i-2,'BiasMean',sep='')]] = nObsPriorMeanBias[i-2]
		yNBSBiasPrec[[paste('ySuperiorNBS',i-2,'BiasPrec',sep='')]] = 1/(nObsPriorBiasDev[i-2])^2
		yNBSPrec[[paste('ySuperiorNBS',i-2,'Prec',sep='')]] = 1/(nObsSD[i-2])^2
	}
}
if(ncol(miHuronNBS_A) >= 3 & !miHuronComponentWBM){
	for(i in 3:ncol(miHuronNBS_A)){
		yNBS[[paste('yMiHuronNBS',i-2,sep='')]] = miHuronNBS_A[,i]
		yNBSBiasMean[[paste('yMiHuronNBS',i-2,'BiasMean',sep='')]] = nObsPriorMeanBias[i-2]
		yNBSBiasPrec[[paste('yMiHuronNBS',i-2,'BiasPrec',sep='')]] = 1/(nObsPriorBiasDev[i-2])^2
		yNBSPrec[[paste('yMiHuronNBS',i-2,'Prec',sep='')]] = 1/(nObsSD[i-2])^2
	}
}
if(ncol(clairNBS_A) >= 3 & !clairComponentWBM){
	if(clairCMS){
		for(i in 3:ncol(clairNBS_A)){
			yNBS[[paste('yClairNBS',i-2,sep='')]] = clairNBS_A[,i]
			yNBSBiasMean[[paste('yClairNBS',i-2,'BiasMean',sep='')]] = nObsPriorMeanBias[i-2]*stcMMtoCMSFactor
			yNBSBiasPrec[[paste('yClairNBS',i-2,'BiasPrec',sep='')]] = 1/(nObsPriorBiasDev[i-2]*stcMMtoCMSFactor)^2
			yNBSPrec[[paste('yClairNBS',i-2,'Prec',sep='')]] = 1/(nObsSD[i-2]*stcMMtoCMSFactor)^2
		}
	}
	else{
		for(i in 3:ncol(clairNBS_A)){
			yNBS[[paste('yClairNBS',i-2,sep='')]] = clairNBS_A[,i]
			yNBSBiasMean[[paste('yClairNBS',i-2,'BiasMean',sep='')]] = nObsPriorMeanBias[i-2]
			yNBSBiasPrec[[paste('yClairNBS',i-2,'BiasPrec',sep='')]] = 1/(nObsPriorBiasDev[i-2])^2
			yNBSPrec[[paste('yClairNBS',i-2,'Prec',sep='')]] = 1/(nObsSD[i-2])^2
		}
	}
}
if(ncol(erieNBS_A) >= 3 & !erieComponentWBM){
	for(i in 3:ncol(erieNBS_A)){
		yNBS[[paste('yErieNBS',i-2,sep='')]] = erieNBS_A[,i]
		yNBSBiasMean[[paste('yErieNBS',i-2,'BiasMean',sep='')]] = nObsPriorMeanBias[i-2]
		yNBSBiasPrec[[paste('yErieNBS',i-2,'BiasPrec',sep='')]] = 1/(nObsPriorBiasDev[i-2])^2
		yNBSPrec[[paste('yErieNBS',i-2,'Prec',sep='')]] = 1/(nObsSD[i-2])^2
	}
}
if(ncol(ontarioNBS_A) >= 3 & !ontarioComponentWBM){	
	for(i in 3:ncol(ontarioNBS_A)){
		yNBS[[paste('yOntarioNBS',i-2,sep='')]] = ontarioNBS_A[,i]
		yNBSBiasMean[[paste('yOntarioNBS',i-2,'BiasMean',sep='')]] = nObsPriorMeanBias[i-2]
		yNBSBiasPrec[[paste('yOntarioNBS',i-2,'BiasPrec',sep='')]] = 1/(nObsPriorBiasDev[i-2])^2
		yNBSPrec[[paste('yOntarioNBS',i-2,'Prec',sep='')]] = 1/(nObsSD[i-2])^2
	}
}

### OUTFLOW

if(ncol(superiorOutflow_A) >= 3){
	for(i in 3:ncol(superiorOutflow_A)){
		yOutflow[[paste('ySuperiorOutflow',i-2,sep='')]] = superiorOutflow_A[,i]
		yOutflowBiasMean[[paste('ySuperiorOutflow',i-2,'BiasMean',sep='')]] = oObsPriorMeanBias[i-2]
		yOutflowBiasPrec[[paste('ySuperiorOutflow',i-2,'BiasPrec',sep='')]] = 1/(oObsPriorBiasDev[i-2])^2
		yOutflowPrec[[paste('ySuperiorOutflow',i-2,'Prec',sep='')]] = 1/(oObsSD[i-2])^2
	}
}
if(ncol(miHuronOutflow_A) >= 3){
	for(i in 3:ncol(miHuronOutflow_A)){
		yOutflow[[paste('yMiHuronOutflow',i-2,sep='')]] = miHuronOutflow_A[,i]
		yOutflowBiasMean[[paste('yMiHuronOutflow',i-2,'BiasMean',sep='')]] = oObsPriorMeanBias[i-2]
		yOutflowBiasPrec[[paste('yMiHuronOutflow',i-2,'BiasPrec',sep='')]] = 1/(oObsPriorBiasDev[i-2])^2
		yOutflowPrec[[paste('yMiHuronOutflow',i-2,'Prec',sep='')]] = 1/(oObsSD[i-2])^2
	}
}
if(ncol(clairOutflow_A) >= 3){
	for(i in 3:ncol(clairOutflow_A)){
		yOutflow[[paste('yClairOutflow',i-2,sep='')]] = clairOutflow_A[,i]
		yOutflowBiasMean[[paste('yClairOutflow',i-2,'BiasMean',sep='')]] = oObsPriorMeanBias[i-2]
		yOutflowBiasPrec[[paste('yClairOutflow',i-2,'BiasPrec',sep='')]] = 1/(oObsPriorBiasDev[i-2])^2
		yOutflowPrec[[paste('yClairOutflow',i-2,'Prec',sep='')]] = 1/(oObsSD[i-2])^2
	}
}
if(ncol(erieOutflow_A) >= 3){
	for(i in 3:ncol(erieOutflow_A)){
		yOutflow[[paste('yErieOutflow',i-2,sep='')]] = erieOutflow_A[,i]
		yOutflowBiasMean[[paste('yErieOutflow',i-2,'BiasMean',sep='')]] = oObsPriorMeanBias[i-2]
		yOutflowBiasPrec[[paste('yErieOutflow',i-2,'BiasPrec',sep='')]] = 1/(oObsPriorBiasDev[i-2])^2
		yOutflowPrec[[paste('yErieOutflow',i-2,'Prec',sep='')]] = 1/(oObsSD[i-2])^2
	}
}
if(ncol(ontarioOutflow_A) >= 3){	
	for(i in 3:ncol(ontarioOutflow_A)){
		yOutflow[[paste('yOntarioOutflow',i-2,sep='')]] = ontarioOutflow_A[,i]
		yOutflowBiasMean[[paste('yOntarioOutflow',i-2,'BiasMean',sep='')]] = oObsPriorMeanBias[i-2]
		yOutflowBiasPrec[[paste('yOntarioOutflow',i-2,'BiasPrec',sep='')]] = 1/(oObsPriorBiasDev[i-2])^2
		yOutflowPrec[[paste('yOntarioOutflow',i-2,'Prec',sep='')]] = 1/(oObsSD[i-2])^2
	}
}

### DIVERSION

if(ncol(superiorDiversion_A) >= 3){
	for(i in 3:ncol(superiorDiversion_A)){
		yDiversion[[paste('ySuperiorDiversion',i-2,sep='')]] = superiorDiversion_A[,i]
		yDiversionBiasMean[[paste('ySuperiorDiversion',i-2,'BiasMean',sep='')]] = dObsPriorMeanBias[i-2]
		yDiversionBiasPrec[[paste('ySuperiorDiversion',i-2,'BiasPrec',sep='')]] = 1/(dObsPriorBiasDev[i-2])^2
		yDiversionPrec[[paste('ySuperiorDiversion',i-2,'Prec',sep='')]] = 1/(dObsSD[i-2])^2
	}
}
if(ncol(miHuronDiversion_A) >= 3){
	for(i in 3:ncol(miHuronDiversion_A)){
		yDiversion[[paste('yMiHuronDiversion',i-2,sep='')]] = miHuronDiversion_A[,i]
		yDiversionBiasMean[[paste('yMiHuronDiversion',i-2,'BiasMean',sep='')]] = dObsPriorMeanBias[i-2]
		yDiversionBiasPrec[[paste('yMiHuronDiversion',i-2,'BiasPrec',sep='')]] = 1/(dObsPriorBiasDev[i-2])^2
		yDiversionPrec[[paste('yMiHuronDiversion',i-2,'Prec',sep='')]] = 1/(dObsSD[i-2])^2
	}
}
if(ncol(erieDiversion_A) >= 3){
	for(i in 3:ncol(erieDiversion_A)){
		yDiversion[[paste('yErieDiversion',i-2,sep='')]] = erieDiversion_A[,i]
		yDiversionBiasMean[[paste('yErieDiversion',i-2,'BiasMean',sep='')]] = dObsPriorMeanBias[i-2]
		yDiversionBiasPrec[[paste('yErieDiversion',i-2,'BiasPrec',sep='')]] = 1/(dObsPriorBiasDev[i-2])^2
		yDiversionPrec[[paste('yErieDiversion',i-2,'Prec',sep='')]] = 1/(dObsSD[i-2])^2		
	}
}


posteriorStartMonth = 1;
posteriorEndMonth = length(m);
nMonths = posteriorEndMonth;

inputDataCoreJAGS = list(
	"posteriorStartMonth"   = posteriorStartMonth, 
	"posteriorEndMonth"     = posteriorEndMonth,
	"rollPeriod"            = rollPeriod,
	"m"                     = m,
	"dayVector"							= dayVector,
	"secondsInADay"					= secondsInADay,
	"supArea"								= supArea,
	"mhgArea"								= mhgArea,
	"stcArea"               = stcArea,
	"ySuperiorRStore"       = ySuperiorRStore, 
	"yMiHuronRStore"	      = yMiHuronRStore,
	"eriArea"								= eriArea,
	"ontArea"								= ontArea,
	"yClairRStore"	        = yClairRStore,
	"yErieRStore"	          = yErieRStore,
	"yOntarioRStore"        = yOntarioRStore
);

superiorComponentNBS = list(
	"superiorPriorPrecipShape"              = superiorPriorPrecipShape,            
	"superiorPriorPrecipRate"               = superiorPriorPrecipRate,             
	"superiorEvapPriorMean"                 = superiorEvapPriorMean,               
	"superiorEvapPriorPrecision"            = superiorEvapPriorPrecision,          
	"superiorRunoffLogPriorMean"            = superiorRunoffLogPriorMean,          
	"superiorRunoffLogPriorPrecision"       = superiorRunoffLogPriorPrecision,     
	"superiorOutflowPriorMean"              = superiorOutflowPriorMean,            
	"superiorOutflowPriorPrecision"         = superiorOutflowPriorPrecision,       
	"superiorDiversionPriorMean"            = superiorDiversionPriorMean,          
	"superiorDiversionPriorPrecision"       = superiorDiversionPriorPrecision  
);

superiorBulkNBS = list(            
	"superiorNBSPriorMean"                 = superiorNBSPriorMean,               
	"superiorNBSPriorPrecision"            = superiorNBSPriorPrecision,          
	"superiorOutflowPriorMean"              = superiorOutflowPriorMean,            
	"superiorOutflowPriorPrecision"         = superiorOutflowPriorPrecision,       
	"superiorDiversionPriorMean"            = superiorDiversionPriorMean,          
	"superiorDiversionPriorPrecision"       = superiorDiversionPriorPrecision
);

miHuronComponentNBS = list(
	"miHuronPriorPrecipShape"              = miHuronPriorPrecipShape,            
	"miHuronPriorPrecipRate"               = miHuronPriorPrecipRate,             
	"miHuronEvapPriorMean"                 = miHuronEvapPriorMean,               
	"miHuronEvapPriorPrecision"            = miHuronEvapPriorPrecision,          
	"miHuronRunoffLogPriorMean"            = miHuronRunoffLogPriorMean,          
	"miHuronRunoffLogPriorPrecision"       = miHuronRunoffLogPriorPrecision,     
	"miHuronOutflowPriorMean"              = miHuronOutflowPriorMean,            
	"miHuronOutflowPriorPrecision"         = miHuronOutflowPriorPrecision,       
	"miHuronDiversionPriorMean"            = miHuronDiversionPriorMean,          
	"miHuronDiversionPriorPrecision"       = miHuronDiversionPriorPrecision 
);

miHuronBulkNBS = list(            
	"miHuronNBSPriorMean"                 = miHuronNBSPriorMean,               
	"miHuronNBSPriorPrecision"            = miHuronNBSPriorPrecision,          
	"miHuronOutflowPriorMean"              = miHuronOutflowPriorMean,            
	"miHuronOutflowPriorPrecision"         = miHuronOutflowPriorPrecision,       
	"miHuronDiversionPriorMean"            = miHuronDiversionPriorMean,          
	"miHuronDiversionPriorPrecision"       = miHuronDiversionPriorPrecision  
);

clairComponentNBS = list(
	"clairPriorPrecipShape"              = clairPriorPrecipShape,            
	"clairPriorPrecipRate"               = clairPriorPrecipRate,             
	"clairEvapPriorMean"                 = clairEvapPriorMean,               
	"clairEvapPriorPrecision"            = clairEvapPriorPrecision,          
	"clairRunoffLogPriorMean"            = clairRunoffLogPriorMean,          
	"clairRunoffLogPriorPrecision"       = clairRunoffLogPriorPrecision,     
	"clairOutflowPriorMean"              = clairOutflowPriorMean,            
	"clairOutflowPriorPrecision"         = clairOutflowPriorPrecision 
);

clairBulkNBS = list(            
	"clairNBSPriorMean"                 = clairNBSPriorMean,               
	"clairNBSPriorPrecision"            = clairNBSPriorPrecision,          
	"clairOutflowPriorMean"              = clairOutflowPriorMean,            
	"clairOutflowPriorPrecision"         = clairOutflowPriorPrecision  
);

erieComponentNBS = list(
	"eriePriorPrecipShape"              = eriePriorPrecipShape,            
	"eriePriorPrecipRate"               = eriePriorPrecipRate,             
	"erieEvapPriorMean"                 = erieEvapPriorMean,               
	"erieEvapPriorPrecision"            = erieEvapPriorPrecision,          
	"erieRunoffLogPriorMean"            = erieRunoffLogPriorMean,          
	"erieRunoffLogPriorPrecision"       = erieRunoffLogPriorPrecision,     
	"erieOutflowPriorMean"              = erieOutflowPriorMean,            
	"erieOutflowPriorPrecision"         = erieOutflowPriorPrecision,       
	"erieDiversionPriorMean"            = erieDiversionPriorMean,          
	"erieDiversionPriorPrecision"       = erieDiversionPriorPrecision  
);

erieBulkNBS = list(            
	"erieNBSPriorMean"                 = erieNBSPriorMean,               
	"erieNBSPriorPrecision"            = erieNBSPriorPrecision,          
	"erieOutflowPriorMean"              = erieOutflowPriorMean,            
	"erieOutflowPriorPrecision"         = erieOutflowPriorPrecision,       
	"erieDiversionPriorMean"            = erieDiversionPriorMean,          
	"erieDiversionPriorPrecision"       = erieDiversionPriorPrecision 
);

ontarioComponentNBS = list(
	"ontarioPriorPrecipShape"              = ontarioPriorPrecipShape,            
	"ontarioPriorPrecipRate"               = ontarioPriorPrecipRate,             
	"ontarioEvapPriorMean"                 = ontarioEvapPriorMean,               
	"ontarioEvapPriorPrecision"            = ontarioEvapPriorPrecision,          
	"ontarioRunoffLogPriorMean"            = ontarioRunoffLogPriorMean,          
	"ontarioRunoffLogPriorPrecision"       = ontarioRunoffLogPriorPrecision,     
	"ontarioOutflowPriorMean"              = ontarioOutflowPriorMean,            
	"ontarioOutflowPriorPrecision"         = ontarioOutflowPriorPrecision
);

ontarioBulkNBS = list(            
	"ontarioNBSPriorMean"                 = ontarioNBSPriorMean,               
	"ontarioNBSPriorPrecision"            = ontarioNBSPriorPrecision,          
	"ontarioOutflowPriorMean"              = ontarioOutflowPriorMean,            
	"ontarioOutflowPriorPrecision"         = ontarioOutflowPriorPrecision  
);

if(superiorComponentWBM){
	inputDataCoreJAGS = c(inputDataCoreJAGS, superiorComponentNBS)
}else{
	inputDataCoreJAGS = c(inputDataCoreJAGS, superiorBulkNBS)
}

if(miHuronComponentWBM){
	inputDataCoreJAGS = c(inputDataCoreJAGS, miHuronComponentNBS)
}else{
	inputDataCoreJAGS = c(inputDataCoreJAGS, miHuronBulkNBS)
}

if(clairComponentWBM){
	inputDataCoreJAGS = c(inputDataCoreJAGS, clairComponentNBS)
}else{
	inputDataCoreJAGS = c(inputDataCoreJAGS, clairBulkNBS)
}

if(erieComponentWBM){
	inputDataCoreJAGS = c(inputDataCoreJAGS, erieComponentNBS)
}else{
	inputDataCoreJAGS = c(inputDataCoreJAGS, erieBulkNBS)
}

if(ontarioComponentWBM){
	inputDataCoreJAGS = c(inputDataCoreJAGS, ontarioComponentNBS)
}else{
	inputDataCoreJAGS = c(inputDataCoreJAGS, ontarioBulkNBS)
}

flowUncertaintyList = list(
	"ySuperiorOutflow1BiasPrec"             = ySuperiorOutflow1BiasPrec,  
	"ySuperiorOutflow2BiasPrec"             = ySuperiorOutflow2BiasPrec,  
	"ySuperiorDiversion1BiasPrec"           = ySuperiorDiversion1BiasPrec,
	"yMiHuronOutflow1BiasPrec"              = yMiHuronOutflow1BiasPrec,   
	"yMiHuronOutflow2BiasPrec"              = yMiHuronOutflow2BiasPrec,   
	"yMiHuronDiversion1BiasPrec"            = yMiHuronDiversion1BiasPrec, 
	"yClairOutflow1BiasPrec"                = yClairOutflow1BiasPrec,     
	"yClairOutflow2BiasPrec"                = yClairOutflow2BiasPrec,     
	"yErieOutflow1BiasPrec"                 = yErieOutflow1BiasPrec,      
	"yErieDiversion1BiasPrec"               = yErieDiversion1BiasPrec,    
	"yOntarioOutflow1BiasPrec"              = yOntarioOutflow1BiasPrec
);

if(flowUncertaintyInPercent){
	inputDataCoreJAGS = c(inputDataCoreJAGS, flowUncertaintyList)
}else{
	inputDataCoreJAGS = c(inputDataCoreJAGS, yOutflowBiasPrec)
	inputDataCoreJAGS = c(inputDataCoreJAGS, yDiversionBiasPrec)
}

inputDataCoreJAGS = c(inputDataCoreJAGS,yPrecip);
inputDataCoreJAGS = c(inputDataCoreJAGS,yEvap);
inputDataCoreJAGS = c(inputDataCoreJAGS,yRunoff);
inputDataCoreJAGS = c(inputDataCoreJAGS,yNBS);
inputDataCoreJAGS = c(inputDataCoreJAGS,yOutflow);
inputDataCoreJAGS = c(inputDataCoreJAGS,yDiversion);

yBias = NULL;

yBias = c(yBias, paste(names(yPrecip),'Bias',sep=''))
yBias = c(yBias, paste(names(yEvap),'Bias',sep=''))
yBias = c(yBias, paste(names(yRunoff),'Bias',sep=''))
yBias = c(yBias, paste(names(yNBS),'Bias',sep=''))
yBias = c(yBias, paste(names(yOutflow),'Bias',sep=''))
yBias = c(yBias, paste(names(yDiversion),'Bias',sep=''))

yPrec = NULL;

yPrec = c(yPrec, paste(names(yPrecip),'Prec',sep=''))
yPrec = c(yPrec, paste(names(yEvap),'Prec',sep=''))
yPrec = c(yPrec, paste(names(yRunoff),'Prec',sep=''))
yPrec = c(yPrec, paste(names(yNBS),'Prec',sep=''))
yPrec = c(yPrec, paste(names(yOutflow),'Prec',sep=''))
yPrec = c(yPrec, paste(names(yDiversion),'Prec',sep=''))

modelCode = 'model {
	for (j in posteriorStartMonth:posteriorEndMonth){   										
		
		############################## 
		## Priors
		##############################
';

if(superiorComponentWBM){

modelCode = paste(modelCode, '
		### SUPERIOR
		superiorPrecip[j] ~ dgamma(superiorPriorPrecipShape[m[j]], superiorPriorPrecipRate[m[j]])
		superiorEvap[j] ~ dnorm(superiorEvapPriorMean[m[j]], superiorEvapPriorPrecision[m[j]])
		superiorRunoff[j] 	<- exp(superiorLogRunoff[j])
		superiorLogRunoff[j] ~ dnorm(superiorRunoffLogPriorMean[m[j]], superiorRunoffLogPriorPrecision[m[j]])	
		superiorOutflow[j] ~ dnorm(superiorOutflowPriorMean[m[j]], superiorOutflowPriorPrecision[m[j]])
		superiorDiversion[j] ~ dnorm(superiorDiversionPriorMean[m[j]], superiorDiversionPriorPrecision[m[j]])

', 
sep=''
);
}else{
modelCode = paste(modelCode, '
		### SUPERIOR
		superiorNBS[j] ~ dnorm(superiorNBSPriorMean[m[j]], superiorNBSPriorPrecision[m[j]])
		superiorOutflow[j] ~ dnorm(superiorOutflowPriorMean[m[j]], superiorOutflowPriorPrecision[m[j]])
		superiorDiversion[j] ~ dnorm(superiorDiversionPriorMean[m[j]], superiorDiversionPriorPrecision[m[j]])

',
sep=''
);
}

if(miHuronComponentWBM){

modelCode = paste(modelCode, '
		### SUPERIOR
		miHuronPrecip[j] ~ dgamma(miHuronPriorPrecipShape[m[j]], miHuronPriorPrecipRate[m[j]])
		miHuronEvap[j] ~ dnorm(miHuronEvapPriorMean[m[j]], miHuronEvapPriorPrecision[m[j]])
		miHuronRunoff[j] 	<- exp(miHuronLogRunoff[j])
		miHuronLogRunoff[j] ~ dnorm(miHuronRunoffLogPriorMean[m[j]], miHuronRunoffLogPriorPrecision[m[j]])	
		miHuronOutflow[j] ~ dnorm(miHuronOutflowPriorMean[m[j]], miHuronOutflowPriorPrecision[m[j]])
		miHuronDiversion[j] ~ dnorm(miHuronDiversionPriorMean[m[j]], miHuronDiversionPriorPrecision[m[j]])

', 
sep=''
);
}else{
modelCode = paste(modelCode, '
		### SUPERIOR
		miHuronNBS[j] ~ dnorm(miHuronNBSPriorMean[m[j]], miHuronNBSPriorPrecision[m[j]])
		miHuronOutflow[j] ~ dnorm(miHuronOutflowPriorMean[m[j]], miHuronOutflowPriorPrecision[m[j]])
		miHuronDiversion[j] ~ dnorm(miHuronDiversionPriorMean[m[j]], miHuronDiversionPriorPrecision[m[j]])
		
',
sep=''
);
}

if(clairComponentWBM){

modelCode = paste(modelCode, '
		### SUPERIOR
		clairPrecip[j] ~ dgamma(clairPriorPrecipShape[m[j]], clairPriorPrecipRate[m[j]])
		clairEvap[j] ~ dnorm(clairEvapPriorMean[m[j]], clairEvapPriorPrecision[m[j]])
		clairRunoff[j] 	<- exp(clairLogRunoff[j])
		clairLogRunoff[j] ~ dnorm(clairRunoffLogPriorMean[m[j]], clairRunoffLogPriorPrecision[m[j]])	
		clairOutflow[j] ~ dnorm(clairOutflowPriorMean[m[j]], clairOutflowPriorPrecision[m[j]])

', 
sep=''
);
}else{
modelCode = paste(modelCode, '
		### SUPERIOR
		clairNBS[j] ~ dnorm(clairNBSPriorMean[m[j]], clairNBSPriorPrecision[m[j]])
		clairOutflow[j] ~ dnorm(clairOutflowPriorMean[m[j]], clairOutflowPriorPrecision[m[j]])
		
',
sep=''
);
}

if(erieComponentWBM){

modelCode = paste(modelCode, '
		### SUPERIOR
		eriePrecip[j] ~ dgamma(eriePriorPrecipShape[m[j]], eriePriorPrecipRate[m[j]])
		erieEvap[j] ~ dnorm(erieEvapPriorMean[m[j]], erieEvapPriorPrecision[m[j]])
		erieRunoff[j] 	<- exp(erieLogRunoff[j])
		erieLogRunoff[j] ~ dnorm(erieRunoffLogPriorMean[m[j]], erieRunoffLogPriorPrecision[m[j]])	
		erieOutflow[j] ~ dnorm(erieOutflowPriorMean[m[j]], erieOutflowPriorPrecision[m[j]])
		erieDiversion[j] ~ dnorm(erieDiversionPriorMean[m[j]], erieDiversionPriorPrecision[m[j]])

', 
sep=''
);
}else{
modelCode = paste(modelCode, '
		### SUPERIOR
		erieNBS[j] ~ dnorm(erieNBSPriorMean[m[j]], erieNBSPriorPrecision[m[j]])
		erieOutflow[j] ~ dnorm(erieOutflowPriorMean[m[j]], erieOutflowPriorPrecision[m[j]])
		erieDiversion[j] ~ dnorm(erieDiversionPriorMean[m[j]], erieDiversionPriorPrecision[m[j]])
		
',
sep=''
);
}

if(ontarioComponentWBM){

modelCode = paste(modelCode, '
		### SUPERIOR
		ontarioPrecip[j] ~ dgamma(ontarioPriorPrecipShape[m[j]], ontarioPriorPrecipRate[m[j]])
		ontarioEvap[j] ~ dnorm(ontarioEvapPriorMean[m[j]], ontarioEvapPriorPrecision[m[j]])
		ontarioRunoff[j] 	<- exp(ontarioLogRunoff[j])
		ontarioLogRunoff[j] ~ dnorm(ontarioRunoffLogPriorMean[m[j]], ontarioRunoffLogPriorPrecision[m[j]])	
		ontarioOutflow[j] ~ dnorm(ontarioOutflowPriorMean[m[j]], ontarioOutflowPriorPrecision[m[j]])

', 
sep=''
);
}else{
modelCode = paste(modelCode, '
		### SUPERIOR
		ontarioNBS[j] ~ dnorm(ontarioNBSPriorMean[m[j]], ontarioNBSPriorPrecision[m[j]])
		ontarioOutflow[j] ~ dnorm(ontarioOutflowPriorMean[m[j]], ontarioOutflowPriorPrecision[m[j]])
		
',
sep=''
);
}


modelCode = paste(modelCode, '

		####################################################
		## LIKELIHOOD FUNCTIONS
		####################################################

',
sep=''
);

### Y OUTFLOW

if(length(names(yOutflow)) > 0){
	for(n in 1:length(names(yOutflow))){
		lake = NULL;
		if(grepl('Superior',names(yOutflow)[n], fixed=TRUE)){
			lake = 'superior';
			if(!("S" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('MiHuron',names(yOutflow)[n], fixed=TRUE)){
			lake = 'miHuron';
			if(!("M" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('Clair',names(yOutflow)[n], fixed=TRUE)){
			lake = 'clair';
			if(!("C" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('Erie',names(yOutflow)[n], fixed=TRUE)){
			lake = 'erie';
			if(!("E" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('Ontario',names(yOutflow)[n], fixed=TRUE)){
			lake = 'ontario';
			if(!("O" %in% lakesOutflow)){
				next;
			}
		}

		if(biasOutflows & flowUncertaintyInPercent){
			modelCode = paste(
				modelCode,
				'\t\t',names(yOutflow)[n],'[j] ~ dnorm(',names(yOutflow)[n],'Mean[j], ',names(yOutflow)[n],'Prec)\n',
				'\t\t',names(yOutflow)[n],'Mean[j] <- ', lake, 'Outflow[j] + ', names(yOutflow)[n],'Bias[m[j]]\n\n',
				sep=''
			);
		}
		else if(!biasOutflows & flowUncertaintyInPercent){
			modelCode = paste(
				modelCode,
				'\t\t',names(yOutflow)[n],'[j] ~ dnorm(',names(yOutflow)[n],'Mean[j], ',names(yOutflow)[n],'Prec[m[j]])\n',
				'\t\t',names(yOutflow)[n],'Mean[j] <- ', lake, 'Outflow[j] + ', names(yOutflow)[n],'Bias[m[j]]\n\n',
				sep=''
			);
		}
		else if(biasOutflows & !flowUncertaintyInPercent){
			modelCode = paste(
				modelCode,
				'\t\t',names(yOutflow)[n],'[j] ~ dnorm(',names(yOutflow)[n],'Mean[j], ',names(yOutflow)[n],'Prec)\n',
				'\t\t',names(yOutflow)[n],'Mean[j] <- ', lake, 'Outflow[j] + ', names(yOutflow)[n],'Bias[m[j]]\n\n',
				sep=''
			);
		}
		else{
			modelCode = paste(
				modelCode,
				'\t\t',names(yOutflow)[n],'[j] ~ dnorm(',names(yOutflow)[n],'Mean[j], ',names(yOutflow)[n],'Prec)\n',
				'\t\t',names(yOutflow)[n],'Mean[j] <- ', lake, 'Outflow[j] + ', names(yOutflow)[n],'Bias[m[j]]\n\n',
				sep=''
			);
		}
		
		
	}
}

### Y DIVERSION

if(length(names(yDiversion)) > 0){
	for(n in 1:length(names(yDiversion))){
		lake = NULL;
		if(grepl('Superior',names(yDiversion)[n], fixed=TRUE)){
			lake = 'superior';
			if(!("S" %in% lakesDiversion)){
				next;
			}
		}
		else if(grepl('MiHuron',names(yDiversion)[n], fixed=TRUE)){
			lake = 'miHuron';
			if(!("M" %in% lakesDiversion)){
				next;
			}
		}
		else if(grepl('Erie',names(yDiversion)[n], fixed=TRUE)){
			lake = 'erie';
			if(!("E" %in% lakesDiversion)){
				next;
			}
		}

		if(biasOutflows & flowUncertaintyInPercent){
			modelCode = paste(
				modelCode,
				'\t\t',names(yDiversion)[n],'[j] ~ dnorm(',names(yDiversion)[n],'Mean[j], ',names(yDiversion)[n],'Prec)\n',
				'\t\t',names(yDiversion)[n],'Mean[j] <- ', lake, 'Diversion[j] + ', names(yDiversion)[n],'Bias[m[j]]\n\n',
				sep=''
			);
		}
		else if(!biasOutflows & flowUncertaintyInPercent){
			modelCode = paste(
				modelCode,
				'\t\t',names(yDiversion)[n],'[j] ~ dnorm(',names(yDiversion)[n],'Mean[j], ',names(yDiversion)[n],'Prec[m[j]])\n',
				'\t\t',names(yDiversion)[n],'Mean[j] <- ', lake, 'Diversion[j] + ', names(yDiversion)[n],'Bias[m[j]]\n\n',
				sep=''
			);
		}
		else if(biasOutflows & !flowUncertaintyInPercent){
			modelCode = paste(
				modelCode,
				'\t\t',names(yDiversion)[n],'[j] ~ dnorm(',names(yDiversion)[n],'Mean[j], ',names(yDiversion)[n],'Prec)\n',
				'\t\t',names(yDiversion)[n],'Mean[j] <- ', lake, 'Diversion[j] + ', names(yDiversion)[n],'Bias[m[j]]\n\n',
				sep=''
			);
		}
		else{
			modelCode = paste(
				modelCode,
				'\t\t',names(yDiversion)[n],'[j] ~ dnorm(',names(yDiversion)[n],'Mean[j], ',names(yDiversion)[n],'Prec)\n',
				'\t\t',names(yDiversion)[n],'Mean[j] <- ', lake, 'Diversion[j] + ', names(yDiversion)[n],'Bias[m[j]]\n\n',
				sep=''
			);
		}
		
	}
}

### Y PRECIP

if(length(names(yPrecip)) > 0){
	for(n in 1:length(names(yPrecip))){
		lake = NULL;
		if(grepl('Superior',names(yPrecip)[n], fixed=TRUE)){
			lake = 'superior';
			if(!("S" %in% lakesPrecip) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yPrecip)[n], fixed=TRUE)){
			lake = 'miHuron';
			if(!("M" %in% lakesPrecip) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yPrecip)[n], fixed=TRUE)){
			lake = 'clair';
			if(!("C" %in% lakesPrecip) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yPrecip)[n], fixed=TRUE)){
			lake = 'erie';
			if(!("E" %in% lakesPrecip) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yPrecip)[n], fixed=TRUE)){
			lake = 'ontario';
			if(!("O" %in% lakesPrecip) | !ontarioComponentWBM){
				next;
			}
		}
		
		modelCode = paste(
			modelCode,
			'\t\t',names(yPrecip)[n],'[j] ~ dnorm(',names(yPrecip)[n],'Mean[j], ',names(yPrecip)[n],'Prec)\n',
			'\t\t',names(yPrecip)[n],'Mean[j] <- ', lake, 'Precip[j] + ', names(yPrecip)[n],'Bias[m[j]]\n\n',
			sep=''
		);
	}
}

### Y EVAP

if(length(names(yEvap)) > 0){
	for(n in 1:length(names(yEvap))){
		lake = NULL;
		if(grepl('Superior',names(yEvap)[n], fixed=TRUE)){
			lake = 'superior';
			if(!("S" %in% lakesEvap) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yEvap)[n], fixed=TRUE)){
			lake = 'miHuron';
			if(!("M" %in% lakesEvap) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yEvap)[n], fixed=TRUE)){
			lake = 'clair';
			if(!("C" %in% lakesEvap) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yEvap)[n], fixed=TRUE)){
			lake = 'erie';
			if(!("E" %in% lakesEvap) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yEvap)[n], fixed=TRUE)){
			lake = 'ontario';
			if(!("O" %in% lakesEvap) | !ontarioComponentWBM){
				next;
			}
		}
		
		modelCode = paste(
			modelCode,
			'\t\t',names(yEvap)[n],'[j] ~ dnorm(',names(yEvap)[n],'Mean[j], ',names(yEvap)[n],'Prec)\n',
			'\t\t',names(yEvap)[n],'Mean[j] <- ', lake, 'Evap[j] + ', names(yEvap)[n],'Bias[m[j]]\n\n',
			sep=''
		);
	}
}

### Y RUNOFF

if(length(names(yRunoff)) > 0){
	for(n in 1:length(names(yRunoff))){
		lake = NULL;
		if(grepl('Superior',names(yRunoff)[n], fixed=TRUE)){
			lake = 'superior';
			if(!("S" %in% lakesRunoff) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yRunoff)[n], fixed=TRUE)){
			lake = 'miHuron';
			if(!("M" %in% lakesRunoff) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yRunoff)[n], fixed=TRUE)){
			lake = 'clair';
			if(!("C" %in% lakesRunoff) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yRunoff)[n], fixed=TRUE)){
			lake = 'erie';
			if(!("E" %in% lakesRunoff) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yRunoff)[n], fixed=TRUE)){
			lake = 'ontario';
			if(!("O" %in% lakesRunoff) | !ontarioComponentWBM){
				next;
			}
		}
		
		modelCode = paste(
			modelCode,
			'\t\t',names(yRunoff)[n],'[j] ~ dnorm(',names(yRunoff)[n],'Mean[j], ',names(yRunoff)[n],'Prec)\n',
			'\t\t',names(yRunoff)[n],'Mean[j] <- ', lake, 'Runoff[j] + ', names(yRunoff)[n],'Bias[m[j]]\n\n',
			sep=''
		);
	}
}

### Y NBS

if(length(names(yNBS)) > 0){
	for(n in 1:length(names(yNBS))){
		lake = NULL;
		if(grepl('Superior',names(yNBS)[n], fixed=TRUE)){
			lake = 'superior';
			if(!("S" %in% lakesNBS) | superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yNBS)[n], fixed=TRUE)){
			lake = 'miHuron';
			if(!("M" %in% lakesNBS) | miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yNBS)[n], fixed=TRUE)){
			lake = 'clair';
			if(!("C" %in% lakesNBS) | clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yNBS)[n], fixed=TRUE)){
			lake = 'erie';
			if(!("E" %in% lakesNBS) | erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yNBS)[n], fixed=TRUE)){
			lake = 'ontario';
			if(!("O" %in% lakesNBS) | ontarioComponentWBM){
				next;
			}
		}
		
		modelCode = paste(
			modelCode,
			'\t\t',names(yNBS)[n],'[j] ~ dnorm(',names(yNBS)[n],'Mean[j], ',names(yNBS)[n],'Prec)\n',
			'\t\t',names(yNBS)[n],'Mean[j] <- ', lake, 'NBS[j] + ', names(yNBS)[n],'Bias[m[j]]\n\n',
			sep=''
		);
	}
}

modelCode = paste(modelCode, '\t}',sep='')

modelCode = paste(modelCode,
'
	### Rolling Period Restraints
	for(k in rollPeriod:posteriorEndMonth){
',
sep=''
);

if(superiorComponentWBM){
modelCode = paste(modelCode, 
'
		### SUPERIOR
		# OBSERVATION MODEL - STORAGE CHANGE
		ySuperiorRStore[k] ~ dnorm(superiorRStore[k], ySuperiorRStorePrec)    	
		
		superiorRStore[k] <- (
			sum(superiorPrecip[(k-rollPeriod+1):k])
			-sum(superiorEvap[(k-rollPeriod+1):k])
			+sum(superiorRunoff[(k-rollPeriod+1):k])
			-superiorOutflow_mm[k]
			+superiorDiversion_mm[k]
			+sum(superiorProcError[m[(k-rollPeriod+1):k]])
		)
		
		superiorOutflow_mm[k] <- sum(superiorOutflow[(k-rollPeriod+1):k]/supArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		superiorDiversion_mm[k] <- sum(superiorDiversion[(k-rollPeriod+1):k]/supArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		
',
sep=''	
);
}else{
modelCode = paste(modelCode, 
'
		### SUPERIOR
		# OBSERVATION MODEL - STORAGE CHANGE
		ySuperiorRStore[k] ~ dnorm(superiorRStore[k], ySuperiorRStorePrec)    	
		
		superiorRStore[k] <- (
			sum(superiorNBS[(k-rollPeriod+1):k])
			-superiorOutflow_mm[k]
			+superiorDiversion_mm[k]
			+sum(superiorProcError[m[(k-rollPeriod+1):k]])
		)
		
		superiorOutflow_mm[k] <- sum(superiorOutflow[(k-rollPeriod+1):k]/supArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		superiorDiversion_mm[k] <- sum(superiorDiversion[(k-rollPeriod+1):k]/supArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		
',
sep=''	
);
}

if(miHuronComponentWBM){
modelCode = paste(modelCode, 
'
		### MICHIGAN-HURON
		yMiHuronRStore[k] ~ dnorm(miHuronRStore[k], yMiHuronRStorePrec) 
		
		miHuronRStore[k] <- (
			sum(miHuronPrecip[(k-rollPeriod+1):k])
			-sum(miHuronEvap[(k-rollPeriod+1):k])
			+sum(miHuronRunoff[(k-rollPeriod+1):k])
			-miHuronOutflow_mm[k]
			+miHuronInflow_mm[k]
			-miHuronDiversion_mm[k]
			+sum(miHuronProcError[m[(k-rollPeriod+1):k]])
		)
		
		miHuronOutflow_mm[k] <- sum(miHuronOutflow[(k-rollPeriod+1):k]/mhgArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		miHuronInflow_mm[k] <- sum(superiorOutflow[(k-rollPeriod+1):k]/mhgArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		miHuronDiversion_mm[k] <- sum(miHuronDiversion[(k-rollPeriod+1):k]/mhgArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		
',
sep=''	
);
}else{
modelCode = paste(modelCode, 
'
		### MICHIGAN-HURON
		yMiHuronRStore[k] ~ dnorm(miHuronRStore[k], yMiHuronRStorePrec) 
		
		miHuronRStore[k] <- (
			sum(miHuronNBS[(k-rollPeriod+1):k])
			-miHuronOutflow_mm[k]
			+miHuronInflow_mm[k]
			-miHuronDiversion_mm[k]
			+sum(miHuronProcError[m[(k-rollPeriod+1):k]])
		)
		
		miHuronOutflow_mm[k] <- sum(miHuronOutflow[(k-rollPeriod+1):k]/mhgArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		miHuronInflow_mm[k] <- sum(superiorOutflow[(k-rollPeriod+1):k]/mhgArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		miHuronDiversion_mm[k] <- sum(miHuronDiversion[(k-rollPeriod+1):k]/mhgArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		
',
sep=''	
);
}

if(clairComponentWBM){
modelCode = paste(modelCode, 
'
		### CLAIR
		yClairRStore[k] ~ dnorm(clairRStore[k], yClairRStorePrec) 
		
		clairRStore[k] <- (
			sum(clairPrecip[(k-rollPeriod+1):k])
			-sum(clairEvap[(k-rollPeriod+1):k])
			+sum(clairRunoff[(k-rollPeriod+1):k])
			-clairOutflow_mm[k]
			+clairInflow_mm[k]
			+sum(clairProcError[m[(k-rollPeriod+1):k]])
		)
		
		clairOutflow_mm[k] <- sum(clairOutflow[(k-rollPeriod+1):k]/stcArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		clairInflow_mm[k] <- sum(miHuronOutflow[(k-rollPeriod+1):k]/stcArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])		
		
',
sep=''	
);
}else{
if(clairCMS){
modelCode = paste(modelCode, 
'
		### CLAIR
		yClairRStore[k] ~ dnorm(clairRStore[k], yClairRStorePrec) 
		
		clairRStore[k] <- (
			sum(clairNBS[(k-rollPeriod+1):k])
			+ sum(miHuronOutflow[(k-rollPeriod+1):k])
			- sum(clairOutflow[(k-rollPeriod+1):k])
			+sum(clairProcError[m[(k-rollPeriod+1):k]])
		)		
',
sep=''	
);
}else{
modelCode = paste(modelCode, 
'
		### CLAIR
		yClairRStore[k] ~ dnorm(clairRStore[k], yClairRStorePrec) 
		
		clairRStore[k] <- (
			sum(clairNBS[(k-rollPeriod+1):k])
			-clairOutflow_mm[k]
			+clairInflow_mm[k]
			+sum(clairProcError[m[(k-rollPeriod+1):k]])
		)
		
		clairOutflow_mm[k] <- sum(clairOutflow[(k-rollPeriod+1):k]/stcArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		clairInflow_mm[k] <- sum(miHuronOutflow[(k-rollPeriod+1):k]/stcArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		
',
sep=''	
);
}


}

if(erieComponentWBM){
modelCode = paste(modelCode, 
'
		### ERIE
		yErieRStore[k] ~ dnorm(erieRStore[k], yErieRStorePrec) 
		
		erieRStore[k] <- (			
			sum(eriePrecip[(k-rollPeriod+1):k])
			-sum(erieEvap[(k-rollPeriod+1):k])
			+sum(erieRunoff[(k-rollPeriod+1):k])
			-erieOutflow_mm[k]
			# DETROIT RIVER
			+erieInflow_mm[k]
			# WELLAND CANAL
			-erieDiversion_mm[k]
			+sum(erieProcError[m[(k-rollPeriod+1):k]])
		)
		
		erieOutflow_mm[k] <- sum(erieOutflow[(k-rollPeriod+1):k]/eriArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		erieInflow_mm[k] <- sum(clairOutflow[(k-rollPeriod+1):k]/eriArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		erieDiversion_mm[k] <- sum(erieDiversion[(k-rollPeriod+1):k]/eriArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		
',
sep=''	
);
}else{
modelCode = paste(modelCode, 
'
		### ERIE
		yErieRStore[k] ~ dnorm(erieRStore[k], yErieRStorePrec) 
		
		erieRStore[k] <- (			
			sum(erieNBS[(k-rollPeriod+1):k])
			-erieOutflow_mm[k]
			# DETROIT RIVER
			+erieInflow_mm[k]
			# WELLAND CANAL
			-erieDiversion_mm[k]
			+sum(erieProcError[m[(k-rollPeriod+1):k]])
		)
		
		erieOutflow_mm[k] <- sum(erieOutflow[(k-rollPeriod+1):k]/eriArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		erieInflow_mm[k] <- sum(clairOutflow[(k-rollPeriod+1):k]/eriArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		erieDiversion_mm[k] <- sum(erieDiversion[(k-rollPeriod+1):k]/eriArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		
',
sep=''	
);
}

if(ontarioComponentWBM){
modelCode = paste(modelCode, 
'
		### ONTARIO
		yOntarioRStore[k] ~ dnorm(ontarioRStore[k], yOntarioRStorePrec) 
		
		ontarioRStore[k] <- (
			sum(ontarioPrecip[(k-rollPeriod+1):k])
			-sum(ontarioEvap[(k-rollPeriod+1):k])
			+sum(ontarioRunoff[(k-rollPeriod+1):k])
			-ontarioOutflow_mm[k]
			+ontarioInflow_mm[k]
			+sum(ontarioProcError[m[(k-rollPeriod+1):k]])
		)
		
		ontarioOutflow_mm[k] <- sum(ontarioOutflow[(k-rollPeriod+1):k]/ontArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		ontarioInflow_mm[k] <- sum(erieOutflow[(k-rollPeriod+1):k]/ontArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k]) + sum(erieDiversion[(k-rollPeriod+1):k]/ontArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		
',
sep=''	
);
}else{
modelCode = paste(modelCode, 
'
		### ONTARIO
		yOntarioRStore[k] ~ dnorm(ontarioRStore[k], yOntarioRStorePrec) 
		
		ontarioRStore[k] <- (
			sum(ontarioNBS[(k-rollPeriod+1):k])
			-ontarioOutflow_mm[k]
			+ontarioInflow_mm[k]
			+sum(ontarioProcError[m[(k-rollPeriod+1):k]])
		)
		
		ontarioOutflow_mm[k] <- sum(ontarioOutflow[(k-rollPeriod+1):k]/ontArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		ontarioInflow_mm[k] <- sum(erieOutflow[(k-rollPeriod+1):k]/ontArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k]) + sum(erieDiversion[(k-rollPeriod+1):k]/ontArea*1000*secondsInADay*dayVector[(k-rollPeriod+1):k])
		
',
sep=''	
);
}

modelCode = paste(modelCode,
'	
	}
	
	#############################
	## BIAS TERMS 
	#############################
	for (i in 1:12){
	',
sep=''
)

if(incProcError){
if(clairCMS){
modelCode = paste(modelCode,
'
		superiorProcError[i] ~ dnorm(',superiorProcErrorPriorMean,',',1/superiorProcErrorPriorSD^2,')
		miHuronProcError[i] ~ dnorm(',miHuronProcErrorPriorMean,',',1/miHuronProcErrorPriorSD^2,')
		clairProcError[i] ~ dnorm(',clairProcErrorPriorMean,',',1/(clairProcErrorPriorSD*stcMMtoCMSFactor)^2,')
		erieProcError[i] ~ dnorm(',erieProcErrorPriorMean,',',1/erieProcErrorPriorSD^2,')
		ontarioProcError[i] ~ dnorm(',ontarioProcErrorPriorMean,',',1/ontarioProcErrorPriorSD^2,')
',
sep=''
)
}else{
modelCode = paste(modelCode,
'
		superiorProcError[i] ~ dnorm(',superiorProcErrorPriorMean,',',1/superiorProcErrorPriorSD^2,')
		miHuronProcError[i] ~ dnorm(',miHuronProcErrorPriorMean,',',1/miHuronProcErrorPriorSD^2,')
		clairProcError[i] ~ dnorm(',clairProcErrorPriorMean,',',1/(clairProcErrorPriorSD)^2,')
		erieProcError[i] ~ dnorm(',erieProcErrorPriorMean,',',1/erieProcErrorPriorSD^2,')
		ontarioProcError[i] ~ dnorm(',ontarioProcErrorPriorMean,',',1/ontarioProcErrorPriorSD^2,')
',
sep=''
)
}

}else{
modelCode = paste(modelCode,
'
		superiorProcError[i] <- 0;
		miHuronProcError[i] <- 0;
		clairProcError[i] <- 0;
		erieProcError[i] <- 0;
		ontarioProcError[i] <- 0;
',
sep=''
)
}

if(biasOutflows & flowUncertaintyInPercent){
modelCode = paste(modelCode,
'
		ySuperiorOutflow1Bias[i] ~ dnorm(',yOutflowBiasMean[['ySuperiorOutflow1BiasMean']],',ySuperiorOutflow1BiasPrec[i])
		ySuperiorOutflow2Bias[i] ~ dnorm(',yOutflowBiasMean[['ySuperiorOutflow2BiasMean']],',ySuperiorOutflow2BiasPrec[i])
		ySuperiorDiversion1Bias[i] ~ dnorm(',yOutflowBiasMean[['ySuperiorDiversion1BiasMean']],',ySuperiorDiversion1BiasPrec[i])
		yMiHuronOutflow1Bias[i] ~ dnorm(',yOutflowBiasMean[['yMiHuronOutflow1BiasMean']],',yMiHuronOutflow1BiasPrec[i])
		yMiHuronOutflow2Bias[i] ~ dnorm(',yOutflowBiasMean[['yMiHuronOutflow2BiasMean']],',yMiHuronOutflow2BiasPrec[i])
		yMiHuronDiversion1Bias[i] ~ dnorm(',yOutflowBiasMean[['yMiHuronDiversion1BiasMean']],',yMiHuronDiversion1BiasPrec[i])
		yClairOutflow1Bias[i] ~ dnorm(',yOutflowBiasMean[['yClairOutflow1BiasMean']],',yClairOutflow1BiasPrec[i])
		yClairOutflow2Bias[i] ~ dnorm(',yOutflowBiasMean[['yClairOutflow2BiasMean']],',yClairOutflow2BiasPrec[i])
		yErieOutflow1Bias[i] ~ dnorm(',yOutflowBiasMean[['yErieOutflow1BiasMean']],',yErieOutflow1BiasPrec[i])
		yErieDiversion1Bias[i] ~ dnorm(',yOutflowBiasMean[['yErieDiversion1BiasMean']],',yErieDiversion1BiasPrec[i])
		yOntarioOutflow1Bias[i] ~ dnorm(',yOutflowBiasMean[['yOntarioOutflow1BiasMean']],',yOntarioOutflow1BiasPrec[i])	
',
sep=''
)
}else if(biasOutflows & !flowUncertaintyInPercent){
modelCode = paste(modelCode,
'
		ySuperiorOutflow1Bias[i] ~ dnorm(',yOutflowBiasMean[['ySuperiorOutflow1BiasMean']],',',yOutflowBiasPrec[['ySuperiorOutflow1BiasPrec']],')
		ySuperiorOutflow2Bias[i] ~ dnorm(',yOutflowBiasMean[['ySuperiorOutflow2BiasMean']],',',yOutflowBiasPrec[['ySuperiorOutflow2BiasPrec']],')
		ySuperiorDiversion1Bias[i] ~ dnorm(',yDiversionBiasMean[['ySuperiorDiversion1BiasMean']],',',yDiversionBiasPrec[['ySuperiorDiversion1BiasPrec']],')
		yMiHuronOutflow1Bias[i] ~ dnorm(',yOutflowBiasMean[['yMiHuronOutflow1BiasMean']],',',yOutflowBiasPrec[['yMiHuronOutflow1BiasPrec']],')
		yMiHuronOutflow2Bias[i] ~ dnorm(',yOutflowBiasMean[['yMiHuronOutflow2BiasMean']],',',yOutflowBiasPrec[['yMiHuronOutflow2BiasPrec']],')
		yMiHuronDiversion1Bias[i] ~ dnorm(',yDiversionBiasMean[['yMiHuronDiversion1BiasMean']],',',yDiversionBiasPrec[['yMiHuronDiversion1BiasPrec']],')
		yClairOutflow1Bias[i] ~ dnorm(',yOutflowBiasMean[['yClairOutflow1BiasMean']],',',yOutflowBiasPrec[['yClairOutflow1BiasPrec']],')
		yClairOutflow2Bias[i] ~ dnorm(',yOutflowBiasMean[['yClairOutflow2BiasMean']],',',yOutflowBiasPrec[['yClairOutflow2BiasPrec']],')
		yErieOutflow1Bias[i] ~ dnorm(',yOutflowBiasMean[['yErieOutflow1BiasMean']],',',yOutflowBiasPrec[['yErieOutflow1BiasPrec']],')
		yErieDiversion1Bias[i] ~ dnorm(',yDiversionBiasMean[['yErieDiversion1BiasMean']],',',yDiversionBiasPrec[['yErieDiversion1BiasPrec']],')
		yOntarioOutflow1Bias[i] ~ dnorm(',yOutflowBiasMean[['yOntarioOutflow1BiasMean']],',',yOutflowBiasPrec[['yOntarioOutflow1BiasPrec']],')	
',
sep=''
)
}else{
modelCode = paste(modelCode,
'
		ySuperiorOutflow1Bias[i] <- 0;
		ySuperiorOutflow2Bias[i] <- 0;
		ySuperiorDiversion1Bias[i] <- 0;
		yMiHuronOutflow1Bias[i] <- 0;
		yMiHuronOutflow2Bias[i] <- 0;
		yMiHuronDiversion1Bias[i] <- 0;
		yClairOutflow1Bias[i] <- 0;
		yClairOutflow2Bias[i] <- 0;
		yErieOutflow1Bias[i] <- 0;
		yErieDiversion1Bias[i] <- 0;
		yOntarioOutflow1Bias[i] <- 0;
',
sep=''
)
}

if(length(names(yPrecip)) > 0){
	for(n in 1:length(names(yPrecip))){
		if(grepl('Superior',names(yPrecip)[n], fixed=TRUE)){
			if(!("S" %in% lakesPrecip) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yPrecip)[n], fixed=TRUE)){
			if(!("M" %in% lakesPrecip) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yPrecip)[n], fixed=TRUE)){
			if(!("C" %in% lakesPrecip) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yPrecip)[n], fixed=TRUE)){
			if(!("E" %in% lakesPrecip) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yPrecip)[n], fixed=TRUE)){
			if(!("O" %in% lakesPrecip) | !ontarioComponentWBM){
				next;
			}
		}
		meanLabel = paste(names(yPrecip)[n],'BiasMean', sep='')
		precLabel = paste(names(yPrecip)[n],'BiasPrec', sep='')
		modelCode = paste(
			modelCode,
			'\t\t',names(yPrecip)[n],'Bias[i] ~ dnorm(',yPrecipBiasMean[[meanLabel]],',',yPrecipBiasPrec[[precLabel]],')\n',
			sep=''
		);
	}
}

if(length(names(yEvap)) > 0){
	for(n in 1:length(names(yEvap))){
		if(grepl('Superior',names(yEvap)[n], fixed=TRUE)){
			if(!("S" %in% lakesEvap) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yEvap)[n], fixed=TRUE)){
			if(!("M" %in% lakesEvap) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yEvap)[n], fixed=TRUE)){
			if(!("C" %in% lakesEvap) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yEvap)[n], fixed=TRUE)){
			if(!("E" %in% lakesEvap) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yEvap)[n], fixed=TRUE)){
			if(!("O" %in% lakesEvap) | !ontarioComponentWBM){
				next;
			}
		}
		meanLabel = paste(names(yEvap)[n],'BiasMean', sep='')
		precLabel = paste(names(yEvap)[n],'BiasPrec', sep='')
		modelCode = paste(
			modelCode,
			'\t\t',names(yEvap)[n],'Bias[i] ~ dnorm(',yEvapBiasMean[[meanLabel]],',',yEvapBiasPrec[[precLabel]],')\n',
			sep=''
		);
	}
}

if(length(names(yRunoff)) > 0){
	for(n in 1:length(names(yRunoff))){
		if(grepl('Superior',names(yRunoff)[n], fixed=TRUE)){
			if(!("S" %in% lakesRunoff) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yRunoff)[n], fixed=TRUE)){
			if(!("M" %in% lakesRunoff) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yRunoff)[n], fixed=TRUE)){
			if(!("C" %in% lakesRunoff) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yRunoff)[n], fixed=TRUE)){
			if(!("E" %in% lakesRunoff) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yRunoff)[n], fixed=TRUE)){
			if(!("O" %in% lakesRunoff) | !ontarioComponentWBM){
				next;
			}
		}
		meanLabel = paste(names(yRunoff)[n],'BiasMean', sep='')
		precLabel = paste(names(yRunoff)[n],'BiasPrec', sep='')
		modelCode = paste(
			modelCode,
			'\t\t',names(yRunoff)[n],'Bias[i] ~ dnorm(',yRunoffBiasMean[[meanLabel]],',',yRunoffBiasPrec[[precLabel]],')\n',
			sep=''
		);
	}
}

if(length(names(yNBS)) > 0){
	for(n in 1:length(names(yNBS))){
		if(grepl('Superior',names(yNBS)[n], fixed=TRUE)){
			if(!("S" %in% lakesNBS) | superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yNBS)[n], fixed=TRUE)){
			if(!("M" %in% lakesNBS) | miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yNBS)[n], fixed=TRUE)){
			if(!("C" %in% lakesNBS) | clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yNBS)[n], fixed=TRUE)){
			if(!("E" %in% lakesNBS) | erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yNBS)[n], fixed=TRUE)){
			if(!("O" %in% lakesNBS) | ontarioComponentWBM){
				next;
			}
		}
		meanLabel = paste(names(yNBS)[n],'BiasMean', sep='')
		precLabel = paste(names(yNBS)[n],'BiasPrec', sep='')
		modelCode = paste(
			modelCode,
			'\t\t',names(yNBS)[n],'Bias[i] ~ dnorm(',yNBSBiasMean[[meanLabel]],',',yNBSBiasPrec[[precLabel]],')\n',
			sep=''
		);
	}
}

modelCode = paste(modelCode,
'
	} # END BIAS AND PROC-ERROR LOOP
',
sep=''
)

modelCode = paste(modelCode, '\t',sep='')

if(dHPrecDefined){

if(clairCMS){
	modelCode = paste(modelCode,
	'
		##############################
		## PRECISION FOR OBSERVATIONS
		##############################  
		
		ySuperiorRStorePrec	 = 1/(',dHPrec,'^2*rollPeriod)	
		yMiHuronRStorePrec = 1/(',dHPrec,'^2*rollPeriod)
		yClairRStorePrec = 1/(',dHPrec*stcMMtoCMSFactor,'^2*rollPeriod)
		yErieRStorePrec = 1/(',dHPrec,'^2*rollPeriod)
		yOntarioRStorePrec = 1/(',dHPrec,'^2*rollPeriod) 

		',
	sep=''
	)
}else{
	modelCode = paste(modelCode,
	'
		##############################
		## PRECISION FOR OBSERVATIONS
		##############################  
		
		ySuperiorRStorePrec	 = 1/(',dHPrec,'^2*rollPeriod)	
		yMiHuronRStorePrec = 1/(',dHPrec,'^2*rollPeriod)
		yClairRStorePrec = 1/(',dHPrec,'^2*rollPeriod)
		yErieRStorePrec = 1/(',dHPrec,'^2*rollPeriod)
		yOntarioRStorePrec = 1/(',dHPrec,'^2*rollPeriod) 

		',
	sep=''
	)
}
	
}else{
	modelCode = paste(modelCode,
	'
		##############################
		## PRECISION FOR OBSERVATIONS
		##############################
		ySuperiorRStorePrec	  ~ dgamma(0.01,0.01)	
		yMiHuronRStorePrec	  ~ dgamma(0.01,0.01)
		yClairRStorePrec   ~ dgamma(0.01,0.01)
		yErieRStorePrec	  ~ dgamma(0.01,0.01)  
		yOntarioRStorePrec ~ dgamma(0.01,0.01)    

		',
	sep=''
	)

}

if(!biasOutflows & flowUncertaintyInPercent){
modelCode = paste(modelCode,
'
	# Outflow and Diversion precision specification
	for (h in 1:12){
		ySuperiorOutflow1Prec[h] <- ySuperiorOutflow1BiasPrec[h]      
		ySuperiorOutflow2Prec[h] <- ySuperiorOutflow2BiasPrec[h]
		ySuperiorDiversion1Prec[h] <- ySuperiorDiversion1BiasPrec[h]
		yMiHuronOutflow1Prec[h] <- yMiHuronOutflow1BiasPrec[h]      
		yMiHuronOutflow2Prec[h] <- yMiHuronOutflow2BiasPrec[h]
		yMiHuronDiversion1Prec[h] <- yMiHuronDiversion1BiasPrec[h]
		yClairOutflow1Prec[h] <- yClairOutflow1BiasPrec[h]
		yClairOutflow2Prec[h] <- yClairOutflow2BiasPrec[h]
		yErieOutflow1Prec[h] <- yErieOutflow1BiasPrec[h]  
		yErieDiversion1Prec[h] <- yErieDiversion1BiasPrec[h]
		yOntarioOutflow1Prec[h] <-	yOntarioOutflow1BiasPrec[h]
		
	} # END FLOW AND DIVERSION SPECIFICATION
',
sep=''
)
}else{

### OUTFLOW

if(length(names(yOutflow)) > 0){
	for(n in 1:length(names(yOutflow))){
		if(grepl('Superior',names(yOutflow)[n], fixed=TRUE)){
			if(!("S" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('MiHuron',names(yOutflow)[n], fixed=TRUE)){
			if(!("M" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('Clair',names(yOutflow)[n], fixed=TRUE)){
			if(!("C" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('Erie',names(yOutflow)[n], fixed=TRUE)){
			if(!("E" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('Ontario',names(yOutflow)[n], fixed=TRUE)){
			if(!("O" %in% lakesOutflow)){
				next;
			}
		}
		precLabel = paste(names(yOutflow)[n],'Prec', sep='');
		if(is.infinite(yOutflowPrec[[precLabel]])){
			modelCode = paste(
				modelCode,
				'\t',names(yOutflow)[n],'Prec ~ dgamma(0.1,0.1)\n',
				sep=''
			);
		}
		else{
			modelCode = paste(
				modelCode,
				'\t',names(yOutflow)[n],'Prec <- ',yOutflowPrec[[precLabel]],'\n',
				sep=''
			);
		}		
	}
}

### DIVERSION

if(length(names(yDiversion)) > 0){
	for(n in 1:length(names(yDiversion))){
		if(grepl('Superior',names(yDiversion)[n], fixed=TRUE)){
			if(!("S" %in% lakesDiversion)){
				next;
			}
		}
		else if(grepl('MiHuron',names(yDiversion)[n], fixed=TRUE)){
			if(!("M" %in% lakesDiversion)){
				next;
			}
		}
		else if(grepl('Erie',names(yDiversion)[n], fixed=TRUE)){
			if(!("E" %in% lakesDiversion)){
				next;
			}
		}
		precLabel = paste(names(yDiversion)[n],'Prec', sep='');
		if(is.infinite(yDiversionPrec[[precLabel]])){
			modelCode = paste(
				modelCode,
				'\t',names(yDiversion)[n],'Prec ~ dgamma(0.1,0.1)\n',
				sep=''
			);
		}
		else{
			modelCode = paste(
				modelCode,
				'\t',names(yDiversion)[n],'Prec <- ',yDiversionPrec[[precLabel]],'\n',
				sep=''
			);
		}		
	}
}

}

### PRECIP

if(length(names(yPrecip)) > 0){
	for(n in 1:length(names(yPrecip))){
		if(grepl('Superior',names(yPrecip)[n], fixed=TRUE)){
			if(!("S" %in% lakesPrecip) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yPrecip)[n], fixed=TRUE)){
			if(!("M" %in% lakesPrecip) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yPrecip)[n], fixed=TRUE)){
			if(!("C" %in% lakesPrecip) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yPrecip)[n], fixed=TRUE)){
			if(!("E" %in% lakesPrecip) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yPrecip)[n], fixed=TRUE)){
			if(!("O" %in% lakesPrecip) | !ontarioComponentWBM){
				next;
			}
		}
		precLabel = paste(names(yPrecip)[n],'Prec', sep='');
		if(is.infinite(yPrecipPrec[[precLabel]])){
			modelCode = paste(
				modelCode,
				'\t',names(yPrecip)[n],'Prec ~ dgamma(0.1,0.1)\n',
				sep=''
			);
		}
		else{
			modelCode = paste(
				modelCode,
				'\t',names(yPrecip)[n],'Prec <- ',yPrecipPrec[[precLabel]],'\n',
				sep=''
			);
		}		
	}
}

### EVAP

if(length(names(yEvap)) > 0){
	for(n in 1:length(names(yEvap))){
		if(grepl('Superior',names(yEvap)[n], fixed=TRUE)){
			if(!("S" %in% lakesEvap) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yEvap)[n], fixed=TRUE)){
			if(!("M" %in% lakesEvap) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yEvap)[n], fixed=TRUE)){
			if(!("C" %in% lakesEvap) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yEvap)[n], fixed=TRUE)){
			if(!("E" %in% lakesEvap) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yEvap)[n], fixed=TRUE)){
			if(!("O" %in% lakesEvap) | !ontarioComponentWBM){
				next;
			}
		}
		precLabel = paste(names(yEvap)[n],'Prec', sep='');
		if(is.infinite(yEvapPrec[[precLabel]])){
			modelCode = paste(
				modelCode,
				'\t',names(yEvap)[n],'Prec ~ dgamma(0.1,0.1)\n',
				sep=''
			);
		}
		else{
			modelCode = paste(
				modelCode,
				'\t',names(yEvap)[n],'Prec <- ',yEvapPrec[[precLabel]],'\n',
				sep=''
			);
		}		
	}
}

### RUNOFF

if(length(names(yRunoff)) > 0){
	for(n in 1:length(names(yRunoff))){
		if(grepl('Superior',names(yRunoff)[n], fixed=TRUE)){
			if(!("S" %in% lakesRunoff) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yRunoff)[n], fixed=TRUE)){
			if(!("M" %in% lakesRunoff) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yRunoff)[n], fixed=TRUE)){
			if(!("C" %in% lakesRunoff) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yRunoff)[n], fixed=TRUE)){
			if(!("E" %in% lakesRunoff) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yRunoff)[n], fixed=TRUE)){
			if(!("O" %in% lakesRunoff) | !ontarioComponentWBM){
				next;
			}
		}
		precLabel = paste(names(yRunoff)[n],'Prec', sep='');
		if(is.infinite(yRunoffPrec[[precLabel]])){
			modelCode = paste(
				modelCode,
				'\t',names(yRunoff)[n],'Prec ~ dgamma(0.1,0.1)\n',
				sep=''
			);
		}
		else{
			modelCode = paste(
				modelCode,
				'\t',names(yRunoff)[n],'Prec <- ',yRunoffPrec[[precLabel]],'\n',
				sep=''
			);
		}		
	}
}

### NBS

if(length(names(yNBS)) > 0){
	for(n in 1:length(names(yNBS))){
		if(grepl('Superior',names(yNBS)[n], fixed=TRUE)){
			if(!("S" %in% lakesNBS) | superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yNBS)[n], fixed=TRUE)){
			if(!("M" %in% lakesNBS) | miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yNBS)[n], fixed=TRUE)){
			if(!("C" %in% lakesNBS) | clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yNBS)[n], fixed=TRUE)){
			if(!("E" %in% lakesNBS) | erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yNBS)[n], fixed=TRUE)){
			if(!("O" %in% lakesNBS) | ontarioComponentWBM){
				next;
			}
		}
		precLabel = paste(names(yNBS)[n],'Prec', sep='');
		if(is.infinite(yNBSPrec[[precLabel]])){
			modelCode = paste(
				modelCode,
				'\t',names(yNBS)[n],'Prec ~ dgamma(0.1,0.1)\n',
				sep=''
			);
		}
		else{
			modelCode = paste(
				modelCode,
				'\t',names(yNBS)[n],'Prec <- ',yNBSPrec[[precLabel]],'\n',
				sep=''
			);
		}		
	}
}


modelCode = paste(modelCode,
'
	################################################################
	# POSTERIOR PREDICTIVE DRAWS FOR VERIFICATION (TODO FOR ALL LAKES, INC FLOW COMPUTATIONS)
	################################################################
	
	### BUDGET COMPONENTS
	
	for(jp in posteriorStartMonth:posteriorEndMonth){	
		# MONTH BY MONTH CHANGE IN STORAGE ANALYSIS

',
sep=''
);

if(superiorComponentWBM){
modelCode = paste(modelCode,
'
		ySuperiorDStorePP[jp] ~ dnorm(superiorDStore[jp], ySuperiorRStorePrec)
		
		superiorDStore[jp] <- (
			superiorPrecip[jp]
			-superiorEvap[jp]
			+superiorRunoff[jp]
			-superiorOutflowPP_mm[jp]
			+superiorDiversionPP_mm[jp]
			+superiorProcError[m[jp]]
		)
		
		superiorOutflowPP_mm[jp] <- superiorOutflow[jp]/supArea*1000*secondsInADay*dayVector[jp]
		superiorDiversionPP_mm[jp] <- superiorDiversion[jp]/supArea*1000*secondsInADay*dayVector[jp]
',
sep=''
);
}else{
modelCode = paste(modelCode,
'
		ySuperiorDStorePP[jp] ~ dnorm(superiorDStore[jp], ySuperiorRStorePrec)
		
		superiorDStore[jp] <- (
			superiorNBS[jp]
			-superiorOutflowPP_mm[jp]
			+superiorDiversionPP_mm[jp]
			+superiorProcError[m[jp]]
		)
		
		superiorOutflowPP_mm[jp] <- superiorOutflow[jp]/supArea*1000*secondsInADay*dayVector[jp]
		superiorDiversionPP_mm[jp] <- superiorDiversion[jp]/supArea*1000*secondsInADay*dayVector[jp]
',
sep=''
);
}

if(miHuronComponentWBM){
modelCode = paste(modelCode,
'
		yMiHuronDStorePP[jp] ~ dnorm(miHuronDStore[jp], yMiHuronRStorePrec)
		
		miHuronDStore[jp] <- (
			miHuronPrecip[jp]
			-miHuronEvap[jp]
			+miHuronRunoff[jp]
			+miHuronInflowPP_mm[jp]
			-miHuronOutflowPP_mm[jp]
			-miHuronDiversionPP_mm[jp]
			+miHuronProcError[m[jp]]
		)
		
		miHuronOutflowPP_mm[jp] <- miHuronOutflow[jp]/mhgArea*1000*secondsInADay*dayVector[jp]
		miHuronInflowPP_mm[jp] <- superiorOutflow[jp]/mhgArea*1000*secondsInADay*dayVector[jp]
		miHuronDiversionPP_mm[jp] <- miHuronDiversion[jp]/mhgArea*1000*secondsInADay*dayVector[jp]
',
sep=''
);
}else{
modelCode = paste(modelCode,
'
		yMiHuronDStorePP[jp] ~ dnorm(miHuronDStore[jp], yMiHuronRStorePrec)
		
		miHuronDStore[jp] <- (
			miHuronNBS[jp]
			+miHuronInflowPP_mm[jp]
			-miHuronOutflowPP_mm[jp]
			-miHuronDiversionPP_mm[jp]
			+miHuronProcError[m[jp]]
		)
		
		miHuronOutflowPP_mm[jp] <- miHuronOutflow[jp]/mhgArea*1000*secondsInADay*dayVector[jp]
		miHuronInflowPP_mm[jp] <- superiorOutflow[jp]/mhgArea*1000*secondsInADay*dayVector[jp]
		miHuronDiversionPP_mm[jp] <- miHuronDiversion[jp]/mhgArea*1000*secondsInADay*dayVector[jp]
		
',
sep=''
);
}

if(clairComponentWBM){
modelCode = paste(modelCode,
'
		yClairDStorePP[jp] ~ dnorm(clairDStore[jp], yClairRStorePrec)
		
		clairDStore[jp] <- (
			clairPrecip[jp]
			-clairEvap[jp]
			+clairRunoff[jp]
			+clairInflowPP_mm[jp]
			-clairOutflowPP_mm[jp]
			+clairProcError[m[jp]]
		)
		
		clairOutflowPP_mm[jp] <- clairOutflow[jp]/stcArea*1000*secondsInADay*dayVector[jp]
		clairInflowPP_mm[jp] <- miHuronOutflow[jp]/stcArea*1000*secondsInADay*dayVector[jp]
',
sep=''
);
}else{
if(clairCMS){
modelCode = paste(modelCode,
'
		yClairDStorePP[jp] ~ dnorm(clairDStore[jp], yClairRStorePrec);
		
		clairDStore[jp] <- (
			clairNBS[jp]
			+ miHuronOutflow[jp]
			- clairOutflow[jp]
			+ clairProcError[m[jp]]
		)
		
',
sep=''
);
}else{
modelCode = paste(modelCode,
'
		yClairDStorePP[jp] ~ dnorm(clairDStore[jp], yClairRStorePrec)
		
		clairDStore[jp] <- (
			clairNBS[jp]
			+clairInflowPP_mm[jp]
			-clairOutflowPP_mm[jp]
			+clairProcError[m[jp]]
		)
		
		clairOutflowPP_mm[jp] <- clairOutflow[jp]/stcArea*1000*secondsInADay*dayVector[jp]
		clairInflowPP_mm[jp] <- miHuronOutflow[jp]/stcArea*1000*secondsInADay*dayVector[jp]
',
sep=''
);
}

}

if(erieComponentWBM){
modelCode = paste(modelCode,
'
		yErieDStorePP[jp] ~ dnorm(erieDStore[jp], yErieRStorePrec) 
		
		erieDStore[jp] <- (			
			eriePrecip[jp]
			-erieEvap[jp]
			+erieRunoff[jp]
			-erieOutflowPP_mm[jp]
			# DETROIT RIVER
			+erieInflowPP_mm[jp]
			# WELLAND CANAL
			-erieDiversionPP_mm[jp]
			+erieProcError[m[jp]]
		)
		
		erieOutflowPP_mm[jp] <- erieOutflow[jp]/eriArea*1000*secondsInADay*dayVector[jp]
		erieInflowPP_mm[jp] <- clairOutflow[jp]/eriArea*1000*secondsInADay*dayVector[jp]
		erieDiversionPP_mm[jp] <- erieDiversion[jp]/eriArea*1000*secondsInADay*dayVector[jp]
',
sep=''
);
}else{
modelCode = paste(modelCode,
'
		yErieDStorePP[jp] ~ dnorm(erieDStore[jp], yErieRStorePrec) 
		
		erieDStore[jp] <- (			
			erieNBS[jp]
			-erieOutflowPP_mm[jp]
			# DETROIT RIVER
			+erieInflowPP_mm[jp]
			# WELLAND CANAL
			-erieDiversionPP_mm[jp]
			+erieProcError[m[jp]]
		)
		
		erieOutflowPP_mm[jp] <- erieOutflow[jp]/eriArea*1000*secondsInADay*dayVector[jp]
		erieInflowPP_mm[jp] <- clairOutflow[jp]/eriArea*1000*secondsInADay*dayVector[jp]
		erieDiversionPP_mm[jp] <- erieDiversion[jp]/eriArea*1000*secondsInADay*dayVector[jp]
',
sep=''
);
}

if(ontarioComponentWBM){
modelCode = paste(modelCode,
'
		yOntarioDStorePP[jp] ~ dnorm(ontarioDStore[jp], yOntarioRStorePrec) 
		
		ontarioDStore[jp] <- (
			ontarioPrecip[jp]
			-ontarioEvap[jp]
			+ontarioRunoff[jp]
			-ontarioOutflowPP_mm[jp]
			+ontarioInflowPP_mm[jp]
			+ontarioProcError[m[jp]]
		)
		
		ontarioOutflowPP_mm[jp] <- ontarioOutflow[jp]/ontArea*1000*secondsInADay*dayVector[jp]
		ontarioInflowPP_mm[jp] <- erieOutflow[jp]/ontArea*1000*secondsInADay*dayVector[jp] + erieDiversion[jp]/ontArea*1000*secondsInADay*dayVector[jp]
',
sep=''
);
}else{
modelCode = paste(modelCode,
'
		yOntarioDStorePP[jp] ~ dnorm(ontarioDStore[jp], yOntarioRStorePrec) 
		
		ontarioDStore[jp] <- (
			ontarioNBS[jp]
			-ontarioOutflowPP_mm[jp]
			+ontarioInflowPP_mm[jp]
			+ontarioProcError[m[jp]]
		)
		
		ontarioOutflowPP_mm[jp] <- ontarioOutflow[jp]/ontArea*1000*secondsInADay*dayVector[jp]
		ontarioInflowPP_mm[jp] <- erieOutflow[jp]/ontArea*1000*secondsInADay*dayVector[jp] + erieDiversion[jp]/ontArea*1000*secondsInADay*dayVector[jp]
',
sep=''
);
}



modelCode = paste(modelCode,
'		
	}
',
sep=''
)

if(checkModel){

### Posterior predictives for variables
modelCode = paste(modelCode,
'
	################################################################
	# POSTERIOR PREDICTIVE DRAWS FOR VERIFICATION (TODO FOR ALL LAKES, INC FLOW COMPUTATIONS)
	################################################################
	for(jp in posteriorStartMonth:posteriorEndMonth){		
		
',
sep=''
);

### PRECIP

if(length(names(yPrecip)) > 0){
	for(n in 1:length(names(yPrecip))){
		if(grepl('Superior',names(yPrecip)[n], fixed=TRUE)){
			if(!("S" %in% lakesPrecip) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yPrecip)[n], fixed=TRUE)){
			if(!("M" %in% lakesPrecip) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yPrecip)[n], fixed=TRUE)){
			if(!("C" %in% lakesPrecip) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yPrecip)[n], fixed=TRUE)){
			if(!("E" %in% lakesPrecip) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yPrecip)[n], fixed=TRUE)){
			if(!("O" %in% lakesPrecip) | !ontarioComponentWBM){
				next;
			}
		}
		modelCode = paste(
			modelCode,
			'\t\t',names(yPrecip)[n],'PP[jp] ~ dnorm(',names(yPrecip)[n],'Mean[jp], ',names(yPrecip)[n],'Prec)\n',
			sep=''
		);
	}
}

### EVAP

if(length(names(yEvap)) > 0){
	for(n in 1:length(names(yEvap))){
		if(grepl('Superior',names(yEvap)[n], fixed=TRUE)){
			if(!("S" %in% lakesEvap) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yEvap)[n], fixed=TRUE)){
			if(!("M" %in% lakesEvap) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yEvap)[n], fixed=TRUE)){
			if(!("C" %in% lakesEvap) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yEvap)[n], fixed=TRUE)){
			if(!("E" %in% lakesEvap) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yEvap)[n], fixed=TRUE)){
			if(!("O" %in% lakesEvap) | !ontarioComponentWBM){
				next;
			}
		}
		modelCode = paste(
			modelCode,
			'\t\t',names(yEvap)[n],'PP[jp] ~ dnorm(',names(yEvap)[n],'Mean[jp], ',names(yEvap)[n],'Prec)\n',
			sep=''
		);
	}
}


### RUNOFF

if(length(names(yRunoff)) > 0){
	for(n in 1:length(names(yRunoff))){
		if(grepl('Superior',names(yRunoff)[n], fixed=TRUE)){
			if(!("S" %in% lakesRunoff) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yRunoff)[n], fixed=TRUE)){
			if(!("M" %in% lakesRunoff) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yRunoff)[n], fixed=TRUE)){
			if(!("C" %in% lakesRunoff) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yRunoff)[n], fixed=TRUE)){
			if(!("E" %in% lakesRunoff) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yRunoff)[n], fixed=TRUE)){
			if(!("O" %in% lakesRunoff) | !ontarioComponentWBM){
				next;
			}
		}
		modelCode = paste(
			modelCode,
			'\t\t',names(yRunoff)[n],'PP[jp] ~ dnorm(',names(yRunoff)[n],'Mean[jp], ',names(yRunoff)[n],'Prec)\n',
			sep=''
		);
	}
}


### NBS

if(length(names(yNBS)) > 0){
	for(n in 1:length(names(yNBS))){
		if(grepl('Superior',names(yNBS)[n], fixed=TRUE)){
			if(!("S" %in% lakesNBS) | superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yNBS)[n], fixed=TRUE)){
			if(!("M" %in% lakesNBS) | miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yNBS)[n], fixed=TRUE)){
			if(!("C" %in% lakesNBS) | clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yNBS)[n], fixed=TRUE)){
			if(!("E" %in% lakesNBS) | erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yNBS)[n], fixed=TRUE)){
			if(!("O" %in% lakesNBS) | ontarioComponentWBM){
				next;
			}
		}
		modelCode = paste(
			modelCode,
			'\t\t',names(yNBS)[n],'PP[jp] ~ dnorm(',names(yNBS)[n],'Mean[jp], ',names(yNBS)[n],'Prec)\n',
			sep=''
		);
	}
}


### OUTFLOW

if(length(names(yOutflow)) > 0){
	for(n in 1:length(names(yOutflow))){
		if(grepl('Superior',names(yOutflow)[n], fixed=TRUE)){
			if(!("S" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('MiHuron',names(yOutflow)[n], fixed=TRUE)){
			if(!("M" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('Clair',names(yOutflow)[n], fixed=TRUE)){
			if(!("C" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('Erie',names(yOutflow)[n], fixed=TRUE)){
			if(!("E" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('Ontario',names(yOutflow)[n], fixed=TRUE)){
			if(!("O" %in% lakesOutflow)){
				next;
			}
		}

		if(!biasOutflows & flowUncertaintyInPercent){
			modelCode = paste(
				modelCode,
				'\t\t',names(yOutflow)[n],'PP[jp] ~ dnorm(',names(yOutflow)[n],'Mean[jp], ',names(yOutflow)[n],'Prec[m[jp]])\n',
				sep=''
			);
		}
		else{
			modelCode = paste(
				modelCode,
				'\t\t',names(yOutflow)[n],'PP[jp] ~ dnorm(',names(yOutflow)[n],'Mean[jp], ',names(yOutflow)[n],'Prec)\n',
				sep=''
			);
		}
	}
}

### DIVERSION

if(length(names(yDiversion)) > 0){
	for(n in 1:length(names(yDiversion))){
		if(grepl('Superior',names(yDiversion)[n], fixed=TRUE)){
			if(!("S" %in% lakesDiversion)){
				next;
			}
		}
		else if(grepl('MiHuron',names(yDiversion)[n], fixed=TRUE)){
			if(!("M" %in% lakesDiversion)){
				next;
			}
		}
		else if(grepl('Erie',names(yDiversion)[n], fixed=TRUE)){
			if(!("E" %in% lakesDiversion)){
				next;
			}
		}
		if(!biasOutflows & flowUncertaintyInPercent){
			modelCode = paste(
				modelCode,
				'\t\t',names(yDiversion)[n],'PP[jp] ~ dnorm(',names(yDiversion)[n],'Mean[jp], ',names(yDiversion)[n],'Prec[m[jp]])\n',
				sep=''
			);
		}
		else{
			modelCode = paste(
				modelCode,
				'\t\t',names(yDiversion)[n],'PP[jp] ~ dnorm(',names(yDiversion)[n],'Mean[jp], ',names(yDiversion)[n],'Prec)\n',
				sep=''
			);
		}
	}
}


modelCode = paste(modelCode,
'
	}

	### CUMULATIVE STORAGE ANALYSIS
	
	# 1 YEAR
	
	for(x in 12:posteriorEndMonth){
',
sep=''
);

if(superiorComponentWBM){
modelCode = paste(modelCode,
'
		ySuperiorR1YStorePP[x] ~ dnorm(superiorR1YStore[x], ySuperiorRStorePrec)
		
		superiorR1YStore[x] <- (
			sum(superiorPrecip[(x-12+1):x])
			-sum(superiorEvap[(x-12+1):x])
			+sum(superiorRunoff[(x-12+1):x])
			-superiorOutflowPPR1_mm[x]
			+superiorDiversionPPR1_mm[x]
			+sum(superiorProcError[m[(x-12+1):x]])
		)
		
		superiorOutflowPPR1_mm[x] <- sum(superiorOutflow[(x-12+1):x]/supArea*1000*secondsInADay*dayVector[(x-12+1):x])
		superiorDiversionPPR1_mm[x] <- sum(superiorDiversion[(x-12+1):x]/supArea*1000*secondsInADay*dayVector[(x-12+1):x])
		
',
sep=''
);
}else{
modelCode = paste(modelCode,
'
		ySuperiorR1YStorePP[x] ~ dnorm(superiorR1YStore[x], ySuperiorRStorePrec)
		
		superiorR1YStore[x] <- (
			sum(superiorNBS[(x-12+1):x])
			-superiorOutflowPPR1_mm[x]
			+superiorDiversionPPR1_mm[x]
			+sum(superiorProcError[m[(x-12+1):x]])
		)
		
		superiorOutflowPPR1_mm[x] <- sum(superiorOutflow[(x-12+1):x]/supArea*1000*secondsInADay*dayVector[(x-12+1):x])
		superiorDiversionPPR1_mm[x] <- sum(superiorDiversion[(x-12+1):x]/supArea*1000*secondsInADay*dayVector[(x-12+1):x])
		
',
sep=''
);
}

if(miHuronComponentWBM){
modelCode = paste(modelCode,
'
		yMiHuronR1YStorePP[x] ~ dnorm(miHuronR1YStore[x], yMiHuronRStorePrec)
		
		miHuronR1YStore[x] <- (
			sum(miHuronPrecip[(x-12+1):x])
			-sum(miHuronEvap[(x-12+1):x])
			+sum(miHuronRunoff[(x-12+1):x])
			+miHuronInflowPPR1_mm[x]
			-miHuronOutflowPPR1_mm[x]
			-miHuronDiversionPPR1_mm[x]
			+sum(miHuronProcError[m[(x-12+1):x]])
		)
		
		miHuronOutflowPPR1_mm[x] <- sum(miHuronOutflow[(x-12+1):x]/mhgArea*1000*secondsInADay*dayVector[(x-12+1):x])
		miHuronInflowPPR1_mm[x] <- sum(superiorOutflow[(x-12+1):x]/mhgArea*1000*secondsInADay*dayVector[(x-12+1):x])
		miHuronDiversionPPR1_mm[x] <- sum(miHuronDiversion[(x-12+1):x]/mhgArea*1000*secondsInADay*dayVector[(x-12+1):x])
		
',
sep=''
)
}else{
modelCode = paste(modelCode,
'
		yMiHuronR1YStorePP[x] ~ dnorm(miHuronR1YStore[x], yMiHuronRStorePrec)
		
		miHuronR1YStore[x] <- (
			sum(miHuronNBS[(x-12+1):x])
			+miHuronInflowPPR1_mm[x]
			-miHuronOutflowPPR1_mm[x]
			-miHuronDiversionPPR1_mm[x]
			+sum(miHuronProcError[m[(x-12+1):x]])
		)
		
		miHuronOutflowPPR1_mm[x] <- sum(miHuronOutflow[(x-12+1):x]/mhgArea*1000*secondsInADay*dayVector[(x-12+1):x])
		miHuronInflowPPR1_mm[x] <- sum(superiorOutflow[(x-12+1):x]/mhgArea*1000*secondsInADay*dayVector[(x-12+1):x])
		miHuronDiversionPPR1_mm[x] <- sum(miHuronDiversion[(x-12+1):x]/mhgArea*1000*secondsInADay*dayVector[(x-12+1):x])
		
',
sep=''
)	
}

if(clairComponentWBM){
modelCode = paste(modelCode,
'
		yClairR1YStorePP[x] ~ dnorm(clairR1YStore[x], yClairRStorePrec)
		
		clairR1YStore[x] <- (
			sum(clairPrecip[(x-12+1):x])
			-sum(clairEvap[(x-12+1):x])
			+sum(clairRunoff[(x-12+1):x])
			+clairInflowPPR1_mm[x]
			-clairOutflowPPR1_mm[x]
			+sum(clairProcError[m[(x-12+1):x]])
		)
		
		clairOutflowPPR1_mm[x] <- sum(clairOutflow[(x-12+1):x]/stcArea*1000*secondsInADay*dayVector[(x-12+1):x])
		clairInflowPPR1_mm[x] <- sum(miHuronOutflow[(x-12+1):x]/stcArea*1000*secondsInADay*dayVector[(x-12+1):x])
		
',
sep=''
)
}else{
if(clairCMS){
modelCode = paste(modelCode,
'
		yClairR1YStorePP[x] ~ dnorm(clairR1YStore[x], yClairRStorePrec);
		
		clairR1YStore[x] <- (
			sum(clairNBS[(x-12+1):x])
			+ sum(miHuronOutflow[(x-12+1):x])
			- sum(clairOutflow[(x-12+1):x])
			+ sum(clairProcError[m[(x-12+1):x]])
		)
',
sep=''
)	
}else{
modelCode = paste(modelCode,
'
		yClairR1YStorePP[x] ~ dnorm(clairR1YStore[x], yClairRStorePrec)
		
		clairR1YStore[x] <- (
			sum(clairNBS[(x-12+1):x])
			+clairInflowPPR1_mm[x]
			-clairOutflowPPR1_mm[x]
			+sum(clairProcError[m[(x-12+1):x]])
		)
		
		clairOutflowPPR1_mm[x] <- sum(clairOutflow[(x-12+1):x]/stcArea*1000*secondsInADay*dayVector[(x-12+1):x])
		clairInflowPPR1_mm[x] <- sum(miHuronOutflow[(x-12+1):x]/stcArea*1000*secondsInADay*dayVector[(x-12+1):x])
		
',
sep=''
)	
}

}

if(erieComponentWBM){
modelCode = paste(modelCode,
'
		yErieR1YStorePP[x] ~ dnorm(erieR1YStore[x], yErieRStorePrec) 
		
		erieR1YStore[x] <- (			
			sum(eriePrecip[(x-12+1):x])
			-sum(erieEvap[(x-12+1):x])
			+sum(erieRunoff[(x-12+1):x])
			-erieOutflowPPR1_mm[x]
			# DETROIT RIVER
			+erieInflowPPR1_mm[x]
			# WELLAND CANAL
			-erieDiversionPPR1_mm[x]
			+sum(erieProcError[m[(x-12+1):x]])
		)
		
		erieOutflowPPR1_mm[x] <- sum(erieOutflow[(x-12+1):x]/eriArea*1000*secondsInADay*dayVector[(x-12+1):x])
		erieInflowPPR1_mm[x] <- sum(clairOutflow[(x-12+1):x]/eriArea*1000*secondsInADay*dayVector[(x-12+1):x])
		erieDiversionPPR1_mm[x] <- sum(erieDiversion[(x-12+1):x]/eriArea*1000*secondsInADay*dayVector[(x-12+1):x])
		
',
sep=''
)
}else{
modelCode = paste(modelCode,
'
		yErieR1YStorePP[x] ~ dnorm(erieR1YStore[x], yErieRStorePrec) 
		
		erieR1YStore[x] <- (			
			sum(erieNBS[(x-12+1):x])
			-erieOutflowPPR1_mm[x]
			# DETROIT RIVER
			+erieInflowPPR1_mm[x]
			# WELLAND CANAL
			-erieDiversionPPR1_mm[x]
			+sum(erieProcError[m[(x-12+1):x]])
		)
		
		erieOutflowPPR1_mm[x] <- sum(erieOutflow[(x-12+1):x]/eriArea*1000*secondsInADay*dayVector[(x-12+1):x])
		erieInflowPPR1_mm[x] <- sum(clairOutflow[(x-12+1):x]/eriArea*1000*secondsInADay*dayVector[(x-12+1):x])
		erieDiversionPPR1_mm[x] <- sum(erieDiversion[(x-12+1):x]/eriArea*1000*secondsInADay*dayVector[(x-12+1):x])
		
',
sep=''
)	
}

if(ontarioComponentWBM){
modelCode = paste(modelCode,
'
		### ONTARIO
		yOntarioR1YStorePP[x] ~ dnorm(ontarioR1YStore[x], yOntarioRStorePrec) 
		
		ontarioR1YStore[x] <- (
			sum(ontarioPrecip[(x-12+1):x])
			-sum(ontarioEvap[(x-12+1):x])
			+sum(ontarioRunoff[(x-12+1):x])
			-ontarioOutflowPPR1_mm[x]
			+ontarioInflowPPR1_mm[x]
			+sum(ontarioProcError[m[(x-12+1):x]])
		)
		
		ontarioOutflowPPR1_mm[x] <- sum(ontarioOutflow[(x-12+1):x]/ontArea*1000*secondsInADay*dayVector[(x-12+1):x])
		ontarioInflowPPR1_mm[x] <- sum(erieOutflow[(x-12+1):x]/ontArea*1000*secondsInADay*dayVector[(x-12+1):x]) + sum(erieDiversion[(x-12+1):x]/ontArea*1000*secondsInADay*dayVector[(x-12+1):x])
',
sep=''
)
}else{
modelCode = paste(modelCode,
'
		### ONTARIO
		yOntarioR1YStorePP[x] ~ dnorm(ontarioR1YStore[x], yOntarioRStorePrec) 
		
		ontarioR1YStore[x] <- (
			sum(ontarioNBS[(x-12+1):x])
			-ontarioOutflowPPR1_mm[x]
			+ontarioInflowPPR1_mm[x]
			+sum(ontarioProcError[m[(x-12+1):x]])
		)
		
		ontarioOutflowPPR1_mm[x] <- sum(ontarioOutflow[(x-12+1):x]/ontArea*1000*secondsInADay*dayVector[(x-12+1):x])
		ontarioInflowPPR1_mm[x] <- sum(erieOutflow[(x-12+1):x]/ontArea*1000*secondsInADay*dayVector[(x-12+1):x]) + sum(erieDiversion[(x-12+1):x]/ontArea*1000*secondsInADay*dayVector[(x-12+1):x])
',
sep=''
)	
}

modelCode = paste(modelCode,
'	
	}	
	
	# 5 YEAR
	
	for(z in 60:posteriorEndMonth){

',
sep=''
);

if(superiorComponentWBM){
modelCode = paste(modelCode,
'
		ySuperiorR5YStorePP[z] ~ dnorm(superiorR5YStore[z], ySuperiorRStorePrec)
		
		superiorR5YStore[z] <- (
			sum(superiorPrecip[(z-60+1):z])
			-sum(superiorEvap[(z-60+1):z])
			+sum(superiorRunoff[(z-60+1):z])
			-superiorOutflowPPR5_mm[z]
			+superiorDiversionPPR5_mm[z]
			+sum(superiorProcError[m[(z-60+1):z]])
		)
		
		superiorOutflowPPR5_mm[z] <- sum(superiorOutflow[(z-60+1):z]/supArea*1000*secondsInADay*dayVector[(z-60+1):z])
		superiorDiversionPPR5_mm[z] <- sum(superiorDiversion[(z-60+1):z]/supArea*1000*secondsInADay*dayVector[(z-60+1):z])
		
',
sep=''
);
}else{
modelCode = paste(modelCode,
'
		ySuperiorR5YStorePP[z] ~ dnorm(superiorR5YStore[z], ySuperiorRStorePrec)
		
		superiorR5YStore[z] <- (
			sum(superiorNBS[(z-60+1):z])
			-superiorOutflowPPR5_mm[z]
			+superiorDiversionPPR5_mm[z]
			+sum(superiorProcError[m[(z-60+1):z]])
		)
		
		superiorOutflowPPR5_mm[z] <- sum(superiorOutflow[(z-60+1):z]/supArea*1000*secondsInADay*dayVector[(z-60+1):z])
		superiorDiversionPPR5_mm[z] <- sum(superiorDiversion[(z-60+1):z]/supArea*1000*secondsInADay*dayVector[(z-60+1):z])
	
',
sep=''
);
}

if(miHuronComponentWBM){
modelCode = paste(modelCode,
'
		yMiHuronR5YStorePP[z] ~ dnorm(miHuronR5YStore[z], yMiHuronRStorePrec)
		
		miHuronR5YStore[z] <- (
			sum(miHuronPrecip[(z-60+1):z])
			-sum(miHuronEvap[(z-60+1):z])
			+sum(miHuronRunoff[(z-60+1):z])
			+miHuronInflowPPR5_mm[z]
			-miHuronOutflowPPR5_mm[z]
			-miHuronDiversionPPR5_mm[z]
			+sum(miHuronProcError[m[(z-60+1):z]])
		)
		
		miHuronOutflowPPR5_mm[z] <- sum(miHuronOutflow[(z-60+1):z]/mhgArea*1000*secondsInADay*dayVector[(z-60+1):z])
		miHuronInflowPPR5_mm[z] <- sum(superiorOutflow[(z-60+1):z]/mhgArea*1000*secondsInADay*dayVector[(z-60+1):z])
		miHuronDiversionPPR5_mm[z] <- sum(miHuronDiversion[(z-60+1):z]/mhgArea*1000*secondsInADay*dayVector[(z-60+1):z])
		
',
sep=''
);
}else{
modelCode = paste(modelCode,
'
		yMiHuronR5YStorePP[z] ~ dnorm(miHuronR5YStore[z], yMiHuronRStorePrec)
		
		miHuronR5YStore[z] <- (
			sum(miHuronNBS[(z-60+1):z])
			+miHuronInflowPPR5_mm[z]
			-miHuronOutflowPPR5_mm[z]
			-miHuronDiversionPPR5_mm[z]
			+sum(miHuronProcError[m[(z-60+1):z]])
		)
		
		miHuronOutflowPPR5_mm[z] <- sum(miHuronOutflow[(z-60+1):z]/mhgArea*1000*secondsInADay*dayVector[(z-60+1):z])
		miHuronInflowPPR5_mm[z] <- sum(superiorOutflow[(z-60+1):z]/mhgArea*1000*secondsInADay*dayVector[(z-60+1):z])
		miHuronDiversionPPR5_mm[z] <- sum(miHuronDiversion[(z-60+1):z]/mhgArea*1000*secondsInADay*dayVector[(z-60+1):z])
		
',
sep=''
);
}

if(clairComponentWBM){
modelCode = paste(modelCode,
'
		yClairR5YStorePP[z] ~ dnorm(clairR5YStore[z], yClairRStorePrec)
		
		clairR5YStore[z] <- (
			sum(clairPrecip[(z-60+1):z])
			-sum(clairEvap[(z-60+1):z])
			+sum(clairRunoff[(z-60+1):z])
			+clairInflowPPR5_mm[z]
			-clairOutflowPPR5_mm[z]
			+sum(clairProcError[m[(z-60+1):z]])
		)
		
		clairOutflowPPR5_mm[z] <- sum(clairOutflow[(z-60+1):z]/stcArea*1000*secondsInADay*dayVector[(z-60+1):z])
		clairInflowPPR5_mm[z] <- sum(miHuronOutflow[(z-60+1):z]/stcArea*1000*secondsInADay*dayVector[(z-60+1):z])
		
',
sep=''
);
}else{
if(clairCMS){
modelCode = paste(modelCode,
'
		yClairR5YStorePP[z] ~ dnorm(clairR5YStore[z], yClairRStorePrec);
		
		clairR5YStore[z] <- (
			sum(clairNBS[(z-60+1):z])
			+ sum(miHuronOutflow[(z-60+1):z])
			- sum(clairOutflow[(z-60+1):z])
			+ sum(clairProcError[m[(z-60+1):z]])
		)	
',
sep=''
);
}else{
modelCode = paste(modelCode,
'
		yClairR5YStorePP[z] ~ dnorm(clairR5YStore[z], yClairRStorePrec)
		
		clairR5YStore[z] <- (
			sum(clairNBS[(z-60+1):z])
			+clairInflowPPR5_mm[z]
			-clairOutflowPPR5_mm[z]
			+sum(clairProcError[m[(z-60+1):z]])
		)
		
		clairOutflowPPR5_mm[z] <- sum(clairOutflow[(z-60+1):z]/stcArea*1000*secondsInADay*dayVector[(z-60+1):z])
		clairInflowPPR5_mm[z] <- sum(miHuronOutflow[(z-60+1):z]/stcArea*1000*secondsInADay*dayVector[(z-60+1):z])
		
',
sep=''
);
}

}

if(erieComponentWBM){
modelCode = paste(modelCode,
'
		yErieR5YStorePP[z] ~ dnorm(erieR5YStore[z], yErieRStorePrec) 
		
		erieR5YStore[z] <- (			
			sum(eriePrecip[(z-60+1):z])
			-sum(erieEvap[(z-60+1):z])
			+sum(erieRunoff[(z-60+1):z])
			-erieOutflowPPR5_mm[z]
			# DETROIT RIVER
			+erieInflowPPR5_mm[z]
			# WELLAND CANAL
			-erieDiversionPPR5_mm[z]
			+sum(erieProcError[m[(z-60+1):z]])
		)
		
		erieOutflowPPR5_mm[z] <- sum(erieOutflow[(z-60+1):z]/eriArea*1000*secondsInADay*dayVector[(z-60+1):z])
		erieInflowPPR5_mm[z] <- sum(clairOutflow[(z-60+1):z]/eriArea*1000*secondsInADay*dayVector[(z-60+1):z])
		erieDiversionPPR5_mm[z] <- sum(erieDiversion[(z-60+1):z]/eriArea*1000*secondsInADay*dayVector[(z-60+1):z])
		
',
sep=''
);
}else{
modelCode = paste(modelCode,
'
		yErieR5YStorePP[z] ~ dnorm(erieR5YStore[z], yErieRStorePrec) 
		
		erieR5YStore[z] <- (			
			sum(erieNBS[(z-60+1):z])
			-erieOutflowPPR5_mm[z]
			# DETROIT RIVER
			+erieInflowPPR5_mm[z]
			# WELLAND CANAL
			-erieDiversionPPR5_mm[z]
			+sum(erieProcError[m[(z-60+1):z]])
		)
		
		erieOutflowPPR5_mm[z] <- sum(erieOutflow[(z-60+1):z]/eriArea*1000*secondsInADay*dayVector[(z-60+1):z])
		erieInflowPPR5_mm[z] <- sum(clairOutflow[(z-60+1):z]/eriArea*1000*secondsInADay*dayVector[(z-60+1):z])
		erieDiversionPPR5_mm[z] <- sum(erieDiversion[(z-60+1):z]/eriArea*1000*secondsInADay*dayVector[(z-60+1):z])
		
',
sep=''
);
}

if(ontarioComponentWBM){
modelCode = paste(modelCode,
'
		### ONTARIO
		yOntarioR5YStorePP[z] ~ dnorm(ontarioR5YStore[z], yOntarioRStorePrec) 
		
		ontarioR5YStore[z] <- (
			sum(ontarioPrecip[(z-60+1):z])
			-sum(ontarioEvap[(z-60+1):z])
			+sum(ontarioRunoff[(z-60+1):z])
			-ontarioOutflowPPR5_mm[z]
			+ontarioInflowPPR5_mm[z]
			+sum(ontarioProcError[m[(z-60+1):z]])
		)
		
		ontarioOutflowPPR5_mm[z] <- sum(ontarioOutflow[(z-60+1):z]/ontArea*1000*secondsInADay*dayVector[(z-60+1):z])
		ontarioInflowPPR5_mm[z] <- sum(erieOutflow[(z-60+1):z]/ontArea*1000*secondsInADay*dayVector[(z-60+1):z]) + sum(erieDiversion[(z-60+1):z]/ontArea*1000*secondsInADay*dayVector[(z-60+1):z])
',
sep=''
);
}else{
modelCode = paste(modelCode,
'
		### ONTARIO
		yOntarioR5YStorePP[z] ~ dnorm(ontarioR5YStore[z], yOntarioRStorePrec) 
		
		ontarioR5YStore[z] <- (
			sum(ontarioNBS[(z-60+1):z])
			-ontarioOutflowPPR5_mm[z]
			+ontarioInflowPPR5_mm[z]
			+sum(ontarioProcError[m[(z-60+1):z]])
		)
		
		ontarioOutflowPPR5_mm[z] <- sum(ontarioOutflow[(z-60+1):z]/ontArea*1000*secondsInADay*dayVector[(z-60+1):z])
		ontarioInflowPPR5_mm[z] <- sum(erieOutflow[(z-60+1):z]/ontArea*1000*secondsInADay*dayVector[(z-60+1):z]) + sum(erieDiversion[(z-60+1):z]/ontArea*1000*secondsInADay*dayVector[(z-60+1):z])
',
sep=''
);
}

modelCode = paste(modelCode,
'
	}
',
sep=''
)

}
modelCode = paste(modelCode,
'
} # END MODEL
',
sep=''
)

############################################################################################
# TODO (2019/04/15): Start here... then go back and reconfigure St. Clair
############################################################################################

paramsToMonitor <- c( 
	"superiorOutflow", "superiorDiversion",
	"superiorOutflow_mm", "superiorDiversion_mm",
	"superiorOutflowPP_mm", "superiorDiversionPP_mm",
	"miHuronOutflow", "miHuronDiversion",
	"miHuronOutflow_mm", "miHuronInflow_mm", "miHuronDiversion_mm",
	"miHuronOutflowPP_mm", "miHuronInflowPP_mm", "miHuronDiversionPP_mm",
	"superiorDStore", "miHuronDStore",
	"ySuperiorDStorePP", "yMiHuronDStorePP", 
	"superiorRStore", "miHuronRStore", 	
	"clairOutflow",	
	"clairDStore",
	"yClairDStorePP",
	"clairRStore",
	"erieOutflow", "erieDiversion",
	"erieOutflow_mm", "erieInflow_mm", "erieDiversion_mm",
	"erieOutflowPP_mm", "erieInflowPP_mm", "erieDiversionPP_mm",
	"ontarioOutflow",
	"ontarioInflow_mm", "ontarioOutflow_mm",
	"ontarioInflowPP_mm", "ontarioOutflowPP_mm",
	"erieDStore", "ontarioDStore",
	"yErieDStorePP", "yOntarioDStorePP",
	"erieRStore", "ontarioRStore"
);

if(clairComponentWBM | (!clairComponentWBM & !clairCMS)){
	paramsToMonitor = c(paramsToMonitor,
		"clairOutflow_mm", "clairInflow_mm", 
		"clairOutflowPP_mm", "clairInflowPP_mm"
	);
}

if(checkModel){
	paramsToMonitor = c(paramsToMonitor,
		"ySuperiorR1YStorePP", "yMiHuronR1YStorePP", "yClairR1YStorePP", "yErieR1YStorePP",	"yOntarioR1YStorePP",
		"ySuperiorR5YStorePP", "yMiHuronR5YStorePP", "yClairR5YStorePP", "yErieR5YStorePP",	"yOntarioR5YStorePP"
	)
}

if(superiorComponentWBM){
	paramsToMonitor = c(paramsToMonitor,
		"superiorPrecip", "superiorEvap", "superiorRunoff"
	);
}else{
	paramsToMonitor = c(paramsToMonitor,
		"superiorNBS"
	);
}

if(miHuronComponentWBM){
	paramsToMonitor = c(paramsToMonitor,
		"miHuronPrecip", "miHuronEvap", "miHuronRunoff"
	);
}else{
	paramsToMonitor = c(paramsToMonitor,
		"miHuronNBS"
	);
}

if(clairComponentWBM){
	paramsToMonitor = c(paramsToMonitor,
		"clairPrecip", "clairEvap", "clairRunoff"
	);
}else{
	paramsToMonitor = c(paramsToMonitor,
		"clairNBS"
	);
}

if(erieComponentWBM){
	paramsToMonitor = c(paramsToMonitor,
		"eriePrecip", "erieEvap", "erieRunoff"
	);
}else{
	paramsToMonitor = c(paramsToMonitor,
		"erieNBS"
	);
}

if(ontarioComponentWBM){
	paramsToMonitor = c(paramsToMonitor,
		"ontarioPrecip", "ontarioEvap", "ontarioRunoff"
	);
}else{
	paramsToMonitor = c(paramsToMonitor,
		"ontarioNBS"
	);
}

if(incProcError){
	paramsToMonitor <- c(
		paramsToMonitor,
		"superiorProcError", 
		"miHuronProcError",
		"clairProcError",
		"erieProcError", 
		"ontarioProcError"
	);
}

### PRECIP

if(length(names(yPrecip)) > 0){
	for(n in 1:length(names(yPrecip))){
		if(grepl('Superior',names(yPrecip)[n], fixed=TRUE)){
			if(!("S" %in% lakesPrecip) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yPrecip)[n], fixed=TRUE)){
			if(!("M" %in% lakesPrecip) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yPrecip)[n], fixed=TRUE)){
			if(!("C" %in% lakesPrecip) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yPrecip)[n], fixed=TRUE)){
			if(!("E" %in% lakesPrecip) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yPrecip)[n], fixed=TRUE)){
			if(!("O" %in% lakesPrecip) | !ontarioComponentWBM){
				next;
			}
		}
		paramsToMonitor = c(paramsToMonitor, paste(names(yPrecip)[n],'Bias', sep=''))
		paramsToMonitor = c(paramsToMonitor, paste(names(yPrecip)[n],'Prec', sep=''))
		if(checkModel){
			paramsToMonitor = c(paramsToMonitor, paste(names(yPrecip)[n],'PP', sep=''))
		}
	}
}

### EVAP

if(length(names(yEvap)) > 0){
	for(n in 1:length(names(yEvap))){
		if(grepl('Superior',names(yEvap)[n], fixed=TRUE)){
			if(!("S" %in% lakesEvap) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yEvap)[n], fixed=TRUE)){
			if(!("M" %in% lakesEvap) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yEvap)[n], fixed=TRUE)){
			if(!("C" %in% lakesEvap) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yEvap)[n], fixed=TRUE)){
			if(!("E" %in% lakesEvap) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yEvap)[n], fixed=TRUE)){
			if(!("O" %in% lakesEvap) | !ontarioComponentWBM){
				next;
			}
		}
		paramsToMonitor = c(paramsToMonitor, paste(names(yEvap)[n],'Bias', sep=''))
		paramsToMonitor = c(paramsToMonitor, paste(names(yEvap)[n],'Prec', sep=''))
		if(checkModel){
			paramsToMonitor = c(paramsToMonitor, paste(names(yEvap)[n],'PP', sep=''))
		}
	}
}

### RUNOFF

if(length(names(yRunoff)) > 0){
	for(n in 1:length(names(yRunoff))){
		if(grepl('Superior',names(yRunoff)[n], fixed=TRUE)){
			if(!("S" %in% lakesRunoff) | !superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yRunoff)[n], fixed=TRUE)){
			if(!("M" %in% lakesRunoff) | !miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yRunoff)[n], fixed=TRUE)){
			if(!("C" %in% lakesRunoff) | !clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yRunoff)[n], fixed=TRUE)){
			if(!("E" %in% lakesRunoff) | !erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yRunoff)[n], fixed=TRUE)){
			if(!("O" %in% lakesRunoff) | !ontarioComponentWBM){
				next;
			}
		}
		paramsToMonitor = c(paramsToMonitor, paste(names(yRunoff)[n],'Bias', sep=''))
		paramsToMonitor = c(paramsToMonitor, paste(names(yRunoff)[n],'Prec', sep=''))
		if(checkModel){
			paramsToMonitor = c(paramsToMonitor, paste(names(yRunoff)[n],'PP', sep=''))
		}
	}
}

### NBS

if(length(names(yNBS)) > 0){
	for(n in 1:length(names(yNBS))){
		if(grepl('Superior',names(yNBS)[n], fixed=TRUE)){
			if(!("S" %in% lakesNBS) | superiorComponentWBM){
				next;
			}
		}
		else if(grepl('MiHuron',names(yNBS)[n], fixed=TRUE)){
			if(!("M" %in% lakesNBS) | miHuronComponentWBM){
				next;
			}
		}
		else if(grepl('Clair',names(yNBS)[n], fixed=TRUE)){
			if(!("C" %in% lakesNBS) | clairComponentWBM){
				next;
			}
		}
		else if(grepl('Erie',names(yNBS)[n], fixed=TRUE)){
			if(!("E" %in% lakesNBS) | erieComponentWBM){
				next;
			}
		}
		else if(grepl('Ontario',names(yNBS)[n], fixed=TRUE)){
			if(!("O" %in% lakesNBS) | ontarioComponentWBM){
				next;
			}
		}
		paramsToMonitor = c(paramsToMonitor, paste(names(yNBS)[n],'Bias', sep=''))
		paramsToMonitor = c(paramsToMonitor, paste(names(yNBS)[n],'Prec', sep=''))
		if(checkModel){
			paramsToMonitor = c(paramsToMonitor, paste(names(yNBS)[n],'PP', sep=''))
		}
	}
}

### OUTFLOW

if(length(names(yOutflow)) > 0){
	for(n in 1:length(names(yOutflow))){
		if(grepl('Superior',names(yOutflow)[n], fixed=TRUE)){
			if(!("S" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('MiHuron',names(yOutflow)[n], fixed=TRUE)){
			if(!("M" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('Clair',names(yOutflow)[n], fixed=TRUE)){
			if(!("C" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('Erie',names(yOutflow)[n], fixed=TRUE)){
			if(!("E" %in% lakesOutflow)){
				next;
			}
		}
		else if(grepl('Ontario',names(yOutflow)[n], fixed=TRUE)){
			if(!("O" %in% lakesOutflow)){
				next;
			}
		}
		paramsToMonitor = c(paramsToMonitor, paste(names(yOutflow)[n],'Bias', sep=''))
		paramsToMonitor = c(paramsToMonitor, paste(names(yOutflow)[n],'Prec', sep=''))
		if(checkModel){
			paramsToMonitor = c(paramsToMonitor, paste(names(yOutflow)[n],'PP', sep=''))
		}
	}
}

### DIVERSION

if(length(names(yDiversion)) > 0){
	for(n in 1:length(names(yDiversion))){
		if(grepl('Superior',names(yDiversion)[n], fixed=TRUE)){
			if(!("S" %in% lakesDiversion)){
				next;
			}
		}
		else if(grepl('MiHuron',names(yDiversion)[n], fixed=TRUE)){
			if(!("M" %in% lakesDiversion)){
				next;
			}
		}
		else if(grepl('Erie',names(yDiversion)[n], fixed=TRUE)){
			if(!("E" %in% lakesDiversion)){
				next;
			}
		}
		paramsToMonitor = c(paramsToMonitor, paste(names(yDiversion)[n],'Bias', sep=''))
		paramsToMonitor = c(paramsToMonitor, paste(names(yDiversion)[n],'Prec', sep=''))
		if(checkModel){
			paramsToMonitor = c(paramsToMonitor, paste(names(yDiversion)[n],'PP', sep=''))
		}
	}
}

### WRITE MODEL TO FILE
modOut = file(paste(modelName,'.bug.r', sep=''), 'w');
cat(modelCode, file=modOut)
close(modOut)




