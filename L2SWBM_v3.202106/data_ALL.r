### GET DATA

### SUPERIOR

superiorBOM = read.csv('data/SUP_BOM_MM.csv', TRUE, skip=6);
superiorDS = cbind(
	superiorBOM[-(nrow(superiorBOM)),1:2],
	(superiorBOM[-1,3] - superiorBOM[-(nrow(superiorBOM)),3])*1000
);

superiorOutflow = read.csv('data/StMarysMonthlyMeanFlows.csv', TRUE, skip=8);
superiorOutflow_Prior = superiorOutflow;
superiorOutflow = superiorOutflow[,c(1,2,(3:ncol(superiorOutflow))*outflowInput)]
superiorOutflowSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(superiorOutflow) >= 3){
	colnames(superiorOutflow) = c('Year', 'Month', colnames(superiorOutflow)[3:length(colnames(superiorOutflow))])
	superiorOutflowSrc = colnames(superiorOutflow)[3:length(colnames(superiorOutflow))]
}

superiorDiversion = read.csv('data/LongLacOgokiMonthlyMeanFlows.csv', TRUE, skip=6);
superiorDiversion_Prior = superiorDiversion;
superiorDiversion = superiorDiversion[,c(1,2,(3:ncol(superiorDiversion))*diversionInput)]
superiorDiversionSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(superiorDiversion) >= 3){
	colnames(superiorDiversion) = c('Year', 'Month', colnames(superiorDiversion)[3:length(colnames(superiorDiversion))])
	superiorDiversionSrc = colnames(superiorDiversion)[3:length(colnames(superiorDiversion))]
}

superiorPrecip = read.csv('data/SUP_lake_Prec.csv', TRUE, na.strings='-9999.9', skip=4);
superiorPrecip[superiorPrecip < 0] = NA;
superiorPrecip_Prior = superiorPrecip
superiorPrecip = superiorPrecip[,c(1,2,(3:ncol(superiorPrecip))*precipInput)]
superiorPrecipSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(superiorPrecip) >= 3){
	colnames(superiorPrecip) = c('Year', 'Month', colnames(superiorPrecip)[3:length(colnames(superiorPrecip))])
	superiorPrecipSrc = colnames(superiorPrecip)[3:length(colnames(superiorPrecip))]
}

superiorEvap = read.csv('data/SUP_lake_Evap.csv', TRUE, na.strings='-9999.9', skip=4);
superiorEvap[superiorEvap < -5000] = NA
superiorEvap_Prior = superiorEvap
superiorEvap = superiorEvap[,c(1,2,(3:ncol(superiorEvap))*evapInput)]
superiorEvapSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(superiorEvap) >= 3){
	colnames(superiorEvap) = c('Year', 'Month', colnames(superiorEvap)[3:length(colnames(superiorEvap))])
	superiorEvapSrc = colnames(superiorEvap)[3:length(colnames(superiorEvap))]
}

superiorRunoff = read.csv('data/SUP_lake_Runoff.csv', TRUE, na.strings='None', skip=4);
superiorRunoff[superiorRunoff < 0] = NA
superiorRunoff_Prior = superiorRunoff
superiorRunoff = superiorRunoff[,c(1,2,(3:ncol(superiorRunoff))*runoffInput)] 
superiorRunoffSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(superiorRunoff) >= 3){
	colnames(superiorRunoff) = c('Year', 'Month', colnames(superiorRunoff)[3:length(colnames(superiorRunoff))])
	superiorRunoffSrc = colnames(superiorRunoff)[3:length(colnames(superiorRunoff))]
}

superiorNBS = read.csv('data/SUP_lake_NBS.csv', TRUE, na.strings='-9999.9', skip=4);
superiorNBS[superiorNBS < -5000] = NA
superiorNBS_Prior = superiorNBS;
superiorNBS = superiorNBS[,c(1,2,(3:ncol(superiorNBS))*nbsInput)]
superiorNBSSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(superiorNBS) >= 3){
	colnames(superiorNBS) = c('Year', 'Month', colnames(superiorNBS)[3:length(colnames(superiorNBS))])
	superiorNBSSrc = colnames(superiorNBS)[3:length(colnames(superiorNBS))]
}

### MI-HURON

miHuronBOM = read.csv('data/MHG_BOM_MM.csv', TRUE, skip=6);
miHuronDS = cbind(
	miHuronBOM[-(nrow(miHuronBOM)),1:2],
	(miHuronBOM[-1,3] - miHuronBOM[-(nrow(miHuronBOM)),3])*1000
);

miHuronOutflow = read.csv('data/StClairMonthlyMeanFlows.csv', TRUE, skip=8);
miHuronOutflow_Prior = miHuronOutflow;
miHuronOutflow = miHuronOutflow[,c(1,2,(3:ncol(miHuronOutflow))*outflowInput)]
miHuronOutflowSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(miHuronOutflow) >= 3){
	colnames(miHuronOutflow) = c('Year', 'Month', colnames(miHuronOutflow)[3:length(colnames(miHuronOutflow))])
	miHuronOutflowSrc = colnames(miHuronOutflow)[3:length(colnames(miHuronOutflow))]
}

miHuronDiversion = read.csv('data/ChicagoMonthlyMeanFlows.csv', TRUE, skip=6);
miHuronDiversion[is.na(miHuronDiversion[,3]),3] = 91;
miHuronDiversion_Prior = miHuronDiversion;
miHuronDiversion = miHuronDiversion[,c(1,2,(3:ncol(miHuronDiversion))*diversionInput)]
miHuronDiversionSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(miHuronDiversion) >= 3){
	colnames(miHuronDiversion) = c('Year', 'Month', colnames(miHuronDiversion)[3:length(colnames(miHuronDiversion))])
	miHuronDiversionSrc = colnames(miHuronDiversion)[3:length(colnames(miHuronDiversion))]
}

miHuronPrecip = read.csv('data/MHG_lake_Prec.csv', TRUE, na.strings='-9999.9', skip=4);
miHuronPrecip[miHuronPrecip < 0] = NA;
miHuronPrecip_Prior = miHuronPrecip
miHuronPrecip = miHuronPrecip[,c(1,2,(3:ncol(miHuronPrecip))*precipInput)]
miHuronPrecipSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(miHuronPrecip) >= 3){
	colnames(miHuronPrecip) = c('Year', 'Month', colnames(miHuronPrecip)[3:length(colnames(miHuronPrecip))])
	miHuronPrecipSrc = colnames(miHuronPrecip)[3:length(colnames(miHuronPrecip))]
}

miHuronEvap = read.csv('data/MHG_lake_Evap.csv', TRUE, na.strings='-9999.9', skip=4);
miHuronEvap[miHuronEvap < -5000] = NA
miHuronEvap_Prior = miHuronEvap
miHuronEvap = miHuronEvap[,c(1,2,(3:ncol(miHuronEvap))*evapInput)]
miHuronEvapSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(miHuronEvap) >= 3){
	colnames(miHuronEvap) = c('Year', 'Month', colnames(miHuronEvap)[3:length(colnames(miHuronEvap))])
	miHuronEvapSrc = colnames(miHuronEvap)[3:length(colnames(miHuronEvap))]
}

miHuronRunoff = read.csv('data/MHG_lake_Runoff.csv', TRUE, na.strings='None', skip=4);
miHuronRunoff[miHuronRunoff < 0] = NA
miHuronRunoff_Prior = miHuronRunoff
miHuronRunoff = miHuronRunoff[,c(1,2,(3:ncol(miHuronRunoff))*runoffInput)]
miHuronRunoffSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(miHuronRunoff) >= 3){
	colnames(miHuronRunoff) = c('Year', 'Month', colnames(miHuronRunoff)[3:length(colnames(miHuronRunoff))])
	miHuronRunoffSrc = colnames(miHuronRunoff)[3:length(colnames(miHuronRunoff))]
}

miHuronNBS = read.csv('data/MHG_lake_NBS.csv', TRUE, na.strings='-9999.9', skip=4);
miHuronNBS[miHuronNBS < -5000] = NA
miHuronNBS_Prior = miHuronNBS;
miHuronNBS = miHuronNBS[,c(1,2,(3:ncol(miHuronNBS))*nbsInput)]
miHuronNBSSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(miHuronNBS) >= 3){
	colnames(miHuronNBS) = c('Year', 'Month', colnames(miHuronNBS)[3:length(colnames(miHuronNBS))])
	miHuronNBSSrc = colnames(miHuronNBS)[3:length(colnames(miHuronNBS))]
}
### ST. CLAIR

clairBOM = read.csv('data/STC_BOM_MM.csv', TRUE, skip=6);
clairDS = cbind(
	clairBOM[-(nrow(clairBOM)),1:2],
	(clairBOM[-1,3] - clairBOM[-(nrow(clairBOM)),3])*1000
);

if(clairCMS & !clairComponentWBM){
	clairDS_CMS = NULL;
	for(i in 1:nrow(clairDS)){
		if(clairDS[i,1] %% 4 == 0){
			clairDS_CMS = c(
				clairDS_CMS,
				clairDS[i,3]/secondsInADay/daysInMonthsWithLeap[clairDS[i,2]]*stcArea/1000
			);
		}
		else{
			clairDS_CMS = c(
				clairDS_CMS,
				clairDS[i,3]/secondsInADay/daysInMonths[clairDS[i,2]]*stcArea/1000
			);
		}
	}

	clairDS[,3] = clairDS_CMS;
}

clairNBS = read.csv('data/STC_lake_NBS.csv', TRUE, na.strings='-9999.9', skip=4);
clairNBS[clairNBS < -5000] = NA

if(clairCMS & !clairComponentWBM){
	clairNBS_CMS = NULL;

	for(i in 1:nrow(clairNBS)){
		if(clairNBS[i,1] %% 4 == 0){
			clairNBS_CMS = rbind(
				clairNBS_CMS,
				clairNBS[i,3:ncol(clairNBS)]/secondsInADay/daysInMonthsWithLeap[clairNBS[i,2]]*stcArea/1000
			);
		}
		else{
			clairNBS_CMS = rbind(
				clairNBS_CMS,
				clairNBS[i,3:ncol(clairNBS)]/secondsInADay/daysInMonths[clairNBS[i,2]]*stcArea/1000
			);
		}
	}

	clairNBS[,3:ncol(clairNBS)] = clairNBS_CMS;
}

clairNBS_Prior = clairNBS;
clairNBS = clairNBS[,c(1,2,(3:ncol(clairNBS))*nbsInput)]

clairNBSSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(clairNBS) >= 3){
	colnames(clairNBS) = c('Year', 'Month', colnames(clairNBS)[3:length(colnames(clairNBS))])
	clairNBSSrc = colnames(clairNBS)[3:length(colnames(clairNBS))]
}

clairOutflow = read.csv('data/DetroitMonthlyMeanFlows.csv', TRUE, skip=8);
clairOutflow_Prior = clairOutflow;
clairOutflow = clairOutflow[,c(1,2,(3:ncol(clairOutflow))*outflowInput)]
clairOutflowSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(clairOutflow) >= 3){
	colnames(clairOutflow) = c('Year', 'Month', colnames(clairOutflow)[3:length(colnames(clairOutflow))])
	clairOutflowSrc = colnames(clairOutflow)[3:length(colnames(clairOutflow))]
}

clairPrecip = read.csv('data/STC_lake_Prec.csv', TRUE, na.strings='-9999.9', skip=4);
clairPrecip[clairPrecip < 0] = NA;
clairPrecip_Prior = clairPrecip
clairPrecip = clairPrecip[,c(1,2,(3:ncol(clairPrecip))*precipInput)]
clairPrecipSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(clairPrecip) >= 3){
	colnames(clairPrecip) = c('Year', 'Month', colnames(clairPrecip)[3:length(colnames(clairPrecip))])
	clairPrecipSrc = colnames(clairPrecip)[3:length(colnames(clairPrecip))]
}

clairEvap = read.csv('data/STC_lake_Evap.csv', TRUE, na.strings='-9999.9', skip=4);
clairEvap[clairEvap < -5000] = NA
clairEvap_Prior = clairEvap
clairEvap = clairEvap[,c(1,2,(3:ncol(clairEvap))*evapInput)]
clairEvapSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(clairEvap) >= 3){
	colnames(clairEvap) = c('Year', 'Month', colnames(clairEvap)[3:length(colnames(clairEvap))])
	clairEvapSrc = colnames(clairEvap)[3:length(colnames(clairEvap))]
}

clairRunoff = read.csv('data/STC_lake_Runoff.csv', TRUE, na.strings='None', skip=4);
clairRunoff[clairRunoff < 0] = NA
clairRunoff_Prior = clairRunoff
clairRunoff = clairRunoff[,c(1,2,(3:ncol(clairRunoff))*runoffInput)]
clairRunoffSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(clairRunoff) >= 3){
	colnames(clairRunoff) = c('Year', 'Month', colnames(clairRunoff)[3:length(colnames(clairRunoff))])
	clairRunoffSrc = colnames(clairRunoff)[3:length(colnames(clairRunoff))]
}

### ERIE

erieBOM = read.csv('data/ERI_BOM_MM.csv', TRUE, skip=6);
erieDS = cbind(
	erieBOM[-(nrow(erieBOM)),1:2],
	(erieBOM[-1,3] - erieBOM[-(nrow(erieBOM)),3])*1000
);

erieOutflow = read.csv('data/NiagaraWellandMonthlyMeanFlows.csv', TRUE, skip=7);
erieOutflow_Prior = erieOutflow;
erieOutflow = erieOutflow[,c(1,2,(3:ncol(erieOutflow))*outflowInput[2:length(outflowInput)])]
erieOutflowSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(erieOutflow) >= 3){
	colnames(erieOutflow) = c('Year', 'Month', colnames(erieOutflow)[3:length(colnames(erieOutflow))])
	erieOutflowSrc = colnames(erieOutflow)[3:length(colnames(erieOutflow))]
}

erieDiversion = read.csv('data/WellandMonthlyMeanFlows.csv', TRUE, skip=6);
erieDiversion_Prior = erieDiversion;
erieDiversion = erieDiversion[,c(1,2,(3:ncol(erieDiversion))*diversionInput)]
erieDiversionSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(erieDiversion) >= 3){
	colnames(erieDiversion) = c('Year', 'Month', colnames(erieDiversion)[3:length(colnames(erieDiversion))])
	erieDiversionSrc = colnames(erieDiversion)[3:length(colnames(erieDiversion))]
}
#erieOutflow[,3] = erieOutflow[,3]-erieDiversion[,3]

eriePrecip = read.csv('data/ERI_lake_Prec.csv', TRUE, na.strings='-9999.9', skip=4);
eriePrecip[eriePrecip < 0] = NA;
eriePrecip_Prior = eriePrecip
eriePrecip = eriePrecip[,c(1,2,(3:ncol(eriePrecip))*precipInput)]
eriePrecipSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(eriePrecip) >= 3){
	colnames(eriePrecip) = c('Year', 'Month', colnames(eriePrecip)[3:length(colnames(eriePrecip))])
	eriePrecipSrc = colnames(eriePrecip)[3:length(colnames(eriePrecip))]
}

erieEvap = read.csv('data/ERI_lake_Evap.csv', TRUE, na.strings='-9999.9', skip=4);
erieEvap[erieEvap < -5000] = NA
erieEvap_Prior = erieEvap
erieEvap = erieEvap[,c(1,2,(3:ncol(erieEvap))*evapInput)]
erieEvapSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(erieEvap) >= 3){
	colnames(erieEvap) = c('Year', 'Month', colnames(erieEvap)[3:length(colnames(erieEvap))])
	erieEvapSrc = colnames(erieEvap)[3:length(colnames(erieEvap))]
}
	
erieRunoff = read.csv('data/ERI_lake_Runoff.csv', TRUE, na.strings='None', skip=4);
erieRunoff[erieRunoff < 0] = NA
erieRunoff_Prior = erieRunoff
erieRunoff = erieRunoff[,c(1,2,(3:ncol(erieRunoff))*runoffInput)]
erieRunoffSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(erieRunoff) >= 3){
	colnames(erieRunoff) = c('Year', 'Month', colnames(erieRunoff)[3:length(colnames(erieRunoff))])
	erieRunoffSrc = colnames(erieRunoff)[3:length(colnames(erieRunoff))]
}

erieNBS = read.csv('data/ERI_lake_NBS.csv', TRUE, na.strings='-9999.9', skip=4);
erieNBS[erieNBS < -5000] = NA
erieNBS_Prior = erieNBS;
erieNBS = erieNBS[,c(1,2,(3:ncol(erieNBS))*nbsInput)]
erieNBSSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(erieNBS) >= 3){
	colnames(erieNBS) = c('Year', 'Month', colnames(erieNBS)[3:length(colnames(erieNBS))])
	erieNBSSrc = colnames(erieNBS)[3:length(colnames(erieNBS))]
}

### ONTARIO

ontarioBOM = read.csv('data/ONT_BOM_MM.csv', TRUE, skip=6);
ontarioDS = cbind(
	ontarioBOM[-(nrow(ontarioBOM)),1:2],
	(ontarioBOM[-1,3] - ontarioBOM[-(nrow(ontarioBOM)),3])*1000
);

ontarioOutflow = read.csv('data/StLawrenceMonthlyMeanFlows.csv', TRUE, skip=7);
ontarioOutflow_Prior = ontarioOutflow;
ontarioOutflow = ontarioOutflow[,c(1,2,(3:ncol(ontarioOutflow))*outflowInput[2:length(outflowInput)])]
ontarioOutflowSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(ontarioOutflow) >= 3){
	colnames(ontarioOutflow) = c('Year', 'Month', colnames(ontarioOutflow)[3:length(colnames(ontarioOutflow))])
	ontarioOutflowSrc = colnames(ontarioOutflow)[3:length(colnames(ontarioOutflow))]
}

ontarioPrecip = read.csv('data/ONT_lake_Prec.csv', TRUE, na.strings='-9999.9', skip=4);
ontarioPrecip[ontarioPrecip < 0] = NA;
ontarioPrecip_Prior = ontarioPrecip
ontarioPrecip = ontarioPrecip[,c(1,2,(3:ncol(ontarioPrecip))*precipInput)]
ontarioPrecipSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(ontarioPrecip) >= 3){
	colnames(ontarioPrecip) = c('Year', 'Month', colnames(ontarioPrecip)[3:length(colnames(ontarioPrecip))])
	ontarioPrecipSrc = colnames(ontarioPrecip)[3:length(colnames(ontarioPrecip))]
}

ontarioEvap = read.csv('data/ONT_lake_Evap.csv', TRUE, na.strings='-9999.9', skip=4);
ontarioEvap[ontarioEvap < -5000] = NA
ontarioEvap_Prior = ontarioEvap
ontarioEvap = ontarioEvap[,c(1,2,(3:ncol(ontarioEvap))*evapInput)]
ontarioEvapSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(ontarioEvap) >= 3){
	colnames(ontarioEvap) = c('Year', 'Month', colnames(ontarioEvap)[3:length(colnames(ontarioEvap))])
	ontarioEvapSrc = colnames(ontarioEvap)[3:length(colnames(ontarioEvap))]
}

ontarioRunoff = read.csv('data/ONT_lake_Runoff.csv', TRUE, na.strings='None', skip=4);
ontarioRunoff[ontarioRunoff < 0] = NA
ontarioRunoff_Prior = ontarioRunoff
ontarioRunoff = ontarioRunoff[,c(1,2,(3:ncol(ontarioRunoff))*runoffInput)]
ontarioRunoffSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(ontarioRunoff) >= 3){
	colnames(ontarioRunoff) = c('Year', 'Month', colnames(ontarioRunoff)[3:length(colnames(ontarioRunoff))])
	ontarioRunoffSrc = colnames(ontarioRunoff)[3:length(colnames(ontarioRunoff))]
}

ontarioNBS = read.csv('data/ONT_lake_NBS.csv', TRUE, na.strings='-9999.9', skip=4);
ontarioNBS[ontarioNBS < -5000] = NA
ontarioNBS_Prior = ontarioNBS;
ontarioNBS = ontarioNBS[,c(1,2,(3:ncol(ontarioNBS))*nbsInput)]
ontarioNBSSrc = c("***MODEL RAN WITHOUT OBSERVATIONS***")
if(ncol(ontarioNBS) >= 3){
	colnames(ontarioNBS) = c('Year', 'Month', colnames(ontarioNBS)[3:length(colnames(ontarioNBS))])
	ontarioNBSSrc = colnames(ontarioNBS)[3:length(colnames(ontarioNBS))]
}
