### DATA GENERATOR FOR THE L2SWBM

write.table(jSum, paste(modelName,'_STATS.csv', sep=''), sep=',', quote=FALSE);

pTime = cbind(yearVector, m);

if(superiorComponentWBM){
superiorPrecip025 = jSum[paste('superiorPrecip[',1:nMonths,']', sep=''),3];
superiorPrecipMedian = jSum[paste('superiorPrecip[',1:nMonths,']', sep=''),5];
superiorPrecip975 = jSum[paste('superiorPrecip[',1:nMonths,']', sep=''),7];
superiorPrecip = cbind(pTime, superiorPrecipMedian, superiorPrecip025, superiorPrecip975);
write.table(format(superiorPrecip, digits=3), paste('superiorPrecip_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

superiorEvap025 = jSum[paste('superiorEvap[',1:nMonths,']', sep=''),3];
superiorEvapMedian = jSum[paste('superiorEvap[',1:nMonths,']', sep=''),5];
superiorEvap975 = jSum[paste('superiorEvap[',1:nMonths,']', sep=''),7];
superiorEvap = cbind(pTime, superiorEvapMedian, superiorEvap025, superiorEvap975);
write.table(format(superiorEvap, digits=5), paste('superiorEvap_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

superiorRunoff025 = jSum[paste('superiorRunoff[',1:nMonths,']', sep=''),3];
superiorRunoffMedian = jSum[paste('superiorRunoff[',1:nMonths,']', sep=''),5];
superiorRunoff975 = jSum[paste('superiorRunoff[',1:nMonths,']', sep=''),7];
superiorRunoff = cbind(pTime, superiorRunoffMedian, superiorRunoff025, superiorRunoff975);
write.table(format(superiorRunoff, digits=3), paste('superiorRunoff_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}

superiorOutflow025 = jSum[paste('superiorOutflow[',1:nMonths,']', sep=''),3];
superiorOutflowMedian = jSum[paste('superiorOutflow[',1:nMonths,']', sep=''),5];
superiorOutflow975 = jSum[paste('superiorOutflow[',1:nMonths,']', sep=''),7];
superiorOutflow = cbind(pTime, superiorOutflowMedian, superiorOutflow025, superiorOutflow975);
write.table(format(superiorOutflow, digits=3), paste('superiorOutflow_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

superiorDiversion025 = jSum[paste('superiorDiversion[',1:nMonths,']', sep=''),3];
superiorDiversionMedian = jSum[paste('superiorDiversion[',1:nMonths,']', sep=''),5];
superiorDiversion975 = jSum[paste('superiorDiversion[',1:nMonths,']', sep=''),7];
superiorDiversion = cbind(pTime, superiorDiversionMedian, superiorDiversion025, superiorDiversion975);
write.table(format(superiorDiversion, digits=3), paste('superiorDiversion_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

superiorDStore025 = jSum[paste('superiorDStore[',1:nMonths,']', sep=''),3];
superiorDStoreMedian = jSum[paste('superiorDStore[',1:nMonths,']', sep=''),5];
superiorDStore975 = jSum[paste('superiorDStore[',1:nMonths,']', sep=''),7];
superiorDStore = cbind(pTime, superiorDStoreMedian, superiorDStore025, superiorDStore975);
write.table(format(superiorDStore, digits=3), paste('superiorDStore_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

if(miHuronComponentWBM){
miHuronPrecip025 = jSum[paste('miHuronPrecip[',1:nMonths,']', sep=''),3];
miHuronPrecipMedian = jSum[paste('miHuronPrecip[',1:nMonths,']', sep=''),5];
miHuronPrecip975 = jSum[paste('miHuronPrecip[',1:nMonths,']', sep=''),7];
miHuronPrecip = cbind(pTime, miHuronPrecipMedian, miHuronPrecip025, miHuronPrecip975);
write.table(format(miHuronPrecip, digits=3), paste('miHuronPrecip_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

miHuronEvap025 = jSum[paste('miHuronEvap[',1:nMonths,']', sep=''),3];
miHuronEvapMedian = jSum[paste('miHuronEvap[',1:nMonths,']', sep=''),5];
miHuronEvap975 = jSum[paste('miHuronEvap[',1:nMonths,']', sep=''),7];
miHuronEvap = cbind(pTime, miHuronEvapMedian, miHuronEvap025, miHuronEvap975);
write.table(format(miHuronEvap, digits=5), paste('miHuronEvap_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

miHuronRunoff025 = jSum[paste('miHuronRunoff[',1:nMonths,']', sep=''),3];
miHuronRunoffMedian = jSum[paste('miHuronRunoff[',1:nMonths,']', sep=''),5];
miHuronRunoff975 = jSum[paste('miHuronRunoff[',1:nMonths,']', sep=''),7];
miHuronRunoff = cbind(pTime, miHuronRunoffMedian, miHuronRunoff025, miHuronRunoff975);
write.table(format(miHuronRunoff, digits=3), paste('miHuronRunoff_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}

miHuronOutflow025 = jSum[paste('miHuronOutflow[',1:nMonths,']', sep=''),3];
miHuronOutflowMedian = jSum[paste('miHuronOutflow[',1:nMonths,']', sep=''),5];
miHuronOutflow975 = jSum[paste('miHuronOutflow[',1:nMonths,']', sep=''),7];
miHuronOutflow = cbind(pTime, miHuronOutflowMedian, miHuronOutflow025, miHuronOutflow975);
write.table(format(miHuronOutflow, digits=3), paste('miHuronOutflow_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

miHuronDiversion025 = jSum[paste('miHuronDiversion[',1:nMonths,']', sep=''),3];
miHuronDiversionMedian = jSum[paste('miHuronDiversion[',1:nMonths,']', sep=''),5];
miHuronDiversion975 = jSum[paste('miHuronDiversion[',1:nMonths,']', sep=''),7];
miHuronDiversion = cbind(pTime, miHuronDiversionMedian, miHuronDiversion025, miHuronDiversion975);
write.table(format(miHuronDiversion, digits=3), paste('miHuronDiversion_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));


miHuronDStore025 = jSum[paste('miHuronDStore[',1:nMonths,']', sep=''),3];
miHuronDStoreMedian = jSum[paste('miHuronDStore[',1:nMonths,']', sep=''),5];
miHuronDStore975 = jSum[paste('miHuronDStore[',1:nMonths,']', sep=''),7];
miHuronDStore = cbind(pTime, miHuronDStoreMedian, miHuronDStore025, miHuronDStore975);
write.table(format(miHuronDStore, digits=3), paste('miHuronDStore_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

if(erieComponentWBM){
eriePrecip025 = jSum[paste('eriePrecip[',1:nMonths,']', sep=''),3];
eriePrecipMedian = jSum[paste('eriePrecip[',1:nMonths,']', sep=''),5];
eriePrecip975 = jSum[paste('eriePrecip[',1:nMonths,']', sep=''),7];
eriePrecip = cbind(pTime, eriePrecipMedian, eriePrecip025, eriePrecip975);
write.table(format(eriePrecip, digits=3), paste('eriePrecip_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

erieEvap025 = jSum[paste('erieEvap[',1:nMonths,']', sep=''),3];
erieEvapMedian = jSum[paste('erieEvap[',1:nMonths,']', sep=''),5];
erieEvap975 = jSum[paste('erieEvap[',1:nMonths,']', sep=''),7];
erieEvap = cbind(pTime, erieEvapMedian, erieEvap025, erieEvap975);
write.table(format(erieEvap, digits=5), paste('erieEvap_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

erieRunoff025 = jSum[paste('erieRunoff[',1:nMonths,']', sep=''),3];
erieRunoffMedian = jSum[paste('erieRunoff[',1:nMonths,']', sep=''),5];
erieRunoff975 = jSum[paste('erieRunoff[',1:nMonths,']', sep=''),7];
erieRunoff = cbind(pTime, erieRunoffMedian, erieRunoff025, erieRunoff975);
write.table(format(erieRunoff, digits=3), paste('erieRunoff_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}

erieOutflow025 = jSum[paste('erieOutflow[',1:nMonths,']', sep=''),3];
erieOutflowMedian = jSum[paste('erieOutflow[',1:nMonths,']', sep=''),5];
erieOutflow975 = jSum[paste('erieOutflow[',1:nMonths,']', sep=''),7];
erieOutflow = cbind(pTime, erieOutflowMedian, erieOutflow025, erieOutflow975);
write.table(format(erieOutflow, digits=3), paste('erieOutflow_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

erieDiversion025 = jSum[paste('erieDiversion[',1:nMonths,']', sep=''),3];
erieDiversionMedian = jSum[paste('erieDiversion[',1:nMonths,']', sep=''),5];
erieDiversion975 = jSum[paste('erieDiversion[',1:nMonths,']', sep=''),7];
erieDiversion = cbind(pTime, erieDiversionMedian, erieDiversion025, erieDiversion975);
write.table(format(erieDiversion, digits=3), paste('erieDiversion_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

erieDStore025 = jSum[paste('erieDStore[',1:nMonths,']', sep=''),3];
erieDStoreMedian = jSum[paste('erieDStore[',1:nMonths,']', sep=''),5];
erieDStore975 = jSum[paste('erieDStore[',1:nMonths,']', sep=''),7];
erieDStore = cbind(pTime, erieDStoreMedian, erieDStore025, erieDStore975);
write.table(format(erieDStore, digits=3), paste('erieDStore_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

if(ontarioComponentWBM){
ontarioPrecip025 = jSum[paste('ontarioPrecip[',1:nMonths,']', sep=''),3];
ontarioPrecipMedian = jSum[paste('ontarioPrecip[',1:nMonths,']', sep=''),5];
ontarioPrecip975 = jSum[paste('ontarioPrecip[',1:nMonths,']', sep=''),7];
ontarioPrecip = cbind(pTime, ontarioPrecipMedian, ontarioPrecip025, ontarioPrecip975);
write.table(format(ontarioPrecip, digits=3), paste('ontarioPrecip_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

ontarioEvap025 = jSum[paste('ontarioEvap[',1:nMonths,']', sep=''),3];
ontarioEvapMedian = jSum[paste('ontarioEvap[',1:nMonths,']', sep=''),5];
ontarioEvap975 = jSum[paste('ontarioEvap[',1:nMonths,']', sep=''),7];
ontarioEvap = cbind(pTime, ontarioEvapMedian, ontarioEvap025, ontarioEvap975);
write.table(format(ontarioEvap, digits=5), paste('ontarioEvap_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

ontarioRunoff025 = jSum[paste('ontarioRunoff[',1:nMonths,']', sep=''),3];
ontarioRunoffMedian = jSum[paste('ontarioRunoff[',1:nMonths,']', sep=''),5];
ontarioRunoff975 = jSum[paste('ontarioRunoff[',1:nMonths,']', sep=''),7];
ontarioRunoff = cbind(pTime, ontarioRunoffMedian, ontarioRunoff025, ontarioRunoff975);
write.table(format(ontarioRunoff, digits=3), paste('ontarioRunoff_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}

ontarioOutflow025 = jSum[paste('ontarioOutflow[',1:nMonths,']', sep=''),3];
ontarioOutflowMedian = jSum[paste('ontarioOutflow[',1:nMonths,']', sep=''),5];
ontarioOutflow975 = jSum[paste('ontarioOutflow[',1:nMonths,']', sep=''),7];
ontarioOutflow = cbind(pTime, ontarioOutflowMedian, ontarioOutflow025, ontarioOutflow975);
write.table(format(ontarioOutflow, digits=3), paste('ontarioOutflow_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

ontarioDStore025 = jSum[paste('ontarioDStore[',1:nMonths,']', sep=''),3];
ontarioDStoreMedian = jSum[paste('ontarioDStore[',1:nMonths,']', sep=''),5];
ontarioDStore975 = jSum[paste('ontarioDStore[',1:nMonths,']', sep=''),7];
ontarioDStore = cbind(pTime, ontarioDStoreMedian, ontarioDStore025, ontarioDStore975);
write.table(format(ontarioDStore, digits=3), paste('ontarioDStore_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

if(clairComponentWBM){
clairPrecip025 = jSum[paste('clairPrecip[',1:nMonths,']', sep=''),3];
clairPrecipMedian = jSum[paste('clairPrecip[',1:nMonths,']', sep=''),5];
clairPrecip975 = jSum[paste('clairPrecip[',1:nMonths,']', sep=''),7];
clairPrecip = cbind(pTime, clairPrecipMedian, clairPrecip025, clairPrecip975);
write.table(format(clairPrecip, digits=3), paste('clairPrecip_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

clairEvap025 = jSum[paste('clairEvap[',1:nMonths,']', sep=''),3];
clairEvapMedian = jSum[paste('clairEvap[',1:nMonths,']', sep=''),5];
clairEvap975 = jSum[paste('clairEvap[',1:nMonths,']', sep=''),7];
clairEvap = cbind(pTime, clairEvapMedian, clairEvap025, clairEvap975);
write.table(format(clairEvap, digits=5), paste('clairEvap_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

clairRunoff025 = jSum[paste('clairRunoff[',1:nMonths,']', sep=''),3];
clairRunoffMedian = jSum[paste('clairRunoff[',1:nMonths,']', sep=''),5];
clairRunoff975 = jSum[paste('clairRunoff[',1:nMonths,']', sep=''),7];
clairRunoff = cbind(pTime, clairRunoffMedian, clairRunoff025, clairRunoff975);
write.table(format(clairRunoff, digits=3), paste('clairRunoff_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}

clairOutflow025 = jSum[paste('clairOutflow[',1:nMonths,']', sep=''),3];
clairOutflowMedian = jSum[paste('clairOutflow[',1:nMonths,']', sep=''),5];
clairOutflow975 = jSum[paste('clairOutflow[',1:nMonths,']', sep=''),7];
clairOutflow = cbind(pTime, clairOutflowMedian, clairOutflow025, clairOutflow975);
write.table(format(clairOutflow, digits=3), paste('clairOutflow_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));

clairDStore025 = jSum[paste('clairDStore[',1:nMonths,']', sep=''),3];
clairDStoreMedian = jSum[paste('clairDStore[',1:nMonths,']', sep=''),5];
clairDStore975 = jSum[paste('clairDStore[',1:nMonths,']', sep=''),7];
clairDStore = cbind(pTime, clairDStoreMedian, clairDStore025, clairDStore975);
write.table(format(clairDStore, digits=3), paste('clairDStore_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));


### NBS ESTIMATES
if(superiorComponentWBM){
superiorPrecip = as.matrix(jSample[,paste('superiorPrecip[',1:nMonths,']',sep='')])
superiorEvap = as.matrix(jSample[,paste('superiorEvap[',1:nMonths,']',sep='')])
superiorRunoff = as.matrix(jSample[,paste('superiorRunoff[',1:nMonths,']',sep='')])
superiorNBSC = superiorPrecip - superiorEvap + superiorRunoff;
superiorNBSCMedian = as.vector(apply(as.matrix(superiorNBSC), 2, median))
superiorNBSC025 = as.vector(apply(as.matrix(superiorNBSC), 2, quantile, probs=c(0.025)))
superiorNBSC975 = as.vector(apply(as.matrix(superiorNBSC), 2, quantile, probs=c(0.975)))
superiorNBSCTab = cbind(pTime, superiorNBSCMedian, superiorNBSC025, superiorNBSC975);
write.table(format(superiorNBSCTab, digits=3), paste('superiorNBSC_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}else{
superiorNBS025 = jSum[paste('superiorNBS[',1:nMonths,']', sep=''),3];
superiorNBSMedian = jSum[paste('superiorNBS[',1:nMonths,']', sep=''),5];
superiorNBS975 = jSum[paste('superiorNBS[',1:nMonths,']', sep=''),7];
superiorNBS = cbind(pTime, superiorNBSMedian, superiorNBS025, superiorNBS975);
write.table(format(superiorNBS, digits=3), paste('superiorNBS_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}

if(miHuronComponentWBM){
miHuronPrecip = as.matrix(jSample[,paste('miHuronPrecip[',1:nMonths,']',sep='')])
miHuronEvap = as.matrix(jSample[,paste('miHuronEvap[',1:nMonths,']',sep='')])
miHuronRunoff = as.matrix(jSample[,paste('miHuronRunoff[',1:nMonths,']',sep='')])
miHuronNBSC = miHuronPrecip - miHuronEvap + miHuronRunoff;
miHuronNBSCMedian = as.vector(apply(as.matrix(miHuronNBSC), 2, median))
miHuronNBSC025 = as.vector(apply(as.matrix(miHuronNBSC), 2, quantile, probs=c(0.025)))
miHuronNBSC975 = as.vector(apply(as.matrix(miHuronNBSC), 2, quantile, probs=c(0.975)))
miHuronNBSCTab = cbind(pTime, miHuronNBSCMedian, miHuronNBSC025, miHuronNBSC975);
write.table(format(miHuronNBSCTab, digits=3), paste('miHuronNBSC_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}else{
miHuronNBS025 = jSum[paste('miHuronNBS[',1:nMonths,']', sep=''),3];
miHuronNBSMedian = jSum[paste('miHuronNBS[',1:nMonths,']', sep=''),5];
miHuronNBS975 = jSum[paste('miHuronNBS[',1:nMonths,']', sep=''),7];
miHuronNBS = cbind(pTime, miHuronNBSMedian, miHuronNBS025, miHuronNBS975);
write.table(format(miHuronNBS, digits=3), paste('miHuronNBS_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}

if(clairComponentWBM){
clairPrecip = as.matrix(jSample[,paste('clairPrecip[',1:nMonths,']',sep='')])
clairEvap = as.matrix(jSample[,paste('clairEvap[',1:nMonths,']',sep='')])
clairRunoff = as.matrix(jSample[,paste('clairRunoff[',1:nMonths,']',sep='')])
clairNBSC = clairPrecip - clairEvap + clairRunoff;
clairNBSCMedian = as.vector(apply(as.matrix(clairNBSC), 2, median))
clairNBSC025 = as.vector(apply(as.matrix(clairNBSC), 2, quantile, probs=c(0.025)))
clairNBSC975 = as.vector(apply(as.matrix(clairNBSC), 2, quantile, probs=c(0.975)))
clairNBSCTab = cbind(pTime, clairNBSCMedian, clairNBSC025, clairNBSC975);
write.table(format(clairNBSCTab, digits=3), paste('clairNBSC_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}else{
clairNBS025 = jSum[paste('clairNBS[',1:nMonths,']', sep=''),3];
clairNBSMedian = jSum[paste('clairNBS[',1:nMonths,']', sep=''),5];
clairNBS975 = jSum[paste('clairNBS[',1:nMonths,']', sep=''),7];
clairNBS = cbind(pTime, clairNBSMedian, clairNBS025, clairNBS975);
write.table(format(clairNBS, digits=3), paste('clairNBS_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}

if(erieComponentWBM){
eriePrecip = as.matrix(jSample[,paste('eriePrecip[',1:nMonths,']',sep='')])
erieEvap = as.matrix(jSample[,paste('erieEvap[',1:nMonths,']',sep='')])
erieRunoff = as.matrix(jSample[,paste('erieRunoff[',1:nMonths,']',sep='')])
erieNBSC = eriePrecip - erieEvap + erieRunoff;
erieNBSCMedian = as.vector(apply(as.matrix(erieNBSC), 2, median))
erieNBSC025 = as.vector(apply(as.matrix(erieNBSC), 2, quantile, probs=c(0.025)))
erieNBSC975 = as.vector(apply(as.matrix(erieNBSC), 2, quantile, probs=c(0.975)))
erieNBSCTab = cbind(pTime, erieNBSCMedian, erieNBSC025, erieNBSC975);
write.table(format(erieNBSCTab, digits=3), paste('erieNBSC_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}else{
erieNBS025 = jSum[paste('erieNBS[',1:nMonths,']', sep=''),3];
erieNBSMedian = jSum[paste('erieNBS[',1:nMonths,']', sep=''),5];
erieNBS975 = jSum[paste('erieNBS[',1:nMonths,']', sep=''),7];
erieNBS = cbind(pTime, erieNBSMedian, erieNBS025, erieNBS975);
write.table(format(erieNBS, digits=3), paste('erieNBS_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}

if(ontarioComponentWBM){
ontarioPrecip = as.matrix(jSample[,paste('ontarioPrecip[',1:nMonths,']',sep='')])
ontarioEvap = as.matrix(jSample[,paste('ontarioEvap[',1:nMonths,']',sep='')])
ontarioRunoff = as.matrix(jSample[,paste('ontarioRunoff[',1:nMonths,']',sep='')])
ontarioNBSC = ontarioPrecip - ontarioEvap + ontarioRunoff;
ontarioNBSCMedian = as.vector(apply(as.matrix(ontarioNBSC), 2, median))
ontarioNBSC025 = as.vector(apply(as.matrix(ontarioNBSC), 2, quantile, probs=c(0.025)))
ontarioNBSC975 = as.vector(apply(as.matrix(ontarioNBSC), 2, quantile, probs=c(0.975)))
ontarioNBSCTab = cbind(pTime, ontarioNBSCMedian, ontarioNBSC025, ontarioNBSC975);
write.table(format(ontarioNBSCTab, digits=3), paste('ontarioNBSC_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}else{
ontarioNBS025 = jSum[paste('ontarioNBS[',1:nMonths,']', sep=''),3];
ontarioNBSMedian = jSum[paste('ontarioNBS[',1:nMonths,']', sep=''),5];
ontarioNBS975 = jSum[paste('ontarioNBS[',1:nMonths,']', sep=''),7];
ontarioNBS = cbind(pTime, ontarioNBSMedian, ontarioNBS025, ontarioNBS975);
write.table(format(ontarioNBS, digits=3), paste('ontarioNBS_',modelName,'.csv',sep=''), append=FALSE, quote=FALSE, sep=',', row.names=FALSE, col.names=c('Year','Month','Median','2.5 Percentile', '97.5 Percentile'));
}
