### PRECIP
#x11();
pdf('precipPriorCompare.pdf', height=10, width=7.5)
layout(matrix(1:60,12,5,TRUE)) #BY ROW
par(mar = c(0,0,0,0))
par(oma = c(4,4,5,4))

for(i in 1:12){
	hist(superiorPrecip_Prior[superiorPrecip_Prior[,2]==i,priorPrecipColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.1), xlim=c(0,250), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 0:250, y = dgamma(0:250, shape=superiorPriorPrecipShape[i], rate = superiorPriorPrecipRate[i]), col='red', lty='11', lwd=3)
	if(i %% 2 == 1){
		axis(2, labels=TRUE);
	}
	else{
		axis(2, labels=FALSE);
	}
	mtext(month.abb[i], 2, line=2.5);
	if(i == 1){
		mtext('SUP', 3, line=1);
	}
	if(i == 12){
		axis(1, labels=FALSE)
		axis(1, at=seq(0,200,50), labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(miHuronPrecip_Prior[miHuronPrecip_Prior[,2]==i,priorPrecipColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.1), xlim=c(0,250), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 0:250, y = dgamma(0:250, shape=miHuronPriorPrecipShape[i], rate = miHuronPriorPrecipRate[i]), col='red', lty='11', lwd=3)
	if(i == 1){
		mtext('MHU', 3, line=1);
	}
	axis(2, labels=FALSE);
	if(i == 12){
		axis(1, labels=FALSE)
		axis(1, at=seq(0,200,50), labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(clairPrecip_Prior[clairPrecip_Prior[,2]==i,priorPrecipColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.1), xlim=c(0,250), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 0:250, y = dgamma(0:250, shape=clairPriorPrecipShape[i], rate = clairPriorPrecipRate[i]), col='red', lty='11', lwd=3)
	if(i == 1){
		mtext('STC', 3, line=1);
	}
	axis(2, labels=FALSE);
	if(i == 12){
		axis(1, labels=FALSE)
		axis(1, at=seq(0,200,50), labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(eriePrecip_Prior[eriePrecip_Prior[,2]==i,priorPrecipColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.1), xlim=c(0,250), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 0:250, y = dgamma(0:250, shape=eriePriorPrecipShape[i], rate = eriePriorPrecipRate[i]), col='red', lty='11', lwd=3)
	if(i == 1){
		mtext('ERI', 3, line=1);
	}
	if(i == 12){
		axis(1, labels=FALSE)
		axis(1, at=seq(0,200,50), labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	axis(2, labels=FALSE);
	hist(ontarioPrecip_Prior[ontarioPrecip_Prior[,2]==i,priorPrecipColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.1), xlim=c(0,250), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 0:250, y = dgamma(0:250, shape=ontarioPriorPrecipShape[i], rate = ontarioPriorPrecipRate[i]), col='red', lty='11', lwd=3)
	if(i == 1){
		mtext('ONT', 3, line=1);
	}
	if(i %% 2 == 1){
		axis(4, labels=FALSE);
	}
	else{
		axis(4, labels=TRUE);
	}
	if(i == 12){
		axis(1, labels=FALSE)
		axis(1, at=seq(0,200,50), labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
}

title(main="Precipitation Priors (mm)", outer=TRUE, line = 3)

dev.off()

# -10:260
# 0, 0.15

### EVAP
pdf('evapPriorCompare.pdf', height=10, width=7.5)
layout(matrix(1:60,12,5,TRUE)) #BY ROW
par(mar = c(0,0,0,0))
par(oma = c(4,4,5,4))

for(i in 1:12){
	hist(superiorEvap_Prior[superiorEvap_Prior[,2]==i,priorEvapColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.15), xlim=c(-50,260), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = -50:260, y = dnorm(-50:260, mean=superiorEvapPriorMean[i], sd = sqrt(1/superiorEvapPriorPrecision[i])), col='red', lty='11', lwd=3)
	if(i %% 2 == 1){
		axis(2, labels=TRUE);
	}
	else{
		axis(2, labels=FALSE);
	}
	mtext(month.abb[i], 2, line=2.5);
	if(i == 1){
		mtext('SUP', 3, line=1);
	}
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(miHuronEvap_Prior[miHuronEvap_Prior[,2]==i,priorEvapColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.15), xlim=c(-50,260), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = -50:260, y = dnorm(-50:260, mean=miHuronEvapPriorMean[i], sd = sqrt(1/miHuronEvapPriorPrecision[i])), col='red', lty='11', lwd=3)	
	
	if(i == 1){
		mtext('MHU', 3, line=1);
	}
	axis(2, labels=FALSE);
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(clairEvap_Prior[clairEvap_Prior[,2]==i,priorEvapColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.15), xlim=c(-50,260), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = -50:260, y = dnorm(-50:260, mean=clairEvapPriorMean[i], sd = sqrt(1/clairEvapPriorPrecision[i])), col='red', lty='11', lwd=3)	
	
	if(i == 1){
		mtext('STC', 3, line=1);
	}
	axis(2, labels=FALSE);
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(erieEvap_Prior[erieEvap_Prior[,2]==i,priorEvapColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.15), xlim=c(-50,260), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = -50:260, y = dnorm(-50:260, mean=erieEvapPriorMean[i], sd = sqrt(1/erieEvapPriorPrecision[i])), col='red', lty='11', lwd=3)		
	if(i == 1){
		mtext('ERI', 3, line=1);
	}
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	axis(2, labels=FALSE);
	hist(ontarioEvap_Prior[ontarioEvap_Prior[,2]==i,priorEvapColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.15), xlim=c(-50,260), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = -50:260, y = dnorm(-50:260, mean=ontarioEvapPriorMean[i], sd = sqrt(1/ontarioEvapPriorPrecision[i])), col='red', lty='11', lwd=3)	
	if(i == 1){
		mtext('ONT', 3, line=1);
	}
	if(i %% 2 == 1){
		axis(4, labels=FALSE);
	}
	else{
		axis(4, labels=TRUE);
	}
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
}

title(main="Evaporation Priors (mm)", outer=TRUE, line = 3)

dev.off()

### RUNOFF
pdf('runoffPriorCompare.pdf', height=10, width=7.5)
layout(matrix(1:60,12,5,TRUE)) #BY ROW
par(mar = c(0,0,0,0))
par(oma = c(4,4,5,4))

for(i in 1:12){
	hist(superiorRunoff_Prior[superiorRunoff_Prior[,2]==i,priorRunoffColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.08), xlim=c(0,600), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 0:600, y = dlnorm(0:600, mean=superiorRunoffLogPriorMean[i], sd = sqrt(1/superiorRunoffLogPriorPrecision[i])), col='red', lty='11', lwd=3)
	if(i %% 2 == 1){
		axis(2, labels=TRUE);
	}
	else{
		axis(2, labels=FALSE);
	}
	mtext(month.abb[i], 2, line=2.5);
	if(i == 1){
		mtext('SUP', 3, line=1);
	}
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(miHuronRunoff_Prior[miHuronRunoff_Prior[,2]==i,priorRunoffColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.08), xlim=c(0,600), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 0:600, y = dlnorm(0:600, mean=miHuronRunoffLogPriorMean[i], sd = sqrt(1/miHuronRunoffLogPriorPrecision[i])), col='red', lty='11', lwd=3)	
	if(i == 1){
		mtext('MHU', 3, line=1);
	}
	axis(2, labels=FALSE);
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(clairRunoff_Prior[clairRunoff_Prior[,2]==i,priorRunoffColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.08), xlim=c(0,600), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 0:600, y = dlnorm(0:600, mean=clairRunoffLogPriorMean[i], sd = sqrt(1/clairRunoffLogPriorPrecision[i])), col='red', lty='11', lwd=3)	
	if(i == 1){
		mtext('STC', 3, line=1);
	}
	axis(2, labels=FALSE);
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(erieRunoff_Prior[erieRunoff_Prior[,2]==i,priorRunoffColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.08), xlim=c(0,600), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 0:600, y = dlnorm(0:600, mean=erieRunoffLogPriorMean[i], sd = sqrt(1/erieRunoffLogPriorPrecision[i])), col='red', lty='11', lwd=3)	
	if(i == 1){
		mtext('ERI', 3, line=1);
	}
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	axis(2, labels=FALSE);
	hist(ontarioRunoff_Prior[ontarioRunoff_Prior[,2]==i,priorRunoffColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.08), xlim=c(0,600), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 0:600, y = dlnorm(0:600, mean=ontarioRunoffLogPriorMean[i], sd = sqrt(1/ontarioRunoffLogPriorPrecision[i])), col='red', lty='11', lwd=3)
	if(i == 1){
		mtext('ONT', 3, line=1);
	}
	if(i %% 2 == 1){
		axis(4, labels=FALSE);
	}
	else{
		axis(4, labels=TRUE);
	}
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
}

title(main="Runoff Priors (mm)", outer=TRUE, line = 3)

dev.off()

### NBS
pdf('nbsPriorCompare.pdf', height=10, width=7.5)
layout(matrix(1:60,12,5,TRUE)) #BY ROW
par(mar = c(0,0,0,0))
par(oma = c(4,4,5,4))

for(i in 1:12){
	hist(superiorNBS_Prior[superiorNBS_Prior[,2]==i,priorNBSColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.15), xlim=c(-50,260), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = -50:260, y = dnorm(-50:260, mean=superiorNBSPriorMean[i], sd = sqrt(1/superiorNBSPriorPrecision[i])), col='red', lty='11', lwd=3)
	if(i %% 2 == 1){
		axis(2, labels=TRUE);
	}
	else{
		axis(2, labels=FALSE);
	}
	mtext(month.abb[i], 2, line=2.5);
	if(i == 1){
		mtext('SUP', 3, line=1);
	}
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(miHuronNBS_Prior[miHuronNBS_Prior[,2]==i,priorNBSColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.15), xlim=c(-50,260), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = -50:260, y = dnorm(-50:260, mean=miHuronNBSPriorMean[i], sd = sqrt(1/miHuronNBSPriorPrecision[i])), col='red', lty='11', lwd=3)	
	
	if(i == 1){
		mtext('MHU', 3, line=1);
	}
	axis(2, labels=FALSE);
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(clairNBS_Prior[clairNBS_Prior[,2]==i,priorNBSColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.15), xlim=c(-50,260), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = -50:260, y = dnorm(-50:260, mean=clairNBSPriorMean[i], sd = sqrt(1/clairNBSPriorPrecision[i])), col='red', lty='11', lwd=3)	
	
	if(i == 1){
		mtext('STC', 3, line=1);
	}
	axis(2, labels=FALSE);
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(erieNBS_Prior[erieNBS_Prior[,2]==i,priorNBSColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.15), xlim=c(-50,260), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = -50:260, y = dnorm(-50:260, mean=erieNBSPriorMean[i], sd = sqrt(1/erieNBSPriorPrecision[i])), col='red', lty='11', lwd=3)		
	if(i == 1){
		mtext('ERI', 3, line=1);
	}
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	axis(2, labels=FALSE);
	hist(ontarioNBS_Prior[ontarioNBS_Prior[,2]==i,priorNBSColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.15), xlim=c(-50,260), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = -50:260, y = dnorm(-50:260, mean=ontarioNBSPriorMean[i], sd = sqrt(1/ontarioNBSPriorPrecision[i])), col='red', lty='11', lwd=3)	
	if(i == 1){
		mtext('ONT', 3, line=1);
	}
	if(i %% 2 == 1){
		axis(4, labels=FALSE);
	}
	else{
		axis(4, labels=TRUE);
	}
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
}

title(main="NBS Priors (mm)", outer=TRUE, line = 3)

dev.off()


### OUTFLOW 
pdf('outflowPriorCompare.pdf', height=10, width=7.5)
layout(matrix(1:60,12,5,TRUE)) #BY ROW
par(mar = c(0,0,0,0))
par(oma = c(4,4,5,4))

for(i in 1:12){
	hist(superiorOutflow_Prior[superiorOutflow_Prior[,2]==i,priorOutflowColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.0025), xlim=c(1000,4000), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 1000:4000, y = dnorm(1000:4000, mean=superiorOutflowPriorMean[i], sd = sqrt(1/superiorOutflowPriorPrecision[i])), col='red', lty='11', lwd=3)
	if(i %% 2 == 1){
		axis(2, labels=TRUE);
	}
	else{
		axis(2, labels=FALSE);
	}
	mtext(month.abb[i], 2, line=2.5);
	if(i == 1){
		mtext('SUP', 3, line=1);
	}
	if(i == 12){
		axis(1, labels=FALSE)
		axis(1, labels=TRUE, at=c(1000, 2000,3000))
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(miHuronOutflow_Prior[miHuronOutflow_Prior[,2]==i,priorOutflowColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.0025), xlim=c(3000,7000), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 3000:7000, y = dnorm(3000:7000, mean=miHuronOutflowPriorMean[i], sd = sqrt(1/miHuronOutflowPriorPrecision[i])), col='red', lty='11', lwd=3)
	if(i == 1){
		mtext('MHU', 3, line=1);
	}
	axis(2, labels=FALSE);
	if(i == 12){
		axis(1, labels=FALSE)
		axis(1, labels=TRUE, at=c(3000, 5000))
	}
	else{
		axis(1, labels=FALSE)
	}
	hist(clairOutflow_Prior[clairOutflow_Prior[,2]==i,priorOutflowColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.0025), xlim=c(3000,7500), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 3000:7500, y = dnorm(3000:7500, mean=clairOutflowPriorMean[i], sd = sqrt(1/clairOutflowPriorPrecision[i])), col='red', lty='11', lwd=3)
	if(i == 1){
		mtext('STC', 3, line=1);
	}
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
	axis(2, labels=FALSE);
	hist(erieOutflow_Prior[erieOutflow_Prior[,2]==i,priorOutflowColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.0025), xlim=c(3500,8000), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 3500:8000, y = dnorm(3500:8000, mean=erieOutflowPriorMean[i], sd = sqrt(1/erieOutflowPriorPrecision[i])), col='red', lty='11', lwd=3)	
	if(i == 1){
		mtext('ERI', 3, line=1);
	}
	if(i == 12){
		axis(1, labels=FALSE)
		axis(1, labels=TRUE, at=c(5000, 7000))
	}
	else{
		axis(1, labels=FALSE)
	}
	axis(2, labels=FALSE);
	hist(ontarioOutflow_Prior[ontarioOutflow_Prior[,2]==i,priorOutflowColumn], main='', yaxt='n', xaxt='n', breaks=7, freq=FALSE, ylim=c(0,0.0025), xlim=c(5000,10000), border=NA, col=rgb(255/255,106/255,106/255,0.5))
	box()
	lines(x = 5000:10000, y = dnorm(5000:10000, mean=ontarioOutflowPriorMean[i], sd = sqrt(1/ontarioOutflowPriorPrecision[i])), col='red', lty='11', lwd=3)	
	if(i == 1){
		mtext('ONT', 3, line=1);
	}
	if(i %% 2 == 1){
		axis(4, labels=FALSE);
	}
	else{
		axis(4, labels=TRUE);
	}
	if(i == 12){
		axis(1, labels=TRUE)
	}
	else{
		axis(1, labels=FALSE)
	}
}

title(main="Outflow Priors (cms)", outer=TRUE, line = 3)

dev.off()

if(clairComponentWBM){

	pdf('clairNBSPriorCompare.pdf', height=10, width=3)
	#x11()
	layout(matrix(1:12,12,1,TRUE)) #BY ROW
	par(mar = c(0,0,0,0))
	par(oma = c(4,4,4,4))

	for(i in 1:12){
		hist(clairNBS_Prior[clairNBS_Prior[,2]==i,priorNBSColumn], main='', freq=FALSE, ylim=c(0,0.02), xlim=c(-100,1000), xaxt='n', yaxt='n', border=NA, col=rgb(255/255,106/255,106/255,0.5))
		box()
		lines(x = -100:1000, y = dnorm(-100:1000, mean=clairNBSPriorMean[i], sd = sqrt(1/clairNBSPriorPrecision[i])), col='red', lty='11', lwd=3)	
		if(i %% 2 == 1){
			axis(2, labels=TRUE);
			axis(4, labels=FALSE);
		}
		else{
			axis(2, labels=FALSE);
			axis(4, labels=TRUE);
		}
		if(i == 12){
			axis(1, labels=TRUE)
		}
		else{
			axis(1, labels=FALSE)
		}
	}
	title(main="St. Clair NBS (cms)", outer=TRUE)

	dev.off()
}