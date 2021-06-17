### TIME SERIES PLOTTER FOR THE L2SWBM
# LMF 2019-06-2018 modified how x-axis is drawn, because it was not drawing correctly when the start date is not January (as most operational runs)

posteriorStartMonth = 1;
posteriorEndMonth = length(m);
decade = 0;

sQ_U = rep(0.02,12)*superiorOutflowPriorMean
mQ_U = rep(0.03,12)*miHuronOutflowPriorMean
cQ_U = rep(0.03,12)*clairOutflowPriorMean
eQ_U = rep(0.02,12)*erieOutflowPriorMean
oQ_U = rep(0.02,12)*ontarioOutflowPriorMean

MNL_U = c(103,102,105,112,114,113,110,109,108,106,103,106)
CD_U = c(206,200,218,230,236,238,240,240,238,237,235,229)

for(startMo in seq(posteriorStartMonth, posteriorEndMonth, 120)){

endMo = (startMo+119);
endMoPlot = (startMo+119);
if(endMo > posteriorEndMonth){
	endMo = posteriorEndMonth;
}

yearRange = (startAnalysisYear+(decade*10)):(startAnalysisYear+((decade+1)*10)-1);

## LMF 2019-06-18 Create date variables to correct the dates in the plotting process, based on fixe in earlier version operating on USACE system.
startNew<-as.Date(paste(startAnalysisYear, startAnalysisMonth, 15, sep="-"))
endNew<-as.Date(paste(endAnalysisYear, endAnalysisMonth, 15, sep="-"))

compLimits = c(-50, 500)
flowLimits = c(-25,7000)
flowLimitsLabs = seq(0,10000,1000)
flowLimitsTicks = seq(0,10000,1000)
diversionLimits = c(-25,600)
compLimitsLabs = seq(0,600,150)
compLimitsTicks = seq(0,600,150)
nbsLimits = c(-100,800)
diversionLimitsLabs = seq(0,500,100)
diversionLimitsTicks = seq(0,500,100)
storeLimits = c(-650,650)
ceoStoreLimits = c(-1000,1000)
clairFlowLimits = c(3000,8000);
erieOntFlowLimits = c(0,1500);
clairDStoreLimits = c(-300,300);
clairNBSLimits = c(-1000,1500);

modelSkillPoints = list();


#colorVector = rainbow(10, v=0.5);

colorVector = c(
	'#800000',
	'#FF8000',
	'#BFBF00',
	'#00BF00',
	'#00BFBF',
	'#0080FF',
	'#8000FF',
	'#FF00FB',
	'#BF0060',
	'#BF8F60',
	'#000000'	
);

# superior

pdf(paste('superiorTS_ALL_d',decade,'_',modelName,'.pdf', sep=''), width = 10, height = 7.5);
if(superiorComponentWBM){
par(mfrow=c(6,1))
}else{
par(mfrow=c(4,1))
}
par(mar = c(0,0,0,0))
par(oma = c(4,4,4,4))

if(superiorComponentWBM){
plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(2, at=compLimitsLabs, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
mtext(paste("P (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('superiorPrecip[',i,']', sep=''),3],
		y1 = jSum[paste('superiorPrecip[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('superiorPrecip[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	superiorPrecipECDF = ecdf(as.matrix(jSample[,paste('superiorPrecip[',i,']',sep='')]))
	
	for(n in 3:ncol(superiorPrecip_A)){
		lines(c(i, i+1), c(superiorPrecip_A[i,n],superiorPrecip_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('superiorPrecip',superiorPrecipSrc[n-2])]] = c(
			modelSkillPoints[[paste0('superiorPrecip',superiorPrecipSrc[n-2])]],
			superiorPrecipECDF(superiorPrecip_A[i,n])
		)
	}
}        

superiorPrecipLegend = c('L2SWBM', superiorPrecipSrc);
superiorPrecipLegendColors = c('gray60', colorVector[1:length(superiorPrecipSrc)]);

legend(
	x = 'topleft',
	legend = superiorPrecipLegend,
	col = superiorPrecipLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(superiorPrecipSrc))),
	lwd = c(0, rep(1.5, length(superiorPrecipSrc))),
	pch = c(15, rep(NA, length(superiorPrecipSrc))),
	ncol = 5
);

plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(4, at=compLimitsLabs, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7);  
mtext(paste("E (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('superiorEvap[',i,']', sep=''),3],
		y1 = jSum[paste('superiorEvap[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('superiorEvap[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	superiorEvapECDF = ecdf(as.matrix(jSample[,paste('superiorEvap[',i,']',sep='')]))
	
	for(n in 3:ncol(superiorEvap_A)){
		lines(c(i, i+1), c(superiorEvap_A[i,n],superiorEvap_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('superiorEvap',superiorEvapSrc[n-2])]] = c(
			modelSkillPoints[[paste0('superiorEvap',superiorEvapSrc[n-2])]],
			superiorEvapECDF(superiorEvap_A[i,n])
		)
	}
}    


superiorEvapLegend = c('L2SWBM', superiorEvapSrc);
superiorEvapLegendColors = c('gray60', colorVector[1:length(superiorEvapSrc)]);

legend(
	x = 'topleft',
	legend = superiorEvapLegend,
	col = superiorEvapLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(superiorEvapSrc))),
	lwd = c(0, rep(1.5, length(superiorEvapSrc))),
	pch = c(15, rep(NA, length(superiorEvapSrc))),
	ncol = 5
);
    

plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(2, at=compLimitsLabs, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
mtext(paste("R (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('superiorRunoff[',i,']', sep=''),3],
		y1 = jSum[paste('superiorRunoff[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('superiorRunoff[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	superiorRunoffECDF = ecdf(as.matrix(jSample[,paste('superiorRunoff[',i,']',sep='')]))
	
	for(n in 3:ncol(superiorRunoff_A)){
		lines(c(i, i+1), c(superiorRunoff_A[i,n],superiorRunoff_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('superiorRunoff',superiorRunoffSrc[n-2])]] = c(
			modelSkillPoints[[paste0('superiorRunoff',superiorRunoffSrc[n-2])]],
			superiorRunoffECDF(superiorRunoff_A[i,n])
		)
	}
}      


superiorRunoffLegend = c('L2SWBM', superiorRunoffSrc);
superiorRunoffLegendColors = c('gray60', colorVector[1:length(superiorRunoffSrc)]);

legend(
	x = 'topleft',
	legend = superiorRunoffLegend,
	col = superiorRunoffLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(superiorRunoffSrc))),
	lwd = c(0, rep(1.5, length(superiorRunoffSrc))),
	pch = c(15, rep(NA, length(superiorRunoffSrc))),
	ncol = 6
);

}else{
plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = nbsLimits); 
box()
axis(2, cex.lab = 0.7); 
axis(2, labels=FALSE, cex.lab = 0.7); 
axis(4, labels=FALSE, cex.lab = 0.7); 
mtext(paste("NBS (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('superiorNBS[',i,']', sep=''),3],
		y1 = jSum[paste('superiorNBS[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('superiorNBS[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	superiorNBSECDF = ecdf(as.matrix(jSample[,paste('superiorNBS[',i,']',sep='')]))
	
	for(n in 3:ncol(superiorNBS_A)){
		lines(c(i, i+1), c(superiorNBS_A[i,n],superiorNBS_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('superiorNBS',superiorNBSSrc[n-2])]] = c(
			modelSkillPoints[[paste0('superiorNBS',superiorNBSSrc[n-2])]],
			superiorNBSECDF(superiorNBS_A[i,n])
		)
	}
}        

superiorNBSLegend = c('L2SWBM', superiorNBSSrc);
superiorNBSLegendColors = c('gray60', colorVector[1:length(superiorNBSSrc)]);

legend(
	x = 'topleft',
	legend = superiorNBSLegend,
	col = superiorNBSLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(superiorNBSSrc))),
	lwd = c(0, rep(1.5, length(superiorNBSSrc))),
	pch = c(15, rep(NA, length(superiorNBSSrc))),
	ncol = 5
);

}


plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = c(0,5000)); 
box()
axis(4, at=flowLimitsLabs, cex.lab = 0.7); 
axis(4, at=flowLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(2, at=flowLimitsTicks, labels=FALSE, cex.lab = 0.7);  
mtext(paste("Q (cms)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('superiorOutflow[',i,']', sep=''),3],
		y1 = jSum[paste('superiorOutflow[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('superiorOutflow[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	superiorOutflowECDF = ecdf(as.matrix(jSample[,paste('superiorOutflow[',i,']',sep='')]))
	
	for(n in 3:ncol(superiorOutflow_A)){
		lines(c(i, i+1), c(superiorOutflow_A[i,n],superiorOutflow_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('superiorOutflow',superiorOutflowSrc[n-2])]] = c(
			modelSkillPoints[[paste0('superiorOutflow',superiorOutflowSrc[n-2])]],
			superiorOutflowECDF(superiorOutflow_A[i,n])
		)
	}
}  
superiorOutflowLegend = c('L2SWBM', superiorOutflowSrc);
superiorOutflowLegendColors = c('gray60', colorVector[1:length(superiorOutflowSrc)]);


legend(
	x = 'topleft',
	legend = superiorOutflowLegend,
	col = superiorOutflowLegendColors,
	bty = 'n',
	lty = c(0, rep(1, 2)),
	lwd = c(0, rep(1.5, 2)),
	pch = c(15, rep(NA, 2)),
	ncol = 2
);


plot(c(0), c(0), type = "n", col = "darkgreen", lwd = 4, axes = FALSE, ylim = diversionLimits, xlim = c(startMo,endMoPlot)); 
box()
abline(h=0, col = 8)
axis(2, at=diversionLimitsLabs, cex.lab = 0.7); 
axis(2, at=diversionLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=diversionLimitsTicks, labels=FALSE, cex.lab = 0.7); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis
mtext('D (cms)', side = 2, line = 2.5, cex = 0.8)

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('superiorDiversion[',i,']', sep=''),3],
		y1 = jSum[paste('superiorDiversion[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('superiorDiversion[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	superiorDiversionECDF = ecdf(as.matrix(jSample[,paste('superiorDiversion[',i,']',sep='')]))
	
	for(n in 3:ncol(superiorDiversion_A)){
		lines(c(i, i+1), c(superiorDiversion_A[i,n],superiorDiversion_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('superiorDiversion',superiorDiversionSrc[n-2])]] = c(
			modelSkillPoints[[paste0('superiorDiversion',superiorDiversionSrc[n-2])]],
			superiorDiversionECDF(superiorDiversion_A[i,n])
		)
	}
}

plot(c(0), c(0), type = "n", col = "darkgreen", lwd = 4, axes = FALSE, ylim = storeLimits, xlim = c(startMo,endMoPlot)); 
box()
abline(h=0, col = 8)
axis(4, cex.lab = 0.7); 
axis(2, labels=FALSE, cex.lab = 0.7); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
# axis(1, at=seq(startMo,endMoPlot,12)+6, labels=yearRange, tick=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=format(seq(startNew, endNew, by="6 months"), '%b %y'), las=2); # LMF 2019-06-18 x-axis fix
mtext(expression(paste(Delta,'H (mm)')), side = 2, line = 2.5, cex = 0.8)

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('ySuperiorDStorePP[',i,']', sep=''),3],
		y1 = jSum[paste('ySuperiorDStorePP[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('ySuperiorDStorePP[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	lines(c(i, i+1), c(superiorDS_A[i,3],superiorDS_A[i,3]), col = "goldenrod",  lwd=1.5, type='s')
}

title(main='Superior - posterior inferences', outer=TRUE);

dev.off();

### miHuron

pdf(paste('miHuronTS_ALL_d',decade,'_',modelName,'.pdf', sep=''), width = 10, height = 7.5);
if(miHuronComponentWBM){
	par(mfrow=c(6,1))
}else{
	par(mfrow=c(4,1))
}
par(mar = c(0,0,0,0))
par(oma = c(4,4,4,4))

if(miHuronComponentWBM){
plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(2, at=compLimitsLabs, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
mtext(paste("P (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('miHuronPrecip[',i,']', sep=''),3],
		y1 = jSum[paste('miHuronPrecip[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('miHuronPrecip[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	miHuronPrecipECDF = ecdf(as.matrix(jSample[,paste('miHuronPrecip[',i,']',sep='')]))
	
	for(n in 3:ncol(miHuronPrecip_A)){
		lines(c(i, i+1), c(miHuronPrecip_A[i,n],miHuronPrecip_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('miHuronPrecip',miHuronPrecipSrc[n-2])]] = c(
			modelSkillPoints[[paste0('miHuronPrecip',miHuronPrecipSrc[n-2])]],
			miHuronPrecipECDF(miHuronPrecip_A[i,n])
		)
	}
}        

miHuronPrecipLegend = c('L2SWBM', miHuronPrecipSrc);
miHuronPrecipLegendColors = c('gray60', colorVector[1:length(miHuronPrecipSrc)]);

legend(
	x = 'topleft',
	legend = miHuronPrecipLegend,
	col = miHuronPrecipLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(miHuronPrecipSrc))),
	lwd = c(0, rep(1.5, length(miHuronPrecipSrc))),
	pch = c(15, rep(NA, length(miHuronPrecipSrc))),
	ncol = 5
);



plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(4, at=compLimitsLabs, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7);  
mtext(paste("E (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('miHuronEvap[',i,']', sep=''),3],
		y1 = jSum[paste('miHuronEvap[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('miHuronEvap[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	miHuronEvapECDF = ecdf(as.matrix(jSample[,paste('miHuronEvap[',i,']',sep='')]))
	
	for(n in 3:ncol(miHuronEvap_A)){
		lines(c(i, i+1), c(miHuronEvap_A[i,n],miHuronEvap_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('miHuronEvap',miHuronEvapSrc[n-2])]] = c(
			modelSkillPoints[[paste0('miHuronEvap',miHuronEvapSrc[n-2])]],
			miHuronEvapECDF(miHuronEvap_A[i,n])
		)
	}
}        

miHuronEvapLegend = c('L2SWBM', miHuronEvapSrc);
miHuronEvapLegendColors = c('gray60', colorVector[1:length(miHuronEvapSrc)]);

legend(
	x = 'topleft',
	legend = miHuronEvapLegend,
	col = miHuronEvapLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(miHuronEvapSrc))),
	lwd = c(0, rep(1.5, length(miHuronEvapSrc))),
	pch = c(15, rep(NA, length(miHuronEvapSrc))),
	ncol = 5
);


plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(2, at=compLimitsLabs, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
mtext(paste("R (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('miHuronRunoff[',i,']', sep=''),3],
		y1 = jSum[paste('miHuronRunoff[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('miHuronRunoff[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	miHuronRunoffECDF = ecdf(as.matrix(jSample[,paste('miHuronRunoff[',i,']',sep='')]))
	
	for(n in 3:ncol(miHuronRunoff_A)){
		lines(c(i, i+1), c(miHuronRunoff_A[i,n],miHuronRunoff_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('miHuronRunoff',miHuronRunoffSrc[n-2])]] = c(
			modelSkillPoints[[paste0('miHuronRunoff',miHuronRunoffSrc[n-2])]],
			miHuronRunoffECDF(miHuronRunoff_A[i,n])
		)
	}
}      


miHuronRunoffLegend = c('L2SWBM', miHuronRunoffSrc);
miHuronRunoffLegendColors = c('gray60', colorVector[1:length(miHuronRunoffSrc)]);

legend(
	x = 'topleft',
	legend = miHuronRunoffLegend,
	col = miHuronRunoffLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(miHuronRunoffSrc))),
	lwd = c(0, rep(1.5, length(miHuronRunoffSrc))),
	pch = c(15, rep(NA, length(miHuronRunoffSrc))),
	ncol = 6
);

}else{
plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = nbsLimits); 
box()
axis(2, cex.lab = 0.7); 
axis(2, labels=FALSE, cex.lab = 0.7); 
axis(4, labels=FALSE, cex.lab = 0.7); 
mtext(paste("NBS (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('miHuronNBS[',i,']', sep=''),3],
		y1 = jSum[paste('miHuronNBS[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('miHuronNBS[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	miHuronNBSECDF = ecdf(as.matrix(jSample[,paste('miHuronNBS[',i,']',sep='')]))
	
	for(n in 3:ncol(miHuronNBS_A)){
		lines(c(i, i+1), c(miHuronNBS_A[i,n],miHuronNBS_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('miHuronNBS',miHuronNBSSrc[n-2])]] = c(
			modelSkillPoints[[paste0('miHuronNBS',miHuronNBSSrc[n-2])]],
			miHuronNBSECDF(miHuronNBS_A[i,n])
		)
	}
}        

miHuronNBSLegend = c('L2SWBM', miHuronNBSSrc);
miHuronNBSLegendColors = c('gray60', colorVector[1:length(miHuronNBSSrc)]);

legend(
	x = 'topleft',
	legend = miHuronNBSLegend,
	col = miHuronNBSLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(miHuronNBSSrc))),
	lwd = c(0, rep(1.5, length(miHuronNBSSrc))),
	pch = c(15, rep(NA, length(miHuronNBSSrc))),
	ncol = 5
);

}


plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = c(3000,8000)); 
box()
axis(4, at=flowLimitsLabs, cex.lab = 0.7); 
axis(4, at=flowLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(2, at=flowLimitsTicks, labels=FALSE, cex.lab = 0.7);  
mtext(paste("Q (cms)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('miHuronOutflow[',i,']', sep=''),3],
		y1 = jSum[paste('miHuronOutflow[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('miHuronOutflow[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	miHuronOutflowECDF = ecdf(as.matrix(jSample[,paste('miHuronOutflow[',i,']',sep='')]))
	
	for(n in 3:ncol(miHuronOutflow_A)){
		lines(c(i, i+1), c(miHuronOutflow_A[i,n],miHuronOutflow_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('miHuronOutflow',miHuronOutflowSrc[n-2])]] = c(
			modelSkillPoints[[paste0('miHuronOutflow',miHuronOutflowSrc[n-2])]],
			miHuronOutflowECDF(miHuronOutflow_A[i,n])
		)
	}
}     

miHuronOutflowLegend = c('L2SWBM', miHuronOutflowSrc);
miHuronOutflowLegendColors = c('gray60', colorVector[1:length(miHuronOutflowSrc)]);

legend(
	x = 'topleft',
	legend = miHuronOutflowLegend,
	col = miHuronOutflowLegendColors,
	bty = 'n',
	lty = c(0, rep(1, 2)),
	lwd = c(0, rep(1.5, 2)),
	pch = c(15, rep(NA, 2)),
	ncol = 2
);


plot(c(0), c(0), type = "n", col = "darkgreen", lwd = 4, axes = FALSE, ylim = diversionLimits, xlim = c(startMo,endMoPlot)); 
box()
abline(h=0, col = 8)
axis(2, at=diversionLimitsLabs, cex.lab = 0.7); 
axis(2, at=diversionLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=diversionLimitsTicks, labels=FALSE, cex.lab = 0.7); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis
mtext('D (cms)', side = 2, line = 2.5, cex = 0.8)

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('miHuronDiversion[',i,']', sep=''),3],
		y1 = jSum[paste('miHuronDiversion[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('miHuronDiversion[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	miHuronDiversionECDF = ecdf(as.matrix(jSample[,paste('miHuronDiversion[',i,']',sep='')]))
	
	for(n in 3:ncol(miHuronDiversion_A)){
		lines(c(i, i+1), c(miHuronDiversion_A[i,n],miHuronDiversion_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('miHuronDiversion',miHuronDiversionSrc[n-2])]] = c(
			modelSkillPoints[[paste0('miHuronDiversion',miHuronDiversionSrc[n-2])]],
			miHuronDiversionECDF(miHuronDiversion_A[i,n])
		)
	}
}

abline(h = 91, col='lightblue3', lty=2, lwd=1.5)

miHuronDiversionLegend = c('L2SWBM', 'Coordinated', 'Regulation Limit');
miHuronDiversionLegendColors = c('gray60', 'darkgreen', 'lightblue3');

legend(
	x = 'topleft',
	legend = miHuronDiversionLegend,
	col = miHuronDiversionLegendColors,
	bty = 'n',
	lty = c(0, 1, 2),
	lwd = c(0, 1.5, 1.5),
	pch = c(15, rep(NA, 2)),
	ncol = 5
);


plot(c(0), c(0), type = "n", col = "darkgreen", lwd = 4, axes = FALSE, ylim = storeLimits, xlim = c(startMo,endMoPlot)); 
box()
abline(h=0, col = 8)
axis(4, cex.lab = 0.7); 
axis(2, labels=FALSE, cex.lab = 0.7); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
# axis(1, at=seq(startMo,endMoPlot,12)+6, labels=yearRange, tick=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=format(seq(startNew, endNew, by="6 months"), '%b %y'), las=2); # LMF 2019-06-18 x-axis fix
mtext(expression(paste(Delta,'H (mm)')), side = 2, line = 2.5, cex = 0.8)

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('yMiHuronDStorePP[',i,']', sep=''),3],
		y1 = jSum[paste('yMiHuronDStorePP[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('yMiHuronDStorePP[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	lines(c(i, i+1), c(miHuronDS_A[i,3],miHuronDS_A[i,3]), col = "goldenrod",  lwd=1.5, type='s')
}

title(main='Michigan-Huron - posterior inferences', outer=TRUE);

dev.off();

### Clair

pdf(paste('clair_ALL_d',decade,'_',modelName,'.pdf', sep=''), width = 10, height = 7.5);
if(clairComponentWBM){
	par(mfrow=c(5,1))
}else{
	par(mfrow=c(3,1))
}
par(mar = c(0,0,0,0))
par(oma = c(4,4,4,4))

if(clairComponentWBM){
plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(2, at=compLimitsLabs, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
mtext(paste("P (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('clairPrecip[',i,']', sep=''),3],
		y1 = jSum[paste('clairPrecip[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('clairPrecip[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	clairPrecipECDF = ecdf(as.matrix(jSample[,paste('clairPrecip[',i,']',sep='')]))
	
	for(n in 3:ncol(clairPrecip_A)){
		lines(c(i, i+1), c(clairPrecip_A[i,n],clairPrecip_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('clairPrecip',clairPrecipSrc[n-2])]] = c(
			modelSkillPoints[[paste0('clairPrecip',clairPrecipSrc[n-2])]],
			clairPrecipECDF(clairPrecip_A[i,n])
		)
	}
}        

clairPrecipLegend = c('L2SWBM', clairPrecipSrc);
clairPrecipLegendColors = c('gray60', colorVector[1:length(clairPrecipSrc)]);

legend(
	x = 'topleft',
	legend = clairPrecipLegend,
	col = clairPrecipLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(clairPrecipSrc))),
	lwd = c(0, rep(1.5, length(clairPrecipSrc))),
	pch = c(15, rep(NA, length(clairPrecipSrc))),
	ncol = 5
);

plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(4, at=compLimitsLabs, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7);  
mtext(paste("E (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('clairEvap[',i,']', sep=''),3],
		y1 = jSum[paste('clairEvap[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('clairEvap[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	clairEvapECDF = ecdf(as.matrix(jSample[,paste('clairEvap[',i,']',sep='')]))
	
	for(n in 3:ncol(clairEvap_A)){
		lines(c(i, i+1), c(clairEvap_A[i,n],clairEvap_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('clairEvap',clairEvapSrc[n-2])]] = c(
			modelSkillPoints[[paste0('clairEvap',clairEvapSrc[n-2])]],
			clairEvapECDF(clairEvap_A[i,n])
		)
	}
}    


clairEvapLegend = c('L2SWBM', clairEvapSrc);
clairEvapLegendColors = c('gray60', colorVector[1:length(clairEvapSrc)]);

legend(
	x = 'topleft',
	legend = clairEvapLegend,
	col = clairEvapLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(clairEvapSrc))),
	lwd = c(0, rep(1.5, length(clairEvapSrc))),
	pch = c(15, rep(NA, length(clairEvapSrc))),
	ncol = 5
);
    

plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(2, at=compLimitsLabs, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
mtext(paste("R (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('clairRunoff[',i,']', sep=''),3],
		y1 = jSum[paste('clairRunoff[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('clairRunoff[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	clairRunoffECDF = ecdf(as.matrix(jSample[,paste('clairRunoff[',i,']',sep='')]))
	
	for(n in 3:ncol(clairRunoff_A)){
		lines(c(i, i+1), c(clairRunoff_A[i,n],clairRunoff_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('clairRunoff',clairRunoffSrc[n-2])]] = c(
			modelSkillPoints[[paste0('clairRunoff',clairRunoffSrc[n-2])]],
			clairRunoffECDF(clairRunoff_A[i,n])
		)
	}
}      


clairRunoffLegend = c('L2SWBM', clairRunoffSrc);
clairRunoffLegendColors = c('gray60', colorVector[1:length(clairRunoffSrc)]);

legend(
	x = 'topleft',
	legend = clairRunoffLegend,
	col = clairRunoffLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(clairRunoffSrc))),
	lwd = c(0, rep(1.5, length(clairRunoffSrc))),
	pch = c(15, rep(NA, length(clairRunoffSrc))),
	ncol = 6
);

}else{
plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = clairNBSLimits); 
box()
axis(2, cex.lab = 0.7); 
axis(2, labels=FALSE, cex.lab = 0.7); 
axis(4, labels=FALSE, cex.lab = 0.7); 
if(clairCMS){
mtext(paste("NBS (cms)"), side = 2, line = 2.5, cex=0.8); 
}else{
mtext(paste("NBS (mm)"), side = 2, line = 2.5, cex=0.8); 
}
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('clairNBS[',i,']', sep=''),3],
		y1 = jSum[paste('clairNBS[',i,']', sep=''),7]
	);

	clairNBSECDF = ecdf(as.matrix(jSample[,paste('clairNBS[',i,']',sep='')]))
	
	for(n in 3:ncol(clairNBS_A)){
		lines(c(i, i+1), c(clairNBS_A[i,n],clairNBS_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('clairNBS',clairNBSSrc[n-2])]] = c(
			modelSkillPoints[[paste0('clairNBS',clairNBSSrc[n-2])]],
			clairNBSECDF(clairNBS_A[i,n])
		)
	}
}        

clairNBSLegend = c('L2SWBM', clairNBSSrc);
clairNBSLegendColors = c('gray60', colorVector[1:length(clairNBSSrc)]);

legend(
	x = 'topleft',
	legend = clairNBSLegend,
	col = clairNBSLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(clairNBSSrc))),
	lwd = c(0, rep(1.5, length(clairNBSSrc))),
	pch = c(15, rep(NA, length(clairNBSSrc))),
	ncol = 5
);
}
plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = clairFlowLimits); 
box()
axis(4, at=flowLimitsLabs, cex.lab = 0.7); 
axis(4, at=flowLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(2, at=flowLimitsTicks, labels=FALSE, cex.lab = 0.7);  
mtext(paste("Q (cms)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('clairOutflow[',i,']', sep=''),3],
		y1 = jSum[paste('clairOutflow[',i,']', sep=''),7]
	);

	clairOutflowECDF = ecdf(as.matrix(jSample[,paste('clairOutflow[',i,']',sep='')]))
	
	for(n in 3:ncol(clairOutflow_A)){
		lines(c(i, i+1), c(clairOutflow_A[i,n],clairOutflow_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('clairOutflow',clairOutflowSrc[n-2])]] = c(
			modelSkillPoints[[paste0('clairOutflow',clairOutflowSrc[n-2])]],
			clairOutflowECDF(clairOutflow_A[i,n])
		)
	}
}         

clairOutflowLegend = c('L2SWBM', clairOutflowSrc);
clairOutflowLegendColors = c('gray60', colorVector[1:length(clairOutflowSrc)]);

legend(
	x = 'topleft',
	legend = clairOutflowLegend,
	col = clairOutflowLegendColors,
	bty = 'n',
	lty = c(0, rep(1, 2), 2),
	lwd = c(0, rep(1.5, 2)),
	pch = c(15, rep(NA, 2)),
	ncol = 2
);


plot(c(0), type = "n", col = "darkgreen", lwd = 4, axes = FALSE, ylim = storeLimits, xlim = c(startMo,endMoPlot)); 
box()
abline(h=0, col = 8)
axis(2, cex.lab = 0.7); 
axis(4, labels=FALSE, cex.lab = 0.7); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
# axis(1, at=seq(startMo,endMoPlot,12)+6, labels=yearRange, tick=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=format(seq(startNew, endNew, by="6 months"), '%b %y'), las=2); # LMF 2019-06-18 x-axis fix

if(clairCMS){
	mtext(expression(paste(Delta,'H (cms)')), side = 2, line = 2.5, cex = 0.8)
}else{
	mtext(expression(paste(Delta,'H (mm)')), side = 2, line = 2.5, cex = 0.8)
}

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('yClairDStorePP[',i,']', sep=''),3],
		y1 = jSum[paste('yClairDStorePP[',i,']', sep=''),7]
	);
	
	lines(c(i, i+1), c(clairDS_A[i,3],clairDS_A[i,3]), col = "goldenrod",  lwd=1.5, type='s')
}

title(main='St. Clair - posterior inferences', outer=TRUE);

dev.off();

### Erie

pdf(paste('erieTS_ALL_d',decade,'_',modelName,'.pdf', sep=''), width = 10, height = 7.5);
if(erieComponentWBM){
	par(mfrow=c(6,1))
}else{
	par(mfrow=c(4,1))
}
par(mar = c(0,0,0,0))
par(oma = c(4,4,4,4))

if(erieComponentWBM){
plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(2, at=compLimitsLabs, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
mtext(paste("P (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('eriePrecip[',i,']', sep=''),3],
		y1 = jSum[paste('eriePrecip[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('eriePrecip[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	eriePrecipECDF = ecdf(as.matrix(jSample[,paste('eriePrecip[',i,']',sep='')]))
	
	for(n in 3:ncol(eriePrecip_A)){
		lines(c(i, i+1), c(eriePrecip_A[i,n],eriePrecip_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('eriePrecip',eriePrecipSrc[n-2])]] = c(
			modelSkillPoints[[paste0('eriePrecip',eriePrecipSrc[n-2])]],
			eriePrecipECDF(eriePrecip_A[i,n])
		)
	}
}        

eriePrecipLegend = c('L2SWBM', eriePrecipSrc);
eriePrecipLegendColors = c('gray60', colorVector[1:length(eriePrecipSrc)]);

legend(
	x = 'topleft',
	legend = eriePrecipLegend,
	col = eriePrecipLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(eriePrecipSrc))),
	lwd = c(0, rep(1.5, length(eriePrecipSrc))),
	pch = c(15, rep(NA, length(eriePrecipSrc))),
	ncol = 5
);


plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(4, at=compLimitsLabs, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7);  
mtext(paste("E (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('erieEvap[',i,']', sep=''),3],
		y1 = jSum[paste('erieEvap[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('erieEvap[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	erieEvapECDF = ecdf(as.matrix(jSample[,paste('erieEvap[',i,']',sep='')]))
	
	for(n in 3:ncol(erieEvap_A)){
		lines(c(i, i+1), c(erieEvap_A[i,n],erieEvap_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('erieEvap',erieEvapSrc[n-2])]] = c(
			modelSkillPoints[[paste0('erieEvap',erieEvapSrc[n-2])]],
			erieEvapECDF(erieEvap_A[i,n])
		)
	}
}        

erieEvapLegend = c('L2SWBM', erieEvapSrc);
erieEvapLegendColors = c('gray60', colorVector[1:length(erieEvapSrc)]);

legend(
	x = 'topleft',
	legend = erieEvapLegend,
	col = erieEvapLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(erieEvapSrc))),
	lwd = c(0, rep(1.5, length(erieEvapSrc))),
	pch = c(15, rep(NA, length(erieEvapSrc))),
	ncol = 5
);
  

plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(2, at=compLimitsLabs, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
mtext(paste("R (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('erieRunoff[',i,']', sep=''),3],
		y1 = jSum[paste('erieRunoff[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('erieRunoff[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	erieRunoffECDF = ecdf(as.matrix(jSample[,paste('erieRunoff[',i,']',sep='')]))
	
	for(n in 3:ncol(erieRunoff_A)){
		lines(c(i, i+1), c(erieRunoff_A[i,n],erieRunoff_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('erieRunoff',erieRunoffSrc[n-2])]] = c(
			modelSkillPoints[[paste0('erieRunoff',erieRunoffSrc[n-2])]],
			erieRunoffECDF(erieRunoff_A[i,n])
		)
	}
} 


erieRunoffLegend = c('L2SWBM', erieRunoffSrc);
erieRunoffLegendColors = c('gray60', colorVector[1:length(erieRunoffSrc)]);

legend(
	x = 'topleft',
	legend = erieRunoffLegend,
	col = erieRunoffLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(erieRunoffSrc))),
	lwd = c(0, rep(1.5, length(erieRunoffSrc))),
	pch = c(15, rep(NA, length(erieRunoffSrc))),
	ncol = 6
);    

}else{
plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = nbsLimits); 
box()
axis(2, cex.lab = 0.7); 
axis(2, labels=FALSE, cex.lab = 0.7); 
axis(4, labels=FALSE, cex.lab = 0.7); 
mtext(paste("NBS (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('erieNBS[',i,']', sep=''),3],
		y1 = jSum[paste('erieNBS[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('erieNBS[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	erieNBSECDF = ecdf(as.matrix(jSample[,paste('erieNBS[',i,']',sep='')]))
	
	for(n in 3:ncol(erieNBS_A)){
		lines(c(i, i+1), c(erieNBS_A[i,n],erieNBS_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('erieNBS',erieNBSSrc[n-2])]] = c(
			modelSkillPoints[[paste0('erieNBS',erieNBSSrc[n-2])]],
			erieNBSECDF(erieNBS_A[i,n])
		)
	}
}        

erieNBSLegend = c('L2SWBM', erieNBSSrc);
erieNBSLegendColors = c('gray60', colorVector[1:length(erieNBSSrc)]);

legend(
	x = 'topleft',
	legend = erieNBSLegend,
	col = erieNBSLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(erieNBSSrc))),
	lwd = c(0, rep(1.5, length(erieNBSSrc))),
	pch = c(15, rep(NA, length(erieNBSSrc))),
	ncol = 5
);

}

plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = c(3000,9000)); 
box()
axis(4, at=flowLimitsLabs, cex.lab = 0.7); 
axis(4, at=flowLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(2, at=flowLimitsTicks, labels=FALSE, cex.lab = 0.7);  
mtext(paste("Q (cms)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('erieOutflow[',i,']', sep=''),3],
		y1 = jSum[paste('erieOutflow[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('erieOutflow[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	erieOutflowECDF = ecdf(as.matrix(jSample[,paste('erieOutflow[',i,']',sep='')]))
	
	for(n in 3:ncol(erieOutflow_A)){
		lines(c(i, i+1), c(erieOutflow_A[i,n],erieOutflow_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('erieOutflow',erieOutflowSrc[n-2])]] = c(
			modelSkillPoints[[paste0('erieOutflow',erieOutflowSrc[n-2])]],
			erieOutflowECDF(erieOutflow_A[i,n])
		)
	}
}        

erieOutflowLegend = c('L2SWBM', erieOutflowSrc);
erieOutflowLegendColors = c('gray60', colorVector[1:length(erieOutflowSrc)]);

legend(
	x = 'topleft',
	legend = erieOutflowLegend,
	col = erieOutflowLegendColors,
	bty = 'n',
	lty = c(0, rep(1, 2)),
	lwd = c(0, rep(1.5, 2)),
	pch = c(15, rep(NA, 2)),
	ncol = 2
);

plot(c(0), c(0), type = "n", col = "darkgreen", lwd = 4, axes = FALSE, ylim = diversionLimits, xlim = c(startMo,endMoPlot)); 
box()
abline(h=0, col = 8)
axis(2, at=diversionLimitsLabs, cex.lab = 0.7); 
axis(2, at=diversionLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=diversionLimitsTicks, labels=FALSE, cex.lab = 0.7); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis
mtext('D (cms)', side = 2, line = 2.5, cex = 0.8)

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('erieDiversion[',i,']', sep=''),3],
		y1 = jSum[paste('erieDiversion[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('erieDiversion[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	erieDiversionECDF = ecdf(as.matrix(jSample[,paste('erieDiversion[',i,']',sep='')]))
	
	for(n in 3:ncol(erieDiversion_A)){
		lines(c(i, i+1), c(erieDiversion_A[i,n],erieDiversion_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('erieDiversion',erieDiversionSrc[n-2])]] = c(
			modelSkillPoints[[paste0('erieDiversion',erieDiversionSrc[n-2])]],
			erieDiversionECDF(erieDiversion_A[i,n])
		)
	}
}

plot(c(0), c(0), type = "n", col = "darkgreen", lwd = 4, axes = FALSE, ylim = storeLimits, xlim = c(startMo,endMoPlot)); 
box()
abline(h=0, col = 8)
axis(4, cex.lab = 0.7); 
axis(2, labels=FALSE, cex.lab = 0.7); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
# axis(1, at=seq(startMo,endMoPlot,12)+6, labels=yearRange, tick=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=format(seq(startNew, endNew, by="6 months"), '%b %y'), las=2); # LMF 2019-06-18 x-axis fix
mtext(expression(paste(Delta,'H (mm)')), side = 2, line = 2.5, cex = 0.8)

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('yErieDStorePP[',i,']', sep=''),3],
		y1 = jSum[paste('yErieDStorePP[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('yErieDStorePP[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	lines(c(i, i+1), c(erieDS_A[i,3],erieDS_A[i,3]), col = "goldenrod",  lwd=1.5, type='s')
}

title(main='Erie - posterior inferences', outer=TRUE);

dev.off();

### ontario

pdf(paste('ontarioTS_ALL_d',decade,'_',modelName,'.pdf', sep=''), width = 10, height = 7.5);
if(ontarioComponentWBM){
	par(mfrow=c(5,1))
}else{
	par(mfrow=c(3,1))
}
par(mar = c(0,0,0,0))
par(oma = c(4,4,4,4))


if(ontarioComponentWBM){
plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(2, at=compLimitsLabs, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
mtext(paste("P (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('ontarioPrecip[',i,']', sep=''),3],
		y1 = jSum[paste('ontarioPrecip[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('ontarioPrecip[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	ontarioPrecipECDF = ecdf(as.matrix(jSample[,paste('ontarioPrecip[',i,']',sep='')]))
	
	for(n in 3:ncol(ontarioPrecip_A)){
		lines(c(i, i+1), c(ontarioPrecip_A[i,n],ontarioPrecip_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('ontarioPrecip',ontarioPrecipSrc[n-2])]] = c(
			modelSkillPoints[[paste0('ontarioPrecip',ontarioPrecipSrc[n-2])]],
			ontarioPrecipECDF(ontarioPrecip_A[i,n])
		)
	}
}        

ontarioPrecipLegend = c('L2SWBM', ontarioPrecipSrc);
ontarioPrecipLegendColors = c('gray60', colorVector[1:length(ontarioPrecipSrc)]);

legend(
	x = 'topleft',
	legend = ontarioPrecipLegend,
	col = ontarioPrecipLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(ontarioPrecipSrc))),
	lwd = c(0, rep(1.5, length(ontarioPrecipSrc))),
	pch = c(15, rep(NA, length(ontarioPrecipSrc))),
	ncol = 5
);

plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = compLimits); 
box()
axis(4, at=compLimitsLabs, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7);  
mtext(paste("E (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('ontarioEvap[',i,']', sep=''),3],
		y1 = jSum[paste('ontarioEvap[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('ontarioEvap[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	ontarioEvapECDF = ecdf(as.matrix(jSample[,paste('ontarioEvap[',i,']',sep='')]))
	
	for(n in 3:ncol(ontarioEvap_A)){
		lines(c(i, i+1), c(ontarioEvap_A[i,n],ontarioEvap_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('ontarioEvap',ontarioEvapSrc[n-2])]] = c(
			modelSkillPoints[[paste0('ontarioEvap',ontarioEvapSrc[n-2])]],
			ontarioEvapECDF(ontarioEvap_A[i,n])
		)
	}
}        

ontarioEvapLegend = c('L2SWBM', ontarioEvapSrc);
ontarioEvapLegendColors = c('gray60', colorVector[1:length(ontarioEvapSrc)]);

legend(
	x = 'topleft',
	legend = ontarioEvapLegend,
	col = ontarioEvapLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(ontarioEvapSrc))),
	lwd = c(0, rep(1.5, length(ontarioEvapSrc))),
	pch = c(15, rep(NA, length(ontarioEvapSrc))),
	ncol = 5
);

plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = c(0,700)); 
box()
axis(2, at=compLimitsLabs, cex.lab = 0.7); 
axis(2, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(4, at=compLimitsTicks, labels=FALSE, cex.lab = 0.7); 
mtext(paste("R (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('ontarioRunoff[',i,']', sep=''),3],
		y1 = jSum[paste('ontarioRunoff[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('ontarioRunoff[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	ontarioRunoffECDF = ecdf(as.matrix(jSample[,paste('ontarioRunoff[',i,']',sep='')]))
	
	for(n in 3:ncol(ontarioRunoff_A)){
		lines(c(i, i+1), c(ontarioRunoff_A[i,n],ontarioRunoff_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('ontarioRunoff',ontarioRunoffSrc[n-2])]] = c(
			modelSkillPoints[[paste0('ontarioRunoff',ontarioRunoffSrc[n-2])]],
			ontarioRunoffECDF(ontarioRunoff_A[i,n])
		)
	}
}      

ontarioRunoffLegend = c('L2SWBM', ontarioRunoffSrc);
ontarioRunoffLegendColors = c('gray60', colorVector[1:length(ontarioRunoffSrc)]);

legend(
	x = 'topleft',
	legend = ontarioRunoffLegend,
	col = ontarioRunoffLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(ontarioRunoffSrc))),
	lwd = c(0, rep(1.5, length(ontarioRunoffSrc))),
	pch = c(15, rep(NA, length(ontarioRunoffSrc))),
	ncol = 6
);

}else{
plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = nbsLimits); 
box()
axis(2, cex.lab = 0.7); 
axis(2, labels=FALSE, cex.lab = 0.7); 
axis(4, labels=FALSE, cex.lab = 0.7); 
mtext(paste("NBS (mm)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('ontarioNBS[',i,']', sep=''),3],
		y1 = jSum[paste('ontarioNBS[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('ontarioNBS[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	ontarioNBSECDF = ecdf(as.matrix(jSample[,paste('ontarioNBS[',i,']',sep='')]))
	
	for(n in 3:ncol(ontarioNBS_A)){
		lines(c(i, i+1), c(ontarioNBS_A[i,n],ontarioNBS_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('ontarioNBS',ontarioNBSSrc[n-2])]] = c(
			modelSkillPoints[[paste0('ontarioNBS',ontarioNBSSrc[n-2])]],
			ontarioNBSECDF(ontarioNBS_A[i,n])
		)
	}
}        

ontarioNBSLegend = c('L2SWBM', ontarioNBSSrc);
ontarioNBSLegendColors = c('gray60', colorVector[1:length(ontarioNBSSrc)]);

legend(
	x = 'topleft',
	legend = ontarioNBSLegend,
	col = ontarioNBSLegendColors,
	bty = 'n',
	lty = c(0, rep(1, length(ontarioNBSSrc))),
	lwd = c(0, rep(1.5, length(ontarioNBSSrc))),
	pch = c(15, rep(NA, length(ontarioNBSSrc))),
	ncol = 5
);

}
plot(c(0), c(0), type = "n", axes = FALSE, xlim = c(startMo,endMoPlot), ylim = c(4000,11000)); 
box()
axis(4, at=flowLimitsLabs, cex.lab = 0.7); 
axis(4, at=flowLimitsTicks, labels=FALSE, cex.lab = 0.7); 
axis(2, at=flowLimitsTicks, labels=FALSE, cex.lab = 0.7);  
mtext(paste("Q (cms)"), side = 2, line = 2.5, cex=0.8); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=FALSE, las=2); # LMF 2019-06-18 fix for x-axis

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('ontarioOutflow[',i,']', sep=''),3],
		y1 = jSum[paste('ontarioOutflow[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('ontarioOutflow[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	ontarioOutflowECDF = ecdf(as.matrix(jSample[,paste('ontarioOutflow[',i,']',sep='')]))
	
	for(n in 3:ncol(ontarioOutflow_A)){
		lines(c(i, i+1), c(ontarioOutflow_A[i,n],ontarioOutflow_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
		
		modelSkillPoints[[paste0('ontarioOutflow',ontarioOutflowSrc[n-2])]] = c(
			modelSkillPoints[[paste0('ontarioOutflow',ontarioOutflowSrc[n-2])]],
			ontarioOutflowECDF(ontarioOutflow_A[i,n])
		)
	}
}        

ontarioOutflowLegend = c('L2SWBM', ontarioOutflowSrc);
ontarioOutflowLegendColors = c('gray60', colorVector[1:length(ontarioOutflowSrc)]);

legend(
	x = 'topleft',
	legend = ontarioOutflowLegend,
	col = ontarioOutflowLegendColors,
	bty = 'n',
	lty = c(0, rep(1, 2)),
	lwd = c(0, rep(1.5, 2)),
	pch = c(15, rep(NA, 2)),
	ncol = 2
);


plot(c(0),c(0), type = "n", col = "darkgreen", lwd = 4, axes = FALSE, ylim = storeLimits, xlim = c(startMo,endMoPlot)); 
box()
abline(h=0, col = 8)
axis(2, cex.lab = 0.7); 
axis(4, labels=FALSE, cex.lab = 0.7); 
# axis(1, at=seq(startMo,endMoPlot,12), labels=FALSE);
# axis(1, at=seq(startMo,endMoPlot,12)+6, labels=yearRange, tick=FALSE);
axis(1, at=seq(startMo,endMoPlot,6), labels=format(seq(startNew, endNew, by="6 months"), '%b %y'), las=2); # LMF 2019-06-18 x-axis fix
mtext(expression(paste(Delta,'H (mm)')), side = 2, line = 2.5, cex = 0.8)

for(i in startMo:endMo){
	segments(   
		x0 = i+(1/2), x1 = i+(1/2), lwd = 4, col = 'gray60', lend = 2,
		y0 = jSum[paste('yOntarioDStorePP[',i,']', sep=''),3],
		y1 = jSum[paste('yOntarioDStorePP[',i,']', sep=''),7]
	);
	#lines(i+(1/2), jSum[paste('yOntarioDStorePP[',i,']', sep=''),5], col = "black",   lwd = 1, type='p', pch=15, cex=0.75)
	
	lines(c(i, i+1), c(ontarioDS_A[i,3],ontarioDS_A[i,3]), col = "goldenrod",  lwd=1.5, type='s')
}

title(main='Ontario - posterior inferences', outer=TRUE);

dev.off();

decade = decade + 1;

}

pdf("model_skill_hists.pdf", width=7.5, height=10)
layout(matrix(1:8, 4, 2, FALSE))
par(mar = c(2,4,2,4))
par(oma = c(3,2,2,2))

for(mdi in 1:length(modelSkillPoints)){
	hist(
		modelSkillPoints[[names(modelSkillPoints)[mdi]]],
		main=names(modelSkillPoints)[mdi]
	)
}
dev.off()

# Breaking the skill down
# m is a vector of months for the analysis (we'll get back to this)
# length(m) should equal the length of each model's skill point vector in the modelSkillPoints object
# The listing of each model begins with [lake][variable]
# So we can probably do something like...

modelListing = names(modelSkillPoints)

superiorPrecipListing = modelListing[grepl('superiorPrecip', modelListing, fixed=TRUE)]
superiorEvapListing = modelListing[grepl('superiorEvap', modelListing, fixed=TRUE)]
superiorRunoffListing = modelListing[grepl('superiorRunoff', modelListing, fixed=TRUE)]

miHuronPrecipListing = modelListing[grepl('miHuronPrecip', modelListing, fixed=TRUE)]
miHuronEvapListing = modelListing[grepl('miHuronEvap', modelListing, fixed=TRUE)]
miHuronRunoffListing = modelListing[grepl('miHuronRunoff', modelListing, fixed=TRUE)]

eriePrecipListing = modelListing[grepl('eriePrecip', modelListing, fixed=TRUE)]
erieEvapListing = modelListing[grepl('erieEvap', modelListing, fixed=TRUE)]
erieRunoffListing = modelListing[grepl('erieRunoff', modelListing, fixed=TRUE)]

ontarioPrecipListing = modelListing[grepl('ontarioPrecip', modelListing, fixed=TRUE)]
ontarioEvapListing = modelListing[grepl('ontarioEvap', modelListing, fixed=TRUE)]
ontarioRunoffListing = modelListing[grepl('ontarioRunoff', modelListing, fixed=TRUE)]

# Assuming the configuration and code aren't changed in odd ways, things should be concurrent across lakes. So...

### PRECIP

totPrecipHists = length(superiorPrecipListing)*4
# If we need to, we can sum(...) them all

pdf("model_skill_hists_precip.pdf", width=7.5, height=10)
layout(matrix(1:totPrecipHists, 4, length(superiorPrecipListing), TRUE)) # TRUE there is for by.row. Additionally, the length(...) may need to be modified if there is an inconsistent number of models across lakes for a component
par(mar = c(2,1,2,1))
par(oma = c(2,3,2,2))

for(mdi in 1:length(superiorPrecipListing)){
	hist(
		modelSkillPoints[[superiorPrecipListing[mdi]]],
		main=gsub('superiorPrecip', '', superiorPrecipListing[mdi], fixed=TRUE), col='dodgerblue2', border='dodgerblue4',
		ylab='', cex.main=0.75, cex.axis=0.75
	)
	
	if(mdi == 1){
		mtext('Superior', 2, line=2, cex=0.85)
	}
}

for(mdi in 1:length(miHuronPrecipListing)){
	hist(
		modelSkillPoints[[miHuronPrecipListing[mdi]]],
		main=gsub('miHuronPrecip', '', miHuronPrecipListing[mdi], fixed=TRUE), col='dodgerblue2', border='dodgerblue4',
		ylab='', cex.main=0.75, cex.axis=0.75
	)
	
	if(mdi == 1){
		mtext('Michigan-Huron', 2, line=2, cex=0.85)
	}
}

for(mdi in 1:length(eriePrecipListing)){
	hist(
		modelSkillPoints[[eriePrecipListing[mdi]]],
		main=gsub('eriePrecip', '', eriePrecipListing[mdi], fixed=TRUE), col='dodgerblue2', border='dodgerblue4',
		ylab='', cex.main=0.75, cex.axis=0.75
	)
	
	if(mdi == 1){
		mtext('Erie', 2, line=2, cex=0.85)
	}
}

for(mdi in 1:length(ontarioPrecipListing)){
	hist(
		modelSkillPoints[[ontarioPrecipListing[mdi]]],
		main=gsub('ontarioPrecip', '', ontarioPrecipListing[mdi], fixed=TRUE), col='dodgerblue2', border='dodgerblue4',
		ylab='', cex.main=0.75, cex.axis=0.75
	)
	
	if(mdi == 1){
		mtext('Ontario', 2, line=2, cex=0.85)
	}
}

dev.off()


### EVAP

totEvapHists = length(superiorEvapListing)*4
# If we need to, we can sum(...) them all

pdf("model_skill_hists_evap.pdf", width=7.5, height=10)
layout(matrix(1:totEvapHists, 4, length(superiorEvapListing), TRUE)) # TRUE there is for by.row. Additionally, the length(...) may need to be modified if there is an inconsistent number of models across lakes for a component
par(mar = c(2,1,2,1))
par(oma = c(2,3,2,2))

for(mdi in 1:length(superiorEvapListing)){
	hist(
		modelSkillPoints[[superiorEvapListing[mdi]]],
		main=gsub('superiorEvap', '', superiorEvapListing[mdi], fixed=TRUE), col='dodgerblue2', border='dodgerblue4',
		ylab='', cex.main=0.75, cex.axis=0.75
	)
	
	if(mdi == 1){
		mtext('Superior', 2, line=2, cex=0.85)
	}
}

for(mdi in 1:length(miHuronEvapListing)){
	hist(
		modelSkillPoints[[miHuronEvapListing[mdi]]],
		main=gsub('miHuronEvap', '', miHuronEvapListing[mdi], fixed=TRUE), col='dodgerblue2', border='dodgerblue4',
		ylab='', cex.main=0.75, cex.axis=0.75
	)
	
	if(mdi == 1){
		mtext('Michigan-Huron', 2, line=2, cex=0.85)
	}
}

for(mdi in 1:length(erieEvapListing)){
	hist(
		modelSkillPoints[[erieEvapListing[mdi]]],
		main=gsub('erieEvap', '', erieEvapListing[mdi], fixed=TRUE), col='dodgerblue2', border='dodgerblue4',
		ylab='', cex.main=0.75, cex.axis=0.75
	)
	
	if(mdi == 1){
		mtext('Erie', 2, line=2, cex=0.85)
	}
}

for(mdi in 1:length(ontarioEvapListing)){
	hist(
		modelSkillPoints[[ontarioEvapListing[mdi]]],
		main=gsub('ontarioEvap', '', ontarioEvapListing[mdi], fixed=TRUE), col='dodgerblue2', border='dodgerblue4',
		ylab='', cex.main=0.75, cex.axis=0.75
	)
	
	if(mdi == 1){
		mtext('Ontario', 2, line=2, cex=0.85)
	}
}

dev.off()


### RUNOFF

totRunoffHists = length(superiorRunoffListing)*4
# If we need to, we can sum(...) them all

pdf("model_skill_hists_runoff.pdf", width=7.5, height=10)
layout(matrix(1:totRunoffHists, 4, length(superiorRunoffListing), TRUE)) # TRUE there is for by.row. Additionally, the length(...) may need to be modified if there is an inconsistent number of models across lakes for a component
par(mar = c(2,1,2,1))
par(oma = c(2,3,2,2))

for(mdi in 1:length(superiorRunoffListing)){
	hist(
		modelSkillPoints[[superiorRunoffListing[mdi]]],
		main=gsub('superiorRunoff', '', superiorRunoffListing[mdi], fixed=TRUE), col='dodgerblue2', border='dodgerblue4',
		ylab='', cex.main=0.75, cex.axis=0.75
	)
	
	if(mdi == 1){
		mtext('Superior', 2, line=2, cex=0.85)
	}
}

for(mdi in 1:length(miHuronRunoffListing)){
	hist(
		modelSkillPoints[[miHuronRunoffListing[mdi]]],
		main=gsub('miHuronRunoff', '', miHuronRunoffListing[mdi], fixed=TRUE), col='dodgerblue2', border='dodgerblue4',
		ylab='', cex.main=0.75, cex.axis=0.75
	)
	
	if(mdi == 1){
		mtext('Michigan-Huron', 2, line=2, cex=0.85)
	}
}

for(mdi in 1:length(erieRunoffListing)){
	hist(
		modelSkillPoints[[erieRunoffListing[mdi]]],
		main=gsub('erieRunoff', '', erieRunoffListing[mdi], fixed=TRUE), col='dodgerblue2', border='dodgerblue4',
		ylab='', cex.main=0.75, cex.axis=0.75
	)
	
	if(mdi == 1){
		mtext('Erie', 2, line=2, cex=0.85)
	}
}

for(mdi in 1:length(ontarioRunoffListing)){
	hist(
		modelSkillPoints[[ontarioRunoffListing[mdi]]],
		main=gsub('ontarioRunoff', '', ontarioRunoffListing[mdi], fixed=TRUE), col='dodgerblue2', border='dodgerblue4',
		ylab='', cex.main=0.75, cex.axis=0.75
	)
	
	if(mdi == 1){
		mtext('Ontario', 2, line=2, cex=0.85)
	}
}

dev.off()


# If we're going to break these down by month, it may be visually sparse, as for an 120 month analysis, you get a scant 10 data points per month. However, it may be just as useful to make a data table of mean values. Using the 'm' month vector described above

meanModelSkillPVals = matrix(NA, length(names(modelSkillPoints)), 12, TRUE)
rownames(meanModelSkillPVals) = names(modelSkillPoints)

for(mdi in 1:length(modelSkillPoints)){
	for(mt in 1:12){
		meanModelSkillPVals[names(modelSkillPoints)[mdi],mt] = 
		mean(
			modelSkillPoints[[names(modelSkillPoints)[mdi]]][m == mt],
			na.rm=TRUE
		)
	}
}

write.table(meanModelSkillPVals, 'model_skill_pvals_bymonth.csv', append=FALSE, quote=FALSE, sep=',', row.names=TRUE, col.names=FALSE);

# Then pop that open in Excel/LibreOffice and apply some conditional formatting for quick browsing