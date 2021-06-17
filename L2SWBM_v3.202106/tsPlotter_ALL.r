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
	
	for(n in 3:ncol(superiorPrecip_A)){
		lines(c(i, i+1), c(superiorPrecip_A[i,n],superiorPrecip_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(superiorEvap_A)){
		lines(c(i, i+1), c(superiorEvap_A[i,n],superiorEvap_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(superiorRunoff_A)){
		lines(c(i, i+1), c(superiorRunoff_A[i,n],superiorRunoff_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(superiorNBS_A)){
		lines(c(i, i+1), c(superiorNBS_A[i,n],superiorNBS_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	
	for(n in 3:ncol(superiorOutflow_A)){
		lines(c(i, i+1), c(superiorOutflow_A[i,n],superiorOutflow_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(superiorDiversion_A)){
		lines(c(i, i+1), c(superiorDiversion_A[i,n],superiorDiversion_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(miHuronPrecip_A)){
		lines(c(i, i+1), c(miHuronPrecip_A[i,n],miHuronPrecip_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(miHuronEvap_A)){
		lines(c(i, i+1), c(miHuronEvap_A[i,n],miHuronEvap_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(miHuronRunoff_A)){
		lines(c(i, i+1), c(miHuronRunoff_A[i,n],miHuronRunoff_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(miHuronNBS_A)){
		lines(c(i, i+1), c(miHuronNBS_A[i,n],miHuronNBS_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	
	for(n in 3:ncol(miHuronOutflow_A)){
		lines(c(i, i+1), c(miHuronOutflow_A[i,n],miHuronOutflow_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(miHuronDiversion_A)){
		lines(c(i, i+1), c(miHuronDiversion_A[i,n],miHuronDiversion_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(clairPrecip_A)){
		lines(c(i, i+1), c(clairPrecip_A[i,n],clairPrecip_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(clairEvap_A)){
		lines(c(i, i+1), c(clairEvap_A[i,n],clairEvap_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(clairRunoff_A)){
		lines(c(i, i+1), c(clairRunoff_A[i,n],clairRunoff_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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

	for(n in 3:ncol(clairNBS_A)){
		lines(c(i, i+1), c(clairNBS_A[i,n],clairNBS_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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

	for(n in 3:ncol(clairOutflow_A)){
		lines(c(i, i+1), c(clairOutflow_A[i,n],clairOutflow_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(eriePrecip_A)){
		lines(c(i, i+1), c(eriePrecip_A[i,n],eriePrecip_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(erieEvap_A)){
		lines(c(i, i+1), c(erieEvap_A[i,n],erieEvap_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(erieRunoff_A)){
		lines(c(i, i+1), c(erieRunoff_A[i,n],erieRunoff_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(erieNBS_A)){
		lines(c(i, i+1), c(erieNBS_A[i,n],erieNBS_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	
	for(n in 3:ncol(erieOutflow_A)){
		lines(c(i, i+1), c(erieOutflow_A[i,n],erieOutflow_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(erieDiversion_A)){
		lines(c(i, i+1), c(erieDiversion_A[i,n],erieDiversion_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(ontarioPrecip_A)){
		lines(c(i, i+1), c(ontarioPrecip_A[i,n],ontarioPrecip_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(ontarioEvap_A)){
		lines(c(i, i+1), c(ontarioEvap_A[i,n],ontarioEvap_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(ontarioRunoff_A)){
		lines(c(i, i+1), c(ontarioRunoff_A[i,n],ontarioRunoff_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(ontarioNBS_A)){
		lines(c(i, i+1), c(ontarioNBS_A[i,n],ontarioNBS_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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
	
	for(n in 3:ncol(ontarioOutflow_A)){
		lines(c(i, i+1), c(ontarioOutflow_A[i,n],ontarioOutflow_A[i,n]), col = colorVector[n-2],  lwd=1.5, type='s')
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

