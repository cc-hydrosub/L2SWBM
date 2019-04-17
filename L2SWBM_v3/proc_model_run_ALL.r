### RUN MODEL
cat(paste('ALL LAKES, ',rollPeriod,' ROLL, 3 CHAINS, ',iters,' ITERATIONS\n\n', sep=''));

date();
startTime = proc.time()[3];

cat('ADAPTING SAMPLER TO MODEL...')

jMod = jags.model(
	file = paste(modelName,'.bug.r', sep=''),
	data = inputDataCoreJAGS,
	n.chains = 3
);
isAdapted = adapt(jMod);
while(!isAdapted){
	cat('More adapting...\n')
	adapt(jMod, 100)
}
gc();

# UPDATE FOR BURNIN
cat('UPDATE STEP (BURNIN)...\n')
update(jMod, halfIters)
gc();

# SAMPLE
cat('SAMPLING... (WITH THINNING)\n')
jSample = coda.samples(jMod, paramsToMonitor, halfIters, ceiling(halfIters/1000), na.rm=TRUE)

sampleEndTime = proc.time()[3] - startTime;

gc();

cat('COMPUTING STATS...\n')
jSumStats = summary(jSample)
jSumStats_MSD = jSumStats$statistics[,1:2]
jSumStats_Q = jSumStats$quantiles
jSumEff = effectiveSize(jSample)
gc();

# GET R-HATS
#cat('GETTING GELMAN-RUBIN STAT (COMPUTATIONALLY EXPENSIVE)...\n')
#jRHat = gelman.diag(jSample, multivariate=FALSE)
#jRHatEsts = jRHat$psrf

### Compose jSum
# Ensure row order

rn = rownames(jSumStats_MSD)
ro_MSD = match(rn, rownames(jSumStats_MSD))
ro_Q = match(rn, rownames(jSumStats_Q))
ro_eff = match(rn, names(jSumEff))
#ro_rhat = match(rn, rownames(jRHatEsts))

jSum = cbind(
	jSumStats_MSD,
	jSumStats_Q[ro_Q,],
	#jRHatEsts[ro_rhat,],
	jSumEff[ro_eff]
);

#colnames(jSum)[10] = 'n.eff'
colnames(jSum)[8] = 'n.eff'

date();
endTime = proc.time()[3] - startTime;
sampleToSumTime = endTime - sampleEndTime;
gc();
