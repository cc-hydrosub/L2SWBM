model {
	for (j in posteriorStartMonth:posteriorEndMonth){   										
		
		############################## 
		## Priors
		##############################

		### SUPERIOR
		superiorPrecip[j] ~ dgamma(superiorPriorPrecipShape[m[j]], superiorPriorPrecipRate[m[j]])
		superiorEvap[j] ~ dnorm(superiorEvapPriorMean[m[j]], superiorEvapPriorPrecision[m[j]])
		superiorRunoff[j] 	<- exp(superiorLogRunoff[j])
		superiorLogRunoff[j] ~ dnorm(superiorRunoffLogPriorMean[m[j]], superiorRunoffLogPriorPrecision[m[j]])	
		superiorOutflow[j] ~ dnorm(superiorOutflowPriorMean[m[j]], superiorOutflowPriorPrecision[m[j]])
		superiorDiversion[j] ~ dnorm(superiorDiversionPriorMean[m[j]], superiorDiversionPriorPrecision[m[j]])


		### SUPERIOR
		miHuronPrecip[j] ~ dgamma(miHuronPriorPrecipShape[m[j]], miHuronPriorPrecipRate[m[j]])
		miHuronEvap[j] ~ dnorm(miHuronEvapPriorMean[m[j]], miHuronEvapPriorPrecision[m[j]])
		miHuronRunoff[j] 	<- exp(miHuronLogRunoff[j])
		miHuronLogRunoff[j] ~ dnorm(miHuronRunoffLogPriorMean[m[j]], miHuronRunoffLogPriorPrecision[m[j]])	
		miHuronOutflow[j] ~ dnorm(miHuronOutflowPriorMean[m[j]], miHuronOutflowPriorPrecision[m[j]])
		miHuronDiversion[j] ~ dnorm(miHuronDiversionPriorMean[m[j]], miHuronDiversionPriorPrecision[m[j]])


		### SUPERIOR
		clairNBS[j] ~ dnorm(clairNBSPriorMean[m[j]], clairNBSPriorPrecision[m[j]])
		clairOutflow[j] ~ dnorm(clairOutflowPriorMean[m[j]], clairOutflowPriorPrecision[m[j]])
		

		### SUPERIOR
		eriePrecip[j] ~ dgamma(eriePriorPrecipShape[m[j]], eriePriorPrecipRate[m[j]])
		erieEvap[j] ~ dnorm(erieEvapPriorMean[m[j]], erieEvapPriorPrecision[m[j]])
		erieRunoff[j] 	<- exp(erieLogRunoff[j])
		erieLogRunoff[j] ~ dnorm(erieRunoffLogPriorMean[m[j]], erieRunoffLogPriorPrecision[m[j]])	
		erieOutflow[j] ~ dnorm(erieOutflowPriorMean[m[j]], erieOutflowPriorPrecision[m[j]])
		erieDiversion[j] ~ dnorm(erieDiversionPriorMean[m[j]], erieDiversionPriorPrecision[m[j]])


		### SUPERIOR
		ontarioPrecip[j] ~ dgamma(ontarioPriorPrecipShape[m[j]], ontarioPriorPrecipRate[m[j]])
		ontarioEvap[j] ~ dnorm(ontarioEvapPriorMean[m[j]], ontarioEvapPriorPrecision[m[j]])
		ontarioRunoff[j] 	<- exp(ontarioLogRunoff[j])
		ontarioLogRunoff[j] ~ dnorm(ontarioRunoffLogPriorMean[m[j]], ontarioRunoffLogPriorPrecision[m[j]])	
		ontarioOutflow[j] ~ dnorm(ontarioOutflowPriorMean[m[j]], ontarioOutflowPriorPrecision[m[j]])



		####################################################
		## LIKELIHOOD FUNCTIONS
		####################################################

		ySuperiorOutflow1[j] ~ dnorm(ySuperiorOutflow1Mean[j], ySuperiorOutflow1Prec[m[j]])
		ySuperiorOutflow1Mean[j] <- superiorOutflow[j] + ySuperiorOutflow1Bias[m[j]]

		ySuperiorOutflow2[j] ~ dnorm(ySuperiorOutflow2Mean[j], ySuperiorOutflow2Prec[m[j]])
		ySuperiorOutflow2Mean[j] <- superiorOutflow[j] + ySuperiorOutflow2Bias[m[j]]

		yMiHuronOutflow1[j] ~ dnorm(yMiHuronOutflow1Mean[j], yMiHuronOutflow1Prec[m[j]])
		yMiHuronOutflow1Mean[j] <- miHuronOutflow[j] + yMiHuronOutflow1Bias[m[j]]

		yMiHuronOutflow2[j] ~ dnorm(yMiHuronOutflow2Mean[j], yMiHuronOutflow2Prec[m[j]])
		yMiHuronOutflow2Mean[j] <- miHuronOutflow[j] + yMiHuronOutflow2Bias[m[j]]

		yClairOutflow1[j] ~ dnorm(yClairOutflow1Mean[j], yClairOutflow1Prec[m[j]])
		yClairOutflow1Mean[j] <- clairOutflow[j] + yClairOutflow1Bias[m[j]]

		yClairOutflow2[j] ~ dnorm(yClairOutflow2Mean[j], yClairOutflow2Prec[m[j]])
		yClairOutflow2Mean[j] <- clairOutflow[j] + yClairOutflow2Bias[m[j]]

		yErieOutflow1[j] ~ dnorm(yErieOutflow1Mean[j], yErieOutflow1Prec[m[j]])
		yErieOutflow1Mean[j] <- erieOutflow[j] + yErieOutflow1Bias[m[j]]

		yOntarioOutflow1[j] ~ dnorm(yOntarioOutflow1Mean[j], yOntarioOutflow1Prec[m[j]])
		yOntarioOutflow1Mean[j] <- ontarioOutflow[j] + yOntarioOutflow1Bias[m[j]]

		ySuperiorDiversion1[j] ~ dnorm(ySuperiorDiversion1Mean[j], ySuperiorDiversion1Prec[m[j]])
		ySuperiorDiversion1Mean[j] <- superiorDiversion[j] + ySuperiorDiversion1Bias[m[j]]

		yMiHuronDiversion1[j] ~ dnorm(yMiHuronDiversion1Mean[j], yMiHuronDiversion1Prec[m[j]])
		yMiHuronDiversion1Mean[j] <- miHuronDiversion[j] + yMiHuronDiversion1Bias[m[j]]

		yErieDiversion1[j] ~ dnorm(yErieDiversion1Mean[j], yErieDiversion1Prec[m[j]])
		yErieDiversion1Mean[j] <- erieDiversion[j] + yErieDiversion1Bias[m[j]]

		ySuperiorPrecip1[j] ~ dnorm(ySuperiorPrecip1Mean[j], ySuperiorPrecip1Prec)
		ySuperiorPrecip1Mean[j] <- superiorPrecip[j] + ySuperiorPrecip1Bias[m[j]]

		ySuperiorPrecip2[j] ~ dnorm(ySuperiorPrecip2Mean[j], ySuperiorPrecip2Prec)
		ySuperiorPrecip2Mean[j] <- superiorPrecip[j] + ySuperiorPrecip2Bias[m[j]]

		ySuperiorPrecip3[j] ~ dnorm(ySuperiorPrecip3Mean[j], ySuperiorPrecip3Prec)
		ySuperiorPrecip3Mean[j] <- superiorPrecip[j] + ySuperiorPrecip3Bias[m[j]]

		ySuperiorPrecip4[j] ~ dnorm(ySuperiorPrecip4Mean[j], ySuperiorPrecip4Prec)
		ySuperiorPrecip4Mean[j] <- superiorPrecip[j] + ySuperiorPrecip4Bias[m[j]]

		ySuperiorPrecip5[j] ~ dnorm(ySuperiorPrecip5Mean[j], ySuperiorPrecip5Prec)
		ySuperiorPrecip5Mean[j] <- superiorPrecip[j] + ySuperiorPrecip5Bias[m[j]]

		ySuperiorPrecip6[j] ~ dnorm(ySuperiorPrecip6Mean[j], ySuperiorPrecip6Prec)
		ySuperiorPrecip6Mean[j] <- superiorPrecip[j] + ySuperiorPrecip6Bias[m[j]]

		yMiHuronPrecip1[j] ~ dnorm(yMiHuronPrecip1Mean[j], yMiHuronPrecip1Prec)
		yMiHuronPrecip1Mean[j] <- miHuronPrecip[j] + yMiHuronPrecip1Bias[m[j]]

		yMiHuronPrecip2[j] ~ dnorm(yMiHuronPrecip2Mean[j], yMiHuronPrecip2Prec)
		yMiHuronPrecip2Mean[j] <- miHuronPrecip[j] + yMiHuronPrecip2Bias[m[j]]

		yMiHuronPrecip3[j] ~ dnorm(yMiHuronPrecip3Mean[j], yMiHuronPrecip3Prec)
		yMiHuronPrecip3Mean[j] <- miHuronPrecip[j] + yMiHuronPrecip3Bias[m[j]]

		yMiHuronPrecip4[j] ~ dnorm(yMiHuronPrecip4Mean[j], yMiHuronPrecip4Prec)
		yMiHuronPrecip4Mean[j] <- miHuronPrecip[j] + yMiHuronPrecip4Bias[m[j]]

		yMiHuronPrecip5[j] ~ dnorm(yMiHuronPrecip5Mean[j], yMiHuronPrecip5Prec)
		yMiHuronPrecip5Mean[j] <- miHuronPrecip[j] + yMiHuronPrecip5Bias[m[j]]

		yMiHuronPrecip6[j] ~ dnorm(yMiHuronPrecip6Mean[j], yMiHuronPrecip6Prec)
		yMiHuronPrecip6Mean[j] <- miHuronPrecip[j] + yMiHuronPrecip6Bias[m[j]]

		yEriePrecip1[j] ~ dnorm(yEriePrecip1Mean[j], yEriePrecip1Prec)
		yEriePrecip1Mean[j] <- eriePrecip[j] + yEriePrecip1Bias[m[j]]

		yEriePrecip2[j] ~ dnorm(yEriePrecip2Mean[j], yEriePrecip2Prec)
		yEriePrecip2Mean[j] <- eriePrecip[j] + yEriePrecip2Bias[m[j]]

		yEriePrecip3[j] ~ dnorm(yEriePrecip3Mean[j], yEriePrecip3Prec)
		yEriePrecip3Mean[j] <- eriePrecip[j] + yEriePrecip3Bias[m[j]]

		yEriePrecip4[j] ~ dnorm(yEriePrecip4Mean[j], yEriePrecip4Prec)
		yEriePrecip4Mean[j] <- eriePrecip[j] + yEriePrecip4Bias[m[j]]

		yEriePrecip5[j] ~ dnorm(yEriePrecip5Mean[j], yEriePrecip5Prec)
		yEriePrecip5Mean[j] <- eriePrecip[j] + yEriePrecip5Bias[m[j]]

		yEriePrecip6[j] ~ dnorm(yEriePrecip6Mean[j], yEriePrecip6Prec)
		yEriePrecip6Mean[j] <- eriePrecip[j] + yEriePrecip6Bias[m[j]]

		yOntarioPrecip1[j] ~ dnorm(yOntarioPrecip1Mean[j], yOntarioPrecip1Prec)
		yOntarioPrecip1Mean[j] <- ontarioPrecip[j] + yOntarioPrecip1Bias[m[j]]

		yOntarioPrecip2[j] ~ dnorm(yOntarioPrecip2Mean[j], yOntarioPrecip2Prec)
		yOntarioPrecip2Mean[j] <- ontarioPrecip[j] + yOntarioPrecip2Bias[m[j]]

		yOntarioPrecip3[j] ~ dnorm(yOntarioPrecip3Mean[j], yOntarioPrecip3Prec)
		yOntarioPrecip3Mean[j] <- ontarioPrecip[j] + yOntarioPrecip3Bias[m[j]]

		yOntarioPrecip4[j] ~ dnorm(yOntarioPrecip4Mean[j], yOntarioPrecip4Prec)
		yOntarioPrecip4Mean[j] <- ontarioPrecip[j] + yOntarioPrecip4Bias[m[j]]

		yOntarioPrecip5[j] ~ dnorm(yOntarioPrecip5Mean[j], yOntarioPrecip5Prec)
		yOntarioPrecip5Mean[j] <- ontarioPrecip[j] + yOntarioPrecip5Bias[m[j]]

		yOntarioPrecip6[j] ~ dnorm(yOntarioPrecip6Mean[j], yOntarioPrecip6Prec)
		yOntarioPrecip6Mean[j] <- ontarioPrecip[j] + yOntarioPrecip6Bias[m[j]]

		ySuperiorEvap1[j] ~ dnorm(ySuperiorEvap1Mean[j], ySuperiorEvap1Prec)
		ySuperiorEvap1Mean[j] <- superiorEvap[j] + ySuperiorEvap1Bias[m[j]]

		ySuperiorEvap2[j] ~ dnorm(ySuperiorEvap2Mean[j], ySuperiorEvap2Prec)
		ySuperiorEvap2Mean[j] <- superiorEvap[j] + ySuperiorEvap2Bias[m[j]]

		ySuperiorEvap3[j] ~ dnorm(ySuperiorEvap3Mean[j], ySuperiorEvap3Prec)
		ySuperiorEvap3Mean[j] <- superiorEvap[j] + ySuperiorEvap3Bias[m[j]]

		yMiHuronEvap1[j] ~ dnorm(yMiHuronEvap1Mean[j], yMiHuronEvap1Prec)
		yMiHuronEvap1Mean[j] <- miHuronEvap[j] + yMiHuronEvap1Bias[m[j]]

		yMiHuronEvap2[j] ~ dnorm(yMiHuronEvap2Mean[j], yMiHuronEvap2Prec)
		yMiHuronEvap2Mean[j] <- miHuronEvap[j] + yMiHuronEvap2Bias[m[j]]

		yMiHuronEvap3[j] ~ dnorm(yMiHuronEvap3Mean[j], yMiHuronEvap3Prec)
		yMiHuronEvap3Mean[j] <- miHuronEvap[j] + yMiHuronEvap3Bias[m[j]]

		yErieEvap1[j] ~ dnorm(yErieEvap1Mean[j], yErieEvap1Prec)
		yErieEvap1Mean[j] <- erieEvap[j] + yErieEvap1Bias[m[j]]

		yErieEvap2[j] ~ dnorm(yErieEvap2Mean[j], yErieEvap2Prec)
		yErieEvap2Mean[j] <- erieEvap[j] + yErieEvap2Bias[m[j]]

		yErieEvap3[j] ~ dnorm(yErieEvap3Mean[j], yErieEvap3Prec)
		yErieEvap3Mean[j] <- erieEvap[j] + yErieEvap3Bias[m[j]]

		yOntarioEvap1[j] ~ dnorm(yOntarioEvap1Mean[j], yOntarioEvap1Prec)
		yOntarioEvap1Mean[j] <- ontarioEvap[j] + yOntarioEvap1Bias[m[j]]

		yOntarioEvap2[j] ~ dnorm(yOntarioEvap2Mean[j], yOntarioEvap2Prec)
		yOntarioEvap2Mean[j] <- ontarioEvap[j] + yOntarioEvap2Bias[m[j]]

		yOntarioEvap3[j] ~ dnorm(yOntarioEvap3Mean[j], yOntarioEvap3Prec)
		yOntarioEvap3Mean[j] <- ontarioEvap[j] + yOntarioEvap3Bias[m[j]]

		ySuperiorRunoff1[j] ~ dnorm(ySuperiorRunoff1Mean[j], ySuperiorRunoff1Prec)
		ySuperiorRunoff1Mean[j] <- superiorRunoff[j] + ySuperiorRunoff1Bias[m[j]]

		ySuperiorRunoff2[j] ~ dnorm(ySuperiorRunoff2Mean[j], ySuperiorRunoff2Prec)
		ySuperiorRunoff2Mean[j] <- superiorRunoff[j] + ySuperiorRunoff2Bias[m[j]]

		ySuperiorRunoff3[j] ~ dnorm(ySuperiorRunoff3Mean[j], ySuperiorRunoff3Prec)
		ySuperiorRunoff3Mean[j] <- superiorRunoff[j] + ySuperiorRunoff3Bias[m[j]]

		ySuperiorRunoff4[j] ~ dnorm(ySuperiorRunoff4Mean[j], ySuperiorRunoff4Prec)
		ySuperiorRunoff4Mean[j] <- superiorRunoff[j] + ySuperiorRunoff4Bias[m[j]]

		ySuperiorRunoff5[j] ~ dnorm(ySuperiorRunoff5Mean[j], ySuperiorRunoff5Prec)
		ySuperiorRunoff5Mean[j] <- superiorRunoff[j] + ySuperiorRunoff5Bias[m[j]]

		yMiHuronRunoff1[j] ~ dnorm(yMiHuronRunoff1Mean[j], yMiHuronRunoff1Prec)
		yMiHuronRunoff1Mean[j] <- miHuronRunoff[j] + yMiHuronRunoff1Bias[m[j]]

		yMiHuronRunoff2[j] ~ dnorm(yMiHuronRunoff2Mean[j], yMiHuronRunoff2Prec)
		yMiHuronRunoff2Mean[j] <- miHuronRunoff[j] + yMiHuronRunoff2Bias[m[j]]

		yMiHuronRunoff3[j] ~ dnorm(yMiHuronRunoff3Mean[j], yMiHuronRunoff3Prec)
		yMiHuronRunoff3Mean[j] <- miHuronRunoff[j] + yMiHuronRunoff3Bias[m[j]]

		yMiHuronRunoff4[j] ~ dnorm(yMiHuronRunoff4Mean[j], yMiHuronRunoff4Prec)
		yMiHuronRunoff4Mean[j] <- miHuronRunoff[j] + yMiHuronRunoff4Bias[m[j]]

		yMiHuronRunoff5[j] ~ dnorm(yMiHuronRunoff5Mean[j], yMiHuronRunoff5Prec)
		yMiHuronRunoff5Mean[j] <- miHuronRunoff[j] + yMiHuronRunoff5Bias[m[j]]

		yErieRunoff1[j] ~ dnorm(yErieRunoff1Mean[j], yErieRunoff1Prec)
		yErieRunoff1Mean[j] <- erieRunoff[j] + yErieRunoff1Bias[m[j]]

		yErieRunoff2[j] ~ dnorm(yErieRunoff2Mean[j], yErieRunoff2Prec)
		yErieRunoff2Mean[j] <- erieRunoff[j] + yErieRunoff2Bias[m[j]]

		yErieRunoff3[j] ~ dnorm(yErieRunoff3Mean[j], yErieRunoff3Prec)
		yErieRunoff3Mean[j] <- erieRunoff[j] + yErieRunoff3Bias[m[j]]

		yErieRunoff4[j] ~ dnorm(yErieRunoff4Mean[j], yErieRunoff4Prec)
		yErieRunoff4Mean[j] <- erieRunoff[j] + yErieRunoff4Bias[m[j]]

		yErieRunoff5[j] ~ dnorm(yErieRunoff5Mean[j], yErieRunoff5Prec)
		yErieRunoff5Mean[j] <- erieRunoff[j] + yErieRunoff5Bias[m[j]]

		yOntarioRunoff1[j] ~ dnorm(yOntarioRunoff1Mean[j], yOntarioRunoff1Prec)
		yOntarioRunoff1Mean[j] <- ontarioRunoff[j] + yOntarioRunoff1Bias[m[j]]

		yOntarioRunoff2[j] ~ dnorm(yOntarioRunoff2Mean[j], yOntarioRunoff2Prec)
		yOntarioRunoff2Mean[j] <- ontarioRunoff[j] + yOntarioRunoff2Bias[m[j]]

		yOntarioRunoff3[j] ~ dnorm(yOntarioRunoff3Mean[j], yOntarioRunoff3Prec)
		yOntarioRunoff3Mean[j] <- ontarioRunoff[j] + yOntarioRunoff3Bias[m[j]]

		yOntarioRunoff4[j] ~ dnorm(yOntarioRunoff4Mean[j], yOntarioRunoff4Prec)
		yOntarioRunoff4Mean[j] <- ontarioRunoff[j] + yOntarioRunoff4Bias[m[j]]

		yOntarioRunoff5[j] ~ dnorm(yOntarioRunoff5Mean[j], yOntarioRunoff5Prec)
		yOntarioRunoff5Mean[j] <- ontarioRunoff[j] + yOntarioRunoff5Bias[m[j]]

		yClairNBS1[j] ~ dnorm(yClairNBS1Mean[j], yClairNBS1Prec)
		yClairNBS1Mean[j] <- clairNBS[j] + yClairNBS1Bias[m[j]]

	}
	### Rolling Period Restraints
	for(k in rollPeriod:posteriorEndMonth){

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
		

		### CLAIR
		yClairRStore[k] ~ dnorm(clairRStore[k], yClairRStorePrec) 
		
		clairRStore[k] <- (
			sum(clairNBS[(k-rollPeriod+1):k])
			+ sum(miHuronOutflow[(k-rollPeriod+1):k])
			- sum(clairOutflow[(k-rollPeriod+1):k])
			+sum(clairProcError[m[(k-rollPeriod+1):k]])
		)		

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
		
	
	}
	
	#############################
	## BIAS TERMS 
	#############################
	for (i in 1:12){
	
		superiorProcError[i] ~ dnorm(0,0.01)
		miHuronProcError[i] ~ dnorm(0,0.01)
		clairProcError[i] ~ dnorm(0,0.0625)
		erieProcError[i] ~ dnorm(0,0.01)
		ontarioProcError[i] ~ dnorm(0,0.01)

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
		ySuperiorPrecip1Bias[i] ~ dnorm(0,0.01)
		ySuperiorPrecip2Bias[i] ~ dnorm(0,0.01)
		ySuperiorPrecip3Bias[i] ~ dnorm(0,0.01)
		ySuperiorPrecip4Bias[i] ~ dnorm(0,0.01)
		ySuperiorPrecip5Bias[i] ~ dnorm(0,0.01)
		ySuperiorPrecip6Bias[i] ~ dnorm(0,0.01)
		yMiHuronPrecip1Bias[i] ~ dnorm(0,0.01)
		yMiHuronPrecip2Bias[i] ~ dnorm(0,0.01)
		yMiHuronPrecip3Bias[i] ~ dnorm(0,0.01)
		yMiHuronPrecip4Bias[i] ~ dnorm(0,0.01)
		yMiHuronPrecip5Bias[i] ~ dnorm(0,0.01)
		yMiHuronPrecip6Bias[i] ~ dnorm(0,0.01)
		yEriePrecip1Bias[i] ~ dnorm(0,0.01)
		yEriePrecip2Bias[i] ~ dnorm(0,0.01)
		yEriePrecip3Bias[i] ~ dnorm(0,0.01)
		yEriePrecip4Bias[i] ~ dnorm(0,0.01)
		yEriePrecip5Bias[i] ~ dnorm(0,0.01)
		yEriePrecip6Bias[i] ~ dnorm(0,0.01)
		yOntarioPrecip1Bias[i] ~ dnorm(0,0.01)
		yOntarioPrecip2Bias[i] ~ dnorm(0,0.01)
		yOntarioPrecip3Bias[i] ~ dnorm(0,0.01)
		yOntarioPrecip4Bias[i] ~ dnorm(0,0.01)
		yOntarioPrecip5Bias[i] ~ dnorm(0,0.01)
		yOntarioPrecip6Bias[i] ~ dnorm(0,0.01)
		ySuperiorEvap1Bias[i] ~ dnorm(0,0.01)
		ySuperiorEvap2Bias[i] ~ dnorm(0,0.01)
		ySuperiorEvap3Bias[i] ~ dnorm(0,0.01)
		yMiHuronEvap1Bias[i] ~ dnorm(0,0.01)
		yMiHuronEvap2Bias[i] ~ dnorm(0,0.01)
		yMiHuronEvap3Bias[i] ~ dnorm(0,0.01)
		yErieEvap1Bias[i] ~ dnorm(0,0.01)
		yErieEvap2Bias[i] ~ dnorm(0,0.01)
		yErieEvap3Bias[i] ~ dnorm(0,0.01)
		yOntarioEvap1Bias[i] ~ dnorm(0,0.01)
		yOntarioEvap2Bias[i] ~ dnorm(0,0.01)
		yOntarioEvap3Bias[i] ~ dnorm(0,0.01)
		ySuperiorRunoff1Bias[i] ~ dnorm(0,0.01)
		ySuperiorRunoff2Bias[i] ~ dnorm(0,0.01)
		ySuperiorRunoff3Bias[i] ~ dnorm(0,0.01)
		ySuperiorRunoff4Bias[i] ~ dnorm(0,0.01)
		ySuperiorRunoff5Bias[i] ~ dnorm(0,0.01)
		yMiHuronRunoff1Bias[i] ~ dnorm(0,0.01)
		yMiHuronRunoff2Bias[i] ~ dnorm(0,0.01)
		yMiHuronRunoff3Bias[i] ~ dnorm(0,0.01)
		yMiHuronRunoff4Bias[i] ~ dnorm(0,0.01)
		yMiHuronRunoff5Bias[i] ~ dnorm(0,0.01)
		yErieRunoff1Bias[i] ~ dnorm(0,0.01)
		yErieRunoff2Bias[i] ~ dnorm(0,0.01)
		yErieRunoff3Bias[i] ~ dnorm(0,0.01)
		yErieRunoff4Bias[i] ~ dnorm(0,0.01)
		yErieRunoff5Bias[i] ~ dnorm(0,0.01)
		yOntarioRunoff1Bias[i] ~ dnorm(0,0.01)
		yOntarioRunoff2Bias[i] ~ dnorm(0,0.01)
		yOntarioRunoff3Bias[i] ~ dnorm(0,0.01)
		yOntarioRunoff4Bias[i] ~ dnorm(0,0.01)
		yOntarioRunoff5Bias[i] ~ dnorm(0,0.01)
		yClairNBS1Bias[i] ~ dnorm(0,0.00694444444444444)

	} # END BIAS AND PROC-ERROR LOOP
	
		##############################
		## PRECISION FOR OBSERVATIONS
		##############################  
		
		ySuperiorRStorePrec	 = 1/(2^2*rollPeriod)	
		yMiHuronRStorePrec = 1/(2^2*rollPeriod)
		yClairRStorePrec = 1/(0.8^2*rollPeriod)
		yErieRStorePrec = 1/(2^2*rollPeriod)
		yOntarioRStorePrec = 1/(2^2*rollPeriod) 

		
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
	ySuperiorPrecip1Prec ~ dgamma(0.1,0.1)
	ySuperiorPrecip2Prec ~ dgamma(0.1,0.1)
	ySuperiorPrecip3Prec ~ dgamma(0.1,0.1)
	ySuperiorPrecip4Prec ~ dgamma(0.1,0.1)
	ySuperiorPrecip5Prec ~ dgamma(0.1,0.1)
	ySuperiorPrecip6Prec ~ dgamma(0.1,0.1)
	yMiHuronPrecip1Prec ~ dgamma(0.1,0.1)
	yMiHuronPrecip2Prec ~ dgamma(0.1,0.1)
	yMiHuronPrecip3Prec ~ dgamma(0.1,0.1)
	yMiHuronPrecip4Prec ~ dgamma(0.1,0.1)
	yMiHuronPrecip5Prec ~ dgamma(0.1,0.1)
	yMiHuronPrecip6Prec ~ dgamma(0.1,0.1)
	yEriePrecip1Prec ~ dgamma(0.1,0.1)
	yEriePrecip2Prec ~ dgamma(0.1,0.1)
	yEriePrecip3Prec ~ dgamma(0.1,0.1)
	yEriePrecip4Prec ~ dgamma(0.1,0.1)
	yEriePrecip5Prec ~ dgamma(0.1,0.1)
	yEriePrecip6Prec ~ dgamma(0.1,0.1)
	yOntarioPrecip1Prec ~ dgamma(0.1,0.1)
	yOntarioPrecip2Prec ~ dgamma(0.1,0.1)
	yOntarioPrecip3Prec ~ dgamma(0.1,0.1)
	yOntarioPrecip4Prec ~ dgamma(0.1,0.1)
	yOntarioPrecip5Prec ~ dgamma(0.1,0.1)
	yOntarioPrecip6Prec ~ dgamma(0.1,0.1)
	ySuperiorEvap1Prec ~ dgamma(0.1,0.1)
	ySuperiorEvap2Prec ~ dgamma(0.1,0.1)
	ySuperiorEvap3Prec ~ dgamma(0.1,0.1)
	yMiHuronEvap1Prec ~ dgamma(0.1,0.1)
	yMiHuronEvap2Prec ~ dgamma(0.1,0.1)
	yMiHuronEvap3Prec ~ dgamma(0.1,0.1)
	yErieEvap1Prec ~ dgamma(0.1,0.1)
	yErieEvap2Prec ~ dgamma(0.1,0.1)
	yErieEvap3Prec ~ dgamma(0.1,0.1)
	yOntarioEvap1Prec ~ dgamma(0.1,0.1)
	yOntarioEvap2Prec ~ dgamma(0.1,0.1)
	yOntarioEvap3Prec ~ dgamma(0.1,0.1)
	ySuperiorRunoff1Prec ~ dgamma(0.1,0.1)
	ySuperiorRunoff2Prec ~ dgamma(0.1,0.1)
	ySuperiorRunoff3Prec ~ dgamma(0.1,0.1)
	ySuperiorRunoff4Prec ~ dgamma(0.1,0.1)
	ySuperiorRunoff5Prec ~ dgamma(0.1,0.1)
	yMiHuronRunoff1Prec ~ dgamma(0.1,0.1)
	yMiHuronRunoff2Prec ~ dgamma(0.1,0.1)
	yMiHuronRunoff3Prec ~ dgamma(0.1,0.1)
	yMiHuronRunoff4Prec ~ dgamma(0.1,0.1)
	yMiHuronRunoff5Prec ~ dgamma(0.1,0.1)
	yErieRunoff1Prec ~ dgamma(0.1,0.1)
	yErieRunoff2Prec ~ dgamma(0.1,0.1)
	yErieRunoff3Prec ~ dgamma(0.1,0.1)
	yErieRunoff4Prec ~ dgamma(0.1,0.1)
	yErieRunoff5Prec ~ dgamma(0.1,0.1)
	yOntarioRunoff1Prec ~ dgamma(0.1,0.1)
	yOntarioRunoff2Prec ~ dgamma(0.1,0.1)
	yOntarioRunoff3Prec ~ dgamma(0.1,0.1)
	yOntarioRunoff4Prec ~ dgamma(0.1,0.1)
	yOntarioRunoff5Prec ~ dgamma(0.1,0.1)
	yClairNBS1Prec ~ dgamma(0.1,0.1)

	################################################################
	# POSTERIOR PREDICTIVE DRAWS FOR VERIFICATION (TODO FOR ALL LAKES, INC FLOW COMPUTATIONS)
	################################################################
	
	### BUDGET COMPONENTS
	
	for(jp in posteriorStartMonth:posteriorEndMonth){	
		# MONTH BY MONTH CHANGE IN STORAGE ANALYSIS


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

		yClairDStorePP[jp] ~ dnorm(clairDStore[jp], yClairRStorePrec);
		
		clairDStore[jp] <- (
			clairNBS[jp]
			+ miHuronOutflow[jp]
			- clairOutflow[jp]
			+ clairProcError[m[jp]]
		)
		

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
		
	}

	################################################################
	# POSTERIOR PREDICTIVE DRAWS FOR VERIFICATION (TODO FOR ALL LAKES, INC FLOW COMPUTATIONS)
	################################################################
	for(jp in posteriorStartMonth:posteriorEndMonth){		
		
		ySuperiorPrecip1PP[jp] ~ dnorm(ySuperiorPrecip1Mean[jp], ySuperiorPrecip1Prec)
		ySuperiorPrecip2PP[jp] ~ dnorm(ySuperiorPrecip2Mean[jp], ySuperiorPrecip2Prec)
		ySuperiorPrecip3PP[jp] ~ dnorm(ySuperiorPrecip3Mean[jp], ySuperiorPrecip3Prec)
		ySuperiorPrecip4PP[jp] ~ dnorm(ySuperiorPrecip4Mean[jp], ySuperiorPrecip4Prec)
		ySuperiorPrecip5PP[jp] ~ dnorm(ySuperiorPrecip5Mean[jp], ySuperiorPrecip5Prec)
		ySuperiorPrecip6PP[jp] ~ dnorm(ySuperiorPrecip6Mean[jp], ySuperiorPrecip6Prec)
		yMiHuronPrecip1PP[jp] ~ dnorm(yMiHuronPrecip1Mean[jp], yMiHuronPrecip1Prec)
		yMiHuronPrecip2PP[jp] ~ dnorm(yMiHuronPrecip2Mean[jp], yMiHuronPrecip2Prec)
		yMiHuronPrecip3PP[jp] ~ dnorm(yMiHuronPrecip3Mean[jp], yMiHuronPrecip3Prec)
		yMiHuronPrecip4PP[jp] ~ dnorm(yMiHuronPrecip4Mean[jp], yMiHuronPrecip4Prec)
		yMiHuronPrecip5PP[jp] ~ dnorm(yMiHuronPrecip5Mean[jp], yMiHuronPrecip5Prec)
		yMiHuronPrecip6PP[jp] ~ dnorm(yMiHuronPrecip6Mean[jp], yMiHuronPrecip6Prec)
		yEriePrecip1PP[jp] ~ dnorm(yEriePrecip1Mean[jp], yEriePrecip1Prec)
		yEriePrecip2PP[jp] ~ dnorm(yEriePrecip2Mean[jp], yEriePrecip2Prec)
		yEriePrecip3PP[jp] ~ dnorm(yEriePrecip3Mean[jp], yEriePrecip3Prec)
		yEriePrecip4PP[jp] ~ dnorm(yEriePrecip4Mean[jp], yEriePrecip4Prec)
		yEriePrecip5PP[jp] ~ dnorm(yEriePrecip5Mean[jp], yEriePrecip5Prec)
		yEriePrecip6PP[jp] ~ dnorm(yEriePrecip6Mean[jp], yEriePrecip6Prec)
		yOntarioPrecip1PP[jp] ~ dnorm(yOntarioPrecip1Mean[jp], yOntarioPrecip1Prec)
		yOntarioPrecip2PP[jp] ~ dnorm(yOntarioPrecip2Mean[jp], yOntarioPrecip2Prec)
		yOntarioPrecip3PP[jp] ~ dnorm(yOntarioPrecip3Mean[jp], yOntarioPrecip3Prec)
		yOntarioPrecip4PP[jp] ~ dnorm(yOntarioPrecip4Mean[jp], yOntarioPrecip4Prec)
		yOntarioPrecip5PP[jp] ~ dnorm(yOntarioPrecip5Mean[jp], yOntarioPrecip5Prec)
		yOntarioPrecip6PP[jp] ~ dnorm(yOntarioPrecip6Mean[jp], yOntarioPrecip6Prec)
		ySuperiorEvap1PP[jp] ~ dnorm(ySuperiorEvap1Mean[jp], ySuperiorEvap1Prec)
		ySuperiorEvap2PP[jp] ~ dnorm(ySuperiorEvap2Mean[jp], ySuperiorEvap2Prec)
		ySuperiorEvap3PP[jp] ~ dnorm(ySuperiorEvap3Mean[jp], ySuperiorEvap3Prec)
		yMiHuronEvap1PP[jp] ~ dnorm(yMiHuronEvap1Mean[jp], yMiHuronEvap1Prec)
		yMiHuronEvap2PP[jp] ~ dnorm(yMiHuronEvap2Mean[jp], yMiHuronEvap2Prec)
		yMiHuronEvap3PP[jp] ~ dnorm(yMiHuronEvap3Mean[jp], yMiHuronEvap3Prec)
		yErieEvap1PP[jp] ~ dnorm(yErieEvap1Mean[jp], yErieEvap1Prec)
		yErieEvap2PP[jp] ~ dnorm(yErieEvap2Mean[jp], yErieEvap2Prec)
		yErieEvap3PP[jp] ~ dnorm(yErieEvap3Mean[jp], yErieEvap3Prec)
		yOntarioEvap1PP[jp] ~ dnorm(yOntarioEvap1Mean[jp], yOntarioEvap1Prec)
		yOntarioEvap2PP[jp] ~ dnorm(yOntarioEvap2Mean[jp], yOntarioEvap2Prec)
		yOntarioEvap3PP[jp] ~ dnorm(yOntarioEvap3Mean[jp], yOntarioEvap3Prec)
		ySuperiorRunoff1PP[jp] ~ dnorm(ySuperiorRunoff1Mean[jp], ySuperiorRunoff1Prec)
		ySuperiorRunoff2PP[jp] ~ dnorm(ySuperiorRunoff2Mean[jp], ySuperiorRunoff2Prec)
		ySuperiorRunoff3PP[jp] ~ dnorm(ySuperiorRunoff3Mean[jp], ySuperiorRunoff3Prec)
		ySuperiorRunoff4PP[jp] ~ dnorm(ySuperiorRunoff4Mean[jp], ySuperiorRunoff4Prec)
		ySuperiorRunoff5PP[jp] ~ dnorm(ySuperiorRunoff5Mean[jp], ySuperiorRunoff5Prec)
		yMiHuronRunoff1PP[jp] ~ dnorm(yMiHuronRunoff1Mean[jp], yMiHuronRunoff1Prec)
		yMiHuronRunoff2PP[jp] ~ dnorm(yMiHuronRunoff2Mean[jp], yMiHuronRunoff2Prec)
		yMiHuronRunoff3PP[jp] ~ dnorm(yMiHuronRunoff3Mean[jp], yMiHuronRunoff3Prec)
		yMiHuronRunoff4PP[jp] ~ dnorm(yMiHuronRunoff4Mean[jp], yMiHuronRunoff4Prec)
		yMiHuronRunoff5PP[jp] ~ dnorm(yMiHuronRunoff5Mean[jp], yMiHuronRunoff5Prec)
		yErieRunoff1PP[jp] ~ dnorm(yErieRunoff1Mean[jp], yErieRunoff1Prec)
		yErieRunoff2PP[jp] ~ dnorm(yErieRunoff2Mean[jp], yErieRunoff2Prec)
		yErieRunoff3PP[jp] ~ dnorm(yErieRunoff3Mean[jp], yErieRunoff3Prec)
		yErieRunoff4PP[jp] ~ dnorm(yErieRunoff4Mean[jp], yErieRunoff4Prec)
		yErieRunoff5PP[jp] ~ dnorm(yErieRunoff5Mean[jp], yErieRunoff5Prec)
		yOntarioRunoff1PP[jp] ~ dnorm(yOntarioRunoff1Mean[jp], yOntarioRunoff1Prec)
		yOntarioRunoff2PP[jp] ~ dnorm(yOntarioRunoff2Mean[jp], yOntarioRunoff2Prec)
		yOntarioRunoff3PP[jp] ~ dnorm(yOntarioRunoff3Mean[jp], yOntarioRunoff3Prec)
		yOntarioRunoff4PP[jp] ~ dnorm(yOntarioRunoff4Mean[jp], yOntarioRunoff4Prec)
		yOntarioRunoff5PP[jp] ~ dnorm(yOntarioRunoff5Mean[jp], yOntarioRunoff5Prec)
		yClairNBS1PP[jp] ~ dnorm(yClairNBS1Mean[jp], yClairNBS1Prec)
		ySuperiorOutflow1PP[jp] ~ dnorm(ySuperiorOutflow1Mean[jp], ySuperiorOutflow1Prec[m[jp]])
		ySuperiorOutflow2PP[jp] ~ dnorm(ySuperiorOutflow2Mean[jp], ySuperiorOutflow2Prec[m[jp]])
		yMiHuronOutflow1PP[jp] ~ dnorm(yMiHuronOutflow1Mean[jp], yMiHuronOutflow1Prec[m[jp]])
		yMiHuronOutflow2PP[jp] ~ dnorm(yMiHuronOutflow2Mean[jp], yMiHuronOutflow2Prec[m[jp]])
		yClairOutflow1PP[jp] ~ dnorm(yClairOutflow1Mean[jp], yClairOutflow1Prec[m[jp]])
		yClairOutflow2PP[jp] ~ dnorm(yClairOutflow2Mean[jp], yClairOutflow2Prec[m[jp]])
		yErieOutflow1PP[jp] ~ dnorm(yErieOutflow1Mean[jp], yErieOutflow1Prec[m[jp]])
		yOntarioOutflow1PP[jp] ~ dnorm(yOntarioOutflow1Mean[jp], yOntarioOutflow1Prec[m[jp]])
		ySuperiorDiversion1PP[jp] ~ dnorm(ySuperiorDiversion1Mean[jp], ySuperiorDiversion1Prec[m[jp]])
		yMiHuronDiversion1PP[jp] ~ dnorm(yMiHuronDiversion1Mean[jp], yMiHuronDiversion1Prec[m[jp]])
		yErieDiversion1PP[jp] ~ dnorm(yErieDiversion1Mean[jp], yErieDiversion1Prec[m[jp]])

	}

	### CUMULATIVE STORAGE ANALYSIS
	
	# 1 YEAR
	
	for(x in 12:posteriorEndMonth){

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
		

		yClairR1YStorePP[x] ~ dnorm(clairR1YStore[x], yClairRStorePrec);
		
		clairR1YStore[x] <- (
			sum(clairNBS[(x-12+1):x])
			+ sum(miHuronOutflow[(x-12+1):x])
			- sum(clairOutflow[(x-12+1):x])
			+ sum(clairProcError[m[(x-12+1):x]])
		)

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
	
	}	
	
	# 5 YEAR
	
	for(z in 60:posteriorEndMonth){


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
		

		yClairR5YStorePP[z] ~ dnorm(clairR5YStore[z], yClairRStorePrec);
		
		clairR5YStore[z] <- (
			sum(clairNBS[(z-60+1):z])
			+ sum(miHuronOutflow[(z-60+1):z])
			- sum(clairOutflow[(z-60+1):z])
			+ sum(clairProcError[m[(z-60+1):z]])
		)	

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

	}

} # END MODEL
