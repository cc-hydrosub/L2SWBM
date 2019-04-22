L2SWBM CONFIGURATION OPTIONS

- Model Name: The name you want for this model run, will help name the folder in results 
  and files contained within it. 

- Analysis Start (YYYY [space] MM)
- Analysis End

- Prior [component] Start 
- Prior [component] End 
- Prior [component] Column
-- For the above priors, we're describing the per calendar month distributions that describe,
   within the model, what values we would expect that parameter to have for that 
   particular lake. Ensure that the time range you've picked per component prior 
   contains observations for the column you picked
- [component] Prior Precision Mod: if a component has, historically, a narrow range of values,
  and you wish to provide flexibility, provide a value between 0 and 1, describing the 
  fraction of calculated historical precision you prefer to use

- Residual NBS column: which column are the residual NBS values in. This is to substitute residual NBS where
values do not exist for the selected NBS prior column

- Outflow Bias: 
-- If we set Outflow Bias to TRUE
--- Outflow estimate ~ Normal(True Value + Bias Term, Precision) 
---- Precision derived from a vague prior or specified below
--- Bias Term ~ Normal(Bias Mean Specified Below, Bias Precision) 
---- Bias Precision derived recommendations from Bruxer or otherwise set below
-- If we set Outflow Bias to FALSE
--- Outflow estimate ~ Normal(True Value, Precision) 
---- Precision derived recommendations from Bruxer or otherwise set below

- [channel flow or diversion] Uncertainty: percentage/100 of the historical (prior) per calendar
  month mean to cast as the standard deviation/precision of the flow observation's bias 
  (if "Outflow Bias" is TRUE AND "Flow Uncertainty In Percent" is TRUE) or 
  flow observation's precision (if "Outflow Bias" is FALSE AND 
  "Flow Uncertainty In Percent" is TRUE). This is where you would insert values 
  as suggested by Bruxer 2010,
  for example

- Flow Uncertainty In Percent: Set to TRUE to use the [channel flow or diversion] 
  Uncertainties, FALSE to manually define per observation below this option

- Lakes [component]: Provide the component observations for 'S'uperior, 'M'iHuron, 
  St. 'C'lair, 'E'rie, and 'O'ntario if their quoted letter is in the space-delimited string.
  Otherwise, don't provide the observations to the model.

- [component] Inputs: For picking NBS component models to input into the L2SWBM, 
  use zeros and ones separated by a space, corresponding to the models available 
  as of 2019/04/12:							
-- Precip: "Year","Month","NOAA.GLERL.GLM.HMD","GLERL.AHPS.Provisional","USACE.AHPS","ECCC.WCPS","ECCC.CaPA",
   "NWS.MPE","Historical.Coordinated","USACE.Thiessen","Merged.MPE.CaPA","L2SWBM.Low"
-- Evap: "NOAA.GLERL.GLM.HMD","GLERL.AHPS.Provisional","USACE.AHPS","ECCC.WCPS","GLERL.FVCOM","L2SWBM.Low","L2SWBM.High"
-- Runoff: "NOAA.GLERL.GLM.HMD","GLM.HMD.Provisional","USACE.AHPS","ECCC.WCPS","ECCC.WATFLOOD","L2SWBM.Low","L2SWBM.High"
-- Outflow: IGS (if applicable), either Accounting OR SFD+ADVM, Coordinated (not complete? As of 2018/02/19)						
-- Diversion: Monthly Mean							
-- NBS: "NOAA.GLERL.GLM.HMD","GLERL.AHPS.Provisional","USACE.AHPS","ECCC.WCPS","ECCC.CaPA","ECCC.WATFLOOD",
        "Residual","L2SWBM.Low","L2SWBM.High"	

The space delimited format by model applies to the options starting here and ends at
"Balance Process Error" 

- [component] Obs Prior Mean Bias and Bias Std. Dev: suggest a bias and standard deviation 
  for that bias in the units of the observation. Standard deviation, if set to zero, is replaced
  with 10 for P, E, and R, 30 for NBS, 200 for Channel Flows, and 10 for Diversions.

- [component] Obs Std. Dev: We define observations y ~ Normal(true + bias, precision). Define
  the precision in terms of standard deviations in the observations' units, or leave zero to 
  give a vague prior for that precision.

- Balance Process Error: if TRUE, simulate the WBM's process error with Prior Mean 
  and Std. Dev defined directly below it. 

- Process Error Prior Mean and Std. Dev

- Rolling window: pick 6 or better. Found 12 was pretty good

- Define dH Uncertainty: Set to TRUE if you would like the model to run with a pre-defined 
  uncertainty (standard deviation) value for observed changes in storage							
  If FALSE, the model will start with a vague idea as far as how precise the observed 
  changes in storage are

- dH Uncertainty in mm - If 'Define dH Uncertainty' is TRUE, define it here. 
  Model will assert uncertainty be at least 1 mm. Model will account for rolling window							

- [lake] Component WBM - Set to TRUE to use the WBM...
-- dH = P - E + R + Q_i - Q_o +/- D + process error
- else: use the WBM
-- dH = NBS + Q_i - Q_o +/- D + process error
--- Your choice of priors were already defined above

- St. Clair CMS if not Component WBM: if St. Clair Component WBM is FALSE above, then the
  entire St. Clair WBM will be done in units of cubic meters per second. 

- MCMC Iterations: How many samples do you wish to draw for this model run? 2000 is adequate
  for a quick run. 100K to a million get you closer to convergence, but that's been elusive
  in general. Play with this a little.

- Model Checks: Run the model such that generated samples include those to check 
  balance closure as well as proper simulation of inputs. Outputs closure summary file.
  Very useful given convergence is difficult.						 
