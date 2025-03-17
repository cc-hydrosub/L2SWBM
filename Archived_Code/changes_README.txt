Changes between versions of the Laurentian Great Lakes L2SWBM:

v1 - v2 ~ Ops - Ops_Res:

NOTE: versions of the L2SWBM here are essentially forks/evolutions of the version on the NOAA-GLERL GitHub, and are here for operational use and development

- Ops, feature listing:

-- configure whether or not the model simulates channel flow and diversion bias (Outflow Bias)
-- configure whether or not the model simulates water balance process error
-- configure whether or not the model performs model checks. If TRUE, model outputs a closure summary 
   file, indicating the rates of water balance closure for all lakes for all 1, 12, and 60 month periods
-- You may define your level of uncertainty regarding observations of change in storage in millimeters. 
   Set 'Define dH Uncertainty' to TRUE, and set your uncertainty value in the field directly below. Must 
   be at least 1 mm, which I've tried, and have been rather pleased with.
   
I came up with that option after realizing the old operational version of the model, directly developed from 
the research version, did not infer acceptable uncertainties with changes in storage. I have not invested too 
much time in that with the historical record project, but figured such a configuration option would be acceptable. 

PLEASE NOTE: this is merely your level of uncertainty which defines the likelihood distribution for changes in storage. 
The distribution is still continuous, and MCMC will explore as much of it as necessary
in discovering the solution space (component estimates that close the water balance)

-- Do not touch the outflow inputs field... it's a placeholder

- Ops_Res, additional feature listing:

-- The new features here can be found in the bottom 3 lines of the config file. 

-- For precip, evap, and runoff, you may select which lakes receive observational input for those variables, where S = Superior, 
   M = Michigan-Huron, E = Erie, and O = Ontario. To remove the variable inputs for a particular lake, just remove its letter 
   on the line for its variable. If you want to remove all precipitation inputs for all lakes, for example, you can put "N N N N".
   
Version 3 (THIS): 

- Many new features, which I try to provide a casual, yet broad summary of here

-- This is an attempt to make a majority, if not all, of the customizable model parameters available in the config file
-- You may specify unique prior distribution time periods for each component of the water balance, and select the data
   set from which it is derived
--- Unfortunately, you may only choose one column of data for the prior. This does not prohibit you from creating a column
    of data which merges 2 or more data sets to develop a prior from (this is a suggestion)
--- You may also tune your priors if the historical data are too narrow or wide ranging (precipitation not included)
-- You may specify the uncertainty percentage of channel flows and diversions (much like Bruxer 2010)
-- You may define the priors for the biases of input observations
-- You may define the precision in terms of standard deviation of the input observations
-- You may define the prior mean and standard deviation for process errors on each of the lakes
-- You may elect to model the water balance with P, E, and R individually or the lumped parameter NBS
-- TODO: You may, if you model the water balance with NBS, elect to have NBS modelled by P, E, and R

- Be sure to read the config_README for more detail

Happy modelling!!!
