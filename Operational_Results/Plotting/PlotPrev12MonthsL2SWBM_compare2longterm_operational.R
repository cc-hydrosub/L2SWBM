# This script plots the components of NBS as predicted by the L2SWBM for the recent operational monthly run relative to the most recent long-term simulation that is produced annually. Resulting plots were presented to Coordinating Committee in the May 2026 meeting, and it was agreed that Hydrology Subcommittee can move to operationalize the plotting and share on the CC website on a monthly basis.

# Created by lauren.fry@noaa.gov

# The monthly operational output is available on the Hydrology Subcommittee's Github site here: https://github.com/cc-hydrosub/L2SWBM/tree/master/Operational_Results.

# The output from the most recent annual update of the long-term run is available on the Coordinating Committee's Zenodo page here: https://doi.org/10.5281/zenodo.10798859.

# To run on your computer:
# 1. Set up your directory structure so that you have a "MonthlyGraphics" directory that includes this script, the ts2table.R utility script and a subdirectory "plots" to write output graphics and
# 2. Update pathnames in the section titled "Set up the workspace."
# 3. Install package zoo
# 4. Download the data from the most recent annual run of the long-term simulation from https://doi.org/10.5281/zenodo.10798859 (this only needs to be done annually). Extract to your "MonthlyGraphics" directory.
# 5. Download the data from the most recent monthly operational run from https://github.com/cc-hydrosub/L2SWBM/tree/master/Operational_Results. Extract to your "MonthlyGraphics" directory.
# 6. Run and check output in the "plots" directory.

# Note that there are lines to determine the start and end date of the plot in the "Set up the workspace" section. For code development, this is just hard-coded to the most recent data available at the time, but this could potentially be coded to automatically determine the start and end date based on the system date, depending on how you want to run it on your system. Likewise the directory names for the monthly and annual runs are hard coded in the same section, and these could be better handled using the system date potentially.

#####################################
# Set up the workspace.
#####################################

library(zoo) # to handle time series

# Update the path for your system
wb.dir <- ''
proj.dir <- paste(wb.dir, '', sep='/')
setwd(proj.dir)
src.dir <- '' # this directory for function ts2table.R

# create a folder in which to store the plots
plot.dir <- paste(wb.dir, sep='/')

# For operational use, consider setting up an easier way to create these variable names, perhaps by using the system date or something.
wbm.dir <- paste(wb.dir, '', sep='/')
mod.dir <- '' #results directory

##Would need to change this based on the new annual run data each year
ann.dir <- '' #directory for annual run data

operational.out.dir<-paste(wbm.dir, mod.dir, '', sep='/')
annual.out.dir <- paste(wbm.dir, ann.dir, sep='/')

# Function to convert time series data (indexed) to table format.
source(paste(src.dir, 'ts2table.R', sep='/'))


# define the period of interest. This could be set up automatically based on the system date or entered manually. Something for operational user to determine how best to handle this.
# edt <- as.Date(paste(format(seq(Sys.Date(), length.out=2, by='-1 month')[2], '%Y-%m'), 15, sep='-'))
edt <- as.Date('2026-07-15')
sdt <- seq(edt, length.out=2, by='-11 months')[2]

lk.names <- c('superior', 'miHuron', 'erie', 'ontario')
vars2plot <- c('Precip', 'Runoff', 'Evap')

#########################
# L2SWBM monthly operational run files
#########################

op.files <- list.files(operational.out.dir, recursive=T, full.names=T)

for (lk in lk.names){
  op.files.lk <- op.files[grepl(lk, op.files)]
  for (component in vars2plot){
    idata <- read.csv(op.files.lk[grepl(component, op.files.lk)], stringsAsFactors=F)
    odata <- zoo(idata[, 4:5], order.by=as.Date(paste(idata$Year, idata$Month, 15, sep='-')))
    if (component == 'Evap'){
      odata <- -odata
    }
    assign(paste(lk, component, 'op', sep='.'), odata)
    rm(odata)
    rm(idata)
  }
  rm(op.files.lk)
}

#########################
# L2SWBM annual run files - just use medians
#########################

an.files <- list.files(annual.out.dir, recursive=T, full.names=T)

for (lk in lk.names){
  an.files.lk <- an.files[grepl(lk, an.files)]
  for (component in vars2plot){
    idata <- read.csv(an.files.lk[grepl(component, an.files.lk)], stringsAsFactors=F)
    odata <- zoo(idata[, 3], order.by=as.Date(paste(idata$Year, idata$Month, 15, sep='-')))
    if (component=='Evap'){
      odata <- -odata
    }
    assign(paste(lk, component, 'an', sep='.'), odata)
    rm(idata)
    rm(odata)
  }
  rm(an.files.lk)
}

##################
# Convert annual run time series medians to table format for data used to create boxplots
##################

for (lk in lk.names){
  for (component in vars2plot){
    if (exists(paste(lk, component, 'an', sep='.'))){
      idata <- get(paste(lk, component, 'an', sep='.'))
      odata <- ts2table(idata)
      assign(paste(lk, component, 'boxdata', sep='.'), odata)
      rm(odata)
      rm(idata)
    }
  }
}

###############################
# Plotting
###############################

par.default <- par(no.readonly=TRUE)

xlims <- c(1, 12)
dts2plot <- seq(sdt, edt, by='1 month')

longnames <- c('Lake Superior', 'Lake Michigan-Huron', 'Lake Erie', 'Lake Ontario')

# vars2plot <- c('precip','runoff', 'evap')
varnames <- c('precipitation', 'runoff', 'evaporation')

# Multipanel plot of all variables for each lake:

for (lk in 1:length(lk.names)){
  lake <- lk.names[lk]

  ofname <- paste(lake, 'Past12MonthsCCWeb.png', sep='_')
  png(file=paste(plot.dir, ofname, sep='/'), width=1100, height=850)

  layout(matrix(c(1:(length(vars2plot) + 1)), nrow=length(vars2plot) + 1), heights=c(.2, rep(1, length(vars2plot))))
  par(mar=c(0,0,0,0), oma=c(18,16,5,8))

  plot.new()

  legend('center', legend=c('Historical Record', 'Past 12 Months'), border=c('gray40', NA), fill=c(NA, 'salmon'), bty='n', ncol=2, cex=2)

  for (v in 1:length(vars2plot)){
    component <- vars2plot[v]
    if (exists(paste(lake, component, 'op', sep='.')) & exists(paste(lake, component, 'an', sep='.'))){
      data2plot <- get(paste(lake, component, 'op', sep='.'))
    }else{
      data2plot <- NA
    }

    if (!all(is.na(data2plot))){
      boxdata <- get(paste(lake, component, 'boxdata', sep='.'))
      data2plot <- window(data2plot, start=sdt, end=edt)
      index(data2plot) <- c(1:12)
      wbm.cols <- which(grepl('Percentile', names(data2plot)))
      ylims=c(min(min(data2plot, na.rm=T), min(boxdata, na.rm=T)), max(max(data2plot, na.rm=T), max(boxdata, na.rm=T)))
      plot(NA, xlim=xlims, ylim=ylims, xaxt='n', yaxt='n', xlab='', ylab='')
      for (m in 1:length(dts2plot)){
        lines(x=rep(m, 2), y=data2plot[m, wbm.cols], col='salmon', lwd=17, lend=1)
      }
      for (m in 1:length(dts2plot)){
        month <- as.numeric(format(dts2plot[m], '%m'))
        boxplot(boxdata[, as.numeric(format(dts2plot[m], '%m'))], border='gray40', at=m, add=T, yaxt='n', col=NA)
      }

      if (v %% 2 == 0){
        axis(2, las=1, cex.axis=2)
        mtext(side=4, varnames[v], las=1, line=1, cex=2)
      }else{
        axis(4, las=1, cex.axis=2)
        mtext(side=2, varnames[v], las=1, line=1, cex=2)
      }
    }else{
      plot(NA, xlim=xlims, ylim=c(0, 1), xlab='', ylab='', xaxt='n', yaxt='n')
      if (v %% 2 == 0){
        mtext(side=4, variable, las=1, line=1, cex=2)
      }else{
        mtext(side=2, variable, las=1, line=1, cex=2)
      }

    }

  }
  axis(1, at=c(1:12), labels=format(dts2plot, '%Y-%m'), las=2, cex.axis=2)
  mtext(side=3, outer=T, paste(longnames[lk], 'Net Basin Supply components (mm over the lake)'), cex=2.5, line=1.5)
  mtext(side=1, outer=T, 'Boxplots represent historical values based on median values from most recent', line=10, cex=2)
  mtext(side=1, outer=T, 'coordinated long-term simulation of L2SWBM available at https://doi.org/10.5281/zenodo.10798859.', line=13, cex=2)
  mtext(side=1, outer=T, 'Red bars indicate the 90% credible interval from the most recent monthly run of L2SWBM.', line=16, cex=2)

  dev.off()
}
