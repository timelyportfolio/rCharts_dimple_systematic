#thanks Systematic Investor, Michael Kapler
#for this post http://systematicinvestor.wordpress.com/2013/03/05/cluster-risk-parity-back-test/

###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data for ETFs
#****************************************************************** 
load.packages('quantmod')

#tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')
#to reduce calls to Yahoo I saved the data in .Rdata
data <- new.env()
load("data.Rdata",envir=data)
#getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
#for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

bt.prep(data, align='remove.na')

#*****************************************************************
# Code Strategies
#******************************************************************   
periodicity = 'months'
lookback.len = 250
cluster.group = cluster.group.kmeans.90

obj = portfolio.allocation.helper(
  data$prices, 
  periodicity = periodicity, lookback.len = lookback.len,
  min.risk.fns = list(
    EW=equal.weight.portfolio,
    RP=risk.parity.portfolio,
    C.EW = distribute.weights(equal.weight.portfolio, cluster.group),
    C.RP=distribute.weights(risk.parity.portfolio, cluster.group)
  )
) 		

models = create.strategies(obj, data)$models

#*****************************************************************
# Create Report
#****************************************************************** 	
#strategy.performance.snapshoot(models, T)

#use rCharts to get some interactive plots
require(rCharts)
require(reshape2)

equity.df <- data.frame(index(models$C.EW$equity),models$C.EW$equity)[-(1:254),]
colnames(equity.df) <- c("date","C.EW")


#get cumulative growth from each of the models in long form                         
equity.melt <- do.call(rbind,
  lapply(
    names(models),
    FUN=function(x){
      x.data <- models[[x]]$equity[endpoints(models[[x]]$equity,"months"),]
      colnames(x.data) <- x      
      x.melt <- melt(data.frame(index(x.data),x.data),id.vars=1)
      colnames(x.melt) <- c("date", "model", "equity")
      #get date in text format that dimple will like
      x.melt$date <- format(x.melt$date, "%Y-%m-%d")
      return(x.melt)
    }
  )
)

dEq <- dPlot(
  equity ~ date,
  groups = "model",
  data = equity.melt,
  type = "line"
)
dEq$xAxis(
  type = "addTimeAxis",
  inputFormat = "%Y-%m-%d", outputFormat = "%b %Y"
)
require(latticeExtra)
dEq$defaultColors(theEconomist.theme()$superpose.line$col, replace=T)
dEq
dEq$defaultColors(brewer.pal(n=9,"Blues"), replace=T)
dEq
dEq$defaultColors("#!d3.scale.category20()!#", replace=T)
dEq
dEq$defaultColors("#!d3.scale.category20b()!#", replace=T)
dEq
dEq$defaultColors("#!d3.scale.category20c()!#", replace=T)
dEq
dEq$defaultColors("#!d3.scale.category10()!#", replace=T)
dEq

#get year so we can test a facet by year
equity.melt$year <- format(as.Date(equity.melt$date),"%Y")
dEq_facet = dPlot(
  equity ~ date,
  groups = "model",
  data = equity.melt,
  type = "line",
  height = 400,
  width = 1000
)
dEq_facet$xAxis(
  type = "addTimeAxis",
  inputFormat = "%Y-%m-%d", outputFormat = "%b"
)
dEq_facet$yAxis( 
  #overrideMin = min(equity.melt$equity) - 0.1,  #min messes up x axis label
  overrideMax = max(equity.melt$equity)
)
dEq_facet$facet( x = "year" )
dEq_facet$defaultColors(theEconomist.theme()$superpose.line$col, replace=T)
dEq_facet$setLib( "libraries/widgets/dimple" )
dEq_facet$templates$script = paste0(getwd(),"/libraries/widgets/dimple/layouts/chartFacet_d3grid.html") 
dEq_facet

#I don't think daily is necessary, so let's try some aggregation
#this time though let's use PerformanceAnalytics to explore risk and return
#Performance Analytics wants the return series so let's merge returns into
#a single xts object
returns.xts <- do.call(merge,
  lapply(
   names(models),
   FUN=function(x){
     x.data <- models[[x]]$equity[endpoints(models[[x]]$equity,"months"),]
     colnames(x.data) <- x
     x.ret <- ROC( x.data, type = "discrete", n = 1 )
     return(x.ret)
   }
  )
)[-(1:11),]  #remove first 11 months since no data; will need to change if not monthly

perfTables <- list()
perfTables$AnnualizedReturns <- table.AnnualizedReturns(returns.xts)
perfTables$Autocorrelation <- table.Autocorrelation(returns.xts)
perfTables$CAPM <- table.CAPM(returns.xts[,-1],returns.xts[,1])
perfTables$CalendarReturns <- table.CalendarReturns(returns.xts)[,-(1:12)]/100 #ignore monthly data
perfTables$CaptureRatios <- t(table.CaptureRatios(returns.xts,returns.xts[,1]))
perfTables$Correlation <- t(table.Correlation(returns.xts,returns.xts[,1]))
perfTables$Distributions <- table.Distributions(returns.xts)
perfTables$DownsideRisk <- table.DownsideRisk(returns.xts)[-7,] #remove drawdown
perfTables$DownsideRiskRatio <- table.DownsideRiskRatio(returns.xts)

require(RColorBrewer)
lapply(
  perfTables,
  FUN = function(x) {
    x.df <- data.frame( rownames(x),x )
    x.melt <- melt( x.df, id.vars = 1 )
    colnames( x.melt ) <- c( "metric", "strategy", "value")

    d1 <- dPlot(
      x = "value",
      y = c("metric","strategy"),
      groups = c("strategy"),
      data = x.melt,
      type = "bar",
      width = 800
    )
    d1$chart( x = 150, width = 550 )  #move over a little to allow room for y axis labels
    d1$yAxis( type = "addCategoryAxis", orderRule = rev(x.melt$metric) )
    d1$xAxis( type = "addMeasureAxis", outputFormat = ".2%"  )
    d1$legend(
      x = 725,
      y = 10,
      width = 75,
      height = 100,
      horizontalAlign = "left"
    )
    d1$defaultColors(
      latticeExtra::theEconomist.theme()$superpose.line$col,
      replace=T
    )
  return (d1)
  }
)