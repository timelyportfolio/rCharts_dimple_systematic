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

weights.df <- data.frame(index(models$C.EW$weight),models$C.EW$weight)[which(models$C.EW$weight[,1]!=0),]
colnames(weights.df)[1] <- "date"

weights.melt <- melt(weights.df, id.vars = 1)
colnames(weights.melt) <- c("date","symbol","value")
weights.melt$date <- format(weights.melt$date)

dWeights <- dPlot(
  value ~ date,
  groups = "symbol",
  data = weights.melt,  
  type = "area"
)
dWeights$xAxis(
  type = "addTimeAxis",
  inputFormat = "%Y-%m-%d",
  outputFormat = "%Y"
)
dWeights$yAxis( overrideMax = 1 )
dWeights

#I don't think daily is necessary, so let's try some aggregation
require(plyr)


#explore with strategy
#do median weight by year
strategy.melt <- do.call(rbind, lapply(names(models),function(x){
  x.melt <- melt(
    data.frame(
      index(models[[x]]$weight[-(1:254),]),    ###do 254 to eliminate 0s at beginning
      rep(x,NROW(models[[x]]$weight[-(1:254),])),
      models[[x]]$weight[-(1:254),]
    ),
    id.vars = 1:2
  )
  colnames(x.melt) <- c("date","strategy","symbol","weight")
  #get median by year
  x.melt <- ddply(x.melt, .(strategy,format(date,"%Y"),symbol), summarise, median = median(weight))
  colnames(x.melt)[2] <- "date"  
  return(x.melt)
}))

dFacet <- dPlot(
  median ~ symbol,
  groups = "symbol",
  data = strategy.melt,
  type= "bar",
  height = 600,
  width = 800
)
dFacet$xAxis( orderRule = "symbol" )
dFacet$yAxis( overrideMax = 0.4, outputFormat = ".2f" )
dFacet$facet( x = "strategy", y = "date" )
dFacet$templates$script = system.file(
  "libraries/dimple/layouts/chartFacet_d3grid.html",
  #"libraries/dimple/layouts/chartFacet.html",
  package = "rCharts"
)
dFacet

dStory <- dPlot(
  y = "median",
  x = "date",
  groups = "symbol",
  data = strategy.melt,
  type = "area",
  height = 400,
  width = 800,
  bounds = list(x=70,y=50,height=320,width=700)
)
dStory$legend(
  x = 100,
  y = 30,
  width = 700,
  height = 20,
  horizontalAlign = "right"
)
dStory$yAxis( type = "addPctAxis" )
dStory$set( storyboard = "strategy" )
dStory



require(ggplot2)
ggplot(
  data = strategy.melt,
  aes(x=strategy, y = median, fill=symbol, stat="identity")) +
  geom_bar( position = "dodge" ) + facet_wrap( ~date ) +  
  scale_fill_brewer(palette="Set1")