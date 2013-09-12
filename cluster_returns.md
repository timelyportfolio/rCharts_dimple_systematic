---
title: rCharts + dimple | Systematic Investor Methods
author: Timely Portfolio
github: {user: timelyportfolio, repo: rCharts_dimple_systematic, branch: "gh-pages"}
framework: bootstrap
mode: selfcontained
highlighter: prettify
hitheme: twitter-bootstrap
assets:
  css:
  - "http://fonts.googleapis.com/css?family=Raleway:300"
  - "http://fonts.googleapis.com/css?family=Oxygen"
---

<style>
iframe{
  height:600px;
  width:900px;
  margin:auto auto;
}

body{
  font-family: 'Oxygen', sans-serif;
  font-size: 16px;
  line-height: 24px;
}

h1,h2,h3,h4 {
font-family: 'Raleway', sans-serif;
}

.container { width: 900px; }

h3 {
background-color: #D4DAEC;
  text-indent: 100px; 
}

h4 {
text-indent: 100px;
}
</style>
  
<a href="https://github.com/timelyportfolio/rCharts_nvd3_perf"><img style="position: absolute; top: 0; right: 0; border: 0;" src="https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png" alt="Fork me on GitHub"></a>

# Interactive Analysis of Systematic Investor - `PerformanceAnalytics` Tables

In a [previous post](http://timelyportfolio.blogspot.com/2013/09/d3-ify-systematic-investor-cluster.html), I thought it would be good fun to take one of the [posts from Systematic Investor](http://systematicinvestor.wordpress.com/2013/03/05/cluster-risk-parity-back-test/) and d3-ify it.  Let's have a look at returns now using the mutliple tables provided by `PerformanceAnalytics`.  I will just use the defaults on each of these tables and then plot the table with a [`dimplejs`](http://dimplejs.org) bar chart.

### Copy/Paste Systematic Investor Brilliance
As before, let's start by getting the data and performing the calculations in R.  This is a direct copy and paste from the Systematic Investor post.  Thanks again Systematic Investor.





### Get Data and Perform Calculations

```r
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
load(url("http://timelyportfolio.github.io/rCharts_dimple_systematic/data.Rdata"),envir=data)
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
```

20 , percent = 11.9% 
30 , percent = 26.9% 
40 , percent = 41.8% 
50 , percent = 56.7% 
60 , percent = 71.6% 
70 , percent = 86.6% 

```r

models = create.strategies(obj, data)$models
```

EW , percent = 25% 
Latest weights :
             EEM   EFA   GLD   IWM   IYR   QQQ   SPY   TLT   USO   UUP
2013-09-09 243.7 161.4 74.68 96.21 156.2 128.5 59.66 96.99 256.8 452.7

Performance summary :
	CAGR	Best	Worst	
	3.9	7.2	-7.3	

RP , percent = 50% 
Latest weights :
             EEM   EFA  GLD   IWM   IYR   QQQ   SPY   TLT   USO   UUP
2013-09-09 186.9 150.4 50.9 86.84 147.3 126.9 67.78 93.49 165.6 923.9

Performance summary :
	CAGR	Best	Worst	
	3.8	5.1	-4.5	

C.EW , percent = 75% 
Latest weights :
             EEM   EFA   GLD   IWM  IYR   QQQ   SPY   TLT   USO  UUP
2013-09-09 87.04 57.63 186.7 34.36 55.8 45.89 21.31 242.5 91.72 1132

Performance summary :
	CAGR	Best	Worst	
	1.8	3	-3	

C.RP , percent = 100% 
Latest weights :
             EEM   EFA   GLD   IWM   IYR   QQQ   SPY   TLT   USO  UUP
2013-09-09 67.34 54.19 106.7 31.29 53.09 45.73 24.42 195.9 59.68 1936

Performance summary :
	CAGR	Best	Worst	
	2.1	1.5	-1.8	



```r
#use rCharts to get some interactive plots
require(rCharts)
require(reshape2)

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
perfTables$DrawdownsRatio <- table.DrawdownsRatio(returns.xts)
perfTables$HigherMoments <- table.HigherMoments(returns.xts[,-1],returns.xts[,1])
perfTables$InformationRatio <- table.InformationRatio(returns.xts[,-1],returns.xts[,1])
perfTables$SpecificRisk <- table.SpecificRisk(returns.xts[,-1],returns.xts[,1])
perfTables$TrailingPeriods <- table.TrailingPeriods(returns.xts)
perfTables$Variability <- table.Variability( returns.xts )
```



```r
require(RColorBrewer)
lapply(
  perfTables, #["InformationRatio"],
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
    d1$show("iframe")
  }
)
```

<iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe><iframe src=assets/fig/unnamed-chunk-4.html seamless></iframe>$AnnualizedReturns
NULL

$Autocorrelation
NULL

$CAPM
NULL

$CalendarReturns
NULL

$CaptureRatios
NULL

$Correlation
NULL

$Distributions
NULL

$DownsideRisk
NULL

$DownsideRiskRatio
NULL

$DrawdownsRatio
NULL

$HigherMoments
NULL

$InformationRatio
NULL

$SpecificRisk
NULL

$TrailingPeriods
NULL

$Variability
NULL

