Project 2T
================
Cassio Monti Smitali Patnaik
10-08-2022

## TESTING COMMIT : 2nD PARTNER

## Retesting -2nd partner

Some packages necessary to run the code

``` r
# Getting relevant packages and calling the api
library(jsonlite)
library(tidyverse)
```

There are two functions so far, the first pull down the aggregate
information about the tickers with some metrics. The second function
pull down the ticker names and some characteristics as location, ticker
type, and more.

``` r
# create the URL for aggregate endpoint
agg_endpoint = function(stocksTicker="AAPL", from = "2021-07-22", to = "2022-07-22",mltplr=30, timespan="day"){
  
  
  base_endpoint = "https://api.polygon.io/v2/aggs/"
  
  last_code = "?adjusted=true&sort=asc&limit=5000"
  
  key = "&apiKey=asWU9di2FThCr1ywIpgyNdqwXMf0fpj4"
  
  mltplr = as.character(mltplr)
  
  call = paste0(base_endpoint,"ticker/",stocksTicker,"/range/",mltplr,"/",
                timespan,"/",from,"/",to,last_code,key)
  
  
  p = fromJSON(call)
  
  d1 = as.Date(from)
  d2 = as.Date(to)
  d = seq(d1,d2, by ="month")
  
  tb = p$results
  
  tckr = p$ticker
  
  out = tibble(tckr,d,tb)
  
  out
  
}
# ex.: crypto
u=agg_endpoint(stocksTicker = "X:1INCHUSD")
```

This function aims to call tickers from common stock mainly and other
markets as well as crypto currencies for further analysis of both.

``` r
# tickers endpoint= get ticker names
# create the URL for the ticker endpoint - two calls: i) ticker names; and
# ii) crypto names
ticker_endpoint = function(ticker="AAPL", market = "stock", limit = 100){
  
  if(limit > 1000){
    limit = 1000
    message("Warning: the max limit is 1000 for free access!")
  }
  
  base_endpoint = "https://api.polygon.io/v3/reference/tickers?ticker="
  
  last_code = "&active=true&sort=ticker&order=asc&limit="
  
  key = "&apiKey=asWU9di2FThCr1ywIpgyNdqwXMf0fpj4"
  
  call = paste0(base_endpoint,ticker,last_code,limit,key)
  
  p = fromJSON(call)
  
  p$results$name
  p$results$ticker
  p$results$type

  
  
}
```

This function takes information from the previous two functions and
combine them when it is possible.

``` r
# call multiple tickers from agg_endpoint and return a df
multiple_calls = function(ticker_list){
  
  t = ticker_endpoint(ticker_list)
  
  agg_endpoint(t)
  
  
}
```
