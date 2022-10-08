Project 2T
================
Cassio Monti Smitali Patnaik
10-08-2022

# Required Packages

Some packages necessary to run the code

``` r
# Getting relevant packages and calling the api
library(jsonlite)
library(tidyverse)
```

# Functions for Calling the API via EndPoints

There are two functions so far, the first pull down the aggregate
information about the tickers with some metrics. The second function
pull down the ticker names and some characteristics as location, ticker
type, and more.

## Aggregate EndPoint

For time data and time EDA

``` r
# create the URL for aggregate endpoint:
# This function has some default values.
agg_endpoint = function(stocksTicker="AAPL", from = "2021-07-22", to = "2022-07-22",mltplr=30, timespan="day"){
  
  # passing the components of the URL for the API:
  
  # base + endpoint 1
  base_endpoint = "https://api.polygon.io/v2/aggs/"
  
  # last part of the URL defining some defaults
  last_code = "?adjusted=true&sort=asc&limit=5000"
  
  # key for accessing API
  key = "&apiKey=asWU9di2FThCr1ywIpgyNdqwXMf0fpj4"
  
  # converting the multiplier to character
  mltplr = as.character(mltplr)
  
  # creating the URL call
  call = paste0(base_endpoint,"ticker/",stocksTicker,"/range/",mltplr,"/",
                timespan,"/",from,"/",to,last_code,key)
  
  # assigning the call to an object
  p = fromJSON(call)

  # getting results from the object
  tb = p$results
  tckr = p$ticker
  
  # working with the dates
  d1 = as.Date(from) # transforms initial date from char to date format
  d2 = as.Date(to) # transforms last date from char to date format
  d = seq(d1,d2, by ="month") # sequence by month
  
  # combining the final object with ticker name, date, and metrics
  out = tibble(tckr,d,tb)
  
  # returning the final tibble object
  return(out)
  
}

# ex.: crypto
u=agg_endpoint(stocksTicker = "X:1INCHUSD")
```

## Grouped Daily EndPoints

For merging with the ticker endpoint data set and go to the EDA.

``` r
grouped_endpoint = function(date= "2022-07-14", adjusted = "true", otc = "true"){
  
  adjusted = tolower(adjusted)
  
  # base + endpoint 1
  base="https://api.polygon.io/v2/aggs/grouped/locale/us/market/stocks/"
  
  # key for accessing API
  key = "&apiKey=asWU9di2FThCr1ywIpgyNdqwXMf0fpj4"
  
  # creating the URL call
  call = paste0(base,date,"?adjusted=",adjusted,"&include_otc=",otc,key)
  
  # assigning the call to an object
  p = fromJSON(call)

  out = tibble(p$results)
  
  return(out)
}

gout = grouped_endpoint(otc = "true")
```

## Ticker EndPoint

This function aims to call tickers from common stock mainly and other
markets as well as crypto currencies for further analysis of both.

``` r
# tickers endpoint= get ticker names
# create the URL for the ticker endpoint - two calls: i) ticker names; and
# ii) crypto names
ticker_endpoint = function(market = "stocks", limit = 1000){
  
  market = tolower(market)
  
  if(limit > 1000){
    limit = 1000
    message("Warning: the max limit is 1000 for free access!")
  }
  
  base_endpoint = "https://api.polygon.io/v3/reference/tickers?market="
  
  last_code = "&active=true&sort=locale&order=asc&limit="
  
  key = "&apiKey=asWU9di2FThCr1ywIpgyNdqwXMf0fpj4"
  
  call = paste0(base_endpoint,market,last_code,limit,key)

  p = fromJSON(call)
  
  return(p$results)

}

#options: crypto, stocks, otc, and fx
tout = ticker_endpoint(market = "stocks", limit = 1000)

tout2 = ticker_endpoint(market = "otc", limit = 1000)
```

## Wrapper Function

This function takes information from the previous two functions and
combine them when it is possible.

``` r
# call multiple tickers from agg_endpoint and return a df
multiple_calls = function(ticker_list){
  
  t = ticker_endpoint(ticker_list)
  
  agg_endpoint(t)
  
  
}
```

# EDA

starts the EDA here!

df object is a tibble, the product of two calls from the API functions
above. You can use this df object to proceed with the EDA part.

df contains the metrics for prices:

c - The close price for the symbol in the given time period

h - The highest price for the symbol in the given time period.

l - The lowest price for the symbol in the given time period.

n - The number of transactions in the aggregate window.

o - The open price for the symbol in the given time period.

otc - Whether or not this aggregate is for an OTC ticker. This field
will be left off if false.

t - The Unix Msec timestamp for the start of the aggregate window.

v - The trading volume of the symbol in the given time period.

vw - The volume weighted average price.

ALSO, df contains some information abou the ticker:

active - Whether or not the asset is actively traded. False means the
asset has been delisted.

cik - The CIK number for this ticker. Find more information here.

composite_figi - The composite OpenFIGI number for this ticker. Find
more information here

currency_name - The name of the currency that this asset is traded with.

delisted_utc - The last date that the asset was traded.

last_updated_utc - The information is accurate up to this time.

locale\*enum \[us, global\] - The locale of the asset.

market\*enum \[stocks, crypto, fx, otc\] - The market type of the asset.

name\*string - The name of the asset. For stocks/equities this will be
the companies registered name. For crypto/fx this will be the name of
the currency or coin pair.

primary_exchange - The ISO code of the primary listing exchange for this
asset.

share_class_figi - The share Class OpenFIGI number for this ticker. Find
more information here

ticker\*string - The exchange symbol that this item is traded under.

type - The type of the asset. Find the types that we support via our
Ticker Types API.

You can play around with these variables are you feel like, but we have
to do some required tasks described on the instructions. I got some
ideas, maybe they help you get started. I am matching variables in both
plots.

some ideas: 1 - categorical data analysis: plot and contingency table:
use variables market and type for categorical analysis (contingency
table and barplots).

There are composite_figi and share_class_figi variables that can also be
used as categorical for the EDA categorical analysis, but there are lots
of them in the data set, so I am not sure if it will be feasible. You
can check if you want to.

2 - For the quantitative EDA All the metrics for prices are there for a
single date in time (defined in the default of the grouped function).
So, here some histograms, boxplots by market or ticker type, or
something like that. Scatter plots between price and another variable
that shows good correlation with it, and other required plots following
the same idea.

I am working on a time data now, it will be multiple calls for different
tickers and with that we can create some line plots and time analysis as
well. For now, the data set below is enough to get all EDA that we need
for the project.

``` r
df1 = inner_join(tout2, gout, by = c("ticker" = "T"))

df11= df1 %>%
  select(ticker, name, market, type, composite_figi,share_class_figi, v:n)

df2 = inner_join(tout, gout, by = c("ticker" = "T"))

df22 = df2 %>%
  select(ticker, name, market, type,composite_figi,share_class_figi, v:n)

df = rbind(df11, df22)

df = df %>% drop_na()

df
```

    ## # A tibble: 871 × 14
    ##    ticker name   market type  compo…¹ share…²      v      vw       o       c       h
    ##    <chr>  <chr>  <chr>  <chr> <chr>   <chr>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 FBMCF  BUFFA… otc    OS    BBG000… BBG001… 1.86e3 9.5 e-3 9.5 e-3 9.5 e-3 9.5 e-3
    ##  2 CNTMF  CANSO… otc    OS    BBG00N… BBG00N… 7.88e4 1.95e-1 2   e-1 1.9 e-1 2.05e-1
    ##  3 BCMRF  BCM R… otc    OS    BBG000… BBG001… 8.02e4 1.28e-1 1.22e-1 1.28e-1 1.3 e-1
    ##  4 CRMK   CERME… otc    CS    BBG000… BBG001… 2   e3 2   e-2 2   e-2 2   e-2 2   e-2
    ##  5 HEOFF  H2O I… otc    OS    BBG000… BBG001… 4.12e3 1.51e+0 1.45e+0 1.54e+0 1.54e+0
    ##  6 BDWBY  BUDWE… otc    ADRC  BBG00Q… BBG00Q… 7.36e3 1.13e+1 1.14e+1 1.12e+1 1.14e+1
    ##  7 HKMPY  HIKMA… otc    ADRC  BBG000… BBG001… 1.58e3 3.99e+1 3.94e+1 3.97e+1 4.05e+1
    ##  8 MKRYF  MANIT… otc    OS    BBG001… BBG001… 5.1 e2 1   e-5 1   e-5 1   e-5 1   e-5
    ##  9 ECAOF  ECO A… otc    OS    BBG000… BBG001… 2.7 e3 3.65e-1 3.61e-1 3.75e-1 3.75e-1
    ## 10 NHMD   NATE'… otc    CS    BBG000… BBG001… 1.89e7 1.46e-3 1.6 e-3 1.4 e-3 1.7 e-3
    ## # … with 861 more rows, 3 more variables: l <dbl>, t <dbl>, n <int>, and
    ## #   abbreviated variable names ¹​composite_figi, ²​share_class_figi
