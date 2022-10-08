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

    ##    ticker                      name market type composite_figi share_class_figi
    ## 1   FBMCF        BUFFALO COAL CORP.    otc   OS   BBG000QMJD58     BBG001SSCS82
    ## 2   CNTMF            CANSORTIUM INC    otc   OS   BBG00NB00924     BBG00NB00933
    ## 3   BCMRF              BCM RES CORP    otc   OS   BBG000PTYV02     BBG001SRFK88
    ## 4    CRMK CERMETEK MICROELECTRONICS    otc   CS   BBG000BGB507     BBG001S5Q532
    ## 5   HEOFF        H2O INNOVATION INC    otc   OS   BBG000N5WFY6     BBG001SGQYB6
    ## 6   BDWBY  BUDWEISER BRWNG UNSP/ADR    otc ADRC   BBG00QYWH6Z4     BBG00QYWH7Q2
    ## 7   HKMPY    HIKMA PHARMS PLC S/ADR    otc ADRC   BBG000W5N809     BBG001T6D0K5
    ## 8   MKRYF    MANITOK ENERGY INC ORD    otc   OS   BBG001MHXT87     BBG001T5Q6F6
    ## 9   ECAOF  ECO ATLANTIC OIL&GAS ORD    otc   OS   BBG000TS5QH5     BBG001T0GKK1
    ## 10   NHMD            NATE'S FOOD CO    otc   CS   BBG000R0M2T9     BBG001SSXL11
    ## 11  HSNGY  HANG SENG BANK LTD S/ADR    otc ADRC   BBG000BCR233     BBG001S6BYB4
    ## 12   PDER   PARDEE RESOURCES CO INC    otc   CS   BBG000BDGHL0     BBG001S6XND2
    ## 13  YAMHY  YAMAHA MOTOR CO UNSP/ADR    otc ADRC   BBG000NVZ9N2     BBG001T3P109
    ## 14   ABIT     ATHENA BITCOIN GLOBAL    otc   CS   BBG000CKBF87     BBG001S6Y023
    ## 15  EDTXF  SPECTRAL MEDICAL INC ORD    otc   OS   BBG000BZX3J6     BBG001S609G6
    ## 16  LNVGY    LENOVO GROUP LTD S/ADR    otc ADRC   BBG000BLWMN1     BBG001S966T6
    ## 17  CJEWY CHOW TAI FOOK JEWELRY ADR    otc ADRC   BBG0038SP112     BBG0038SP1T2
    ## 18  PTOAF   PIERIDAE ENERGY LIMITED    otc   OS   BBG000LHKG28     BBG001SK0MJ3
    ## 19   IPIX INNOVATION PHARMACEUTICAL    otc   CS   BBG000BF2Q94     BBG001S8STW0
    ## 20  MWSNF           MAWSON GOLD LTD    otc   OS   BBG000P1CSJ4     BBG001SL2CQ3
    ## 21   FSCR       FEDERAL SCREW WORKS    otc   CS   BBG000BJX213     BBG001S5RCQ1
    ## 22  ESALY            EISAI CO S/ADR    otc ADRC   BBG000BXH728     BBG001S98X88
    ## 23  GUYGF         G2 GOLDFIELDS INC    otc   CS   BBG001730W02     BBG001SR4TD5
    ## 24  FDVXF        FENIXORO GOLD CORP    otc   OS   BBG00LMYXCG4     BBG00LMYXDG2
    ## 25   ICNB         ICONIC BRANDS INC    otc   CS   BBG000THC4F2     BBG001T05T15
    ## 26   ICCT          ICORECONNECT INC    otc   CS   BBG006TM0XQ5     BBG006TM0XX7
    ## 27  MNDJF         MANDALAY RES CORP    otc   OS   BBG000DBGJQ3     BBG001SHZ3L1
    ## 28   NUVM                 NUVIM INC    otc   CS   BBG000BTNJV2     BBG001SHR3F7
    ## 29  JUVAF             JUVA LIFE INC    otc   OS   BBG00XYJ9YF8     BBG00XYJ9Z83
    ## 30  ATRWF ALTIUS RENEWABLE ROYALTIS    otc   OS   BBG00Y0B1HZ3     BBG00Y0B1J07
    ## 31  MXROF          MAX RESOURCE CRP    otc   OS   BBG000BS2XT9     BBG001S6Z246
    ## 32  ALNPY    ANA HOLDINGS INC S/ADR    otc ADRC   BBG000BV8G31     BBG001S7QNR3
    ## 33  SITKF           SITKA GOLD CORP    otc   OS   BBG00GXJ53M0     BBG00GXJ53N9
    ## 34   QBAN            TELCO CUBA INC    otc   CS   BBG000BLZC96     BBG001SP8TQ1
    ## 35  AIQUY L'AIR LIQUIDE SA UNSP/ADR    otc ADRC   BBG000BKTQ12     BBG001S8R1D3
    ## 36  MGLDF MEDGOLD RESOURCES CRP ORD    otc   OS   BBG000BXKM49     BBG001S5XTV1
    ## 37   PSFT       POWERSAFE TECH CORP    otc   CS   BBG000TGJ573     BBG001T03YY0
    ## 38   GBTC   GRAYSCALE BITCOIN TRUST    otc   CS   BBG008748J88     BBG008748J97
    ## 39   BERI        BLUE EARTH RES INC    otc   CS   BBG000DPD8B5     BBG001SDWCT5
    ## 40   MSBN             MESSABEN CORP    otc   CS   BBG000JKSNW8     BBG001T3NLH9
    ## 41   JMIH JUPITER MARINE INTL HLDGS    otc   CS   BBG000C0MN82     BBG001SD3M06
    ## 42   WYPH WAYPOINT BIOMED HLDGS INC    otc   CS   BBG000P36FP8     BBG001SL3GQ3
    ## 43  HANNF     HANNAN METALS LTD ORD    otc   OS   BBG000BBFJH6     BBG001S5Y4W4
    ## 44   GLEC       GLOBAL ECOLOGY CORP    otc   CS   BBG000BRY3J3     BBG001S9TFR3
    ## 45  WHGOF       WHITE GOLD CORP ORD    otc   OS   BBG000BZ3586     BBG001SBL687
    ## 46   ADHI ARSENAL DIGITAL HLDGS INC    otc   CS   BBG000Q1F5K8     BBG001SM23F3
    ## 47   OSCI          OSCEOLA GOLD INC    otc   CS   BBG000Q09Q58     BBG001T66TW7
    ## 48  LTMCF         LITHIUM CHILE INC    otc   OS   BBG0018NR8Y4     BBG001TFBJY1
    ## 49  CANOF CALIFORNIA NANOTECHS CORP    otc   OS   BBG000QMYCQ0     BBG001SN7G49
    ## 50   MFON    MOBIVITY HOLDINGS CORP    otc   CS   BBG000PZ4V38     BBG001T657V7
    ## 51  JBAXY  JULIUS BAER GRP UNSP/ADR    otc ADRC   BBG000PT2PG3     BBG001T5ZDQ9
    ## 52   HQGE   HQ GLOBAL EDUCATION INC    otc   CS   BBG000BBL491     BBG001S7R9W7
    ## 53   MRNJ              METATRON INC    otc   CS   BBG000BHNQY9     BBG001SB3X33
    ## 54   MNTR        MENTOR CAPITAL INC    otc   CS   BBG000C1KW64     BBG001SC0BC2
    ## 55  FLOOF   FLOWER ONE HOLDINGS INC    otc   OS   BBG000R2WB32     BBG001ST12P0
    ## 56   HWNI   HIGH WIRE NETWORKS INC.    otc   CS   BBG000BFMGM9     BBG001S99Y12
    ## 57   REPO   NATIONAL ASSET RECOVERY    otc   CS   BBG000BDRS43     BBG001SDB559
    ## 58  DFIFF    DIAMOND FIELDS RES INC    otc   OS   BBG000BCJS71     BBG001S64NZ0
    ## 59   VGLS      VG LIFE SCIENCES INC    otc   CS   BBG000CPS8S2     BBG001SFC5F2
    ## 60  CNNEF        CANACOL ENERGY ORD    otc   OS   BBG000C12RY4     BBG001S65X32
    ## 61  TWMIF TIDEWATER MIDSTRM & INFRA    otc   OS   BBG008871P56     BBG008871P65
    ## 62  CNIKF      CANADA NICKEL CO INC    otc   OS   BBG00QGG7B34     BBG00QGG7B43
    ## 63  IDCBY INDUSTRIAL & COM UNSP/ADR    otc ADRC   BBG000RLS431     BBG001T3SGX7
    ## 64  UNCRY   UNICREDITO SPA UNSP/ADR    otc ADRC   BBG00HNL0GJ4     BBG00HNL0H75
    ## 65   PHBI   PHARMAGREEN BIOTECH INC    otc   CS   BBG000H1B586     BBG001T33Z26
    ## 66  EKTAY     ELEKTA B SHS UNSP/ADR    otc ADRC   BBG000KK09J2     BBG001T0K066
    ## 67  EXCOF            EXCO TECHS LTD    otc   OS   BBG000C01N85     BBG001S60YB6
    ## 68   ITOX             IIOT-OXYS INC    otc   CS   BBG000BYF8X2     BBG001SJR6S2
    ## 69   CGRW    CANNAGROW HOLDINGS INC    otc   CS   BBG000CM3V23     BBG001S82H27
    ## 70  EJPRY   EAST JAPAN RWY UNSP/ADR    otc ADRC   BBG000FMZ899     BBG001T2HYY1
    ## 71   LQWC        LIFEQUEST WORLD CP    otc   CS   BBG000BXJH06     BBG001SCWKB8
    ##             v           vw         o         c         h          l            t
    ## 1        1859   0.00950000   0.00950   0.00950   0.00950   0.009500 1.657829e+12
    ## 2       78797   0.19492000   0.20000   0.19000   0.20500   0.190000 1.657829e+12
    ## 3       80250   0.12845000   0.12220   0.12765   0.13000   0.122200 1.657829e+12
    ## 4        2000   0.02000000   0.02000   0.02000   0.02000   0.020000 1.657829e+12
    ## 5        4115   1.51020000   1.45000   1.54000   1.54000   1.450000 1.657829e+12
    ## 6        7358  11.32390000  11.37500  11.21250  11.37500  11.071000 1.657829e+12
    ## 7        1578  39.94920000  39.37000  39.72500  40.53000  39.370000 1.657829e+12
    ## 8         510   0.00001000   0.00001   0.00001   0.00001   0.000010 1.657829e+12
    ## 9        2700   0.36463000   0.36100   0.37500   0.37500   0.361000 1.657829e+12
    ## 10   18892471   0.00145790   0.00160   0.00140   0.00170   0.001250 1.657829e+12
    ## 11      56471  16.43510000  16.80000  16.46000  16.80000  16.110000 1.657829e+12
    ## 12        225 242.93520000 250.00000 244.01000 250.00000 244.010000 1.657829e+12
    ## 13       7737   8.82350000   8.67000   8.80500   8.93000   8.660000 1.657829e+12
    ## 14       4045   0.30294000   0.30000   0.30000   0.34000   0.300000 1.657829e+12
    ## 15      11000   0.29518000   0.31000   0.28820   0.31000   0.288100 1.657829e+12
    ## 16      34985  18.09540000  18.10010  18.12000  18.40000  17.990000 1.657829e+12
    ## 17        221  18.80600000  18.80500  18.80500  18.80500  18.805000 1.657829e+12
    ## 18       1216   0.61204000   0.61213   0.61213   0.61213   0.612130 1.657829e+12
    ## 19     312002   0.04540800   0.04700   0.04600   0.04700   0.043000 1.657829e+12
    ## 20      54340   0.08280400   0.08000   0.08150   0.08500   0.080000 1.657829e+12
    ## 21        100   7.14000000   7.14000   7.14000   7.14000   7.140000 1.657829e+12
    ## 22      20474  44.82200000  45.39000  44.90000  45.39000  44.600000 1.657829e+12
    ## 23      10000   0.42833000   0.43915   0.41890   0.43915   0.418900 1.657829e+12
    ## 24       5700   0.12369000   0.12000   0.13090   0.13445   0.120000 1.657829e+12
    ## 25      10730   0.37367000   0.37060   0.36600   0.38240   0.366000 1.657829e+12
    ## 26      10100   0.06970000   0.06970   0.06970   0.06970   0.069700 1.657829e+12
    ## 27      22005   1.80330000   1.91000   1.70500   1.91500   1.690000 1.657829e+12
    ## 28      50200   0.01000000   0.01000   0.01000   0.01000   0.010000 1.657829e+12
    ## 29      75329   0.14508000   0.14710   0.14880   0.14880   0.140020 1.657829e+12
    ## 30       3991   6.45060000   6.39500   6.51900   6.51900   6.274000 1.657829e+12
    ## 31      31851   0.28324000   0.29000   0.27175   0.29500   0.270100 1.657829e+12
    ## 32      14958   3.45720000   3.50000   3.46000   3.56000   3.430000 1.657829e+12
    ## 33       3000   0.10400000   0.10400   0.10400   0.10400   0.104000 1.657829e+12
    ## 34  113009989   0.00040842   0.00050   0.00050   0.00050   0.000300 1.657829e+12
    ## 35     353050  24.87260000  24.76000  24.98500  25.06000  24.555000 1.657829e+12
    ## 36      38461   0.01300000   0.01300   0.01300   0.01300   0.013000 1.657829e+12
    ## 37      10000   0.00100000   0.00100   0.00100   0.00100   0.001000 1.657829e+12
    ## 38    3358071  12.88640000  12.35000  13.13000  13.30000  12.320000 1.657829e+12
    ## 39     484436   0.12792000   0.19150   0.12000   0.19150   0.119000 1.657829e+12
    ## 40      24900   0.29094000   0.30030   0.28000   0.30300   0.280000 1.657829e+12
    ## 41      10000   0.00100000   0.00100   0.00100   0.00100   0.001000 1.657829e+12
    ## 42      16000   0.04031300   0.04500   0.03800   0.04500   0.038000 1.657829e+12
    ## 43      69280   0.15648000   0.17530   0.16170   0.17530   0.144060 1.657829e+12
    ## 44      68900   0.00020000   0.00020   0.00020   0.00020   0.000200 1.657829e+12
    ## 45      28612   0.29512000   0.29830   0.28720   0.29830   0.280500 1.657829e+12
    ## 46        600   0.13800000   0.13800   0.13800   0.13800   0.138000 1.657829e+12
    ## 47     169135   0.04823100   0.05000   0.05000   0.05000   0.043100 1.657829e+12
    ## 48      53225   0.34984000   0.35148   0.36000   0.36000   0.337215 1.657829e+12
    ## 49      30900   0.06338200   0.06396   0.06500   0.06500   0.063000 1.657829e+12
    ## 50        100   1.14000000   1.14000   1.14000   1.14000   1.140000 1.657829e+12
    ## 51     110104   8.61860000   8.58000   8.64000   8.66000   8.535000 1.657829e+12
    ## 52      48024   0.00010000   0.00010   0.00010   0.00010   0.000100 1.657829e+12
    ## 53    6583500   0.00019998   0.00020   0.00010   0.00020   0.000100 1.657829e+12
    ## 54        482   0.04470000   0.04480   0.04480   0.04480   0.044800 1.657829e+12
    ## 55     115546   0.02664000   0.02360   0.02500   0.02790   0.021600 1.657829e+12
    ## 56     682055   0.06694200   0.05600   0.07750   0.08500   0.056000 1.657829e+12
    ## 57     113703   0.06599100   0.06645   0.05500   0.07500   0.051000 1.657829e+12
    ## 58       3707   0.17629000   0.17940   0.17000   0.17940   0.170000 1.657829e+12
    ## 59 1139334498   0.00022210   0.00030   0.00030   0.00030   0.000100 1.657829e+12
    ## 60      48367   1.70940000   1.72000   1.71000   1.73000   1.690000 1.657829e+12
    ## 61       5530   0.93494000   0.92975   0.93904   0.93904   0.929750 1.657829e+12
    ## 62      21310   1.24100000   1.21000   1.22000   1.28000   1.180000 1.657829e+12
    ## 63     142442  10.26020000  10.48000  10.26000  10.52000   9.840000 1.657829e+12
    ## 64     524056   4.18160000   4.24000   4.20000   4.38000   4.090000 1.657829e+12
    ## 65      34605   0.00831980   0.00850   0.00810   0.00850   0.008100 1.657829e+12
    ## 66      61025   6.45270000   6.48000   6.47000   6.51000   6.390000 1.657829e+12
    ## 67        898   5.99940000   5.99700   5.99700   5.99700   5.997000 1.657829e+12
    ## 68     476000   0.00593150   0.00610   0.00600   0.00610   0.006000 1.657829e+12
    ## 69       1600   0.01505000   0.01540   0.01540   0.01540   0.015000 1.657829e+12
    ## 70      94113   7.90030000   7.88000   7.89000   7.95000   7.860000 1.657829e+12
    ## 71      11068   0.03333500   0.03500   0.03200   0.03800   0.032000 1.657829e+12
    ##       n
    ## 1     2
    ## 2    27
    ## 3    13
    ## 4     1
    ## 5    16
    ## 6    10
    ## 7    15
    ## 8     1
    ## 9     2
    ## 10   62
    ## 11   95
    ## 12    6
    ## 13   16
    ## 14    8
    ## 15    5
    ## 16  144
    ## 17    4
    ## 18    3
    ## 19   33
    ## 20   33
    ## 21    1
    ## 22   93
    ## 23    6
    ## 24    4
    ## 25   11
    ## 26    2
    ## 27   30
    ## 28    2
    ## 29   10
    ## 30   23
    ## 31   12
    ## 32   41
    ## 33    1
    ## 34   91
    ## 35  703
    ## 36    1
    ## 37    1
    ## 38 9972
    ## 39   45
    ## 40    7
    ## 41    1
    ## 42    3
    ## 43   19
    ## 44    1
    ## 45    7
    ## 46    1
    ## 47   22
    ## 48   28
    ## 49    5
    ## 50    1
    ## 51  174
    ## 52    4
    ## 53   12
    ## 54    2
    ## 55    5
    ## 56   44
    ## 57   29
    ## 58    6
    ## 59  285
    ## 60   31
    ## 61    4
    ## 62   38
    ## 63  140
    ## 64  300
    ## 65    4
    ## 66   65
    ## 67    5
    ## 68    6
    ## 69    3
    ## 70  146
    ## 71    9
    ##  [ reached 'max' / getOption("max.print") -- omitted 800 rows ]
