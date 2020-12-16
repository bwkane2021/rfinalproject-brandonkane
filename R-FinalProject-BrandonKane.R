# File Name:    R-FinalProject-BrandonKane.R
# Name:         Brandon Kane
# Instructor:   Professor Podeschi
# Course:       IS 470 01
# Date:         15 December 2020
# Purpose:      The purpose of this code file is to create data visualizations 
#               such as, ggplot2 bar plots, line plots, and bar and line 
#               combination plots to answer questions related to the relationship
#               between Apple's stock and their net iPhone sales. 

# imported libraries to manipulate data
library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
theme_set(theme_minimal())

# Set working directory and read in the APPL stock prices .csv's
# This creates a data frame for each .csv
setwd("/Users/brandonkane/Documents/Millikin/Fall 2020/IS 470/FinalDataProjectR")
dfAPPL_JunToOct19_StockPrices <- read.csv("IS470_FinalDataProjectR_APPL_JunToOct19_StockPrices.csv")
dfAPPL_JunToOct20_StockPrices <- read.csv("IS470_FinalDataProjectR_APPL_JunToOct20_StockPrices.csv")
dfAPPL_JunToOct19And20_StockPrices <- read.csv("IS470_FinalDataProjectR_APPL_JunToOct19And20_StockPrices.csv")
dfAPPL_3MonthsEnded_Sept2020And2019 <- read.csv("IS470_FinalDataProjectR_Apple_3MonthsEndedSept2020And2019.csv")
dfAPPL_SP500_NASDAQ_JunToOct20_StockPrices <- read.csv("IS470_FinalDataProjectR_APPL_SP500_NASDAQ_JunToOct20_StockPrices.csv")
dfNASDAQ_JunToOct19And20_StockPrices <- read.csv("IS470_FinalDataProjectR_NQ=F_JunToOct19And20_StockPrices.csv")
dfSP500_JunToOct19And20_StockPrices <- read.csv("IS470_FinalDataProjectR_^GSPC_JunToOct19And20_StockPrices.csv")
dfAPPL_JunToOct19AndQ4_StockPrices_iPhoneSales <- read.csv("IS470_FinalDataProjectR_APPL_JunToOct19AndQ4_StockPrices_iPhoneSales.csv")
dfAPPL_JunToOct20AndQ4_StockPrices_iPhoneSales <- read.csv("IS470_FinalDataProjectR_APPL_JunToOct20AndQ4_StockPrices_iPhoneSales.csv")

# Line Plot of Apple's Stock Prices Jun to Oct 2020 and 2019--------------------------------------------------------------------------------

# create year and month variables
dfAPPL_JunToOct19And20_StockPrices$year <- year(dfAPPL_JunToOct19And20_StockPrices$date)
dfAPPL_JunToOct19And20_StockPrices$month <- month(dfAPPL_JunToOct19And20_StockPrices$date)

# Line Plot of Apple's Stock Prices Jun to Oct 2020 and 2019
lnAPPL_JunToOct19And20_StockPrices <- ggplot(dfAPPL_JunToOct19And20_StockPrices, aes(x=month, y=Close))

lnAPPL_JunToOct19And20_StockPrices <- lnAPPL_JunToOct19And20_StockPrices + geom_line(aes(color=factor(year), group=year))

lnAPPL_JunToOct19And20_StockPrices <- lnAPPL_JunToOct19And20_StockPrices + scale_color_discrete(name="Year")

lnAPPL_JunToOct19And20_StockPrices <- lnAPPL_JunToOct19And20_StockPrices + scale_y_continuous(labels=comma)

# Line Plot of Apple's Stock Prices Jun to Oct 2020 
lnAPPL_JunToOct19And20_StockPrices <- lnAPPL_JunToOct19And20_StockPrices + 
  labs(title="Apple Closing Stock Prices Jun to Oct 2020 and 2019", x="Month", y ="Closing Price")

lnAPPL_JunToOct19And20_StockPrices

# create year and month variables
dfNASDAQ_JunToOct19And20_StockPrices$year <- year(dfNASDAQ_JunToOct19And20_StockPrices$date)
dfNASDAQ_JunToOct19And20_StockPrices$month <- month(dfNASDAQ_JunToOct19And20_StockPrices$date)

# Line Plot of NASDAQ's Stock Prices Jun to Oct 2020 and 2019
lnNASDAQ_JunToOct19And20_StockPrices <- ggplot(dfNASDAQ_JunToOct19And20_StockPrices, aes(x=month, y=Close))

lnNASDAQ_JunToOct19And20_StockPrices <- lnNASDAQ_JunToOct19And20_StockPrices + geom_line(aes(color=factor(year), group=year))

lnNASDAQ_JunToOct19And20_StockPrices <- lnNASDAQ_JunToOct19And20_StockPrices + scale_color_discrete(name="Year")

lnNASDAQ_JunToOct19And20_StockPrices <- lnNASDAQ_JunToOct19And20_StockPrices + scale_y_continuous(labels=comma)

lnNASDAQ_JunToOct19And20_StockPrices <- lnNASDAQ_JunToOct19And20_StockPrices + 
  labs(title="NASDAQ Closing Stock Prices Jun to Oct 2020 and 2019", x="Month", y ="Closing Price")

lnNASDAQ_JunToOct19And20_StockPrices

# create year and month variables
dfSP500_JunToOct19And20_StockPrices$year <- year(dfSP500_JunToOct19And20_StockPrices$date)
dfSP500_JunToOct19And20_StockPrices$month <- month(dfSP500_JunToOct19And20_StockPrices$date)

# Line Plot of SP500's Stock Prices Jun to Oct 2020 and 2019
lnSP500_JunToOct19And20_StockPrices <- ggplot(dfSP500_JunToOct19And20_StockPrices, aes(x=month, y=Close))

lnSP500_JunToOct19And20_StockPrices <- lnSP500_JunToOct19And20_StockPrices + geom_line(aes(color=factor(year), group=year))

lnSP500_JunToOct19And20_StockPrices <- lnSP500_JunToOct19And20_StockPrices + scale_color_discrete(name="Year")

lnSP500_JunToOct19And20_StockPrices <- lnSP500_JunToOct19And20_StockPrices + scale_y_continuous(labels=comma)

lnSP500_JunToOct19And20_StockPrices <- lnSP500_JunToOct19And20_StockPrices + 
  labs(title="SP500 Closing Stock Prices Jun to Oct 2020 and 2019", x="Month", y ="Closing Price")

lnSP500_JunToOct19And20_StockPrices

# Apple, NASDAQ, and SP500 Stock Prices Jun to Oct 2020------------------------------------------------------------------------------------

# create month variable
dfAPPL_SP500_NASDAQ_JunToOct20_StockPrices$month <- month(dfAPPL_JunToOct20_StockPrices$date)

# Line Plot of Apple, NASDAQ, and SP500 Stock Prices Jun to Oct 2020 
lnAPPL_SP500_NASDAQ_JunToOct20_StockPrices <- ggplot(dfAPPL_SP500_NASDAQ_JunToOct20_StockPrices, aes(x=month, y=Close))

lnAPPL_SP500_NASDAQ_JunToOct20_StockPrices <- lnAPPL_SP500_NASDAQ_JunToOct20_StockPrices + geom_line(aes(color=factor(stock), group=stock))

lnAPPL_SP500_NASDAQ_JunToOct20_StockPrices <- lnAPPL_SP500_NASDAQ_JunToOct20_StockPrices + scale_color_discrete(name="Stock")

lnAPPL_SP500_NASDAQ_JunToOct20_StockPrices <- lnAPPL_SP500_NASDAQ_JunToOct20_StockPrices + scale_y_continuous(labels=comma)

lnAPPL_SP500_NASDAQ_JunToOct20_StockPrices <- lnAPPL_SP500_NASDAQ_JunToOct20_StockPrices + 
  labs(title="Apple, NASDAQ, and SP500 Closing Stock Prices Jun to Oct 2020", x="Month", y ="Closing Price")

lnAPPL_SP500_NASDAQ_JunToOct20_StockPrices

# create combo chart for Jun to Oct 2019 Apple Stock Prices and iPhone Sales----------------------------------------------------------------

# create year and month variables
dfAPPL_JunToOct19AndQ4_StockPrices_iPhoneSales$month <- month(dfAPPL_JunToOct19AndQ4_StockPrices_iPhoneSales$date)

# create combo chart for Jun to Oct 2019 Apple Stock Prices and iPhone Sales
cmboAPPLJunToOct19AndQ4_StockPrices_iPhoneSales <- ggplot(dfAPPL_JunToOct19AndQ4_StockPrices_iPhoneSales) + 
  geom_col(aes(x = month, y = series1), size = 1, color = "darkblue", fill = "darkblue") +
  geom_line(aes(x = month, y = series2), size = 1.5, color="red", group = 1)

cmboAPPLJunToOct19AndQ4_StockPrices_iPhoneSales <- ggplot(dfAPPL_JunToOct19AndQ4_StockPrices_iPhoneSales) + 
  geom_col(aes(x = month, y = series1), size = 1, color = "darkblue", fill = "darkblue") +
  geom_line(aes(x = month, y = 1*series2), size = .5, color="red", group = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~./1, name = "series2")) + 
  labs(title="Apple Closing Stock Prices and iPhone Sales Jun to Oct 2019")

cmboAPPLJunToOct19AndQ4_StockPrices_iPhoneSales

# create combo chart for Jun to Oct 2020 Apple Stock Prices and iPhone Sales----------------------------------------------------------------

# create year and month variables
dfAPPL_JunToOct20AndQ4_StockPrices_iPhoneSales$month <- month(dfAPPL_JunToOct20AndQ4_StockPrices_iPhoneSales$date)

# create combo chart for Jun to Oct 2020 Apple Stock Prices and iPhone Sales
cmboAPPLJunToOct20AndQ4_StockPrices_iPhoneSales <- ggplot(dfAPPL_JunToOct20AndQ4_StockPrices_iPhoneSales) + 
  geom_col(aes(x = month, y = series1), size = 1, color = "darkblue", fill = "darkblue") +
  geom_line(aes(x = month, y = series2), size = 1.5, color="red", group = 1)

cmboAPPLJunToOct20AndQ4_StockPrices_iPhoneSales <- ggplot(dfAPPL_JunToOct20AndQ4_StockPrices_iPhoneSales) + 
  geom_col(aes(x = month, y = series1), size = 1, color = "darkblue", fill = "darkblue") +
  geom_line(aes(x = month, y = 1*series2), size = .5, color="red", group = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~./1, name = "series2")) + 
  labs(title="Apple Closing Stock Prices and iPhone Sales Jun to Oct 2020")

cmboAPPLJunToOct20AndQ4_StockPrices_iPhoneSales

# ggplot bar plot to compare percent increase or decrease in Apple stock July to Oct 2019 and 2020------------------------------------------

# Increase = New Number - Original Number
intAppleJul19ClosePrice = 52.19
intAppleOct19ClosePrice = 66.81
intAppleJulToOct19ClosePriceIncrease = intAppleOct19ClosePrice - intAppleJul19ClosePrice

# Decrease = Original Number - New Number
intAppleJul20ClosePrice = 129.04
intAppleOct20ClosePrice = 119.05
intAppleJulToOct20ClosePriceDecrease = intAppleJul20ClosePrice - intAppleOct20ClosePrice

# % increase = Increase ÷ Original Number × 100
intAppleJulToOct19ClosePricePercentIncrease = intAppleJulToOct19ClosePriceIncrease/intAppleJul19ClosePrice * 100

# % Decrease = Decrease ÷ Original Number × 100
intAppleJulToOct20ClosePricePercentDecrease = -(intAppleJulToOct20ClosePriceDecrease/intAppleJul20ClosePrice * 100)

# Data frame of percent increase for Apple stock July to Oct 2019 and 2020
dfPercentIncreaseOrDecreaseAppleStock19And20 <- data.frame(monthsAndYear=c('Jul to Oct 2019', 'Jul to Oct 2020'),
      percentIncreaseOrDecreaseAppleStockJulToOct=c(intAppleJulToOct19ClosePricePercentIncrease,intAppleJulToOct20ClosePricePercentDecrease))

# ggplot bar plot to compare percent increase or decrease in Apple stock July to Oct 2019 and 2020
brPercentIncreaseOrDecreaseAppleStockJulToOct19And20 <- ggplot(data=dfPercentIncreaseOrDecreaseAppleStock19And20, aes(x=monthsAndYear, y=percentIncreaseOrDecreaseAppleStockJulToOct, fill=monthsAndYear)) + 
  geom_bar(stat="identity")+theme_minimal()+labs(title="Apple Closing Stock Prices Jul to Oct 2020 and 2019")

brPercentIncreaseOrDecreaseAppleStockJulToOct19And20

# ggplot bar plot to compare percent increase or decrease in Apple's net iPhone sales Jun to Sep 2019 and 2020------------------------------------------

# Data frame of percent decrease for iPhone net sales between Jun to Sept 2020 and 2019-------------------------------------------------------------

# Covers Jun to Sep 2020 and 2019
dfAPPL_3MonthsEnded_Sept2020And2019

# Decrease = Original Number - New Number
intJunToSept2019NetIphoneSales = 33362000000.00
intJunToSept2020NetIphoneSales = 26444000000.00

intJunToSept2020NetIphoneSalesDecrease = intJunToSept2019NetIphoneSales - intJunToSept2020NetIphoneSales

# % Decrease = Decrease ÷ Original Number × 100
intJunToSep2020NetIphoneSalesPercentDecrease = -(intJunToSept2020NetIphoneSalesDecrease /
  intJunToSept2019NetIphoneSales * 100)

# Data frame iPhone net sales Jun to Sept 2019 and 2020
dfIphoneNetSales19And20 <- data.frame(monthsAndYear=c('Jun to Sept 2019', 'Jun to Sept 2020'),
  percentIphoneNetSales19And20=c(intJunToSept2019NetIphoneSales,intJunToSept2020NetIphoneSales))

# Data frame of percent decrease for iPhone net sales Jun to Sept 2019 and 2018-------------------------------------------------------------

# Decrease = Original Number - New Number
intJunToSept2019NetIphoneSales = 33362000000.00
intJunToSept2018NetIphoneSales = 36755000000.00

intJunToSept2019NetIphoneSalesDecrease = intJunToSept2018NetIphoneSales - intJunToSept2019NetIphoneSales

# % Decrease = Decrease ÷ Original Number × 100
intJunToSep2019NetIphoneSalesPercentDecrease = -(intJunToSept2019NetIphoneSalesDecrease /
                                                   intJunToSept2018NetIphoneSales * 100)

# Data frame iPhone net sales Jun to Sept 2019 and 2020
dfIphoneNetSales18And19 <- data.frame(monthsAndYear=c('Jun to Sept 2018', 'Jun to Sept 2019'),
                                                     percentIphoneNetSales18And19=c(intJunToSept2018NetIphoneSales,intJunToSept2019NetIphoneSales))

# Data frame of percent decrease for iPhone net sales Jun to Sept 2019 and 2020
dfPercentDecreaseIphoneNetSales19And20 <- data.frame(monthsAndYear=c('Jun to Sept 2019', 'Jun to Sept 2020'),
                                      percentIphoneNetSales19And20=c(intJunToSep2019NetIphoneSalesPercentDecrease,intJunToSep2020NetIphoneSalesPercentDecrease))

# ggplot bar plot to compare percent decrease iPhone net sales Jun to Sept 2019 and 2020
brPercentIncreaseOrDecreaseJunToSep19And20_NetIphoneSales <- ggplot(data=dfPercentDecreaseIphoneNetSales19And20, aes(x=monthsAndYear, y=percentIphoneNetSales19And20, fill=monthsAndYear)) + 
  geom_bar(stat="identity")+theme_minimal()

brPercentIncreaseOrDecreaseJunToSep19And20_NetIphoneSales

# # ggplot bar plot to compare percent decrease of Apple's stock in 2019 and 2020----------------------------------------------------------

# Percent increase or decrease in Apple stock 1 month before October 2020 iPhone release (Aug 2020 to Sep 2020)------------------------------

# Decrease = Original Number - New Number
intAug20ClosePrice = 129.04
intSep20ClosePrice = 115.81
intAugToSep20ClosePriceDecrease = intAug20ClosePrice - intSep20ClosePrice

# % Decrease = Decrease ÷ Original Number × 100
intAugToSep20ClosePricePercentDecrease = -(intAugToSep20ClosePriceDecrease/intAug20ClosePrice * 100)

# Percent increase or decrease in Apple stock 1 month before September 2019 iPhone release (Jul 2019 to Aug 2019)--------------------------
# Decrease = Original Number - New Number
intJul19ClosePrice = 53.26
intAug19ClosePrice = 52.19
intJulToAug19ClosePriceDecrease = intJul19ClosePrice - intAug19ClosePrice

# % Decrease = Decrease ÷ Original Number × 100
intJulToAug19ClosePricePercentDecrease = -(intJulToAug19ClosePriceDecrease/intJul19ClosePrice * 100)

# Data frame for stock price percentage decreases in 2019 and 2020
dfPercentDecreaseAppleStock19And20 <- data.frame(monthsAndYear=c('Jul to Aug 2019', 'Aug to Sep 2020'),
  percentDecreaseAppleStock1MoBeforeIphoneRelease=c(intJulToAug19ClosePricePercentDecrease,intAugToSep20ClosePricePercentDecrease))

# ggplot bar plot to compare percent decrease of Apple's stock in 2019 and 2020
brPercentDecreaseAppleStock1MoBeforeIphoneRelease <- ggplot(data=dfPercentDecreaseAppleStock19And20, aes(x=monthsAndYear, y=percentDecreaseAppleStock1MoBeforeIphoneRelease, fill=monthsAndYear)) + 
  geom_bar(stat="identity")+theme_minimal()

brPercentDecreaseAppleStock1MoBeforeIphoneRelease
