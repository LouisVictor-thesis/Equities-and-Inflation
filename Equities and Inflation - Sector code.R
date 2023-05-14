###################################### READY R #######################################

# Clearing console
cat("\014")
# Clear environment
rm(list = ls())
# Clear figures
graphics.off()

# Set working directory
#Home
setwd('/Users/victorstaun/OneDrive - CBS - Copenhagen Business School/Speciale/Data/CSV files')

#CBS
#setwd('C:/Users/vije18ad/OneDrive - CBS - Copenhagen Business School/Speciale/Data/CSV files')

#Load packages
library(tidyverse); library(AER); library(stargazer); library(dynlm);
library(quantmod); library(forecast); library(strucchange); library(readr);
library(vars); library(xts); library(mfx); library(fGarch); library(MASS);
library(urca); library(haven); library(plm);library(car); library(pscl); 
library(sandwich); library(lmtest); library(zoo); library(readxl); 
library(MLmetrics); library(stargazer); library(readr); 
library(dplyr); library(car); library(ggplot2);library(fBasics);
library(pander); library(knitr); library(gt); library(gapminder); 
library(writexl); library(data.table); library(rollRegres); 
library(latticeExtra); library(viridis); library(treemap); library(patchwork)
library(hrbrthemes); library(ggthemes); library(scales); library(RColorBrewer)

##################################### READY DATA #####################################

# Load data
TR_data = read.csv("S&P total returns 1989-2022 (hard paste) v8.csv", sep = ";")
Inflation_data = read.csv("Inflation data - FRED v4.csv", sep = ";")
Marketcap_data = read.csv("S&P market value 1989-2022 (hard paste) v2.csv", sep = ";")
SP_500 = read.csv("S&P 500 TR.csv", sep = ";")
Factors = read.csv("Fama French + Momentum.csv", sep = ";")
Yields = read.csv("Yield curve.csv", sep = ";")
Sector_portfolios = read.csv("S&P 500 sector returns.csv", sep = ";")

#Subset sectors
Sectors <- TR_data[,1:3]
Sectors <- Sectors[,-2]
colnames(Sectors)[1] <- "Company"
colnames(Sectors)[2] <- "Sector"

#Transpose total returns dataset
TR_data <- t(TR_data)

#Remove ISIN and GICS codes and make company names be row names
TR_data <- TR_data[-2:-5,]
colnames(TR_data) <- TR_data[1,]
TR_data <- TR_data[-1,]

#Remove the 'X' from the row names
rownames(TR_data) <- sub("^X", "", rownames(TR_data))

#Let dates be in the first column, instead of being row names
TR_data <- cbind(Date=rownames(TR_data),TR_data)
rownames(TR_data) <- NULL

#Convert data to dataframe
TR_data <- as.data.frame(TR_data)

#Let R know the date format in total return and inflation dataset
TR_data$Date <- as.Date(TR_data$Date, format = "%d.%m.%Y")
Inflation_data$Date <- as.Date(Inflation_data$Date, format = "%d-%m-%Y")

#Remove dec 1989 in total return dataset
TR_data <- TR_data[-1,]

#Replace "," with "."
Inflation_data[,-1] <- Inflation_data[,-1] %>%
  mutate_all(list(~ gsub(",", ".", .)))

#Convert CPI Index to numeric values
Inflation_data$US.CPI.Index <- as.numeric(Inflation_data$US.CPI.Index)
Inflation_data$US.CPI.YoY <- as.numeric(Inflation_data$US.CPI.YoY)

#Calculate the month-on-month inflation
Inflation_data$MoM_inflation <- c(NA, diff(Inflation_data$US.CPI.Index) / 
                                    Inflation_data$US.CPI.Index[-nrow(Inflation_data)]) 

#Make a subset of inflation data from 1990 - 2022
Inflation_data <- subset(Inflation_data, 
                         Date >= as.Date("1990-01-31") 
                         & Date <= as.Date("2022-12-31"))

#Merge TR_data and Inflation_data
TR_Inflation <- merge(TR_data, Inflation_data, by = "Date")

#Replace Not incl. with NA
TR_Inflation <- TR_Inflation %>% 
  mutate_at(vars(-1), funs(replace(., . == "Not incl.", NA)))
TR_data <- TR_data %>%
  mutate_at(vars(-1), funs(replace(., . == "Not incl.", NA)))

#Replace "," with "."
TR_Inflation[,-1] <- TR_Inflation[,-1] %>%
  mutate_all(list(~ gsub(",", ".", .)))

#Convert inflation columns to numeric and decimals
TR_Inflation$US.CPI.YoY <- as.numeric(TR_Inflation$US.CPI.YoY)/100
TR_Inflation$US.CPI.YoY.Core <- as.numeric(TR_Inflation$US.CPI.YoY.Core)/100
TR_Inflation$USD.Swap.5Y5Y <- as.numeric(TR_Inflation$USD.Swap.5Y5Y)/100
TR_Inflation$MoM_inflation <- as.numeric(TR_Inflation$MoM_inflation)

#Convert return columns to numeric and decimals without %-sign
TR_Inflation[, c(2:(ncol(TR_Inflation)-6))] <- lapply(TR_Inflation[, c(2:(ncol(TR_Inflation)-6))],
                                                      function(x) as.numeric(gsub("%", "", x))/100)
rownames(TR_Inflation) <- NULL

#Transpose market cap dataset
Marketcap_data <- t(Marketcap_data)

#Convert data to dataframe
Marketcap_data <- as.data.frame(Marketcap_data)

#Remove ISIN codes and make company names be row names
Marketcap_data <- Marketcap_data[-2,]
colnames(Marketcap_data) <- Marketcap_data[1,]
Marketcap_data <- Marketcap_data[-1,]

#Remove the 'X' from the row names
rownames(Marketcap_data) <- sub("^X", "", rownames(Marketcap_data))

#Let dates be in the first column, instead of being row names
Marketcap_data <- cbind(Date=rownames(Marketcap_data),Marketcap_data)
rownames(Marketcap_data) <- NULL

#Let R know the date format market cap dataset
Marketcap_data$Date <- as.Date(Marketcap_data$Date, format = "%d.%m.%Y")

#Replace "," with "."
Marketcap_data[,-1] <- Marketcap_data[,-1] %>%
  mutate_all(list(~ gsub(",", ".", .)))

#Remove date column
Marketcap_data <- Marketcap_data[,-1]

#Remove the last 3 observations
Marketcap_data <- Marketcap_data[-397:-399,]

#Add date column again so it becomes primo values
Marketcap_data <- cbind(TR_Inflation$Date, Marketcap_data)

#Rename date column
colnames(Marketcap_data)[1] <- "Date"

#Convert market cap to numeric values
Marketcap_data[, -1] <- apply(Marketcap_data[, -1], 2, function(x) as.numeric(as.character(x)))

#Let R know the date format SP 500 dataset
SP_500$Date <- as.Date(SP_500$Date, format = "%d-%m-%Y")

#Let R know the date format SP 500 dataset
Factors$Date <- as.Date(Factors$Date, format = "%d-%m-%Y")

#Recalculate factor returns to nominal terms
Factors[,-1] <- Factors[,-1] / 100

#Remove risk free rate
Factors <- Factors[,-6]

#Let R know the date format of Yields data set
Yields$Date <- as.Date(Yields$Date, format = "%d-%m-%Y")

# Remove the 'X' from the column names
colnames(Yields) <- sub("^X", "", colnames(Yields))

#Replace "," with "."
Yields[,-1] <- Yields[,-1] %>%
  mutate_all(list(~ gsub(",", ".", .)))

#Convert Yields data set to numeric values
Yields[, -1] <- apply(Yields[, -1], 2, function(x) as.numeric(as.character(x)))

#Make yield data set percentages
Yields[, -1] <- Yields[, -1] / 100

#Let R know the date format of Sector_portfolios data set
Sector_portfolios$Date <- as.Date(Sector_portfolios$Date, format = "%d/%m/%Y")

#Replace "," with "."
Sector_portfolios[,-1] <- Sector_portfolios[,-1] %>%
  mutate_all(list(~ gsub(",", ".", .)))

#Convert Sector_portfolios data set to numeric values
Sector_portfolios[, -1] <- apply(Sector_portfolios[, -1], 2, function(x) as.numeric(as.character(x)))

Sector_portfolios <- Sector_portfolios[,]

################################### Define period ####################################

# Set start and end dates for the regression
start_date <- as.Date("1990-01-31")
end_date <- as.Date("2022-12-31")

Inflation_data <- Inflation_data[Inflation_data$Date >= start_date & Inflation_data$Date <= end_date, ]
Marketcap_data <- Marketcap_data[Marketcap_data$Date >= start_date & Marketcap_data$Date <= end_date, ]
TR_data <- TR_data[TR_data$Date >= start_date & TR_data$Date <= end_date, ]
TR_Inflation <- TR_Inflation[TR_Inflation$Date >= start_date & TR_Inflation$Date <= end_date, ]
SP_500 <- SP_500[SP_500$Date >= start_date & SP_500$Date <= end_date, ]
Factors <- Factors[Factors$Date >= start_date & Factors$Date <= end_date, ]
Yields <- Yields[Yields$Date >= start_date & Yields$Date <= end_date, ]
Sector_portfolios <- Sector_portfolios[Sector_portfolios$Date >= start_date & Sector_portfolios$Date <= end_date, ]

Date <- TR_Inflation$Date


################################# Sector portfolios ##################################



#Create dataframe to store results
Sector_betas <- data.frame(Company = character(),
                              beta = numeric(),
                              t_value = numeric(),
                              Obs. = numeric(),
                              stringsAsFactors = FALSE)

# Loop through each portfolio column and run a regression
for (i in 2:ncol(Sector_portfolios)) {
  Portfolio_name <- colnames(Sector_portfolios)[i]
  reg_data <- as.data.frame(cbind(Date, 
                                  Sector_portfolios[, i], 
                                  TR_Inflation$MoM_inflation))
  
  colnames(reg_data) <- c("Date", 
                          "Total_Return", 
                          "MoM_inflation")  # Rename the columns
  reg_data$Date <- as.Date(reg_data$Date)
  
  reg_model <- lm(Total_Return ~ MoM_inflation, data = reg_data)  # Run the regression
  
  reg_model_coef <- coeftest(reg_model, vcov = NeweyWest(reg_model, prewhite = FALSE)) # HAC Standard Errors
  
  beta_coef <- reg_model_coef[2,1]
  t_value <- reg_model_coef[2,3]
  obs <- sum(!is.na(reg_data[,2]))
  
  # Add the results to the data frame
  Sector_betas <- rbind(Sector_betas,
                           data.frame(Company = Portfolio_name,
                                      Beta = beta_coef,
                                      t = t_value,
                                      Obs. = obs,
                                      stringsAsFactors = FALSE)) 
}
warnings()
rownames(Sector_betas) <- NULL

#Format the data set with the portfolio coefficients
Sector_betas <- t(Sector_betas)
colnames(Sector_betas) <- Sector_betas[1,]
Sector_betas <- Sector_betas[-1,]
Sector_betas <- as.data.frame(Sector_betas)
Sector_betas[] <- lapply(Sector_betas, as.numeric)

#Create descriptive statistics of sector portfolios
# Create a matrix with 7 rows and 11 columns
Descr_stat_sector <- matrix(nrow = 7,
                                ncol = 11)

# Set column names
colnames(Descr_stat_sector) <- c("Consumer Discretionary",
                                 "Consumer Staples",
                                 "Energy",
                                 "Financials",
                                 "Health Care",
                                 "Indsutrials",
                                 "Communication Services",
                                 "Information Technology",
                                 "Materials",
                                 "Real Estate",
                                 "Utilities")

# Set row names
rownames(Descr_stat_sector) <- c("Annual mean %",
                                 "Annual mean, real %",
                                 "Monthly mean %",
                                 "Maximum %",
                                 "Minimum %",
                                 "Standard Deviation %",
                                 "Obs.")

# Convert matrix to data frame
Descr_stat_sector <- as.data.frame(Descr_stat_sector)

# Loop over the columns and apply the function
for (i in 2:ncol(Sector_portfolios)) { # Start from the second column
  
  #Find annual and monthly returns
  annual_mean <- sum(Sector_portfolios[,i], na.rm = TRUE) * 12 / sum(!is.na(Sector_portfolios[,i]))
  Descr_stat_sector[1, i-1] <- annual_mean * 100
  
  mth_mean <- sum(Sector_portfolios[,i], na.rm = TRUE) / sum(!is.na(Sector_portfolios[,i]))
  Descr_stat_sector[3, i-1] <- mth_mean * 100
  
  
  #Find median, maximum, minimum and number of observations
  Descr_stat_sector[4, i-1] <- max(Sector_portfolios[,i], na.rm = TRUE) * 100
  Descr_stat_sector[5, i-1] <- min(Sector_portfolios[,i], na.rm = TRUE) * 100
  Descr_stat_sector[6, i-1] <- sd(Sector_portfolios[,i], na.rm = TRUE) * 100 * sqrt(12)
  Descr_stat_sector[7, i-1] <- sum(!is.na(Sector_portfolios[,i]))
}

Sector_portfolios_real <- Sector_portfolios[,-1] - TR_Inflation$MoM_inflation
Sector_portfolios_real <- cbind(Date, Sector_portfolios_real)

for (i in 2:ncol(Sector_portfolios_real)) { # Start from the second column
  
  #Find annual real returns
  annual_mean_real <- sum(Sector_portfolios_real[,i], na.rm = TRUE) * 12 / sum(!is.na(Sector_portfolios_real[,i]))
  Descr_stat_sector[2, i-1] <- annual_mean_real * 100
  
}


################################### Define period ####################################

# Set start and end dates for the regression
start_date_new <- as.Date("2021-01-29")
end_date_new <- as.Date("2022-12-31")

Inflation_data <- Inflation_data[Inflation_data$Date >= start_date_new & Inflation_data$Date <= end_date_new, ]
TR_data <- TR_data[TR_data$Date >= start_date_new & TR_data$Date <= end_date_new, ]
TR_Inflation <- TR_Inflation[TR_Inflation$Date >= start_date_new & TR_Inflation$Date <= end_date_new, ]
Sector_portfolios <- Sector_portfolios[Sector_portfolios$Date >= start_date_new & Sector_portfolios$Date <= end_date_new, ]
Sector_portfolios_real <- Sector_portfolios_real[Sector_portfolios_real$Date >= start_date_new & Sector_portfolios_real$Date <= end_date_new, ]


Date <- TR_Inflation$Date


############################ Sector portfolios 2021-2022 #############################

#Create dataframe to store results
Sector_betas_new <- data.frame(Company = character(),
                           beta = numeric(),
                           t_value = numeric(),
                           Obs. = numeric(),
                           stringsAsFactors = FALSE)

# Loop through each portfolio column and run a regression
for (i in 2:ncol(Sector_portfolios)) {
  Portfolio_name <- colnames(Sector_portfolios)[i]
  reg_data <- as.data.frame(cbind(Date, 
                                  Sector_portfolios[, i], 
                                  TR_Inflation$MoM_inflation))
  
  colnames(reg_data) <- c("Date", 
                          "Total_Return", 
                          "MoM_inflation")  # Rename the columns
  reg_data$Date <- as.Date(reg_data$Date)
  
  reg_model <- lm(Total_Return ~ MoM_inflation, data = reg_data)  # Run the regression
  
  reg_model_coef <- coeftest(reg_model, vcov = NeweyWest(reg_model, prewhite = FALSE)) # HAC Standard Errors
  
  beta_coef <- reg_model_coef[2,1]
  t_value <- reg_model_coef[2,3]
  obs <- sum(!is.na(reg_data[,2]))
  
  # Add the results to the data frame
  Sector_betas_new <- rbind(Sector_betas_new,
                        data.frame(Company = Portfolio_name,
                                   Beta = beta_coef,
                                   t = t_value,
                                   Obs. = obs,
                                   stringsAsFactors = FALSE)) 
}
warnings()
rownames(Sector_betas_new) <- NULL

#Format the data set with the portfolio coefficients
Sector_betas_new <- t(Sector_betas_new)
colnames(Sector_betas_new) <- Sector_betas_new[1,]
Sector_betas_new <- Sector_betas_new[-1,]
Sector_betas_new <- as.data.frame(Sector_betas_new)
Sector_betas_new[] <- lapply(Sector_betas_new, as.numeric)

#Create descriptive statistics of sector portfolios
# Create a matrix with 8 rows and 11 columns
Descr_stat_sector_new <- matrix(nrow = 8,
                            ncol = 11)

# Set column names
colnames(Descr_stat_sector_new) <- c("Consumer Discretionary",
                                 "Consumer Staples",
                                 "Energy",
                                 "Financials",
                                 "Health Care",
                                 "Indsutrials",
                                 "Communication Services",
                                 "Information Technology",
                                 "Materials",
                                 "Real Estate",
                                 "Utilities")

# Set row names
rownames(Descr_stat_sector_new) <- c("Annual mean %",
                                 "Annual mean, real %",
                                 "Monthly mean %",
                                 "Maximum %",
                                 "Minimum %",
                                 "Standard Deviation %",
                                 "Obs.",
                                 "Compounded return")

# Convert matrix to dataframe
Descr_stat_sector_new <- as.data.frame(Descr_stat_sector_new)

# Loop over the columns and apply the function
for (i in 2:ncol(Sector_portfolios)) { # Start from the second column
  
  #Find annual and monthly returns
  annual_mean <- sum(Sector_portfolios[,i], na.rm = TRUE) * 12 / sum(!is.na(Sector_portfolios[,i]))
  Descr_stat_sector_new[1, i-1] <- annual_mean * 100
  
  mth_mean <- sum(Sector_portfolios[,i], na.rm = TRUE) / sum(!is.na(Sector_portfolios[,i]))
  Descr_stat_sector_new[3, i-1] <- mth_mean * 100
  
  
  #Find median, maximum, minimum and number of observations
  Descr_stat_sector_new[4, i-1] <- max(Sector_portfolios[,i], na.rm = TRUE) * 100
  Descr_stat_sector_new[5, i-1] <- min(Sector_portfolios[,i], na.rm = TRUE) * 100
  Descr_stat_sector_new[6, i-1] <- sd(Sector_portfolios[,i], na.rm = TRUE) * 100 * sqrt(12)
  Descr_stat_sector_new[7, i-1] <- sum(!is.na(Sector_portfolios[,i]))
  Descr_stat_sector_new[8, i-1]
}

for (i in 2:ncol(Sector_portfolios_real)) { # Start from the second column
  
  #Find annual real returns
  annual_mean_real <- sum(Sector_portfolios_real[,i], na.rm = TRUE) * 12 / sum(!is.na(Sector_portfolios_real[,i]))
  Descr_stat_sector_new[2, i-1] <- annual_mean_real * 100
  
}


#clean data
rm(annual_mean, annual_mean_real, beta_coef, i, obs, mth_mean, Portfolio_name, reg_model_coef, t_value,
   reg_data, reg_model)

