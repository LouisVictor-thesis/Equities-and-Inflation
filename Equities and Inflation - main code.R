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
library(urca); library(haven); library(plm);library(car); 
library(sandwich); library(lmtest); library(zoo); library(readxl); 
library(MLmetrics); library(stargazer); library(readr); 
library(dplyr); library(car); library(ggplot2);library(fBasics);
library(pander); library(knitr); library(gt); library(gapminder); 
library(writexl); library(data.table); library(rollRegres); 
library(latticeExtra); library(viridis); library(treemap); library(patchwork)
library(hrbrthemes); library(ggthemes); library(scales); library(RColorBrewer);
library(extrafont)

##################################### READY DATA #####################################

#Load data
TR_data = read.csv("S&P total returns 1989-2022 (hard paste) v8.csv", sep = ";")
Inflation_data = read.csv("Inflation data - FRED v4.csv", sep = ";")
Marketcap_data = read.csv("S&P market value 1989-2022 (hard paste) v2.csv", sep = ";")
SP_500 = read.csv("S&P 500 TR.csv", sep = ";")
Factors = read.csv("Fama French + Momentum.csv", sep = ";")
Yields = read.csv("Yield curve.csv", sep = ";")

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

# Remove the 'X' from the row names
rownames(TR_data) <- sub("^X", "", rownames(TR_data))

# dates in the first column, instead of being row names
TR_data <- cbind(Date=rownames(TR_data),TR_data)
rownames(TR_data) <- NULL

# Convert TR_data to dataframe
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

# Calculate the month-on-month inflation
Inflation_data$MoM_inflation <- c(NA, diff(Inflation_data$US.CPI.Index) / 
                                    Inflation_data$US.CPI.Index[-nrow(Inflation_data)]) 

#Make a subset of inflation data from 1990 - 2022
Inflation_data <- subset(Inflation_data, 
                         Date >= as.Date("1990-01-31") 
                         & Date <= as.Date("2022-12-31"))

# Merge TR_data and Inflation_data
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

Date <- TR_Inflation$Date


################################# Data vizualisation #################################
####Line graph: CPI YoY and MoM 1990-2022####
Inflation_data$US.CPI.YoY_pct <- Inflation_data$US.CPI.YoY / 100

CPI_YoY <- ggplot(Inflation_data, aes(x=Date, y=US.CPI.YoY_pct)) +
  geom_line(color="Black", size=0.5, linetype=1) +
  ggtitle("US CPI YoY 1990-2022") +
  ylab(label = NULL) +
  scale_x_date(labels = date_format("%Y"), 
               limits = as.Date(c("1990-01-01", "2022-12-31")), 
               expand = c(0,0),
               date_breaks = ("4 years")) +
  scale_y_continuous(breaks=c(-0.02,0,0.02,0.04,0.06,0.08,0.10), 
                     labels = percent,
                     limits = c(-0.025,0.1),
                     expand = c(0,0)) +
  geom_hline(yintercept=0, 
             color = "grey", 
             size=0.3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        axis.title.x = element_blank())

CPI_YoY


CPI_MoM <- ggplot(Inflation_data, aes(x=Date, y=MoM_inflation)) +
  geom_line(color="Black", size=0.5, linetype=1) +
  ggtitle("US CPI MoM 1990-2022") +
  ylab(label = NULL) +
  scale_x_date(labels = date_format("%Y"), 
               limits = as.Date(c("1990-01-01", "2022-12-31")), 
               expand = c(0,0),
               date_breaks = ("4 years")) +
  scale_y_continuous(breaks=c(-0.02,-0.015,-0.01,-0.005,0,0.005,0.01,0.015,0.02,0.025, 0.03), 
                     labels = percent,
                     limits = c(-0.02, 0.015),
                     expand = c(0,0)) +
  geom_hline(yintercept=0, 
             color = "grey", 
             size=0.3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        axis.title.x = element_blank())

CPI_MoM

CPI_YoY + CPI_MoM


####Line graph: 3M and 2Y yields 1990-2022####

`3M+2Y` <- ggplot(Yields, aes(x=Date)) +
  geom_line(aes(y = `3M`, colour = "3M"), size=0.5, linetype=1) +
  geom_line(aes(y = `2Y`, colour = "2Y"), size=0.5, linetype=1) +
  scale_colour_manual("",
                      values = c("3M"="black", "2Y"="dark grey")) +
  ggtitle("US Government Yields 1990-2022") +
  ylab(label = NULL) +
  scale_x_date(labels = date_format("%Y"), 
               limits = as.Date(c("1990-01-01", "2022-12-31")), 
               expand = c(0,0),
               date_breaks = ("4 years")) +
  scale_y_continuous(breaks=c(-0.01,0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09), 
                     labels = percent,
                     limits = c(-0.01,0.09),
                     expand = c(0,0),) +
  geom_hline(yintercept=0, 
             color = "grey", 
             size=0.3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        text=element_text(family="Times New Roman", size=11))

`3M+2Y`


####Line Graph: S&P 500 1990-2022####
`S&P500` <- ggplot(SP_500, aes(x=Date, y=S.P.500.TR.USD)) +
  geom_line(color="Black", size=0.5, linetype=1) +
  ggtitle("S&P 500 index 1990-2022") +
  ylab(label = NULL) +
  scale_x_date(labels = date_format("%Y"), 
               limits = as.Date(c("1990-01-01", "2022-12-31")), 
               expand = c(0,0),
               date_breaks = ("4 years")) +
  scale_y_continuous(breaks=c(-0.2,-0.15,-0.1,-0.05,0,0.05,0.1,0.15,0.2), 
                     labels = percent,
                     limits = c(-0.2,0.2),
                     expand = c(0,0)) +
  geom_hline(yintercept=0, 
             color = "grey", 
             size=0.3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        axis.title.x = element_blank())

`S&P500`

######################## Linear regression of inflation betas ########################

# Create an empty data frame to store the results
Equity_betas <- data.frame(Company = character(),
                           Annual.mean = numeric(),
                           Annual.mean.real = numeric(),
                           beta = numeric(), 
                           t_value = numeric(),
                           Obs. = numeric(),
                           stringsAsFactors = FALSE)


# Loop through each equity column and run a regression if there are at least 60 non NA observations
for (i in 2:(ncol(TR_Inflation)-6)) {
  equity_name <- colnames(TR_Inflation)[i]
  equity_data <- TR_Inflation[, c(1, i, ncol(TR_Inflation))]
  if (sum(!is.na(equity_data[,2])) >= 60) {
    reg_data <- na.omit(equity_data)  # Remove rows with NA
    colnames(reg_data) <- c("Date", 
                            "Total_Return", 
                            "MoM_inflation")  # Rename the columns
    
    reg_model <- lm(Total_Return ~ MoM_inflation, data = reg_data)  # Run the regression
    
    # durbin_test<- dwtest(formula = reg_model, alternative = "two.sided") #Run Durbin-Watson test
    
    reg_model_coef <- coeftest(reg_model, vcov = NeweyWest(reg_model, prewhite = FALSE)) # HAC Standard Errors
    
    beta_coef <- reg_model_coef[2,1]
    t_value <- reg_model_coef[2,3]
    
    # beta_coef <- reg_model$coefficients[2]  # Get the beta coefficient
    # t_value <- summary(reg_model)$coefficients[2,3] # Get the t-value
    obs <- sum(!is.na(equity_data[,2]))
    # p_value <- durbin_test$p.value
    
    annual_mean <- equity_data[,2] * 12
    annual_mean <- sum(annual_mean, na.rm = TRUE) / obs
    
    annual_mean_real <- (equity_data[,2] - equity_data[,3]) * 12
    annual_mean_real <- sum(annual_mean_real, na.rm = TRUE) / obs
    
    
    
    # Add the results to the data frame
    Equity_betas <- rbind(Equity_betas, 
                          data.frame(Company = equity_name, 
                                     Annual.mean = annual_mean,
                                     Annual.mean.real = annual_mean_real,
                                     Beta = beta_coef, 
                                     t = t_value,
                                     Obs. = obs,
                                     stringsAsFactors = FALSE))
                          
    # Dw_test <- rbind(Dw_test,
    #                  data.frame(Company = equity_name,
    #                             p_value = p_value))
  }
}


# Dw_test[,1] <- lapply(Dw_test[,1], round, 4)
# Dw_test <- Dw_test %>% mutate(across(where(is.numeric), ~ round(., 4)))

#SP 500 regression
reg_model_SP500 <- lm(SP_500$S.P.500.TR.USD ~ TR_Inflation$MoM_inflation)

reg_model_SP500_coef <- coeftest(reg_model_SP500, vcov = NeweyWest(reg_model_SP500, prewhite = FALSE)) # HAC Standard Errors

beta_coef_SP500 <- reg_model_SP500_coef[2,1]
t_value_SP500 <- reg_model_SP500_coef[2,3]

# beta_coef_SP500 <- reg_model_SP500$coefficients[2]  # Get the beta coefficient
# t_value_SP500 <- summary(reg_model_SP500)$coefficients[2,3] # Get the t-value
obs_SP500 <- nrow(SP_500)
annual_mean_SP500 <- sum(SP_500[,2] * 12) / obs_SP500
annual_mean_real_SP500 <- sum((SP_500[,2] - TR_Inflation$MoM_inflation) * 12) / obs_SP500

SP500_beta <- data.frame(Company = as.character("S&P 500"),
                         "Sector" = as.character(""),
                         "Annual mean" = as.numeric(annual_mean_SP500),
                         "Annual mean real" = as.numeric(annual_mean_real_SP500),
                         Beta = as.numeric(beta_coef_SP500),
                         t = as.numeric(t_value_SP500),
                         Obs. = as.numeric(obs_SP500),
                         stringsAsFactors = FALSE)

#Clean dataframe
rownames(Equity_betas) <- NULL
rownames(SP500_beta) <- NULL

#Rank after highest beta
Equity_betas <- Equity_betas[order(Equity_betas$Beta, decreasing = TRUE), ]

#Add sectors to equity beta list
Equity_betas$Sector <- NA
Equity_betas <- Equity_betas %>% relocate(Sector, .after = Company)

# Merge the two data frames based on "Company"
merged_data <- merge(Equity_betas, Sectors, by = "Company", all.x = TRUE)

# Fill the Sector column in Equity_betas with corresponding values from Sectors
Equity_betas$Sector <- merged_data$Sector.y[match(Equity_betas$Company, merged_data$Company)]

rm(merged_data)

# Subset the top 20 rows of Equity_betas
top20_Equity_betas <- rbind(Equity_betas[1:20,], SP500_beta)
top20_Equity_betas$Annual.mean <- top20_Equity_betas$Annual.mean * 100
top20_Equity_betas$Annual.mean.real <- top20_Equity_betas$Annual.mean.real * 100

#Subset the top 100 rows of Equity_betas
top100_Equity_betas <- Equity_betas[1:100,]

################################# Portfolio Analysis #################################

# #Include this code to only use significant betas at 10% significance level
# Equity_betas <- Equity_betas[Equity_betas$t >= 1.671 | Equity_betas$t <= -1.671,]

#Extract list of equities from the Equity_betas dataframe
equities <- Equity_betas[, 1]

#Create dataset only with researchable equities (DO NOT DELETE - IT ENSURES THE RANKING OF EQUITIES)
Portfolio_equities <- TR_Inflation[, c("Date", equities[equities %in% colnames(TR_Inflation)])]
Portfolio_marketcap <- Marketcap_data[, c("Date", equities[equities %in% colnames(Marketcap_data)])]


##Create portfolios##

# Create dataframes for each portfolio by removing nonresearchable equities
Portfolio_1 <- Portfolio_equities[,-1]
Portfolio_2 <- Portfolio_equities[,-1]
Portfolio_3 <- Portfolio_equities[,-1]
Portfolio_4 <- Portfolio_equities[,-1]
Portfolio_5 <- Portfolio_equities[,-1]

#Loop through each month in the portfolio dataframes within the start and end date defined in the regression
for (i in 1:nrow(Portfolio_equities)) {
  
  #Count number of nonmissing values in row 1
  num_non_missing <- sum(!is.na(Portfolio_equities[i,-1]))
  
  #Remove nonmissing values in row i (except for the first x non-missing values)
  x <- floor(num_non_missing / 5)
  row_i_1 <- Portfolio_1[i,]
  row_i_2 <- Portfolio_2[i,]
  row_i_3 <- Portfolio_3[i,]
  row_i_4 <- Portfolio_4[i,]
  row_i_5 <- Portfolio_5[i,]
  
  non_missing_1 <- which(!is.na(row_i_1))
  non_missing_2 <- which(!is.na(row_i_2))
  non_missing_3 <- which(!is.na(row_i_3))
  non_missing_4 <- which(!is.na(row_i_4))
  non_missing_5 <- which(!is.na(row_i_5))
  
  row_i_1[non_missing_1[-c(1:x)]] <- NA
  row_i_2[non_missing_2[-c((x+1):(2*x))]] <- NA
  row_i_3[non_missing_3[-c((2*x+1):(3*x))]] <- NA
  row_i_4[non_missing_4[-c((3*x+1):(4*x))]] <- NA
  row_i_5[non_missing_5[-c((4*x+1):num_non_missing)]] <- NA
  
  Portfolio_1[i,] <- row_i_1
  Portfolio_2[i,] <- row_i_2
  Portfolio_3[i,] <- row_i_3
  Portfolio_4[i,] <- row_i_4
  Portfolio_5[i,] <- row_i_5
}

#Add date column
Portfolio_1 <- cbind(Date, Portfolio_1)
Portfolio_2 <- cbind(Date, Portfolio_2)
Portfolio_3 <- cbind(Date, Portfolio_3)
Portfolio_4 <- cbind(Date, Portfolio_4)
Portfolio_5 <- cbind(Date, Portfolio_5)


##Find weights for each portfolio##

#Make datasets with market cap for the portfolios
Marketcap_1 <- Portfolio_marketcap
Marketcap_1[is.na(Portfolio_1)] <- NA

Marketcap_2 <- Portfolio_marketcap
Marketcap_2[is.na(Portfolio_2)] <- NA

Marketcap_3 <- Portfolio_marketcap
Marketcap_3[is.na(Portfolio_3)] <- NA

Marketcap_4 <- Portfolio_marketcap
Marketcap_4[is.na(Portfolio_4)] <- NA

Marketcap_5 <- Portfolio_marketcap
Marketcap_5[is.na(Portfolio_5)] <- NA

Marketcap_SP500 <- Marketcap_data
Marketcap_SP500[is.na(TR_data)] <- NA

#Find vector with the sum of market caps for each month
Cap_p1 <- rowSums(Marketcap_1[,-1], na.rm = TRUE)
Cap_p2 <- rowSums(Marketcap_2[,-1], na.rm = TRUE)
Cap_p3 <- rowSums(Marketcap_3[,-1], na.rm = TRUE)
Cap_p4 <- rowSums(Marketcap_4[,-1], na.rm = TRUE)
Cap_p5 <- rowSums(Marketcap_5[,-1], na.rm = TRUE)
Cap_SP500 <- rowSums(Marketcap_SP500[,-1], na.rm = TRUE)

#Divide individual market caps by the total portfolio market cap for each month
W_portfolio_1 <- cbind(Date, Marketcap_1[,-1] / Cap_p1)
W_portfolio_2 <- cbind(Date, Marketcap_2[,-1] / Cap_p2)
W_portfolio_3 <- cbind(Date, Marketcap_3[,-1] / Cap_p3)
W_portfolio_4 <- cbind(Date, Marketcap_4[,-1] / Cap_p4)
W_portfolio_5 <- cbind(Date, Marketcap_5[,-1] / Cap_p5)
W_SP500 <- cbind(Date, Marketcap_SP500[,-1] / Cap_SP500)

##data sets ready for descriptive statistics##

#Create data sets with weighted real returns
wrr_portfolio_1 <- cbind(Date, (Portfolio_1[,-1] - TR_Inflation$MoM_inflation) * W_portfolio_1[,-1]) 
wrr_portfolio_2 <- cbind(Date, (Portfolio_2[,-1] - TR_Inflation$MoM_inflation) * W_portfolio_2[,-1])
wrr_portfolio_3 <- cbind(Date, (Portfolio_3[,-1] - TR_Inflation$MoM_inflation) * W_portfolio_3[,-1])
wrr_portfolio_4 <- cbind(Date, (Portfolio_4[,-1] - TR_Inflation$MoM_inflation) * W_portfolio_4[,-1])
wrr_portfolio_5 <- cbind(Date, (Portfolio_5[,-1] - TR_Inflation$MoM_inflation) * W_portfolio_5[,-1])

#Create data sets with weighted returns
wr_portfolio_1 <- cbind(Date, Portfolio_1[,-1] * W_portfolio_1[,-1])
wr_portfolio_2 <- cbind(Date, Portfolio_2[,-1] * W_portfolio_2[,-1])
wr_portfolio_3 <- cbind(Date, Portfolio_3[,-1] * W_portfolio_3[,-1])
wr_portfolio_4 <- cbind(Date, Portfolio_4[,-1] * W_portfolio_4[,-1])
wr_portfolio_5 <- cbind(Date, Portfolio_5[,-1] * W_portfolio_5[,-1])


#Create dollar-neutral P1-P5 portfolio
wr_portfolio_1[is.na(wr_portfolio_1)] <- 0
wr_portfolio_5[is.na(wr_portfolio_5)] <- 0

wr_portfolio_1and5 <- cbind(Date, wr_portfolio_1[,-1] - wr_portfolio_5[,-1])

wr_portfolio_1[wr_portfolio_1 == 0] <- NA
wr_portfolio_5[wr_portfolio_5 == 0] <- NA
wr_portfolio_1and5[wr_portfolio_1and5 == 0] <- NA

wrr_portfolio_1[is.na(wrr_portfolio_1)] <- 0
wrr_portfolio_5[is.na(wrr_portfolio_5)] <- 0

wrr_portfolio_1and5 <- cbind(Date, wrr_portfolio_1[,-1] - wrr_portfolio_5[,-1])

wrr_portfolio_1[wrr_portfolio_1 == 0] <- NA
wrr_portfolio_5[wrr_portfolio_5 == 0] <- NA
wrr_portfolio_1and5[wrr_portfolio_1and5 == 0] <- NA


#Create return dataset for the portfolios
Portfolios <- as.data.frame(Date)

Portfolios$"Q1" <- rowSums(wr_portfolio_1[,-1], na.rm = TRUE)
Portfolios$"Q2" <- rowSums(wr_portfolio_2[,-1], na.rm = TRUE)
Portfolios$"Q3" <- rowSums(wr_portfolio_3[,-1], na.rm = TRUE)
Portfolios$"Q4" <- rowSums(wr_portfolio_4[,-1], na.rm = TRUE)
Portfolios$"Q5" <- rowSums(wr_portfolio_5[,-1], na.rm = TRUE)
Portfolios$"Q1-Q5" <- rowSums(wr_portfolio_1and5[,-1], na.rm = TRUE)

Portfolios_real <- as.data.frame(Date)
Portfolios_real$"Q1" <- rowSums(wrr_portfolio_1[,-1], na.rm = TRUE)
Portfolios_real$"Q2" <- rowSums(wrr_portfolio_2[,-1], na.rm = TRUE)
Portfolios_real$"Q3" <- rowSums(wrr_portfolio_3[,-1], na.rm = TRUE)
Portfolios_real$"Q4" <- rowSums(wrr_portfolio_4[,-1], na.rm = TRUE)
Portfolios_real$"Q5" <- rowSums(wrr_portfolio_5[,-1], na.rm = TRUE)
Portfolios_real$"Q1-Q5" <- rowSums(wrr_portfolio_1and5[,-1], na.rm = TRUE)


#Calculate success rate for portfolios and S&P 500

#Create an empty dataframe to store the results
Succes_dummy <- data.frame(Date = Portfolios$Date)

#Loop through the portfolios and check if the returns exceed inflation
for (i in 2:7) {
  exceeds_inflation <- ifelse(Portfolios[, i] > TR_Inflation$MoM_inflation, 1, 0)
  Succes_dummy[, paste0("Portfolio", i-1)] <- exceeds_inflation
  
}

Succes_dummy_SP500 <- data.frame(Date = SP_500$Date)
exceeds_inflation <- ifelse(SP_500[, 2] > TR_Inflation$MoM_inflation, 1, 0)
Succes_dummy_SP500[, paste0("S&P 500", i-1)] <- exceeds_inflation


#Create data set to store descriptive statistics

#Create a matrix with 7 rows and 9 columns
Descr_stat_portfolios <- matrix(nrow = 9,
                                ncol = 7)

#Set column names
colnames(Descr_stat_portfolios) <- c("Q1",
                                     "Q2",
                                     "Q3",
                                     "Q4",
                                     "Q5",
                                     "Q1-Q5",
                                     "S&P 500")

#Set row names
rownames(Descr_stat_portfolios) <- c("Annual mean %",
                                     "Annual mean, real %",
                                     "Monthly mean %",
                                     "Median %",
                                     "Maximum %",
                                     "Minimum %",
                                     "Standard Deviation %",
                                     "Success Rate",
                                     "Obs.")

#Convert matrix to data frame
Descr_stat_portfolios <- as.data.frame(Descr_stat_portfolios)


# Loop over the columns and apply the function
for (i in 2:ncol(Portfolios)) { # Start from the second column
  
  #Find annual and monthly returns
  annual_mean <- sum(Portfolios[,i] * 12) / nrow(Portfolios)
  Descr_stat_portfolios[1, i-1] <- annual_mean * 100
  
  mth_mean <- sum(Portfolios[,i]) / nrow(Portfolios)
  Descr_stat_portfolios[3, i-1] <- mth_mean * 100
  
  
  #Find median, maximum, minimum and number of observations
  Descr_stat_portfolios[4, i-1] <- median(Portfolios[,i]) * 100
  Descr_stat_portfolios[5, i-1] <- max(Portfolios[,i]) * 100
  Descr_stat_portfolios[6, i-1] <- min(Portfolios[,i]) * 100
  Descr_stat_portfolios[7, i-1] <- sd(Portfolios[,i]) * 100 * sqrt(12)
  Descr_stat_portfolios[8, i-1] <- sum(Succes_dummy[,i]) / nrow(Succes_dummy)
  Descr_stat_portfolios[9, i-1] <- nrow(Portfolios)
}


for (i in 2:ncol(Portfolios_real)) { # Start from the second column
  
  #Find annual real returns
  annual_mean_real <- sum(Portfolios_real[,i] * 12) / nrow(Portfolios_real)
  Descr_stat_portfolios[2, i-1] <- annual_mean_real * 100
  
}

Descr_stat_portfolios[1, ncol(Descr_stat_portfolios)] <- SP500_beta$Annual.mean * 100
Descr_stat_portfolios[2, ncol(Descr_stat_portfolios)] <- SP500_beta$Annual.mean.real * 100
Descr_stat_portfolios[3, ncol(Descr_stat_portfolios)] <- mean(SP_500$S.P.500.TR.USD) * 100
Descr_stat_portfolios[4, ncol(Descr_stat_portfolios)] <- median(SP_500$S.P.500.TR.USD) * 100
Descr_stat_portfolios[5, ncol(Descr_stat_portfolios)] <- max(SP_500$S.P.500.TR.USD) * 100
Descr_stat_portfolios[6, ncol(Descr_stat_portfolios)] <- min(SP_500$S.P.500.TR.USD) * 100
Descr_stat_portfolios[7, ncol(Descr_stat_portfolios)] <- sd(SP_500$S.P.500.TR.USD) * 100 * sqrt(12)
Descr_stat_portfolios[8, ncol(Descr_stat_portfolios)] <- sum(Succes_dummy_SP500[,2]) / nrow(Succes_dummy_SP500)
Descr_stat_portfolios[9, ncol(Descr_stat_portfolios)] <- nrow(SP_500)


#Create dataframe to store results
Portfolio_betas <- data.frame(Company = character(),
                              beta = numeric(),
                              t_value = numeric(),
                              Obs. = numeric(),
                              stringsAsFactors = FALSE)

#Loop through each portfolio column and run a regression
for (i in 2:ncol(Portfolios)) {
  Portfolio_name <- colnames(Portfolios)[i]
  reg_data <- as.data.frame(cbind(Date, 
                                  Portfolios[, i], 
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
    
    #Add the results to the data frame
    Portfolio_betas <- rbind(Portfolio_betas,
                             data.frame(Company = Portfolio_name,
                                        Beta = beta_coef,
                                        t = t_value,
                                        Obs. = obs,
                                        stringsAsFactors = FALSE)) 
}

rownames(Portfolio_betas) <- NULL

#Format the data set with the portfolio coefficients
Portfolio_betas <- t(Portfolio_betas)
colnames(Portfolio_betas) <- Portfolio_betas[1,]
Portfolio_betas <- Portfolio_betas[-1,]
Portfolio_betas <- as.data.frame(Portfolio_betas)
Portfolio_betas[] <- lapply(Portfolio_betas, as.numeric)

#Add SP500 beta, t-value and obs. to Portfolio_betas dataset
Portfolio_betas$"S&P 500" <- c(beta_coef_SP500, 
                               t_value_SP500, 
                               obs_SP500)

################################## Sector analysis ###################################


#Find the total weight for each stock in the period
W_portfolio_1_total <- W_portfolio_1[,-1] / nrow(W_portfolio_1)
W_portfolio_1_total <- colSums(W_portfolio_1_total, na.rm = TRUE)
W_portfolio_1_total <- as.data.frame(W_portfolio_1_total)
W_portfolio_1_total$Sector <- Equity_betas$Sector
colnames(W_portfolio_1_total)[1] <- "P1"

#Sum the weights of the stocks in each sector
Sector_weights <- aggregate(P1 ~ Sector, data=W_portfolio_1_total, sum)
Sector_weights$P1 <- Sector_weights$P1 * 100
Sector_weights <- as.data.frame(Sector_weights)

#Repeat for portfolio 5
W_portfolio_5_total <- W_portfolio_5[,-1] / nrow(W_portfolio_5)
W_portfolio_5_total <- colSums(W_portfolio_5_total, na.rm = TRUE)
W_portfolio_5_total <- as.data.frame(W_portfolio_5_total)
W_portfolio_5_total$Sector <- Equity_betas$Sector
colnames(W_portfolio_5_total)[1] <- "P5"
Q5_sector_weights <- aggregate(P5 ~ Sector, data = W_portfolio_5_total, sum)
Q5_sector_weights$P5 <- Q5_sector_weights$P5 * 100
Sector_weights$P5 <- Q5_sector_weights$P5

#Repeat for S&P 500
W_SP500_total <- W_SP500[,-1] / nrow(W_SP500)
W_SP500_total <- colSums(W_SP500_total, na.rm = TRUE)
W_SP500_total <- as.data.frame(W_SP500_total)
W_SP500_total$Sector <- Sectors$Sector
colnames(W_SP500_total)[1] <- "SP500"
Sp500_sector_weights <- aggregate(SP500 ~ Sector, data = W_SP500_total, sum)
Sp500_sector_weights$SP500 <- Sp500_sector_weights$SP500 * 100
Sector_weights$SP500 <- Sp500_sector_weights$SP500 

# convert the Sectors to factor with ordered levels
Sector_weights$Sector <- factor(Sector_weights$Sector, levels = c("Utilities", 
                                                                  "Real Estate", 
                                                                  "Materials", 
                                                                  "Information Technology", 
                                                                  "Industrials",
                                                                  "Health Care",
                                                                  "Financials",
                                                                  "Energy",
                                                                  "Consumer Staples",
                                                                  "Consumer Discretionary",
                                                                  "Communication Services",
                                                                  "#N/A"), ordered = TRUE)

#Convert data frame to long-format
Sector_long <- tidyr::gather(Sector_weights, key = "Metric", value = "Value", -Sector)


################################ FFC factors #################################

#Add SP500 returns to portfolio return data set
Portfolios$"S&P 500" <- SP_500$S.P.500.TR.USD 
Portfolios_real$"S&P 500" <- SP_500$S.P.500.TR.USD - TR_Inflation$MoM_inflation

#Create data frame to store results
Factor_betas <- data.frame(Portfolio = character(),
                           Beta_MKT = numeric(),
                           t_MKT = numeric(),
                           Beta_SMB = numeric(),
                           t_SMB = numeric(),
                           Beta_HML = numeric(),
                           t_HML = numeric(),
                           Beta_MOM = numeric(),
                           t_MOM = numeric(),
                           "R^2" = numeric(),
                           Obs. = numeric(),
                           stringsAsFactors = FALSE)


# Loop through each portfolio column and run a regression
for (i in 2:ncol(Portfolios)) {
  Portfolio_name <- colnames(Portfolios)[i]
  reg_data <- as.data.frame(cbind(Date, 
                                  Portfolios[, i], 
                                  Factors$Mkt.RF, 
                                  Factors$SMB, 
                                  Factors$HML, 
                                  Factors$Mom))
  
  colnames(reg_data) <- c("Date", 
                          "Total_Return", 
                          "MKT.RF",
                          "SMB",
                          "HML",
                          "MOM")  # Rename the columns
  
  reg_data$Date <- as.Date(reg_data$Date)
  
  reg_model <- lm(Total_Return ~ MKT.RF + SMB + HML + MOM, data = reg_data)  # Run the regression
  
  reg_model_coef <- coeftest(reg_model, vcov = NeweyWest(reg_model, prewhite = FALSE)) # HAC Standard Errors
  
  
  # Store the regression results in the Factor_betas data frame
  Factor_betas[i-1, "Portfolio"] <- Portfolio_name
  Factor_betas[i-1, "Beta_MKT"] <- reg_model_coef[2,1]
  Factor_betas[i-1, "t_MKT"] <- reg_model_coef[2,3]
  Factor_betas[i-1, "Beta_SMB"] <- reg_model_coef[3,1]
  Factor_betas[i-1, "t_SMB"] <- reg_model_coef[3,3]
  Factor_betas[i-1, "Beta_HML"] <- reg_model_coef[4,1]
  Factor_betas[i-1, "t_HML"] <- reg_model_coef[4,3]
  Factor_betas[i-1, "Beta_MOM"] <- reg_model_coef[5,1]
  Factor_betas[i-1, "t_MOM"] <- reg_model_coef[5,3]
  Factor_betas[i-1, "R.2"] <- summary(reg_model)$r.squared
  Factor_betas[i-1, "Obs."] <- length(reg_model$residuals)
  
}

#Format the data set with the factor coefficients
Factor_betas <- t(Factor_betas)
colnames(Factor_betas) <- Factor_betas[1,]
Factor_betas <- Factor_betas[-1,]
Factor_betas <- as.data.frame(Factor_betas)
Factor_betas[] <- lapply(Factor_betas, as.numeric)


################################### State analysis ###################################

#Create a data set with the states through time
Yields$Exp_inflation <- Yields$`2Y` - Yields$`3M`

States <- as.data.frame(TR_Inflation$Date)
colnames(States)[1] <- "Date"
States$Exp_inflation <- Yields$Exp_inflation
States$YoY_inflation <- TR_Inflation$US.CPI.YoY

States$Current <- NA
States$Forward <- NA

#Apply the conditions and assign values to current column
States$Current <- ifelse(States$YoY_inflation < 0.01, "Low", 
                         ifelse(States$YoY_inflation >= 0.01 & States$YoY_inflation <= 0.03, "Neutral", "High"))

#Create a subset of the data for each level of the "Current" column
high <- subset(States, Current == "High")
neutral <- subset(States, Current == "Neutral")
low <- subset(States, Current == "Low")

#Subtract term premium
high_termprem <- mean(high$Exp_inflation)
neutral_termprem <- mean(neutral$Exp_inflation)
low_termprem <- mean(low$Exp_inflation)

high$Exp_inflation <- high$Exp_inflation - high_termprem
neutral$Exp_inflation <- neutral$Exp_inflation - neutral_termprem
low$Exp_inflation <- low$Exp_inflation - low_termprem

#Create a histogram for each subset, with a different color
ggplot() + 
  geom_histogram(data = high, aes(x = Exp_inflation, fill = "High"), alpha = 0.5) + 
  geom_histogram(data = neutral, aes(x = Exp_inflation, fill = "Neutral"), alpha = 0.5) + 
  geom_histogram(data = low, aes(x = Exp_inflation, fill = "Low"), alpha = 0.5) + 
  labs(title = "Histogram of Exp_inflation by Current") +
  xlab("Exp_inflation") +
  ylab("Count") +
  scale_fill_manual(values = c("High" = "red", "Neutral" = "blue", "Low" = "green"),
                    name = "Current")

ggplot(low, aes(x=Exp_inflation)) +
  geom_histogram()

high_sd <- sd(high$Exp_inflation)
neutral_sd <- sd(neutral$Exp_inflation)
low_sd <- sd(low$Exp_inflation)

#Match adjusted Exp_inflation values with original dataframe by Date
States$Exp_inflation[match(high$Date, States$Date)] <- high$Exp_inflation
States$Exp_inflation[match(neutral$Date, States$Date)] <- neutral$Exp_inflation
States$Exp_inflation[match(low$Date, States$Date)] <- low$Exp_inflation


#Apply condition and assign values to forward-looking column
States$Forward <- ifelse(States$Exp_inflation >= 0.005, "High",
                         ifelse(States$Exp_inflation <= -0.005, "Low",
                                ifelse(States$Exp_inflation >= 0.0025 & States$Current == "Low", "High",
                                       ifelse(States$Exp_inflation <= -0.0025 & States$Current == "Low", "Low", "Neutral"))))

States$State <- ifelse(States$Current == "Low" & States$Forward == "Low", "1",
                       ifelse(States$Current == "Low" & States$Forward == "Neutral", "2",
                              ifelse(States$Current == "Low" & States$Forward == "High", "3",
                                     ifelse(States$Current == "Neutral" & States$Forward == "Low", "4",
                                            ifelse(States$Current == "Neutral" & States$Forward == "Neutral", "5",
                                                   ifelse(States$Current == "Neutral" & States$Forward == "High", "6",
                                                          ifelse(States$Current == "High" & States$Forward == "Low", "7",
                                                                 ifelse(States$Current == "High" & States$Forward == "Neutral", "8", "9"))))))))

States$Exp_inflation_notmeanadj <- Yields$Exp_inflation

#Plot mean adjusted yield spread
Yield_spread <- ggplot(States, aes(x=Date)) +
  geom_line(aes(y = Exp_inflation, color="Mean adjusted spread"), size=0.7, linetype=1) +
  geom_line(aes(y = Exp_inflation_notmeanadj, color="Spread"), size=0.7, linetype=1) +
  scale_color_manual("", values = c("black", "dark grey"),
                     labels = c("Mean adjusted spread", "Spread")) +
  ylab(label = NULL) +
  scale_x_date(labels = date_format("%Y"), 
               limits = as.Date(c("1990-01-01", "2022-12-31")), 
               expand = c(0,0),
               date_breaks = ("4 years")) +
  scale_y_continuous(breaks=c(-0.015,-0.01,-0.005,0,0.005,0.01,0.015,0.02), 
                     labels = percent,
                     limits = c(-0.015,0.0225),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        text=element_text(family="Times New Roman", size=11))

Yield_spread


####Line graph: CPI YoY 1990-2022 with states####

#Plot YoY inflation with states

CPI_YoY_states <- ggplot(States, aes(x=Date, y=YoY_inflation, color = State, group = 1)) +
  geom_line(size = 1.6) +
  ylab(label = NULL) +
  scale_x_date(labels = date_format("%Y"), 
               limits = as.Date(c("1990-01-01", "2022-12-31")), 
               expand = c(0,0),
               date_breaks = ("4 years")) +
  scale_y_continuous(breaks=c(-0.020,0.0,0.020,0.040,0.060,0.080,0.100), 
                     labels = percent,
                     limits = c(-0.025,0.1),
                     expand = c(0,0)) +
  geom_hline(yintercept=0, 
             color = "grey", 
             size=0.3) +
  guides(color=guide_legend("State")) +
  scale_color_manual(labels = c("L/L", "L/N", "L/H", 
                                "N/L", "N/N", "N/H", 
                                "H/L", "H/N", "H/H"),
                     values = c("#c7ddb5", "#95bb72", "#4b6043",
                                "#f1ee8e", "#e6cc00", "#e47200",
                                "#ffa590", "#ff4122", "#c61a09")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        axis.title.x = element_blank(),
        text=element_text(family="Times New Roman", size=11))

CPI_YoY_states


############################## In-sample State analysis ##############################

#Make list of portfolio returns in the different states

#Add the states to the return data set for the portfolios
Portfolios$State <- States$State
Portfolios_real$State <- States$State

#list to store the portfolio return data sets
in_sample_list <- list()

#loop through the states and create a separate data set for each state
for (State in 1:9) {
  # extract the rows with the current state
  in_sample_data <- Portfolios[Portfolios$State == State, ]
  # append the data set to the list
  in_sample_list[[paste0("In_sample_", State)]] <- in_sample_data
  
  rm(in_sample_data)
}

#initialize a list to store the portfolio real return data sets
in_sample_real_list <- list()

#loop through the states and create a separate data set for each state
for (State in 1:9) {
  # extract the rows with the current state
  in_sample_real_data <- Portfolios_real[Portfolios_real$State == State, ]
  
  in_sample_real_list[[paste0("In_sample_", State)]] <- in_sample_real_data
  
  rm(in_sample_real_data)
}

#Find descriptive statistics for P1 in the different states

# create a function to calculate the stats of Q1
calculate_stats <- function(in_sample_list) {
  stats <- lapply(in_sample_list, function(x) {
    c(mean = mean(x[, 2] * 12 * 100),
      mth_mean = mean(x[,2] * 100),
      median = median(x[,2] * 100),
      max = max(x[,2] * 100),
      min = min(x[,2] * 100),
      SD = sd(x[,2] * 100) * sqrt(12),
      Obs = nrow(x))
  })
  return(stats)
}

#Store to list
stats <- calculate_stats(in_sample_list)

#Transpond and store to dataframe
Q1_states_descr_IS <- data.frame(t(do.call(rbind, stats)))

#Add to descriptive data set
rownames(Q1_states_descr_IS) <- c("Annual mean %",
                               "Monthly mean %",
                               "Median %",
                               "Maximum %",
                               "Minimum %",
                               "Standard Deviation %",
                               "Obs.")

colnames(Q1_states_descr_IS) <- c("Low -> Low", "Low -> Neutral", "Low -> High",
                               "Neutral -> Low", "Neutral -> Neutral", "Neutral -> High",
                               "High -> Low", "High -> Neutral", "High -> High")


#Function for annual mean
Real_mean <- function(x) {
  mean(x[, 2] * 12 * 100)
}

#Apply the function to each data set in the list
means_real_list <- lapply(in_sample_real_list, Real_mean) #Apply to the real return list

#Combine the real means into a data frame
Q1_real_states_descr_IS <- data.frame(t(do.call(rbind, means_real_list)))

#Add annual mean real to the descriptive data set
Q1_states_descr_IS["Annual mean, real %", ] <- Q1_real_states_descr_IS[1, ]

Q1_states_descr_IS[1:8, ] <- format(round(Q1_states_descr_IS[1:8, ], 2), nsmall = 2)

#Rearrange rows
Q1_states_descr_IS <- Q1_states_descr_IS[c("Annual mean %",
                                     "Annual mean, real %",
                                     "Monthly mean %",
                                     "Median %",
                                     "Maximum %",
                                     "Minimum %",
                                     "Standard Deviation %",
                                     "Obs."),]

Q1_states_descr_IS[] <- sapply(Q1_states_descr_IS, as.numeric)


#Find descriptive statistics for P5 in the different states

# create a function to calculate the stats of P5
calculate_stats <- function(in_sample_list) {
  stats <- lapply(in_sample_list, function(x) {
    c(mean = mean(x[, 6] * 12 * 100),
      mth_mean = mean(x[,6] * 100),
      median = median(x[,6] * 100),
      max = max(x[,6] * 100),
      min = min(x[,6] * 100),
      SD = sd(x[,6] * 100) * sqrt(12),
      Obs = nrow(x))
  })
  return(stats)
}

#Store to list
stats <- calculate_stats(in_sample_list)

#Transpond and store to dataframe
Q5_states_descr_IS <- data.frame(t(do.call(rbind, stats)))

#Add to descriptive data set
rownames(Q5_states_descr_IS) <- c("Annual mean %",
                               "Monthly mean %",
                               "Median %",
                               "Maximum %",
                               "Minimum %",
                               "Standard Deviation %",
                               "Obs.")

colnames(Q5_states_descr_IS) <- c("Low -> Low", "Low -> Neutral", "Low -> High",
                               "Neutral -> Low", "Neutral -> Neutral", "Neutral -> High",
                               "High -> Low", "High -> Neutral", "High -> High")


#Function for annual mean
Real_mean <- function(x) {
  mean(x[, 6] * 12 * 100)
}

#Apply the function to each data set in the list
means_real_list <- lapply(in_sample_real_list, Real_mean) #Apply to the real return list

#Combine the real means into a data frame
Q5_real_states_descr_IS <- data.frame(t(do.call(rbind, means_real_list)))

#Add annual mean real to the descriptive data set
Q5_states_descr_IS["Annual mean, real %", ] <- Q5_real_states_descr_IS[1, ]

Q5_states_descr_IS[1:8, ] <- format(round(Q5_states_descr_IS[1:8, ], 2), nsmall = 2)

#Rearrange rows
Q5_states_descr_IS <- Q5_states_descr_IS[c("Annual mean %",
                                     "Annual mean, real %",
                                     "Monthly mean %",
                                     "Median %",
                                     "Maximum %",
                                     "Minimum %",
                                     "Standard Deviation %",
                                     "Obs."),]

Q5_states_descr_IS[] <- sapply(Q5_states_descr_IS, as.numeric)


#Find descriptive statistics for P1-P5 in the different states

#Create a function to calculate the stats of P1-P5
calculate_stats <- function(in_sample_list) {
  stats <- lapply(in_sample_list, function(x) {
    c(mean = mean(x[, 7] * 12 * 100),
      mth_mean = mean(x[,7] * 100),
      median = median(x[,7] * 100),
      max = max(x[,7] * 100),
      min = min(x[,7] * 100),
      SD = sd(x[,7] * 100) * sqrt(12),
      Obs = nrow(x))
  })
  return(stats)
}

#Store to list
stats <- calculate_stats(in_sample_list)

#Transpond and store to the dataframe
Q1and5_states_descr_IS <- data.frame(t(do.call(rbind, stats)))

#Add to descriptive data set
rownames(Q1and5_states_descr_IS) <- c("Annual mean %",
                                  "Monthly mean %",
                                  "Median %",
                                  "Maximum %",
                                  "Minimum %",
                                  "Standard Deviation %",
                                  "Obs.")

colnames(Q1and5_states_descr_IS) <- c("Low -> Low", "Low -> Neutral", "Low -> High",
                                  "Neutral -> Low", "Neutral -> Neutral", "Neutral -> High",
                                  "High -> Low", "High -> Neutral", "High -> High")


#Function for annual mean
Real_mean <- function(x) {
  mean(x[, 7] * 12 * 100)
}

#Apply the function to each data set in the list
means_real_list <- lapply(in_sample_real_list, Real_mean) #Apply to the real return list

#Combine the real means into a data frame
Q1and5_real_states_descr_IS <- data.frame(t(do.call(rbind, means_real_list)))

#Add annual mean real to the descriptive data set
Q1and5_states_descr_IS["Annual mean, real %", ] <- Q1and5_real_states_descr_IS[1, ]


Q1and5_states_descr_IS[1:8, ] <- format(round(Q1and5_states_descr_IS[1:8, ], 2), nsmall = 2)

#Rearrange rows
Q1and5_states_descr_IS <- Q1and5_states_descr_IS[c("Annual mean %",
                                           "Annual mean, real %",
                                           "Monthly mean %",
                                           "Median %",
                                           "Maximum %",
                                           "Minimum %",
                                           "Standard Deviation %",
                                           "Obs."),]

Q1and5_states_descr_IS[] <- sapply(Q1and5_states_descr_IS, as.numeric)


#Find descriptive statistics for SP500 in the different states

#Create a function to calculate the stats of SP500
calculate_stats <- function(in_sample_list) {
  stats <- lapply(in_sample_list, function(x) {
    c(mean = mean(x[, 8] * 12 * 100),
      mth_mean = mean(x[,8] * 100),
      median = median(x[,8] * 100),
      max = max(x[,8] * 100),
      min = min(x[,8] * 100),
      SD = sd(x[,8] * 100) * sqrt(12),
      Obs = nrow(x))
  })
  return(stats)
}

#Store to list
stats <- calculate_stats(in_sample_list)

#Transpond and store to the dataframe
SP500_states_descr_IS <- data.frame(t(do.call(rbind, stats)))

#Add to descriptive data set
rownames(SP500_states_descr_IS) <- c("Annual mean %",
                                      "Monthly mean %",
                                      "Median %",
                                      "Maximum %",
                                      "Minimum %",
                                      "Standard Deviation %",
                                      "Obs.")

colnames(SP500_states_descr_IS) <- c("Low -> Low", "Low -> Neutral", "Low -> High",
                                      "Neutral -> Low", "Neutral -> Neutral", "Neutral -> High",
                                      "High -> Low", "High -> Neutral", "High -> High")


#Function for annual mean
Real_mean <- function(x) {
  mean(x[, 8] * 12 * 100)
}

#Apply the function to each data set in the list
means_real_list <- lapply(in_sample_real_list, Real_mean) #Apply to the real return list

#Combine the real means into a dataframe
SP500_real_states_descr_IS <- data.frame(t(do.call(rbind, means_real_list)))

#Add annual mean real to the descriptive data set
SP500_states_descr_IS["Annual mean, real %", ] <- SP500_real_states_descr_IS[1, ]

#Round to 2 decimal places
SP500_states_descr_IS[1:8, ] <- format(round(SP500_states_descr_IS[1:8, ], 2), nsmall = 2)

#Rearrange rows
SP500_states_descr_IS <- SP500_states_descr_IS[c("Annual mean %",
                                                   "Annual mean, real %",
                                                   "Monthly mean %",
                                                   "Median %",
                                                   "Maximum %",
                                                   "Minimum %",
                                                   "Standard Deviation %",
                                                   "Obs."),]

SP500_states_descr_IS[] <- sapply(SP500_states_descr_IS, as.numeric)

#Create data set with descriptive stats for P1, P5, P1-P5 and SP500
Descr_states_IS <- data.frame(rbind(Q1_states_descr_IS[1:2,], 
                                    Q1_states_descr_IS[7,],
                                    Q5_states_descr_IS[1:2,], 
                                    Q5_states_descr_IS[7,],
                                    Q1and5_states_descr_IS[1:2,], 
                                    Q1and5_states_descr_IS[7,],
                                    SP500_states_descr_IS[1:2,], 
                                    SP500_states_descr_IS[7,],
                                    Q1_states_descr_IS[8,]))
  
rownames(Descr_states_IS) <- c("Q1 Annual mean %",
                              "Q1 Annual mean, real %",
                              "Q1 Standard Deviation %",
                              "Q5 Annual mean %",
                              "Q5 Annual mean, real %",
                              "Q5 Standard Deviation %",
                              "Q1-Q5 Annual mean %",
                              "Q1-Q5 Annual mean, real %",
                              "Q1-Q5 Standard Deviation %",
                              "S&P 500 Annual mean %",
                              "S&P 500 Annual mean, real %",
                              "S&P 500 Standard Deviation %",
                              "Obs.")

#Add column names
colnames(SP500_states_descr_IS) <- c("Low -> Low", "Low -> Neutral", "Low -> High",
                                     "Neutral -> Low", "Neutral -> Neutral", "Neutral -> High",
                                     "High -> Low", "High -> Neutral", "High -> High")



rm(in_sample_list, in_sample_real_list, means_real_list,
   Q1_real_states_descr_IS, Q5_real_states_descr_IS,
   Q1and5_real_states_descr_IS, SP500_real_states_descr_IS,
   stats, mth_mean, State, calculate_stats, Real_mean)



################################### Out-of-sample ####################################

#Merge the Portfolio_equities frames by month
Portfolio_equities$MoM_inflation <- TR_Inflation$MoM_inflation

#Create data set to store rolling betas
Rolling_betas <- as.data.frame(Portfolio_equities$Date)
colnames(Rolling_betas)[1] <- "Date"

#for-loop that subsets a data set with Date, return for i'th equity and MoM inflation and runs a rolling regression
for (i in 2:(ncol(Portfolio_equities)-1)) { # start at 2 since the first column is the Date column
  
Portfolio_sub_test <- Portfolio_equities[,c(1, i, ncol(Portfolio_equities))]

Portfolio_sub_test <- na.omit(Portfolio_sub_test)

out <- roll_regres(Portfolio_sub_test[,2] ~ Portfolio_sub_test[,3], 
                   width = 60, do_downdates = TRUE, min_obs = 60) #TRUE = ROLLING WINDOW, FALSE = EXPANDING WINDOW

#Store rolling betas in subset
Portfolio_sub_test[, 2] <- out$coefs[,2]

#Merge subset with rolling betas data set based on date column
Rolling_betas <- merge(Rolling_betas, Portfolio_sub_test[1:2], by = "Date", all = TRUE)

}

rm(out, i, Portfolio_sub_test)

#Shift all betas one month forward
Rolling_betas <- Rolling_betas[, -1] #Remove dates
Rolling_betas <- Rolling_betas[-(nrow(Rolling_betas)-1):-nrow(Rolling_betas),] #Remove last two rows in Rolling_betas
Rolling_betas <- cbind(TR_Inflation$Date[-1:-2], Rolling_betas) #Add date vector with the right period
colnames(Rolling_betas)[1] <- "Date"

# Merge the Portfolio_equities frames by month
SP_500$MoM_inflation <- TR_Inflation$MoM_inflation


################################### Define period ####################################

OOS_start_date <- as.Date("1995-02-28")

Rolling_betas <- Rolling_betas[Rolling_betas$Date >= OOS_start_date & Rolling_betas$Date <= end_date, ]
Portfolio_equities <- Portfolio_equities[Portfolio_equities$Date >= OOS_start_date & Portfolio_equities$Date <= end_date, ]
Portfolio_marketcap <- Portfolio_marketcap[Portfolio_marketcap$Date >= OOS_start_date & Portfolio_marketcap$Date <= end_date, ]
TR_Inflation <- TR_Inflation[TR_Inflation$Date >= OOS_start_date & TR_Inflation$Date <= end_date, ]
Marketcap_SP500 <- Marketcap_SP500[Marketcap_SP500$Date >= OOS_start_date & Marketcap_SP500$Date <= end_date, ]
SP_500 <- SP_500[SP_500$Date >= OOS_start_date & SP_500$Date <= end_date, ]
Factors <- Factors[Factors$Date >= OOS_start_date & Factors$Date <= end_date, ]
Yields <- Yields[Yields$Date >= OOS_start_date & Yields$Date <= end_date, ]
States <- States[States$Date >= OOS_start_date & States$Date <= end_date, ]

Date <- Portfolio_equities$Date


############################## Out-of-sample continued ###############################

#SP 500 out-of-sample regression
reg_model_SP500 <- lm(SP_500$S.P.500.TR.USD ~ TR_Inflation$MoM_inflation)

reg_model_SP500_coef <- coeftest(reg_model_SP500, vcov = NeweyWest(reg_model_SP500, prewhite = FALSE)) # HAC Standard Errors

beta_coef_SP500 <- reg_model_SP500_coef[2,1]
t_value_SP500 <- reg_model_SP500_coef[2,3]

obs_SP500 <- nrow(SP_500)
annual_mean_SP500 <- sum(SP_500[,2] * 12) / obs_SP500
annual_mean_real_SP500 <- sum((SP_500[,2] - TR_Inflation$MoM_inflation) * 12) / obs_SP500

SP500_beta <- data.frame(Company = as.character("S&P 500"),
                         "Sector" = as.character(""),
                         "Annual mean" = as.numeric(annual_mean_SP500),
                         "Annual mean real" = as.numeric(annual_mean_real_SP500),
                         Beta = as.numeric(beta_coef_SP500),
                         t = as.numeric(t_value_SP500),
                         Obs. = as.numeric(obs_SP500),
                         stringsAsFactors = FALSE)

#Clean dataframe
rownames(SP500_beta) <- NULL


#Rolling regression betas

#Create datasets for portfolios with betas
OOS_Q1 <- Rolling_betas[, -1]
OOS_Q2 <- Rolling_betas[, -1]
OOS_Q3 <- Rolling_betas[, -1]
OOS_Q4 <- Rolling_betas[, -1]
OOS_Q5 <- Rolling_betas[, -1]


# Loop through each month in the portfolio data frames
for (i in 1:nrow(Rolling_betas)) {
  
  # Count number of non-missing values in row 1
  num_non_missing <- sum(!is.na(Rolling_betas[i,-1]))
  
  # Remove non-missing values in row i, except for the first x non-missing values
  x <- floor(num_non_missing / 5)
  row_i_1 <- OOS_Q1[i,]
  row_i_2 <- OOS_Q2[i,]
  row_i_3 <- OOS_Q3[i,]
  row_i_4 <- OOS_Q4[i,]
  row_i_5 <- OOS_Q5[i,]
  
  row_i_1 <- sort(row_i_1, decreasing = TRUE)[1:x]
  row_i_2 <- sort(row_i_2, decreasing = TRUE)[(x+1):(2*x)]
  row_i_3 <- sort(row_i_3, decreasing = TRUE)[(2*x+1):(3*x)]
  row_i_4 <- sort(row_i_4, decreasing = TRUE)[(3*x+1):(4*x)]
  row_i_5 <- sort(row_i_5, decreasing = TRUE)[(4*x+1):num_non_missing]
  
  OOS_Q1[i, !(names(OOS_Q1) %in% names(row_i_1))] <- NA
  OOS_Q2[i, !(names(OOS_Q2) %in% names(row_i_2))] <- NA
  OOS_Q3[i, !(names(OOS_Q3) %in% names(row_i_3))] <- NA
  OOS_Q4[i, !(names(OOS_Q4) %in% names(row_i_4))] <- NA
  OOS_Q5[i, !(names(OOS_Q5) %in% names(row_i_5))] <- NA
  
}

#Add date column
OOS_Q1 <- cbind(Rolling_betas$Date, OOS_Q1)
colnames(OOS_Q1)[1] <- "Date"

OOS_Q2 <- cbind(Rolling_betas$Date, OOS_Q2)
colnames(OOS_Q2)[1] <- "Date"

OOS_Q3 <- cbind(Rolling_betas$Date, OOS_Q3)
colnames(OOS_Q3)[1] <- "Date"

OOS_Q4 <- cbind(Rolling_betas$Date, OOS_Q4)
colnames(OOS_Q4)[1] <- "Date"

OOS_Q5 <- cbind(Rolling_betas$Date, OOS_Q5)
colnames(OOS_Q5)[1] <- "Date"

#Remove inflation column from portfolio_equities
Portfolio_equities <- Portfolio_equities[, -ncol(Portfolio_equities)]

#Create OOS return data sets
OOS_Q1_returns <- OOS_Q1
OOS_Q2_returns <- OOS_Q2
OOS_Q3_returns <- OOS_Q3
OOS_Q4_returns <- OOS_Q4
OOS_Q5_returns <- OOS_Q5

#Replace betas with returns
OOS_Q1_returns[!is.na(OOS_Q1_returns)] <- Portfolio_equities[!is.na(OOS_Q1_returns)]
OOS_Q1_returns[,-1] <- lapply(OOS_Q1_returns[,-1], as.numeric)

OOS_Q2_returns[!is.na(OOS_Q2_returns)] <- Portfolio_equities[!is.na(OOS_Q2_returns)]
OOS_Q2_returns[,-1] <- lapply(OOS_Q2_returns[,-1], as.numeric)

OOS_Q3_returns[!is.na(OOS_Q3_returns)] <- Portfolio_equities[!is.na(OOS_Q3_returns)]
OOS_Q3_returns[,-1] <- lapply(OOS_Q3_returns[,-1], as.numeric)

OOS_Q4_returns[!is.na(OOS_Q4_returns)] <- Portfolio_equities[!is.na(OOS_Q4_returns)]
OOS_Q4_returns[,-1] <- lapply(OOS_Q4_returns[,-1], as.numeric)

OOS_Q5_returns[!is.na(OOS_Q5_returns)] <- Portfolio_equities[!is.na(OOS_Q5_returns)]
OOS_Q5_returns[,-1] <- lapply(OOS_Q5_returns[,-1], as.numeric)


rm(row_i_1, row_i_2, row_i_3, row_i_4, row_i_5, 
   x, i, num_non_missing, equities)


##Find weights for each portfolio##

#Make dataset with market caps for the portfolios
OOS_Marketcap_1 <- Portfolio_marketcap
OOS_Marketcap_1[is.na(OOS_Q1_returns)] <- NA

OOS_Marketcap_2 <- Portfolio_marketcap
OOS_Marketcap_2[is.na(OOS_Q2_returns)] <- NA

OOS_Marketcap_3 <- Portfolio_marketcap
OOS_Marketcap_3[is.na(OOS_Q3_returns)] <- NA

OOS_Marketcap_4 <- Portfolio_marketcap
OOS_Marketcap_4[is.na(OOS_Q4_returns)] <- NA

OOS_Marketcap_5 <- Portfolio_marketcap
OOS_Marketcap_5[is.na(OOS_Q5_returns)] <- NA


#Find vector with the sum of market caps each month
OOS_Cap_p1 <- rowSums(OOS_Marketcap_1[,-1], na.rm = TRUE)
OOS_Cap_p2 <- rowSums(OOS_Marketcap_2[,-1], na.rm = TRUE)
OOS_Cap_p3 <- rowSums(OOS_Marketcap_3[,-1], na.rm = TRUE)
OOS_Cap_p4 <- rowSums(OOS_Marketcap_4[,-1], na.rm = TRUE)
OOS_Cap_p5 <- rowSums(OOS_Marketcap_5[,-1], na.rm = TRUE)
OOS_Cap_SP500 <- rowSums(Marketcap_SP500[,-1], na.rm = TRUE)

#Divide individual market caps by the total portfolio market cap for each month
W_OOS_Q1 <- cbind(Date, OOS_Marketcap_1[,-1] / OOS_Cap_p1)
W_OOS_Q2 <- cbind(Date, OOS_Marketcap_2[,-1] / OOS_Cap_p2)
W_OOS_Q3 <- cbind(Date, OOS_Marketcap_3[,-1] / OOS_Cap_p3)
W_OOS_Q4 <- cbind(Date, OOS_Marketcap_4[,-1] / OOS_Cap_p4)
W_OOS_Q5 <- cbind(Date, OOS_Marketcap_5[,-1] / OOS_Cap_p5)
W_OOS_SP500 <- cbind(Date, Marketcap_SP500[,-1] / OOS_Cap_SP500)

##Ready data sets for descriptive statistics##

#Create data sets with weighted real returns
wrr_OOS_Q1 <- cbind(Date, (OOS_Q1_returns[,-1] - TR_Inflation$MoM_inflation) * W_OOS_Q1[,-1]) 
wrr_OOS_Q2 <- cbind(Date, (OOS_Q2_returns[,-1] - TR_Inflation$MoM_inflation) * W_OOS_Q2[,-1])
wrr_OOS_Q3 <- cbind(Date, (OOS_Q3_returns[,-1] - TR_Inflation$MoM_inflation) * W_OOS_Q3[,-1])
wrr_OOS_Q4 <- cbind(Date, (OOS_Q4_returns[,-1] - TR_Inflation$MoM_inflation) * W_OOS_Q4[,-1])
wrr_OOS_Q5 <- cbind(Date, (OOS_Q5_returns[,-1] - TR_Inflation$MoM_inflation) * W_OOS_Q5[,-1])

#Create data sets with weighted returns
wr_OOS_Q1 <- cbind(Date, OOS_Q1_returns[,-1] * W_OOS_Q1[,-1])
wr_OOS_Q2 <- cbind(Date, OOS_Q2_returns[,-1] * W_OOS_Q2[,-1])
wr_OOS_Q3 <- cbind(Date, OOS_Q3_returns[,-1] * W_OOS_Q3[,-1])
wr_OOS_Q4 <- cbind(Date, OOS_Q4_returns[,-1] * W_OOS_Q4[,-1])
wr_OOS_Q5 <- cbind(Date, OOS_Q5_returns[,-1] * W_OOS_Q5[,-1])


#Create dollar-neutral P1-P5 portfolio
wr_OOS_Q1[is.na(wr_OOS_Q1)] <- 0
wr_OOS_Q5[is.na(wr_OOS_Q5)] <- 0

wr_OOS_Q1andQ5 <- cbind(Date, wr_OOS_Q1[,-1] - wr_OOS_Q5[-1])

wr_OOS_Q1[wr_OOS_Q1 == 0] <- NA
wr_OOS_Q5[wr_OOS_Q5 == 0] <- NA
wr_OOS_Q1andQ5[wr_OOS_Q1andQ5 == 0] <- NA

wrr_OOS_Q1[is.na(wrr_OOS_Q1)] <- 0
wrr_OOS_Q5[is.na(wrr_OOS_Q5)] <- 0

wrr_OOS_Q1andQ5 <- cbind(Date, wrr_OOS_Q1[,-1] - wrr_OOS_Q5[-1])

wrr_OOS_Q1[wrr_OOS_Q1 == 0] <- NA
wrr_OOS_Q5[wrr_OOS_Q5 == 0] <- NA
wrr_OOS_Q1andQ5[wrr_OOS_Q1andQ5 == 0] <- NA


#Create return dataset for the portfolios
OOS_portfolios <- as.data.frame(Date)

OOS_portfolios$"Q1" <- rowSums(wr_OOS_Q1[,-1], na.rm = TRUE)
OOS_portfolios$"Q2" <- rowSums(wr_OOS_Q2[,-1], na.rm = TRUE)
OOS_portfolios$"Q3" <- rowSums(wr_OOS_Q3[,-1], na.rm = TRUE)
OOS_portfolios$"Q4" <- rowSums(wr_OOS_Q4[,-1], na.rm = TRUE)
OOS_portfolios$"Q5" <- rowSums(wr_OOS_Q5[,-1], na.rm = TRUE)
OOS_portfolios$"Q1-Q5" <- rowSums(wr_OOS_Q1andQ5[,-1], na.rm = TRUE)

OOS_portfolios_real <- as.data.frame(Date)
OOS_portfolios_real$"Q1" <- rowSums(wrr_OOS_Q1[,-1], na.rm = TRUE)
OOS_portfolios_real$"Q2" <- rowSums(wrr_OOS_Q2[,-1], na.rm = TRUE)
OOS_portfolios_real$"Q3" <- rowSums(wrr_OOS_Q3[,-1], na.rm = TRUE)
OOS_portfolios_real$"Q4" <- rowSums(wrr_OOS_Q4[,-1], na.rm = TRUE)
OOS_portfolios_real$"Q5" <- rowSums(wrr_OOS_Q5[,-1], na.rm = TRUE)
OOS_portfolios_real$"Q1-Q5" <- rowSums(wrr_OOS_Q1andQ5[,-1], na.rm = TRUE)


#Calculate success rate for portfolios and S&P 500

#Create an empty data frame to store results
Succes_dummy <- data.frame(Date = OOS_portfolios$Date)

#Loop through the portfolios and check if the returns exceed inflation
for (i in 2:7) {
  exceeds_inflation <- ifelse(OOS_portfolios[, i] > TR_Inflation$MoM_inflation, 1, 0)
  Succes_dummy[, paste0("Portfolio", i-1)] <- exceeds_inflation
  
}

Succes_dummy_SP500 <- data.frame(Date = SP_500$Date)
exceeds_inflation <- ifelse(SP_500[, 2] > TR_Inflation$MoM_inflation, 1, 0)
Succes_dummy_SP500[, paste0("S&P 500", i-1)] <- exceeds_inflation


#Create data set to store descriptive statistics

#Create a matrix
OOS_descr_stat_portfolios <- matrix(nrow = 9,
                                ncol = 7)

#Set column names
colnames(OOS_descr_stat_portfolios) <- c("Q1",
                                     "Q2",
                                     "Q3",
                                     "Q4",
                                     "Q5",
                                     "Q1-Q5",
                                     "S&P 500")

#Set row names
rownames(OOS_descr_stat_portfolios) <- c("Annual mean %",
                                     "Annual mean, real %",
                                     "Monthly mean %",
                                     "Median %",
                                     "Maximum %",
                                     "Minimum %",
                                     "Standard Deviation %",
                                     "Success Rate",
                                     "Obs.")

#Convert matrix to dataframe
OOS_descr_stat_portfolios <- as.data.frame(OOS_descr_stat_portfolios)


#Loop over the columns and apply the function
for (i in 2:ncol(OOS_portfolios)) { # Start from the second column
  
  #Find annual and monthly returns
  annual_mean <- sum(OOS_portfolios[,i] * 12) / nrow(OOS_portfolios)
  OOS_descr_stat_portfolios[1, i-1] <- annual_mean * 100
  
  mth_mean <- sum(OOS_portfolios[,i]) / nrow(OOS_portfolios)
  OOS_descr_stat_portfolios[3, i-1] <- mth_mean * 100
  
  
  #Find median, maximum, minimum and number of observations
  OOS_descr_stat_portfolios[4, i-1] <- median(OOS_portfolios[,i]) * 100
  OOS_descr_stat_portfolios[5, i-1] <- max(OOS_portfolios[,i]) * 100
  OOS_descr_stat_portfolios[6, i-1] <- min(OOS_portfolios[,i]) * 100
  OOS_descr_stat_portfolios[7, i-1] <- sd(OOS_portfolios[,i]) * 100 * sqrt(12)
  OOS_descr_stat_portfolios[8, i-1] <- sum(Succes_dummy[,i]) / nrow(Succes_dummy)
  OOS_descr_stat_portfolios[9, i-1] <- nrow(OOS_portfolios)
}


for (i in 2:ncol(OOS_portfolios_real)) { # Start from the second column
  
  #Find annual real returns
  annual_mean_real <- sum(OOS_portfolios_real[,i] * 12) / nrow(OOS_portfolios_real)
  OOS_descr_stat_portfolios[2, i-1] <- annual_mean_real * 100
  
}

OOS_descr_stat_portfolios[1, ncol(OOS_descr_stat_portfolios)] <- SP500_beta$Annual.mean * 100
OOS_descr_stat_portfolios[2, ncol(OOS_descr_stat_portfolios)] <- SP500_beta$Annual.mean.real * 100
OOS_descr_stat_portfolios[3, ncol(OOS_descr_stat_portfolios)] <- mean(SP_500$S.P.500.TR.USD) * 100
OOS_descr_stat_portfolios[4, ncol(OOS_descr_stat_portfolios)] <- median(SP_500$S.P.500.TR.USD) * 100
OOS_descr_stat_portfolios[5, ncol(OOS_descr_stat_portfolios)] <- max(SP_500$S.P.500.TR.USD) * 100
OOS_descr_stat_portfolios[6, ncol(OOS_descr_stat_portfolios)] <- min(SP_500$S.P.500.TR.USD) * 100
OOS_descr_stat_portfolios[7, ncol(OOS_descr_stat_portfolios)] <- sd(SP_500$S.P.500.TR.USD) * 100 * sqrt(12)
OOS_descr_stat_portfolios[8, ncol(OOS_descr_stat_portfolios)] <- sum(Succes_dummy_SP500[,2]) / nrow(Succes_dummy_SP500)
OOS_descr_stat_portfolios[9, ncol(OOS_descr_stat_portfolios)] <- nrow(SP_500)


#Create dataframe to store results
OOS_portfolio_betas <- data.frame(Company = character(),
                              beta = numeric(),
                              t_value = numeric(),
                              Obs. = numeric(),
                              stringsAsFactors = FALSE)

#Loop through each portfolio column and run a regression
for (i in 2:ncol(OOS_portfolios)) {
  Portfolio_name <- colnames(OOS_portfolios)[i]
  reg_data <- as.data.frame(cbind(Date, 
                                  OOS_portfolios[, i], 
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
  OOS_portfolio_betas <- rbind(OOS_portfolio_betas,
                           data.frame(Company = Portfolio_name,
                                      Beta = beta_coef,
                                      t = t_value,
                                      Obs. = obs,
                                      stringsAsFactors = FALSE)) 
}

rownames(OOS_portfolio_betas) <- NULL

#Format the data set with the portfolio coefficients
OOS_portfolio_betas <- t(OOS_portfolio_betas)
colnames(OOS_portfolio_betas) <- OOS_portfolio_betas[1,]
OOS_portfolio_betas <- OOS_portfolio_betas[-1,]
OOS_portfolio_betas <- as.data.frame(OOS_portfolio_betas)
OOS_portfolio_betas[] <- lapply(OOS_portfolio_betas, as.numeric)


################################ OOS Sector analysis #################################


#Find the total weight for each stock in the period
W_OOS_Q1_total <- W_OOS_Q1[,-1] / nrow(W_OOS_Q1)
W_OOS_Q1_total <- colSums(W_OOS_Q1_total, na.rm = TRUE)
W_OOS_Q1_total <- as.data.frame(W_OOS_Q1_total)
W_OOS_Q1_total$Sector <- Equity_betas$Sector
colnames(W_OOS_Q1_total)[1] <- "P1"

#Sum the weights of the stocks in each sector
Sector_weights_OOS <- aggregate(P1 ~ Sector, data=W_OOS_Q1_total, sum)
Sector_weights_OOS$P1 <- Sector_weights_OOS$P1 * 100
Sector_weights_OOS <- as.data.frame(Sector_weights_OOS)

#Repeat for portfolio 5
W_OOS_Q5_total <- W_OOS_Q5[,-1] / nrow(W_OOS_Q5)
W_OOS_Q5_total <- colSums(W_OOS_Q5_total, na.rm = TRUE)
W_OOS_Q5_total <- as.data.frame(W_OOS_Q5_total)
W_OOS_Q5_total$Sector <- Equity_betas$Sector
colnames(W_OOS_Q5_total)[1] <- "Q1"
Q5_sector_weights_OOS <- aggregate(Q1 ~ Sector, data = W_OOS_Q5_total, sum)
Q5_sector_weights_OOS$Q1 <- Q5_sector_weights_OOS$Q1 * 100
Sector_weights_OOS$P5 <- Q5_sector_weights_OOS$Q1

#Repeat for S&P 500
W_OOS_SP500_total <- W_OOS_SP500[,-1] / nrow(W_OOS_SP500)
W_OOS_SP500_total <- colSums(W_OOS_SP500_total, na.rm = TRUE)
W_OOS_SP500_total <- as.data.frame(W_OOS_SP500_total)
W_OOS_SP500_total$Sector <- Sectors$Sector
colnames(W_OOS_SP500_total)[1] <- "SP500"
Sp500_sector_weights_OOS <- aggregate(SP500 ~ Sector, data = W_OOS_SP500_total, sum)
Sp500_sector_weights_OOS$SP500 <- Sp500_sector_weights_OOS$SP500 * 100
Sector_weights_OOS$SP500 <- Sp500_sector_weights_OOS$SP500 

#Convert Sectors to a factor with ordered levels
Sector_weights_OOS$Sector <- factor(Sector_weights_OOS$Sector, levels = c("Utilities", 
                                                                          "Real Estate", 
                                                                          "Materials", 
                                                                          "Information Technology", 
                                                                          "Industrials",
                                                                          "Health Care",
                                                                          "Financials",
                                                                          "Energy",
                                                                          "Consumer Staples",
                                                                          "Consumer Discretionary",
                                                                          "Communication Services",
                                                                          "#N/A"), ordered = TRUE)

#Convert data frame to long format
Sector_long_OOS <- tidyr::gather(Sector_weights_OOS, key = "Metric", value = "Value", -Sector)



############################## OOS Fama-French factors ###############################

#Add SP500 returns to portfolio return data set
OOS_portfolios$"S&P 500" <- SP_500$S.P.500.TR.USD 
OOS_portfolios_real$"S&P 500" <- SP_500$S.P.500.TR.USD - TR_Inflation$MoM_inflation

#Create dataframe to store results
OOS_factor_betas <- data.frame(Portfolio = character(),
                           Beta_MKT = numeric(),
                           t_MKT = numeric(),
                           Beta_SMB = numeric(),
                           t_SMB = numeric(),
                           Beta_HML = numeric(),
                           t_HML = numeric(),
                           Beta_MOM = numeric(),
                           t_MOM = numeric(),
                           "R^2" = numeric(),
                           Obs. = numeric(),
                           stringsAsFactors = FALSE)


#Loop through each portfolio column and run a regression
for (i in 2:ncol(OOS_portfolios)) {
  Portfolio_name <- colnames(OOS_portfolios)[i]
  reg_data <- as.data.frame(cbind(Date, 
                                  OOS_portfolios[, i], 
                                  Factors$Mkt.RF, 
                                  Factors$SMB, 
                                  Factors$HML, 
                                  Factors$Mom))
  
  colnames(reg_data) <- c("Date", 
                          "Total_Return", 
                          "MKT.RF",
                          "SMB",
                          "HML",
                          "MOM")  # Rename the columns
  
  reg_data$Date <- as.Date(reg_data$Date)
  
  reg_model <- lm(Total_Return ~ MKT.RF + SMB + HML + MOM, data = reg_data)  # Run the regression
  
  reg_model_coef <- coeftest(reg_model, vcov = NeweyWest(reg_model, prewhite = FALSE)) # HAC Standard Errors
  
  
  #Store the regression results in the OOS_factor_betas data frame
  OOS_factor_betas[i-1, "Portfolio"] <- Portfolio_name
  OOS_factor_betas[i-1, "Beta_MKT"] <- reg_model_coef[2,1]
  OOS_factor_betas[i-1, "t_MKT"] <- reg_model_coef[2,3]
  OOS_factor_betas[i-1, "Beta_SMB"] <- reg_model_coef[3,1]
  OOS_factor_betas[i-1, "t_SMB"] <- reg_model_coef[3,3]
  OOS_factor_betas[i-1, "Beta_HML"] <- reg_model_coef[4,1]
  OOS_factor_betas[i-1, "t_HML"] <- reg_model_coef[4,3]
  OOS_factor_betas[i-1, "Beta_MOM"] <- reg_model_coef[5,1]
  OOS_factor_betas[i-1, "t_MOM"] <- reg_model_coef[5,3]
  OOS_factor_betas[i-1, "R.2"] <- summary(reg_model)$r.squared
  OOS_factor_betas[i-1, "Obs."] <- length(reg_model$residuals)
  
}

#Format the data set with the factor coefficients
OOS_factor_betas <- t(OOS_factor_betas)
colnames(OOS_factor_betas) <- OOS_factor_betas[1,]
OOS_factor_betas <- OOS_factor_betas[-1,]
OOS_factor_betas <- as.data.frame(OOS_factor_betas)
OOS_factor_betas[] <- lapply(OOS_factor_betas, as.numeric)


################################ Additional analysis #################################

#Average rolling betas
Avg_beta <- mean(Equity_betas$Beta) #In-sample
Avg_OOS_beta <- rowSums(Rolling_betas[,-1], na.rm = TRUE) / ncol(Rolling_betas[,-1]) #Out-of-sample

SD_Avg_OOS_beta <- sd(Avg_OOS_beta)


Avg_beta_data <- as.data.frame(TR_Inflation$Date)
colnames(Avg_beta_data)[1] <- "Date"
Avg_beta_data$Avg_OOS_beta <- Avg_OOS_beta



Avg_beta_data$"+1 sd" <- Avg_beta_data$Avg_OOS_beta + SD_Avg_OOS_beta
Avg_beta_data$"-1 sd" <- Avg_beta_data$Avg_OOS_beta - SD_Avg_OOS_beta


#Yield data
sd_3M <- sd(Yields$`3M`)
sd_2Y <- sd(Yields$`2Y`)


################################# OOS State analysis #################################

#Make list of portfolio returns in the different states

#Add the states to the return data set for the portfolios
OOS_portfolios$State <- States$State
OOS_portfolios_real$State <- States$State


# make a list to store the portfolio return data sets
oos_list <- list()

# loop through the states and create a separate data set for each state
for (State in 1:9) {
  # extract the rows with the current state
  oos_data <- OOS_portfolios[OOS_portfolios$State == State, ]

  oos_list[[paste0("OOS_", State)]] <- oos_data
  
  rm(oos_data)
}

#Make a list to store the portfolio real return data sets
oos_real_list <- list()

# loop through the states and create a separate data set for each state
for (State in 1:9) {
  # extract the rows with the current state
  oos_real_data <- OOS_portfolios_real[OOS_portfolios_real$State == State, ]
  
  oos_real_list[[paste0("OOS_", State)]] <- oos_real_data
  
  rm(oos_real_data)
}

#Find descriptive statistics for P1 in the different states

#Create a function to calculate the stats of P1
calculate_stats <- function(oos_list) {
  stats <- lapply(oos_list, function(x) {
    c(mean = mean(x[, 2] * 12 * 100),
      mth_mean = mean(x[,2] * 100),
      median = median(x[,2] * 100), 
      max = max(x[,2] * 100), 
      min = min(x[,2] * 100),
      SD = sd(x[,2] * 100) * sqrt(12),
      Obs = nrow(x))
  })
  return(stats)
}

#Store to list
stats <- calculate_stats(oos_list)

#Transpond and store to dataframe
Q1_states_descr <- data.frame(t(do.call(rbind, stats)))

#Add to descriptive data set
rownames(Q1_states_descr) <- c("Annual mean %",
                               "Monthly mean %",
                               "Median %", 
                               "Maximum %", 
                               "Minimum %",
                               "Standard Deviation %", 
                               "Obs.")

colnames(Q1_states_descr) <- c("Low -> Low", "Low -> Neutral", "Low -> High",
                               "Neutral -> Low", "Neutral -> Neutral", "Neutral -> High",
                               "High -> Low", "High -> Neutral", "High -> High")


#Function for annual mean
Real_mean <- function(x) {
  mean(x[, 2] * 12 * 100)
}

#Apply the function to each data set in the list
means_real_list <- lapply(oos_real_list, Real_mean) #Apply to the real return list

#Combine the real means into a data frame
Q1_real_states_descr <- data.frame(t(do.call(rbind, means_real_list)))

#Add annual mean real to the descriptive data set
Q1_states_descr["Annual mean, real %", ] <- Q1_real_states_descr[1, ]

Q1_states_descr[1:8, ] <- format(round(Q1_states_descr[1:8, ], 2), nsmall = 2)

#Rearrange rows
Q1_states_descr <- Q1_states_descr[c("Annual mean %", 
                                     "Annual mean, real %",
                                     "Monthly mean %",
                                     "Median %",
                                     "Maximum %",
                                     "Minimum %",
                                     "Standard Deviation %",
                                     "Obs."),]

Q1_states_descr[] <- sapply(Q1_states_descr, as.numeric)


#Find descriptive statistics for P5 in the different states

# create a function to calculate the stats of P5
calculate_stats <- function(oos_list) {
  stats <- lapply(oos_list, function(x) {
    c(mean = mean(x[, 6] * 12 * 100),
      mth_mean = mean(x[,6] * 100),
      median = median(x[,6] * 100), 
      max = max(x[,6] * 100), 
      min = min(x[,6] * 100),
      SD = sd(x[,6] * 100) * sqrt(12),
      Obs = nrow(x))
  })
  return(stats)
}

#Store to list
stats <- calculate_stats(oos_list)

#Transpond and store to dataframe
Q5_states_descr <- data.frame(t(do.call(rbind, stats)))

#Add to descriptive data set
rownames(Q5_states_descr) <- c("Annual mean %",
                               "Monthly mean %",
                               "Median %", 
                               "Maximum %", 
                               "Minimum %",
                               "Standard Deviation %", 
                               "Obs.")

colnames(Q5_states_descr) <- c("Low -> Low", "Low -> Neutral", "Low -> High",
                               "Neutral -> Low", "Neutral -> Neutral", "Neutral -> High",
                               "High -> Low", "High -> Neutral", "High -> High")


#Function for annual mean
Real_mean <- function(x) {
  mean(x[, 6] * 12 * 100)
}

#Apply the function to each data set in the list
means_real_list <- lapply(oos_real_list, Real_mean) #Apply to the real return list

#Combine the real means into a data frame
Q5_real_states_descr <- data.frame(t(do.call(rbind, means_real_list)))

#Add annual mean real to the descriptive data set
Q5_states_descr["Annual mean, real %", ] <- Q5_real_states_descr[1, ]

Q5_states_descr[1:8, ] <- format(round(Q5_states_descr[1:8, ], 2), nsmall = 2)

#Rearrange rows
Q5_states_descr <- Q5_states_descr[c("Annual mean %", 
                                     "Annual mean, real %",
                                     "Monthly mean %",
                                     "Median %",
                                     "Maximum %",
                                     "Minimum %",
                                     "Standard Deviation %",
                                     "Obs."),]

Q5_states_descr[] <- sapply(Q5_states_descr, as.numeric)


#Find descriptive statistics for P1-P5 in the different states

# create a function to calculate the stats of P1-P5
calculate_stats <- function(oos_list) {
  stats <- lapply(oos_list, function(x) {
    c(mean = mean(x[, 7] * 12 * 100),
      mth_mean = mean(x[,7] * 100),
      median = median(x[,7] * 100), 
      max = max(x[,7] * 100), 
      min = min(x[,7] * 100),
      SD = sd(x[,7] * 100) * sqrt(12),
      Obs = nrow(x))
  })
  return(stats)
}

#Store to list
stats <- calculate_stats(oos_list)

#Transpond and store to dataframe
Q1and5_states_descr <- data.frame(t(do.call(rbind, stats)))

#Add to descriptive data set
rownames(Q1and5_states_descr) <- c("Annual mean %",
                               "Monthly mean %",
                               "Median %", 
                               "Maximum %", 
                               "Minimum %",
                               "Standard Deviation %", 
                               "Obs.")

colnames(Q1and5_states_descr) <- c("Low -> Low", "Low -> Neutral", "Low -> High",
                               "Neutral -> Low", "Neutral -> Neutral", "Neutral -> High",
                               "High -> Low", "High -> Neutral", "High -> High")


#Function for annual mean
Real_mean <- function(x) {
  mean(x[, 7] * 12 * 100)
}

#Apply the function to each data set in the list
means_real_list <- lapply(oos_real_list, Real_mean) #Apply to the real return list

#Combine the real means into a data frame
Q1and5_real_states_descr <- data.frame(t(do.call(rbind, means_real_list)))

#Add annual mean real to the descriptive data set
Q1and5_states_descr["Annual mean, real %", ] <- Q1and5_real_states_descr[1, ]

Q1and5_states_descr[1:8, ] <- format(round(Q1and5_states_descr[1:8, ], 2), nsmall = 2)

#Rearrange rows
Q1and5_states_descr <- Q1and5_states_descr[c("Annual mean %", 
                                     "Annual mean, real %",
                                     "Monthly mean %",
                                     "Median %",
                                     "Maximum %",
                                     "Minimum %",
                                     "Standard Deviation %",
                                     "Obs."),]

Q1and5_states_descr[] <- sapply(Q1and5_states_descr, as.numeric)

#Find descriptive statistics for SP500 in the different states

# create a function to calculate the stats of SP500
calculate_stats <- function(oos_list) {
  stats <- lapply(oos_list, function(x) {
    c(mean = mean(x[, 8] * 12 * 100),
      mth_mean = mean(x[,8] * 100),
      median = median(x[,8] * 100), 
      max = max(x[,8] * 100), 
      min = min(x[,8] * 100),
      SD = sd(x[,8] * 100) * sqrt(12),
      Obs = nrow(x))
  })
  return(stats)
}

#Store to list
stats <- calculate_stats(oos_list)

#Transpond and store to the dataframe
SP500_states_descr <- data.frame(t(do.call(rbind, stats)))

#Add to descriptive data set
rownames(SP500_states_descr) <- c("Annual mean %",
                                   "Monthly mean %",
                                   "Median %", 
                                   "Maximum %", 
                                   "Minimum %",
                                   "Standard Deviation %", 
                                   "Obs.")

colnames(SP500_states_descr) <- c("Low -> Low", "Low -> Neutral", "Low -> High",
                                   "Neutral -> Low", "Neutral -> Neutral", "Neutral -> High",
                                   "High -> Low", "High -> Neutral", "High -> High")


#Function for annual mean
Real_mean <- function(x) {
  mean(x[, 8] * 12 * 100)
}

#Apply the function to each data set within the list
means_real_list <- lapply(oos_real_list, Real_mean) #Apply to the real return list

#Combine the real means into a data frame
SP500_real_states_descr <- data.frame(t(do.call(rbind, means_real_list)))

#Add annual mean real to the descriptive data set
SP500_states_descr["Annual mean, real %", ] <- SP500_real_states_descr[1, ]

SP500_states_descr[1:8, ] <- format(round(SP500_states_descr[1:8, ], 2), nsmall = 2)

#Rearrange rows
SP500_states_descr <- SP500_states_descr[c("Annual mean %", 
                                             "Annual mean, real %",
                                             "Monthly mean %",
                                             "Median %",
                                             "Maximum %",
                                             "Minimum %",
                                             "Standard Deviation %",
                                             "Obs."),]

SP500_states_descr[] <- sapply(SP500_states_descr, as.numeric)


#Create data set with descriptive stats for P1, P5, P1-P5 and SP500
Descr_states <- data.frame(rbind(Q1_states_descr[1:2,], 
                                    Q1_states_descr[7,],
                                    Q5_states_descr[1:2,], 
                                    Q5_states_descr[7,],
                                    Q1and5_states_descr[1:2,], 
                                    Q1and5_states_descr[7,],
                                    SP500_states_descr[1:2,], 
                                    SP500_states_descr[7,],
                                    Q1_states_descr[8,]))

rownames(Descr_states) <- c("Q1 Annual mean %",
                               "Q1 Annual mean, real %",
                               "Q1 Standard Deviation %",
                               "Q5 Annual mean %",
                               "Q5 Annual mean, real %",
                               "Q5 Standard Deviation %",
                               "Q1-Q5 Annual mean %",
                               "Q1-Q5 Annual mean, real %",
                               "Q1-Q5 Standard Deviation %",
                               "S&P 500 Annual mean %",
                               "S&P 500 Annual mean, real %",
                               "S&P 500 Standard Deviation %",
                               "Obs.")

#Add column names
colnames(SP500_states_descr) <- c("Low -> Low", "Low -> Neutral", "Low -> High",
                                     "Neutral -> Low", "Neutral -> Neutral", "Neutral -> High",
                                     "High -> Low", "High -> Neutral", "High -> High")



################################### Vizualisation ####################################


####Table: Top 20 Equity betas####

#Create a table for the top 20 beta equities
gttable_top20 <-
  top20_Equity_betas %>%
  gt(
  ) %>%
  opt_table_font(
    font = "Times New Roman"
  ) %>%
  tab_header(
    title = md("Regression Results for Total Return vs. Monthly Inflation (1990 - 2022)"),
    subtitle = NULL,
  ) %>%
  fmt_number(
    columns = c("Annual.mean", "Annual.mean.real", "Beta", "t"),
    suffixing = FALSE,
    decimals = 2
  ) %>%
  cols_label(
    Beta = html("&beta;"),
    Annual.mean = ("Annual mean %"),
    Annual.mean.real = ("Annual mean, real %"),
    t = ("t-Stat")
  ) %>%
  cols_align(
    align = "left",
    columns = c("Company")
  ) %>%
  cols_align(
    align = "center",
    columns = c("Sector", "Annual.mean", "Annual.mean.real", "Beta", "t", "Obs.")
  ) %>%
  cols_width(
    "Company" ~ px(220),
    "Sector" ~ px(200),
    "Annual.mean" ~ px(95),
    "Annual.mean.real" ~ px(95),
    "Beta" ~ px(70),
    "t" ~ px(70),
    "Obs." ~ px(70)
  ) %>%
  tab_source_note(
    source_note = "Source: Authors own creation"
  ) %>%
  tab_options(
    data_row.padding = px(8),
  )

gttable_top20

####Descriptive stats: IS portfolios####
#Create table for the descriptive statistics for the portfolios
gttable_descr_stat <-
  Descr_stat_portfolios %>%
  gt(
    rownames_to_stub = TRUE,
    auto_align = TRUE
  ) %>%
  opt_table_font(
    font = "Times New Roman"
  ) %>%
  tab_header(
    title = md("Descriptive statistics of in-sample portfolios (1990 - 2022)"),
    subtitle = NULL,
  ) %>%
  fmt_number(
    columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q1-Q5", "S&P 500"),
    suffixing = FALSE,
    decimals = 2
  ) %>%
  cols_align(
    align = "center",
    columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q1-Q5", "S&P 500")
  ) %>%
  cols_width(
    "Q1" ~ px(80),
    "Q2" ~ px(80),
    "Q3" ~ px(80),
    "Q4" ~ px(80),
    "Q5" ~ px(80),
    "Q1-Q5" ~ px(80),
    "S&P 500" ~ px(80)
  ) %>%
  tab_source_note(
    source_note = "Source: Authors own creation"
  )

gttable_descr_stat

####Table: IS portfolio coefficients####
#Create table for the portfolio coefficients

gttable_portfolio_betas <-
  Portfolio_betas %>%
  gt(
    rownames_to_stub = TRUE,
    auto_align = TRUE
  ) %>%
  opt_table_font(
    font = "Times New Roman"
  ) %>%
  tab_header(
    title = md("Portfolio coefficients (1990 - 2022)"),
    subtitle = NULL,
  ) %>%
  fmt_number(
    columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q1-Q5", "S&P 500"),
    suffixing = FALSE,
    decimals = 2
  ) %>%
  cols_align(
    align = "center",
    columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q1-Q5", "S&P 500")
  ) %>%
  cols_width(
    "Q1" ~ px(80),
    "Q2" ~ px(80),
    "Q3" ~ px(80),
    "Q4" ~ px(80),
    "Q5" ~ px(80),
    "Q1-Q5" ~ px(80),
    "S&P 500" ~ px(80)
  ) %>%
  tab_source_note(
    source_note = "Source: Authors own creation"
  )

gttable_portfolio_betas

####Table: IS factor coefficients####
#Create table for factor coefficients
gttable_factor_betas <-
  Factor_betas %>%
  gt(
    rownames_to_stub = TRUE,
    auto_align = TRUE
  ) %>%
  opt_table_font(
    font = "Times New Roman"
  ) %>%
  tab_header(
    title = md("FFC Factor coefficients (1990 - 2022)"),
    subtitle = NULL,
  ) %>%
  fmt_number(
    columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q1-Q5", "S&P 500"),
    suffixing = FALSE,
    decimals = 2
  ) %>%
  cols_align(
    align = "center",
    columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q1-Q5", "S&P 500")
  ) %>%
  cols_width(
    "Q1" ~ px(80),
    "Q2" ~ px(80),
    "Q3" ~ px(80),
    "Q4" ~ px(80),
    "Q5" ~ px(80),
    "Q1-Q5" ~ px(80),
    "S&P 500" ~ px(80)
  ) %>%
  tab_source_note(
    source_note = "Source: Authors own creation"
  )

gttable_factor_betas

####Bar plot: Sector allocations of IS portfolios####
#Make a barplot of sectors in P1, P5 and S&P 500
ggplot(Sector_long, aes(y = Sector, x = Value, fill = Metric)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_bar(position = position_dodge(width = 0.7), stat = "identity") +
  scale_fill_manual(values = c("#CCCCCC", "#707070", "black")) +
  labs(y = "Sector", x = "Percentage", fill = "") +
  theme_minimal()+
  theme(text=element_text(family="Times New Roman", size=11))

####Treemaps: Sector allocations of equity betas####

top100_sectors <- top100_Equity_betas %>%
  group_by(Sector) %>%
  summarize(num_companies = n())

Equity_betas_sectors <- Equity_betas %>%
group_by(Sector) %>%
  summarize(num_companies = n())

top100_sectors$mycolor = c("#808080",
                           "#808080",
                           "#808080",
                           "#808080",
                           "#808080",
                           "#808080",
                           "#808080",
                           "#808080",
                           "#808080",
                           "#808080",
                           "#808080")

Equity_betas_sectors$mycolor <- top100_sectors$mycolor


# Plot top100
treemap(top100_sectors,
                index = "Sector",
                vSize = "num_companies",
                type="color",
                title = "Sector distribution of top 100",
                vColor = "mycolor",
                border.col = "white",
                border.lwds = 1,
                fontsize.labels = 7,
                fontcolor.labels = "white",
                bg.labels = 255,
                fontface.labels = 1,
                align.labels = c("left", "top"),
                overlap.labels = 0.5,
                inflate.labels = TRUE)

# Plot all equity betas
treemap(Equity_betas_sectors,
        index = "Sector",
        vSize = "num_companies",
        type="color",
        title = "Sector distribution of S&P 500",
        vColor = "mycolor",
        border.col = "white",
        border.lwds = 1,
        fontsize.labels = 7,
        fontcolor.labels = "white",
        bg.labels = 255,
        fontface.labels = 1,
        align.labels = c("left", "top"),
        overlap.labels = 0.5,
        inflate.labels = TRUE)


####Descriptive stats: OOS portfolios####
#Create table for the descriptive statistics for the portfolios
gttable_OOS_descr_stat <-
  OOS_descr_stat_portfolios %>%
  gt(
    rownames_to_stub = TRUE,
    auto_align = TRUE
  ) %>%
  opt_table_font(
    font = "Times New Roman"
  ) %>%
  tab_header(
    title = md("Descriptive statistics of out-of-sample portfolios (1995 - 2022)"),
    subtitle = NULL,
  ) %>%
  fmt_number(
    columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q1-Q5", "S&P 500"),
    suffixing = FALSE,
    decimals = 2
  ) %>%
  cols_align(
    align = "center",
    columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q1-Q5", "S&P 500")
  ) %>%
  cols_width(
    "Q1" ~ px(80),
    "Q2" ~ px(80),
    "Q3" ~ px(80),
    "Q4" ~ px(80),
    "Q5" ~ px(80),
    "Q1-Q5" ~ px(80),
    "S&P 500" ~ px(80)
  ) %>%
  tab_source_note(
    source_note = "Source: Authors own creation"
  )

gttable_OOS_descr_stat

####Table: OOS portfolio coefficients####
#Create table for the portfolio coefficients
OOS_portfolio_betas$"S&P 500" <- c(beta_coef_SP500, 
                                   t_value_SP500, 
                                   obs_SP500)

gttable_OOS_portfolio_betas <-
  OOS_portfolio_betas %>%
  gt(
    rownames_to_stub = TRUE,
    auto_align = TRUE
  ) %>%
  opt_table_font(
    font = "Times New Roman"
  ) %>%
  tab_header(
    title = md("Out-of-sample portfolio coefficients (1995 - 2022)"),
    subtitle = NULL,
  ) %>%
  fmt_number(
    columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q1-Q5", "S&P 500"),
    suffixing = FALSE,
    decimals = 2
  ) %>%
  cols_align(
    align = "center",
    columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q1-Q5", "S&P 500")
  ) %>%
  cols_width(
    "Q1" ~ px(80),
    "Q2" ~ px(80),
    "Q3" ~ px(80),
    "Q4" ~ px(80),
    "Q5" ~ px(80),
    "Q1-Q5" ~ px(80),
    "S&P 500" ~ px(80)
  ) %>%
  tab_source_note(
    source_note = "Source: Authors own creation"
  )

gttable_OOS_portfolio_betas


####Bar plot: Sector allocations of OOS portfolios####
#Make a barplot of sectors in P1, P5 and S&P 500
ggplot(Sector_long_OOS, aes(y = Sector, x = Value, fill = Metric)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_bar(position = position_dodge(width = 0.7), stat = "identity") +
  scale_fill_manual(values = c("#CCCCCC", "#707070", "black")) +
  labs(y = "Sector", x = "Percentage", fill = "") +
  theme_minimal()+
  theme(text=element_text(family="Times New Roman", size=11))

####Table: OOS factor coefficients####
#Create table for factor coefficients
gttable_OOS_factor_betas <-
  OOS_factor_betas %>%
  gt(
    rownames_to_stub = TRUE,
    auto_align = TRUE
  ) %>%
  opt_table_font(
    font = "Times New Roman"
  ) %>%
  tab_header(
    title = md("OOS FFC Factor coefficients (1995 - 2022)"),
    subtitle = NULL,
  ) %>%
  fmt_number(
    columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q1-Q5", "S&P 500"),
    suffixing = FALSE,
    decimals = 2
  ) %>%
  cols_align(
    align = "center",
    columns = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q1-Q5", "S&P 500")
  ) %>%
  cols_width(
    "Q1" ~ px(80),
    "Q2" ~ px(80),
    "Q3" ~ px(80),
    "Q4" ~ px(80),
    "Q5" ~ px(80),
    "Q1-Q5" ~ px(80),
    "S&P 500" ~ px(80)
  ) %>%
  tab_source_note(
    source_note = "Source: Authors own creation"
  )

gttable_OOS_factor_betas


####Line graph: Average beta####
# Plot Avg beta
Avg_beta_graph <- ggplot(Avg_beta_data, aes(x=Date)) + #JEG HAR INGEN IDE OM HVORFOR MAN SKAL BYTTE RUNDT
  geom_line(aes(y = Avg_OOS_beta, color = "± 1 Standard Deviation", linetype = "± 1 Standard Deviation"), size=0.5) +
  geom_line(aes(y = `+1 sd`, color = "Average IS beta", linetype = "Average IS beta"), size=0.5) +
  geom_line(aes(y = `-1 sd`, color = "Average IS beta", linetype = "Average IS beta"), size=0.5) +
  geom_hline(aes(yintercept = Avg_beta, 
             color="Average OOS beta", 
             linetype = "Average OOS beta"), 
             size=0.5) +
  scale_linetype_manual("", values = c(1,3,1),
                        labels = c("Average OOS beta", "± 1 Standard Deviation", "Average IS beta")) +
  scale_color_manual("", values = c("black", "dark grey", "dark grey"),
                     labels = c("Average OOS beta", "± 1 Standard Deviation", "Average IS beta")) +
  ggtitle("Average inflation beta of S&P 500 equities") +
  ylab(label = NULL) +
  scale_x_date(labels = date_format("%Y"), 
               limits = as.Date(c("1995-01-01", "2022-12-31")), 
               expand = c(0,0),
               date_breaks = ("3 years")) +
  scale_y_continuous(breaks=c(-6,-4,-2,0,2,4,6),
                     limits = c(-6,6),
                     expand = c(0,0),) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom")

Avg_beta_graph

####Line graph: Average beta####
# Plot Avg beta
Avg_beta_graph <- ggplot(Avg_beta_data, aes(x=Date)) + #JEG HAR INGEN IDE OM HVORFOR MAN SKAL BYTTE RUNDT
  geom_line(aes(y = Avg_OOS_beta, color = "Average IS beta", linetype = "Average IS beta"), size=0.8) +
  geom_hline(aes(yintercept = Avg_beta, 
                 color="Average OOS beta", 
                 linetype = "Average OOS beta"), 
             size=0.8) +
  scale_linetype_manual("", values = c(1,1),
                        labels = c("Average OOS beta", "Average IS beta")) +
  scale_color_manual("", values = c("black", "dark grey"),
                     labels = c("Average OOS beta", "Average IS beta")) +
  ylab(label = NULL) +
  scale_x_date(labels = date_format("%Y"), 
               limits = as.Date(c("1995-01-01", "2022-12-31")), 
               expand = c(0,0),
               date_breaks = ("3 years")) +
  scale_y_continuous(breaks=c(-6,-4,-2,0,2,4,6),
                     limits = c(-6,6),
                     expand = c(0,0),) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        text=element_text(family="Times New Roman", size=11))

Avg_beta_graph


####Line graph: CPI YoY 1945-2022####
#Plot CPI YoY
Historic_inflation = read.csv("Inflation data - FRED v4.csv", sep = ";")
Historic_inflation$Date <- as.Date(Historic_inflation$Date, format = "%d-%m-%Y")

#Replace "," with "."
Historic_inflation[,-1] <- Historic_inflation[,-1] %>%
  mutate_all(list(~ gsub(",", ".", .)))

Historic_inflation$US.CPI.YoY <- as.numeric(Historic_inflation$US.CPI.YoY)

Historic_inflation <- Historic_inflation[Historic_inflation$Date >= as.Date("1945-01-31") 
                                 & Historic_inflation$Date <= as.Date("2022-12-31"),]

Historic_inflation$US.CPI.YoY <- Historic_inflation$US.CPI.YoY / 100

CPI_YoY_historic <- ggplot(Historic_inflation, aes(x=Date, y=US.CPI.YoY)) +
  geom_line(color="Black", size=0.5, linetype=1) +
  ggtitle("US CPI YoY 1945-2022") +
  ylab(label = NULL) +
  scale_x_date(labels = date_format("%Y"), 
               limits = as.Date(c("1945-01-01", "2022-12-31")), 
               expand = c(0,0),
               date_breaks = ("7 years")) +
  scale_y_continuous(breaks=c(-0.05,0,0.05,0.1,0.15,0.2), 
                     labels = percent,
                     limits = c(-0.05,0.2),
                     expand = c(0,0)) +
  geom_hline(yintercept=0, 
             color = "grey", 
             size=0.3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust = 1),
        axis.title.x = element_blank())

CPI_YoY_historic

###################################### Cleaning ######################################

rm(OOS_Cap_p1, OOS_Cap_p2, OOS_Cap_p3, OOS_Cap_p4, OOS_Cap_p5,
   t_value, Portfolio_name, obs, mth_mean, i, exceeds_inflation,
   beta_coef, annual_mean, annual_mean_real, reg_data, reg_model,
   Succes_dummy, wr_OOS_Q1, wr_OOS_Q2, wr_OOS_Q3, wr_OOS_Q4, wr_OOS_Q5,
   wr_OOS_Q1andQ5, wrr_OOS_Q1, wrr_OOS_Q2, wrr_OOS_Q3, wrr_OOS_Q4,
   wrr_OOS_Q5, wrr_OOS_Q1andQ5, OOS_Cap_SP500, annual_mean_real_SP500,
   annual_mean_SP500, Succes_dummy_SP500, W_IIS_SP500, W_OOS_Q5,
   W_OOS_Q4, W_OOS_Q3, W_OOS_Q2, W_OOS_Q1, reg_model_SP500,
   OOS_Marketcap_1, OOS_Marketcap_2, OOS_Marketcap_3, OOS_Marketcap_4,
   OOS_Marketcap_5,OOS_Q1, OOS_Q2, OOS_Q3, OOS_Q4, OOS_Q5, W_OOS_SP500,
   row_i_1, row_i_2, row_i_3, row_i_4, row_i_5, non_missing_1,
   non_missing_2, non_missing_3, non_missing_4, non_missing_5,
   num_non_missing, Cap_p1, Cap_p2, Cap_p3, Cap_p4, Cap_p5,
   Marketcap_1, Marketcap_2, Marketcap_3, Marketcap_4, Marketcap_5,
   i, x, Marketcap_data, beta_coef, equity_name, equity_data, Inflation_data,
   reg_model, TR_data, reg_data, t_value, beta_coef_SP500, t_value_SP500,
   annual_mean, annual_mean_SP500, obs, obs_SP500, annual_mean_real,
   annual_mean_real_SP500, reg_model_SP500, mth_mean, Succes_dummy,
   Succes_dummy_SP500, exceeds_inflation, Date, wrr_portfolio_1,
   wrr_portfolio_2, wrr_portfolio_3, wrr_portfolio_4, wrr_portfolio_5,
   wrr_portfolio_1and5,wr_portfolio_1, wr_portfolio_2, wr_portfolio_3,
   wr_portfolio_4, wr_portfolio_5, wr_portfolio_1and5, Portfolio_name,
   Cap_SP500, Q5_sector_weights, Sector_weights, Sectors, W_portfolio_1_total,
   W_portfolio_5_total, W_SP500_total, Sp500_sector_weights, Portfolio_1,
   Portfolio_2, Portfolio_3, Portfolio_4, Portfolio_5, W_portfolio_1,
   W_portfolio_2, W_portfolio_3, W_portfolio_4, W_portfolio_5, Factors,
   Sector_long, W_SP500, Portfolios, Portfolios_real, top20_Equity_betas,
   SP500_beta, SP_500, gttable_portfolio_betas, gttable_descr_stat,
   gttable_factor_betas, gttable_OOS_descr_stat, gttable_OOS_portfolio_betas,
   gttable_top20, Factor_betas, Equity_betas, Marketcap_SP500, Portfolio_marketcap,
   out)

rm(Q1_real_states_descr, calculate_stats,
   Liquidty_prem, stats, Real_mean, State, means_real_list,
   Q5_real_states_descr, gttable_OOS_descr_stat_states_Q1,
   gttable_OOS_descr_stat_states_Q5, oos_list, Yields, Portfolio_betas,
   oos_real_list, gttable_IS_descr_stat_states_Q1, gttable_IS_descr_stat_states_Q5)

rm(reg_model_coef, reg_model_SP500_coef)

rm(`3M+2Y`, Avg_beta_data, Avg_beta_graph, CPI_MoM,
   CPI_YoY, CPI_YoY_historic, Equity_betas_sectors,
   gttable_IS_descr_stat_states_Q1and5, gttable_OOS_descr_stat_states_Q1and5,
   gttable_OOS_factor_betas, Historic_inflation, Avg_beta)

rm(Avg_OOS_beta, sd_2Y, sd_3M, SD_Avg_OOS_beta, sd_yield_spread,
   Yield_spread, OOS_Q1_returns, OOS_Q2_returns, OOS_Q3_returns,
   OOS_Q4_returns, OOS_Q5_returns, Q1and5_real_states_descr_IS,
   SP500_real_states_descr, Q1and5_real_states_descr, "S&P500")

rm(high_termprem, high_sd, low_sd, neutral_sd, neutral_termprem, low_termprem
   , W_OOS_SP500_total, W_OOS_Q1_total, W_OOS_Q5_total, high, low, neutral,
   Q1and5_states_descr, Q1and5_states_descr_IS, Q5_states_descr, Q5_states_descr_IS,
   Q1_states_descr, Q1_states_descr_IS, Q5_sector_weights_OOS, Sector_long_OOS,
   Sp500_sector_weights_OOS, SP500_states_descr, SP500_states_descr_IS, Sector_weights_OOS)


# write_xlsx(OOS_factor_betas,"C:\\Users\\vije18ad\\Desktop\\oos_factors.xlsx")
# write.csv(Descr_stat_portfolios,"C:\\Users\\vije18ad\\Desktop\\Descriptivestats.csv")
# write.csv(States,"C:\\Users\\vije18ad\\Desktop\\States.csv")









