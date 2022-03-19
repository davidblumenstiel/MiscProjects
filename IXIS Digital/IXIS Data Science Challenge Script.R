###############################################
###############################################
# IXIS Data Science Challenge Exercise
# David Blumenstiel
# March 2022
###############################################
###############################################



###############################################
# Libraries

library(dplyr)                     
library(openxlsx)
library(ggplot2)
library(lubridate)

###############################################
# Data Import


# Data paths
sessionsDir <- "https://raw.githubusercontent.com/davidblumenstiel/MiscProjects/main/IXIS%20Digital/data/DataAnalyst_Ecom_data_sessionCounts.csv"
addsDir <- "https://raw.githubusercontent.com/davidblumenstiel/MiscProjects/main/IXIS%20Digital/data/DataAnalyst_Ecom_data_addsToCart.csv"

# Reads in data and creates data-frames
sessions <- read.csv(sessionsDir)
adds <- read.csv(addsDir)


###############################################
# Data Preparation

# Adds a year/month variable to sessions. 
# Found from here: https://stackoverflow.com/questions/61465929/how-to-remove-the-day-from-the-date-and-leave-only-the-month-and-year-from-a-col 
sessions$MonthYear <- format(as.Date(sessions$dim_date, "%m/%d/%y"), "%m/%Y")



# Creates a data frame for Sheet 1
sheet1 <- sessions %>%
  group_by(MonthYear, dim_deviceCategory) %>%                # Groups by the year/month and by device type
  summarise(Sessions = sum(sessions),                        # Sums up sessions
            Transactions = sum(transactions),                # Sums up transactions
            QTY = sum(QTY),                                  # Sums up QTY
            ECR = sum(transactions)/sum(sessions)) %>%       # Calculates ECR
  arrange(mdy(MonthYear))                                    # Arranges by date




# Adds a similar date format to 'adds'.  Useful for joining
adds$MonthYear <- format(as.Date(paste0(adds$dim_month, "/1/" ,adds$dim_year), "%m/%d/%Y"), "%m/%Y")

# Joins the adds and sessions data sets
# This removes device distinction; it's now purely by month
combined <- inner_join(x = adds, 
                       y = sessions %>%
                         group_by(MonthYear) %>%                             # Groups by year/month
                         summarise(Sessions = sum(sessions),                 # Sums up sessions
                         Transactions = sum(transactions),                   # Sums up transactions
                         QTY = sum(QTY),                                     # Sums up QTY
                         ECR = sum(transactions)/sum(sessions)),             # Calculates up ECR
                       by = "MonthYear")                                     # Joins on year/month

combined[c("dim_year","dim_month")] <- NULL  # Removes redundant date information




# Creates a data frame for Sheet2
# Keeping this in a wide format for simplicity and ease of creating figures
sheet2 <- combined %>% 
  mutate(LastMonth_Sessions = lag(Sessions, n=1),            # Adds last month's observations; Looks one observations behind
         LastMonth_Transactions = lag(Transactions, n=1),    
         LastMonth_QTY = lag(QTY, n=1),
         LastMonth_ECR = lag(ECR, n=1),
         LastMonth_addsToCart = lag(addsToCart, n=1),
         PriorMonth_Sessions = lag(Sessions, n=2),           # Adds prior (to last month) observations; looks two observations behind
         PriorMonth_Transactions = lag(Transactions, n=2), 
         PriorMonth_QTY = lag(QTY, n=2),
         PriorMonth_ECR = lag(ECR, n=2),
         PriorMonth_addsToCart = lag(addsToCart, n=2)) %>%   
  mutate(LastMonth_Comparison_Absolute_Sessions = Sessions - LastMonth_Sessions,                 # Finds the absolute difference between metrics from this month and the last month
         LastMonth_Comparison_Absolute_Transactions = Transactions - LastMonth_Transactions,     # This will introduce some missing values for the first month observations
         LastMonth_Comparison_Absolute_QTY = QTY - LastMonth_QTY,
         LastMonth_Comparison_Absolute_ECR = ECR - LastMonth_ECR,
         LastMonth_Comparison_Absolute_addsToCart = addsToCart - LastMonth_addsToCart,
         PriorMonth_Comparison_Absolute_Sessions = Sessions - PriorMonth_Sessions,               # Finds the absolute difference between metrics from this month and the prior (to last month)
         PriorMonth_Comparison_Absolute_Transactions = Transactions - PriorMonth_Transactions,   # This will introduce some missing values for the first and second month observations
         PriorMonth_Comparison_Absolute_QTY = QTY - PriorMonth_QTY,
         PriorMonth_Comparison_Absolute_ECR = ECR - PriorMonth_ECR,
         PriorMonth_Comparison_Absolute_addsToCart = addsToCart - PriorMonth_addsToCart) %>%
  mutate(LastMonth_Comparison_Relative_Sessions = (Sessions - LastMonth_Sessions) / LastMonth_Sessions,      # Finds the relative difference between metrics from this month and the last month
         LastMonth_Comparison_Relative_Transactions = (Transactions - LastMonth_Transactions) / LastMonth_Transactions,
         LastMonth_Comparison_Relative_QTY = (QTY - LastMonth_QTY) / LastMonth_QTY,
         LastMonth_Comparison_Relative_ECR = (ECR - LastMonth_ECR) / LastMonth_ECR,
         LastMonth_Comparison_Relative_addsToCart = (addsToCart - LastMonth_addsToCart) / LastMonth_addsToCart,
         PriorMonth_Comparison_Relative_Sessions = (Sessions - PriorMonth_Sessions) / PriorMonth_Sessions,   # Finds the relative difference between metrics from this month and the prior (to last month)
         PriorMonth_Comparison_Relative_Transactions = (Transactions - PriorMonth_Transactions) / PriorMonth_Transactions,   
         PriorMonth_Comparison_Relative_QTY = (QTY - PriorMonth_QTY) / PriorMonth_QTY,   
         PriorMonth_Comparison_Relative_ECR = (ECR - PriorMonth_ECR) / PriorMonth_ECR,   
         PriorMonth_Comparison_Relative_addsToCart = (addsToCart - PriorMonth_addsToCart) / PriorMonth_addsToCart) %>%
  select(MonthYear, everything())   # Puts the date column first (looks nicer)
  
  
###############################################
# Data Export

# Writes the sheet data frames to an excel file as separate worksheets. 
write.xlsx(list(sheet1,sheet2),                                    # List of sheets to include (as data frames)
           file="IXIS_DS_Challange.xlsx",                          # File Name
           sheetName=c("Month_by_Device", "Monthly_comparison"),   # Sheet names
           rowNames=FALSE)                                         


###############################################
# Data Analysis (for my own reference when making the slide deck)
 
# For finding relationships
cor(sheet1[,3:6]) # Sessions, transactions, and QTY are highly correlated; especially QTY (almost fully)
cor(sheet2[,2:6])

plot(Sessions ~ Transactions, data = sheet2)

# Mean growth stats
mean(sheet2$LastMonth_Comparison_Relative_Transactions, na.rm = TRUE)
mean(sheet2$LastMonth_Comparison_Relative_Sessions, na.rm = TRUE)
mean(sheet2$LastMonth_Comparison_Relative_QTY, na.rm = TRUE)


###############################################
# Visualizations (for the slide deck and my own reference)

options(scipen = 9999999)   #Avoids scientific notation

# Stacked bar chart by month, segmented by device
# Sessions, transactions, and QTY have gone up over time, especially last three months
# Has gone up roughly proportionately to device
TransPerMonth_Plot <- ggplot(sheet1, aes(fill = dim_deviceCategory, 
                                         y = Transactions, 
                                         x = my(MonthYear))) +   # Changes x axis to date format
  geom_bar(position = "stack",                                   # Adds bars
           stat = "identity") +
  scale_x_date() +      
  scale_y_continuous(labels = scales::comma) +                   # Adds commas for clarity
  labs(x = "Month",                                              # Amends labels
       y = "Number of Transactions", 
       fill = "Device",
       title = "Transactions Per Month") 

ggsave("TransPerMonth_Plot.png", plot = TransPerMonth_Plot)      # Saves it as .png


# ECR tends to stay the same over time but is different depending on advice
# Desktop has higher ECR than tablet, which has higher than mobile
ECRByDevice_Plot <- ggplot(sheet1, aes(y = ECR, 
                                       x = dim_deviceCategory, 
                                       fill=dim_deviceCategory)) +   # Adds fill to get color
  geom_bar(stat = "summary" ) +                                      # Summary will give the mean by group
  labs(x = "Device", 
       y = "ECR", 
       fill = "Device",
       title = "ECR by Device") +
  theme(legend.position = "None")                                    # Removes legend: redundant

ggsave("ECRByDevice_Plot.png", plot = ECRByDevice_Plot)              # Saves it as .png

 
# Transactions, Sessions, and QTY track closely
# Overall Growth
# Large growth between Feb and Apr 2013
RelativeMonthlyChange_Plot <- ggplot(sheet2[-1,], aes(x = my(MonthYear))) +    # Skip the first observation (no comparisons)
  geom_line(aes(y = LastMonth_Comparison_Relative_Transactions,                # Adds line for Transactions
                color = "Transactions"),  
                size = 1) +
  geom_line(aes(y = LastMonth_Comparison_Relative_Sessions,                    # Adds line for Sessions
                color = "Sessions"),
                size = 1) +
  geom_line(aes(y = LastMonth_Comparison_Relative_QTY,                         # Adds line for QTY
                color = "QTY"),
                size = 1) +
  geom_hline(yintercept=0,                                                     # Adds an intercept at 0 for reference
             linetype = "dashed") +
  labs(x = "Month",                                                            # Amends the labeling
       y = "Relative Monthly Change", 
       color = "Metric",
       title = "Relative Changes from Previous Months: Transactions, Sessions, and QTY") +
  scale_x_date(date_breaks = "1 month",     # Adds more X ticks
               date_labels =  "%b %Y",      # Removes days from tick labels
               limits = my(c("08/2012", "06/2013")))+   # Shaves off the first month (no comparison) 
  scale_color_manual(values = c("Transactions" = "blue", "Sessions" = "darkred", "QTY" = "orange"))

ggsave("RelativeMonthlyChange_Plot.png", plot = RelativeMonthlyChange_Plot)    # Saves it as .png























