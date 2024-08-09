#installing and including a function that reads excell files
install.packages("readxl")
install.packages("anytime")
library("readxl")
install.packages("Amelia")
install.packages("naniar")
install.packages("gtExtras")
install.packages("haven")
install.packages(carat)
install.packages("summarytools")
install.packages("forecast")


library(summarytools)
library(dplyr) 
library(tidyr)
library(MASS)
library(anytime)
library(lubridate)
library(tidyverse) # For data manipulation and visualization
library(ggplot2)   # For plotting
library(readr)     # For reading data
library(summarytools) # For summary statistics
library(naniar)
library(gtExtras)
library(haven)
library(carat)
library(ggcorrplot)
library(summarytools)
library(dbscan)
library(factoextra)
library(dplyr)
library(lubridate)
library(forecast)


#importing the excel by appending it to the object- mobcash
mobcash_df <- read_excel("mobcash.xlsx")

#importing the excel by appending it to the object MobBanking dataframe
#In this analysis this object will be used 
MobBanking_df <- read.csv("MobileCash.csv")
print(MobBanking_df)

#At this stage cleaning data will be performed and transformed into a standard acceptable format
#Variable types
str(MobBanking_df)

#Formating the last deposit date into a standard format
MobBanking_df %>%
  mutate(NEW_DATE_FORMAT = as.Date(as.character(LAST_DEPOSIT_DT), 
                                   format = c("%d/%m/%Y", "%m/%d/%Y")))
#Splitting the date column into Date Month and Year
MobBanking_df %>%
  dplyr::mutate(day = lubridate::day(NEW_DATE_FORMAT),
                year = lubridate::year(NEW_DATE_FORMAT), 
                month = lubridate::month(NEW_DATE_FORMAT))


#Viewing the variable data type for a variable that I need to be categorized
#At the glimpse channel using the class function is a character variable
class(MobBanking_df$CHANNEL)

#To view what is found in the particular variable above (CHANNEL)
#Use the unique function
# The out put is Cash and mobile
unique(MobBanking_df$CHANNEL)

#Changing from the character variable into a factor variable using factor function
#Function factor is used to encode a vector as a factor (categorize)
#create the variable (MobBanking_df$CHANNEL) that will store the factor variable
#The argument is passed to the as.factor function
#The channel variable in the Mobcash data set will be stored as a factor
#The out put will be stored in the created variable (MobBanking_df$CHANNEL)
MobBanking_df$CHANNEL <- as.factor(MobBanking_df$CHANNEL)

#Check if the character variable is captured as a factor variable
class(MobBanking_df$CHANNEL)

#View the order of the classes in the variable channel
#In this case the cash is on level one and mobile is on level
levels(MobBanking_df$CHANNEL)

#Now changing and have mobile on the first level and cash level 2.
#Use level function to re-arrange the classes in the preferred order
#Use factor function and levels function
#concatenate the classes using c function
MobBanking_df$CHANNEL <- factor((MobBanking_df$CHANNEL),
                                levels = c("mobile", "cash"))
# view the changes in levels
levels(MobBanking_df$CHANNEL)


#selecting and filter


#find and handling missing values
#Imputing missing values is done using the mean function
#In this case the missing value are in the ledger_bal variable
#The out put is NA 
mean(MobBanking_df$LEDGER_BAL)

#The value are imputed using na.rm (Not available remove) function together with mean value
#The out is the variable is 99904.28
mean_value <- mean(MobBanking_df$LEDGER_BAL, na.rm = TRUE)
data <- MobBanking_df %>%
  mutate(LEDGER_BAL = ifelse(is.na(LEDGER_BAL), mean_value, LEDGER_BAL))
print(mean_value)

#dealing with all observation that could have any missing value romoving them
#appended the dplyr package to the select function to avoid the clashing of the selection function from the mass package and dplyr package
MobBanking_df %>% 
  dplyr::select(CUST_NM,CUST_NO,BRANCH,ACCT_NO,TRAN_DT,TXN_AMT,CHANNEL,LEDGER_BAL,LAST_DEPOSIT_AMT,LAST_DEPOSIT_DT)%>%
  na.omit()


#finding out what observations were lost when omitting the missing values
#Know where the missing values are
#appended the dplyr package to the select function to avoid the clashing of the selection function from the mass package and dplyr package
#Use the filter function together with !complete.cases to see the observations with missing values
#In this case they were three observations associated with ledger bal variable
MobBanking_df %>% 
  dplyr::select(CUST_NM,CUST_NO,BRANCH,ACCT_NO,TRAN_DT,TXN_AMT,CHANNEL,LEDGER_BAL,LAST_DEPOSIT_AMT,LAST_DEPOSIT_DT)%>%
  filter(!complete.cases(.))

#getting rid of the missing value
#Use drop_na function
MobBanking_df %>% 
  dplyr::select(CUST_NM,CUST_NO,BRANCH,ACCT_NO,TRAN_DT,TXN_AMT,CHANNEL,LEDGER_BAL,LAST_DEPOSIT_AMT,LAST_DEPOSIT_DT)%>%
  drop_na(LEDGER_BAL) %>% 
  View()

#replacing NA values with none in ledger balance column
#use mutate function to either create a new variable or change the value of an existing variable
#In steady of getting rid of the Na value I created another variable to include none values using mutate function
MobBanking_df %>% 
  dplyr::select(CUST_NM,CUST_NO,BRANCH,ACCT_NO,TRAN_DT,TXN_AMT,CHANNEL,LEDGER_BAL,LAST_DEPOSIT_AMT,LAST_DEPOSIT_DT)%>%
  filter(!complete.cases(.)) %>% 
  mutate(LEDGER_BAL1 = replace_na(as.character(LEDGER_BAL, "none")))
View()


#find and dealing with duplicates
#First Identify the duplicates in the data set using duplicate function
#The false observations represent the duplicates and they were 63208 entries omitted
#All variables had duplicates
dup_data=duplicated(MobBanking_df)
summary(dup_data)

#viewing the rows that are duplicated
#Removing duplicates
#Created an object that will handle the duplicated values
#omitted 2686 rows duplicated
data_unique <- MobBanking_df[duplicated(MobBanking_df),]
summary(data_unique)
view(data_unique)

#viewing the rows that are not duplicated based on CUST_NM
#Created an object that will handle value that are not duplicated
#omitted 9514 rows not duplicated
not_duplicated <- MobBanking_df[!duplicated(MobBanking_df$ACCT_NO),]
summary(not_duplicated)
view(not_duplicated)


#Handle Outliers
#Identify outliers using boxplot
#The varaible values should be numeric
boxplot(MobBanking_df$TXN_AM,
        main = "Transactions for both mobile and cash channel",
        xlab = "Transactions per person",
        ylab = "Transactions",
        col = "orange",
        border = "brown",
        notch = TRUE)

#Multiple boxplots
boxplot(TXN_AMT~LAST_DEPOSIT_DT,
        data=MobBanking_df,
        main="Different boxplots for each month",
        xlab="Month Number",
        ylab="Transaction Amount",
        col="orange",
        border="brown"
)
# Remove outliers
MobBanking_df %>% 
  filter(TXN_AMT > quantile(MobBanking_df, 0.55, na.rm = TRUE))
# Convert a column to a numeric type
MobBanking_df$TXN_AM <- as.numeric(MobBanking_df$TXN_AM)

#Recoding new values
#In this case I need to create another variable that called channel recoded
#I need the values for channel to be recoded as 1 and 2 in the created variable using mutata function
MobBanking_df %>% 
  dplyr::select(CUST_NM,CUST_NO,BRANCH,ACCT_NO,TRAN_DT,TXN_AMT,CHANNEL,LEDGER_BAL,LAST_DEPOSIT_AMT,LAST_DEPOSIT_DT)%>%
  mutate(CHANNEL_coded = recode(CHANNEL,"mobile"=1,"cash"=2 ))
View()


#data transformation  
#standardizing the formats (using min-max normalization and z score standardization on the data set)
#The standardization will enable to transform data to have mean of zero and standard deviation of 1
#Standardization is used to make data consistent and in the same range for machine learning model
#Normalization is used to transform the data to have values between 0 and 1
# Standardize data (z-score normalization)
not_duplicated <- not_duplicated %>% mutate_if(is.numeric, scale)
summary(not_duplicated)
view(not_duplicated)

# Normalize data (min-max scaling)
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
not_duplicated <- not_duplicated %>% mutate_if(is.numeric, normalize)
view(not_duplicated)

#Correcting the errors/Correct Inconsistent Data
#Identify and correct inconsistent data entries, such as typos or inconsistent formatting.
# Correct typos or inconsistent entries
MobBanking_df$LEDGER_BAL <- str_replace_all(MobBanking_df$LEDGER_BAL, "typo", "correct_value")
view(MobBanking_df)

# Convert text to lower case for consistency
not_duplicated$CUST_NM <- tolower(not_duplicated$CUST_NM)
view(not_duplicated)

# Save the cleaned data to a new CSV file
write.csv(not_duplicated, "C:/Users/amina/OneDrive/Documents/BA project assignment/cleaned_data_file.csv", row.names = FALSE)

#TASK THREE (Create another smaller data set related to the first one)
#Two functions can be used to create a subset data set i.e the subset function and filter function from the dplyr packages
#The subset data set is based on the customers whose last deposits are above 500,000 and they used mobile as the channel to deposit
#All the columns are maintained but the rows are fetched based on the set condition
#In this case I used the subset function to create the data set
#After cleaning and standardising the variable the condition of >500000 will not not work based on MobBanking_df
#but when data_unique_df is use it will work 
MobBanking_channel_deposit=subset(MobBanking_df,LAST_DEPOSIT_AMT > 500000  & CHANNEL =="mobile")
View(MobBanking_channel_deposit$LAST_DEPOSIT_AMT, MobBanking_channel_deposit$CHANNEL)

#TASK THREE merging the data sets Below are the steps
#I create a data frame (merged_data_df) that is going to be used to merge the two data sets i.e  MobBanking_channel_deposit and MobBanking_df
#Joins (left, right, inner and outer) together with the merge function is used to merge the two data sets
#The data sets have two variables (LAST_DEPOSIT_AMT and CHANNEL) in combine and that is what is to merged

#Below is the right join it gives the less number of the observations 763 compared to the original data set
#After the standardization, the following snippet will out put 0 observations because it executed based on MobBanking_df
#However if data_unique is used the observations will be out putted
merged_data_df <- merge(MobBanking_df,MobBanking_channel_deposit, by = "LAST_DEPOSIT_AMT","CHANNEL", all.y = T)

#Below is the outer join it gives the higher number of the observations 64971 compared to the original data set
#After the standardization, the following snippet will out put 0 observations because it executed based on MobBanking_df
#However if data_unique is used the observations will be out putted
merged_data_df <- merge(MobBanking_df,MobBanking_channel_deposit, by = "LAST_DEPOSIT_AMT","CHANNEL", all = T)

#Below is the left join it gives the same number of the observations like in the original data set
#After the standardization, the following snippet will out put 0 observations because it executed based on MobBanking_df
#However if data_unique is used the observations will be out putted
#In my analysis this code be is going to be used
merged_data_df <- merge(MobBanking_df,MobBanking_channel_deposit, by = "LAST_DEPOSIT_AMT","CHANNEL", all.x = T)

#Transform the merged data
data_trans <- merged_data_df %>% 
  group_by(LAST_DEPOSIT_AMT) %>% 
  summarise(total= sum(LAST_DEPOSIT_AMT,na.rm = TRUE))

#Explain the rationale for integrating these sources.
#Integrating the sources gives a unified view, making reporting and analytics more efficient. 
#It will be easier to make better, data-driven decisions and gain insights into your business performance.


#TASK 4 #exploratory data analysis to identify patterns, trends,insights
#1 know the structure of the data to be analysed
#Load necessary Libraries
#Inspect the Data
# View the structure and summary of the data
str(MobBanking_df)
summary(MobBanking_df)
head(MobBanking_df)

#2 Finding and handling Missing Values.
#Finding out the magnitude of the missing values for each variable
#naniar package is used to visualise and understand the missingness in the dataset MobBanking_df
#The results show that LEDGER_BAL variable is the only one with the missing values with 100%
miss_var_summary(MobBanking_df) %>% 
  gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Missing Values in the variables")
#Visualize the missing values
gg_miss_var(MobBanking_df)

#Extracting only the observations with missing values
miss_var_summary(MobBanking_df) %>% 
  filter(!complete.cases(.)) %>% 
  head(10) %>% 
  gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Missing Values in the variables")
#The distribution of missing data
#The missing values contributed 0.9% and  99.9% was the present value
#The output shows what is going on in the set with respect to missingness
vis_miss(MobBanking_df)

#The relationship of the variable with missing values to othe variables
#It is extremely important to establish whether or not the missing values in one variable are related to values in another variables or in multiples other variables
#The code below shows the extent to which the missingness is random
# The missing values in LEDGER_BAL are related to Transaction amount/TXN_AMT
MobBanking_df %>% 
  mutate(
    missing_ledger = factor(is.na(LEDGER_BAL),
                            levels = c("TRUE", "FALSE"),
                            labels = c("Missing", "Not Missing")),
    ggplot(aes(x =TXN_AMT, fill = missing_ledger))+
      geom_histogram(position = "stack")+
      labs(title = "Distribution of transaction amount for ledger balance",
           x = "Transaction Amount",
           y = "Ledger Balance Observations",
           Fill = "Missingness")+
      theme_bw()
  )
#Imputation
#The execution of this code will out put the characters/records with the na/missing values ledger balance is missing
MobBanking_df %>% 
  select(CUST_NM,CUST_NO,BRANCH,ACCT_NO,TRAN_DT,TXN_AMT,CHANNEL,LEDGER_BAL,LAST_DEPOSIT_AMT,LAST_DEPOSIT_DT) %>% 
  filter(is.na(LEDGER_BAL)) %>% 
  gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Data set characters/records with NA ")
#Finding the approximation of what could be the missing values
#whenever the ledger balance is missing will be replaced with median value for all the ledger balance
#Case when ledge bal value is na will be filled up with them median
MobBanking_df %>% 
  mutate(LEDGER_BAL = case_when(is.na(LEDGER_BAL)~median(MobBanking_df$LEDGER_BAL,
                                                         na.rm = TRUE),
                                TRUE~LEDGER_BAL)) %>%
  select(CUST_NM,CUST_NO,BRANCH,ACCT_NO,TRAN_DT,TXN_AMT,CHANNEL,LEDGER_BAL,LAST_DEPOSIT_AMT,LAST_DEPOSIT_DT) %>% 
  arrange(CUST_NM) %>% 
  gt() %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Imputed missing values for ledger balance varaibale") %>% 
  gt_highlight_rows(rows = CUST_NM %in% c("ZALWANGO DEBORAH","ZALWANGO DEBORAH",
                                          Fill = "lightblue"))

#3 Univariate Analysis for analyzing individual variables.
#Numerical Variables
#Histogram for visualising numerical variables
#The visualisation is done using ggplot function
data_unique %>% 
  ggplot(aes(LEDGER_BAL))+
  geom_histogram(binwidth =  10000, fill = "maroon")+
  theme_bw()+
  labs(x = "Ledge Balance",
       y = NULL,
       title = "Histogram for customers' Ledger Balance")


#Categorical Variables
#Bar charts or bar graphs are used to analyse a single categorical variable
MobBanking_df %>%
  ggplot(aes(x= CHANNEL))+
  geom_bar(fill = "brown")+
  theme_bw()+
  labs(x = "Channels",
       y = "Customers",
       title = "Channels used used by the customers to deposit")

# Boxplot for both categorical and numerical variables
#The numerical variable has one outlier at the higher extreme
MobBanking_df %>%
  ggplot(aes(CHANNEL, LAST_DEPOSIT_AMT)) + 
  geom_boxplot(fill = "maroon")+
  theme_minimal()+
  labs(y = "Last Deposit",
       title="Relationship Between the categorical and numerical variable")

#4 Bivariate Analysis to analyze relationships between variables.
#Numerical vs Numerical
#Scatter plot is used to analyse the relationship
#Filtered the customer whose transaction amount is great 5millions
#The colour is mapped against LAST_DEPOSIT_AMT variable
#Kabusu and Mbarara branch have the outliers at the extreme high
MobBanking_df %>% 
  filter(TXN_AMT > 5000000) %>% 
  ggplot(aes(x = BRANCH,
             y = TXN_AMT,
             colour = LAST_DEPOSIT_AMT))+
  geom_point()+
  coord_flip()+
  geom_smooth(method = lm,
              se = F)+
  labs(x = "Branches",
       y = "Transaction Amount",
       title = "Analysing the relationships among three variables")+
  theme_minimal()
#5 Correlation Analysis to analyze correlations between or among variables
#Determining the degree to variables are correlated
#correlation coefficient to determine how strongly the variables are connected to one another
#It is denoted by r and ranges from +1 to -1
#correlation can be positive, negative or no correlation
#positive correlation the degree to which two variables increase or decrease in tandem r=+1
#Negative correlation an increase in one variable decrease another variable
#No correlation change in one variable does not cause a change in another variable

#Correlation analysis for numeric variables
#the correlation between CUST_NO and CUST_NO is positive 1.000000000 which is a higher
#the correlation between CUST_NO and ACC_NO is negative -0.07163941 meaning an increase in one reduces the other
#the correlation between CUST_NO and TXN_AMT is negative -0.08225498 meaning an increase in one reduces the other
#the correlation between CUST_NO and TXN_AMT is positive 0.008832636 though the correlation is low
cor(MobBanking_df %>% select_if(is.numeric))

# Correlation matrix using cor function
cor_matrix <- cor(MobBanking_df %>% select_if(is.numeric))

# Visualize the correlation matrix
ggcorrplot(cor_matrix, method = c("square", "circle"),
           type = c("full", "lower", "upper"),
           ggtheme = ggplot2::theme_minimal,
           title = "",
           show.legend = TRUE,
           legend.title = "Corr",
           show.diag = NULL,
           colors = c("blue", "white", "red"),
           outline.color = "gray",
           hc.order = FALSE,
           hc.method = "complete",
           lab = FALSE,
           lab_col = "black",
           lab_size = 4,
           p.mat = NULL,
           sig.level = 0.05,
           insig = c("pch", "blank"),
           pch = 4,
           pch.col = "black",
           pch.cex = 5,
           tl.cex = 12,
           tl.col = "black",
           tl.srt = 45,
           digits = 2,
           as.is = FALSE)
#Generate a detailed summary report
#Data Frame Summary  
#MobBanking_df  
#Dimensions: 64208 x 10  
#Duplicates: 2786 
dfSummary(MobBanking_df)

#EDA Branch with the Most Customers
#Calculate Branch Metrics
#Branch with the Most Customers:
# Count the number of unique customers per branch.
#Branch with the Highest Total Deposits:
#Sum the transaction amounts per branch.
#Channels Used per Branch:
# Count the number of transactions per channel in each branch.

# Count the number of unique customers per branch
branch_customers <- MobBanking_df %>%
  group_by(BRANCH) %>%
  summarise(customers_count = n_distinct(CUST_NO)) %>%
  arrange(desc(customers_count))

# Display the branch with the most customers
branch_customers %>% top_n(1, customers_count)

#Branch with the Highest Total Deposits
# Sum the transaction amounts per branch
branch_deposits <- MobBanking_df %>%
  group_by(BRANCH) %>%
  summarise(total_deposits = sum(TXN_AMT, na.rm = TRUE)) %>%
  arrange(desc(total_deposits))
# Display the branch with the highest total deposits
branch_deposits %>% top_n(1, total_deposits)

#Channels Used per Branch
# Count the number of transactions per channel in each branch
branch_channels <- MobBanking_df %>%
  group_by(BRANCH, CHANNEL) %>%
  summarise(transaction_count = n(),) %>%
  arrange(BRANCH, desc(transaction_count))

# Display the channels used in the branch with the most customers or highest total deposits
branch_name <- MobBanking_df %>% top_n(1, CUST_NO) %>% pull(BRANCH)
branch_channels %>% filter(BRANCH == branch_name)

#Combining the Results to get the consolidated view the result are combined in one summary
# Combine branch metrics
branch_summary <- MobBanking_df %>%
  group_by(BRANCH) %>%
  summarise(
    customers_count = n_distinct(CUST_NO),
    total_deposits = sum(TXN_AMT, na.rm = TRUE),
    cash_count = sum(CHANNEL == "cash"),
    mobile_count = sum(CHANNEL == "mobile")
  ) %>%
  arrange(desc(customers_count), desc(total_deposits))

# Display the top branch
#The top branch is Mityana
branch_summary %>% top_n(1, customers_count)

#Calculate Thresholds for Higher and Lower Deposits
# Calculate thresholds for higher and lower deposits
deposit_thresholds <- MobBanking_df %>%
  summarise(
    lower_threshold = quantile(TXN_AMT, 0.1, na.rm = TRUE),
    higher_threshold = quantile(TXN_AMT, 0.9, na.rm = TRUE)
  )

# Filter customers with higher deposits
higher_deposits <- MobBanking_df %>%
  filter(TXN_AMT >= deposit_thresholds$higher_threshold) %>%
  select(CUST_NM, TXN_AMT, BRANCH, CHANNEL)
print(higher_deposits)

# Filter customers with lower deposits
lower_deposits <- MobBanking_df %>%
  filter(TXN_AMT <= deposit_thresholds$lower_threshold) %>%
  select(CUST_NM, TXN_AMT, BRANCH, CHANNEL)
# Summarize branch and channel usage for higher deposits
higher_deposits_summary <- higher_deposits %>%
  group_by(BRANCH, CHANNEL) %>%
  summarise(
    total_customers = n_distinct(CUST_NM),
    total_amount = sum(TXN_AMT, na.rm = TRUE),
    avg_amount = mean(TXN_AMT, na.rm = TRUE)
  )

# Summarize branch and channel usage for lower deposits
lower_deposits_summary <- lower_deposits %>%
  group_by(BRANCH, CHANNEL) %>%
  summarise(
    total_customers = n_distinct(CUST_NM),
    total_amount = sum(TXN_AMT, na.rm = TRUE),
    avg_amount = mean(TXN_AMT, na.rm = TRUE)
  )

# Find the top 5 customers with the highest deposits
top_customers <- MobBanking_df %>%
  arrange(desc(TXN_AMT)) %>%
  slice(1:5) %>%
  select(CUST_NM, TXN_AMT, BRANCH, CHANNEL)
# Find the top 20 customers with the lowest deposits
low_customers <- MobBanking_df %>%
  arrange(TXN_AMT) %>%
  distinct(CUST_NM, .keep_all = TRUE) %>%
  slice(1:10) %>%
  select(CUST_NM, TXN_AMT, BRANCH, CHANNEL)

#Total transaction generated by Mobile channel
# Filter to remove missing transaction amounts and for mobile channel
mobile_channel_data <- MobBanking_df %>%
  filter(!is.na(TXN_AMT) & CHANNEL == "mobile")

# Calculate the total transaction amount generated by the mobile channel
# The amount generated by Mobile channel is 1190005753
total_mobile_transactions <- mobile_channel_data %>%
  summarise(total_amount = sum(TXN_AMT, na.rm = TRUE))

# Display the total transaction amount for the mobile channel
print(total_mobile_transactions)

#Total transaction generated by Cash channel
# Filter to remove missing transaction amounts and for mobile channel
cash_channel_data <- MobBanking_df %>%
  filter(!is.na(TXN_AMT) & CHANNEL == "cash")

# Calculate the total transaction amount generated by the cash channel
# The amount generated by Mobile channel is 31495406450
total_cash_transactions <- cash_channel_data %>%
  summarise(total_amount = sum(TXN_AMT, na.rm = TRUE))

# Display the total transaction amount for the mobile channel
print(total_cash_transactions)

#TASK 4 
# Check for missing values
analysis_data <- MobBanking_df %>%
  filter(!is.na(LEDGER_BAL))
MobBanking_df <- na.omit( MobBanking_df)
names(MobBanking_df)
# Encode categorical variables because
#Statistical Analysis: Many statistical methods and models require categorical variables to be in factor form to properly interpret and analyze data.
#Machine Learning: Algorithms often require categorical variables to be converted into factors for effective training and prediction.
#Before Encoding: Categorical variables might be stored as character strings (e.g., "mobile", "cash").
#After Encoding: They are converted to factors (e.g., 1, 2) with levels that represent each category. This helps algorithms to understand and process these variables correctly.
#Encoding Categorical Variables: Transforms categorical data into a format suitable for statistical analysis and machine learning models.
analysis_data <- MobBanking_df %>%
  mutate(CHANNEL = as.factor(CHANNEL),
         BRANCH = as.factor(BRANCH),
         CUST_NM = as.factor(CUST_NM),
         ACCT_NO = as.factor(ACCT_NO),
         CUST_NO = as.factor(CUST_NO))

#statistical analysis using Regression Analysis technique
# Linear regression to predict transaction amount based on multiple variables
#Liner regression was Used to predict the transaction amount based on other variables such as ledger balance and last deposit amount. This model provided insights into how different factors influence transaction amounts.
#new object(regression_model) created to handle the model
#Residual in photo
#Using two explanatory variable
#low p value in F-statistics implies that one of the explain variable in the model has explanatory power of the outcome variable
#Multiple r squared value is  0.05448 there  5.448% of the Transaction amount(TXN_AMT) this used when dealing with one one independent variable eg TXN_AMT~LEDGER
#Adjusted r-squrared determine more than one variable the value is 0.05365 implying 5.365% of change in Transaction amount(TXN_AMT) can be explained by changes in both independent variables
#for LEDGER_BAL and LAST_DEPOSIT_AMT are both statistically significant
# As both LEDGER_BAL and LAST_DEPOSIT_AMT increase the TXN_AMT increases by the values in the table
#when more variables are added to the model the value of r, coefficients and p are increased leading to the betterment of the model
# Check for missing values and remove or impute them
dataregress <- MobBanking_df %>%
  filter(!is.na(TXN_AMT) & !is.na(LEDGER_BAL) & !is.na(LAST_DEPOSIT_AMT) & !is.na(BRANCH))
#Building a  model
regression_model <- lm(TXN_AMT ~ LEDGER_BAL + LAST_DEPOSIT_AMT + CHANNEL + BRANCH, data = MobBanking_df)
summary(regression_model)



# Calculate residuals
MobBanking_df$residuals <- residuals(regression_model)
summary(dataregress)


# Identify outliers
outliers <- MobBanking_df[abs(MobBanking_df$residuals) > 3 * sd(MobBanking_df$residuals), ]


# Predict transaction amounts
#Based on the code below, a new column will be created based on the existing data set
MobBanking_df$predicted_amount <- predict(regression_model, MobBanking_df)

# Plot actual vs predicted transaction amounts
ggplot(MobBanking_df, aes(x = TXN_AMT, y = predicted_amount)) +
  geom_point(color = 'blue', alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = "dashed") +
  theme_minimal() +
  labs(title = "Actual vs Predicted Transaction Amounts",
       x = "Actual Transaction Amount",
       y = "Predicted Transaction Amount") +
  theme(plot.title = element_text(hjust = 0.5))


# Plot residuals
ggplot(MobBanking_df, aes(x = predicted_amount, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Predicted Values",
       x = "Predicted Transaction Amount",
       y = "Residuals") +
  theme_minimal()



























#Machine Learning Techniques using clustering
#K-means Clustering: was Used to segment customers into distinct groups based on deposit behaviors. The Elbow method was employed to determine the optimal number of clusters. Clustering helps in identifying patterns and grouping similar customers for targeted strategies.
# Select relevant features for clustering
clustering_data <- MobBanking_df %>% select(LEDGER_BAL, TXN_AMT, LAST_DEPOSIT_AMT)
# Normalize the data
clustering_data <- scale(clustering_data)

# Determine the optimal number of clusters using the Elbow method
#This code is used to determine the optimal number of clusters for a k-means clustering algorithm by plotting the within-cluster sum of squares (WSS) against the number of clusters. The goal is to find the "elbow point," where the addition of more clusters no longer significantly reduces the WSS.
#nrow(clustering_data)-1: This part calculates the degrees of freedom, which is the number of rows minus 1
#apply(clustering_data, 2, var): The apply function computes the variance for each column (feature) of the clustering_data dataframe. The 2 indicates that the function should be applied to columns.
#sum(...): Sums up the variances of all columns.
#(nrow(clustering_data)-1) * sum(...): Multiplies the sum of variances by the degrees of freedom, giving the total variance if the data were treated as a single cluster.
wss <- (nrow(clustering_data)-1)*sum(apply(clustering_data, 2, var))

#Calculating WSS for Multiple Clusters (from 2 to 15):
#for (i in 2:15): Iterates over the number of clusters, from 2 to 15.
#kmeans(clustering_data, centers=i): Runs the k-means clustering algorithm on clustering_data with i clusters.
#$tot.withinss: Extracts the total within-cluster sum of squares for the i clusters.
#sum(...): Although tot.withinss is already a sum, this is used to store the value in the wss vector at index i.
for (i in 2:15) wss[i] <- sum(kmeans(clustering_data, centers=i)$tot.withinss)

#Visualizing the clusters
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#Purpose
#Within-Cluster Sum of Squares (WSS): Measures the variability of the data points within each cluster. Lower WSS values indicate tighter, more compact clusters.
#Elbow Method: By plotting the number of clusters (x-axis) against the WSS (y-axis), you can look for an "elbow point." This point represents the optimal number of clusters beyond which adding more clusters results in a diminishing reduction in WSS.


clustering_data <- kmeans(data, centers = 3, iter.max = 100, nstart = 100, data = data)



# Map 'cash' to 0 and 'mobile' to 1
MobBanking_df$CHANNEL <- ifelse(MobBanking_df$CHANNEL == "cash", 0, 
                                ifelse(MobBanking_df$CHANNEL == "mobile", 1, NA))

# Encode channel as a binary variable
MobBanking_df$CHANNEL <- as.numeric(MobBanking_df$CHANNEL) - 1

# Stratified sampling using dplyr
set.seed(123)
train_data <- MobBanking_df %>% 
  group_by(CHANNEL) %>% 
  sample_frac(0.8) %>% 
  ungroup()

test_data <- anti_join( MobBanking_df, train_data)


# Train a logistic regression model
classification_model <- glm(CHANNEL ~ LEDGER_BAL + TXN_AM + LAST_AMT_DT + BRANCH, 
                            MobBanking_df = train_data, 
                            family = binomial)

# Summary of the model
summary(classification_model)

# Predict the channel on the test set
test_data$predicted_channel <- predict(classification_model, test_data, type = "response")
test_data$predicted_channel <- ifelse(test_data$predicted_channel > 0.5, 1, 0)

# Confusion matrix
library(caret)
confusionMatrix(factor(test_data$predicted_channel), factor(test_data$channel))



