#
# Loan Risk Analysis
#
# Assumptions
# Data Set File   - loan.csv is available in the working directory
#


#--------------------------------------------------------------------------------------------------#

#
# Load required libraries
#
library(ggplot2)
library(stringr)
library(dplyr)

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(lubridate)
library(corrplot)

library(ggthemes)
options("scipen"=100, "digits"=4)

#
# Load & view the Data Set
#
DataSetFile <- "loan.csv"

if (file.exists(DataSetFile)) {
  # Data Set file has NA, N/A, empty values in many records. 
  # such data needs to be imported as NA in data frame
  loan <- read.csv(DataSetFile, stringsAsFactors = FALSE, na.strings=c("NA", "N/A","", " "))
  # 39717 loan records with 111 columns of data
  
  cat("Info  : Data Set file \"",DataSetFile,"\" successfully loaded\n")
  #View(loan)
} else {
  cat("Error : Unable to access Data Set csv file -",DataSetFile,"\n")
  cat("Info  : Please verify the working directory and set as per Data Set file location\n")
}

#--------------------------------------------------------------------------------------------------#

#
# Data Cleaning and Manipulation
#

str(loan)
summary(loan)

length(unique(loan$id))
length(unique(loan$member_id))
length(loan$id)

#
# Duplicate Records
# id and member_id fields are unique identifiers. Verify for duplicate entries in these columns

sum(duplicated(loan$id))
# sum = 0 - No duplicate loan ids 

sum(duplicated(loan$member_id))
# sum = 0 - No duplicate member ids 

str(loan)
#
# NA values analysis
# Following columns have values 0 or NA :
#       collections_12_mths_ex_med, chargeoff_within_12_mths, tax_liens
# NA can be replaced with value 0 since NA values are a very small %
loan$collections_12_mths_ex_med[is.na(loan$collections_12_mths_ex_med)] <- 0
loan$chargeoff_within_12_mths[is.na(loan$chargeoff_within_12_mths)] <- 0
loan$tax_liens[is.na(loan$tax_liens)] <- 0

#
# Data Selection - Remove redundant columns
# Based on duplicate entries analysis, we can conclude that there is 1-1 mapping of loan id and 
# member id. So we can eliminate member id column
# Eliminate member id column
loan <- subset(loan, select = -c(member_id))


# 
# Data Selection - Loan Status Basis
# For loan defaulters analysis we need only records with status "Fully Paid" and "Charged Off"
# Loans with status "Current" cannot be considered as the future status of these records is unknown
# Eliminate records with loan_status = "Current" - 38577 records remain
loan <- loan[-which(toupper(loan$loan_status) == "CURRENT"), ]
# should/can we find out which of the current loans might be at risk?

#
# Data Selection - Remove columns with same value
# Data Set has 111 columns of data. Some columns have same value for all records/rows.
# Therefore nothing can be inferred from these columns.
# Such columns can be removed from final data. Eg - pymnt_plan, initial_list_status
# Eliminate columns with same value for all records
# Ref - https://stackoverflow.com/questions/30544282/how-to-remove-columns-with-same-value-in-r
loan <- loan[vapply(loan, function(x) length(unique(x)) > 1, logical(1L))]


#
# Data Selection - Remove columns not required based on their data
# url   - Link for loan details. Not required for analysis - Eliminate
# desc  - Loan description. Not required for analysis - Eliminate
# title - Title for loan description. Not required for analysis - Eliminate
# Eliminate listed columns
loan <- subset(loan, select = -c(url, desc, title))

summary(loan)
str(loan)


#
# Data Manipulation - Convert % columns to numbers
# int_rate and revol_util columns have % symbol. Remove % symbol and convert to numeric
loan$int_rate <- as.numeric(gsub("%", "", loan$int_rate))
loan$revol_util <- as.numeric(gsub("%", "", loan$revol_util))


#
# Data Manipulation - Convert all characters to upper case
loan$term                <- toupper(str_trim(loan$term))
loan$grade               <- toupper(loan$grade)
loan$sub_grade           <- toupper(loan$sub_grade)
loan$emp_title           <- toupper(loan$emp_title)
loan$emp_length          <- toupper(loan$emp_length)
loan$home_ownership      <- toupper(loan$home_ownership)
loan$verification_status <- toupper(loan$verification_status)
loan$issue_d             <- toupper(loan$issue_d)
loan$loan_status         <- toupper(loan$loan_status)
loan$purpose             <- toupper(loan$purpose)
loan$zip_code            <- toupper(loan$zip_code)
loan$addr_state          <- toupper(loan$addr_state)
loan$earliest_cr_line    <- toupper(loan$earliest_cr_line)
loan$last_pymnt_d        <- toupper(loan$last_pymnt_d)
loan$last_credit_pull_d  <- toupper(loan$last_credit_pull_d)


#
# Data Manipulation - Employer Title
# Employer is in many different forms. We need to manipulate and bring similar employers together

# Remove all special characters
loan$emp_title <- gsub("\\.", "", loan$emp_title)
loan$emp_title <- gsub("\\,", "", loan$emp_title)
loan$emp_title <- gsub("\\'", "", loan$emp_title)
loan$emp_title <- gsub("\\:", "", loan$emp_title)
loan$emp_title <- gsub("\\-", "", loan$emp_title)
loan$emp_title <- gsub("\\)", "", loan$emp_title)
loan$emp_title <- gsub("\\(", "", loan$emp_title)

# Remove all white space
loan$emp_title <- gsub("[[:space:]]", "", loan$emp_title) 
loan$emp_title[which(loan$emp_title == "SELFEMPLOYED")] <- "SELF"
loan$emp_title[which(loan$emp_title == "SELFEMP")] <- "SELF"


#
# Data Manipulation - Handle dates

# Convert dates
loan$earliest_cr_line <- as.Date(paste(loan$earliest_cr_line,"-01",sep=""), "%b-%y-%d")

# Check for future dates
sum(year(loan$earliest_cr_line) > 2018)
# 85 dates > 2018

# All dates greater than current date, convert to 1900s by subtracting 100 years.
loan[which(year(loan$earliest_cr_line)>year(Sys.Date())),]$earliest_cr_line <- 
  as.POSIXct(ymd(loan[which(year(loan$earliest_cr_line)>year(Sys.Date())),]$earliest_cr_line) - years(100))


loan$issue_d <- as.Date(paste(loan$issue_d,"-01",sep=""), "%b-%y-%d")
# No future dates
sum(year(loan$issue_d) > 2018)

loan$last_credit_pull_d <- as.Date(paste(loan$last_credit_pull_d,"-01",sep=""), "%b-%y-%d")
# No future dates
sum(year(loan$last_credit_pull_d) > 2018)

loan$last_pymnt_d <- as.Date(paste(loan$last_pymnt_d,"-01",sep=""), "%b-%y-%d")
# No future dates
sum(year(loan$last_pymnt_d) > 2018)


#
# Data Manipulations - Handling numerics
# Rounding amounts to two decimal places
loan$collection_recovery_fee <- round(loan$collection_recovery_fee,2)
loan$funded_amnt_inv <- round(loan$funded_amnt_inv,2)
loan$total_pymnt <- round(loan$total_pymnt,2)
loan$recoveries <- round(loan$recoveries,2)
loan$total_rec_late_fee <- round(loan$total_rec_late_fee,2)
loan$annual_inc <- round(loan$annual_inc)


#--------------------------------------------------------------------------------------------------#

#
# Univariate Analysis
#


#
# Data - annual_inc
# Annual income of individuals
summary(loan$annual_inc)
# 50% of the annual incomes lie between 40K and 82K
loan %>%
  filter(annual_inc<=1000000) %>%
  ggplot(aes(annual_inc)) + geom_histogram( binwidth = 1000) + 
      ggtitle("Annual income analysis") +
      labs(x="Annual Income", y="Applicants Count")
# Possibility: Low-medium-high income categories can be created based on distribution
summary(loan$annual_inc)
# Low: <40K
# Mid: 40K-82K
# High: >82K


#
# Data - collection_recovery_fee
# post charge off collection fee
summary(loan$collection_recovery_fee)
boxplot(loan$collection_recovery_fee)
# most of the values are zero
# remove column, as this is after charge off has happened
loan$collection_recovery_fee <- NULL


#
# Data - delinq_2yrs
# The number of 30+ days past-due incidences of delinquency(default)
# in the borrower's credit file for the past 2 years
summary(loan$delinq_2yrs)
loan %>%
  ggplot(aes(delinq_2yrs)) + geom_histogram( binwidth = 1) + 
      labs(x="Delinquency incident count", y="Applicants Count")
# maximum people have no incidences of delinquency
# important variable


#
# Data - dti
# A ratio calculated using the borrower's total monthly debt payments 
# on the total debt obligations, excluding mortgage and the requested 
# LC loan, divided by the borrower's self-reported monthly income.
summary(loan$dti)
loan %>%
  ggplot(aes(dti)) + geom_histogram( binwidth = 1) + 
    labs(x="DTI Ratio", y="Applicants Count")
# good distribution of data
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    8.13   13.37   13.27   18.56   29.99 


#
# Data - earliest_cr_line 
# The month the borrower's earliest reported credit line was opened
summary(loan$earliest_cr_line)
#might be important. might also signify the LC's age and experience with taking loans


# 
# Data - employment length
summary(as.factor(loan$emp_length))
ggplot(loan, aes(emp_length)) + geom_bar() + 
  ggtitle("Employment time length analysis") +
  labs(x="Employment length", y="Applicants count")
# make this ordered
loan$emp_length <- ordered(loan$emp_length, levels = c("N/A","< 1 YEAR", "1 YEAR","2 YEARS","3 YEARS","4 YEARS","5 YEARS","6 YEARS","7 YEARS","8 YEARS","9 YEARS","10+ YEARS"))
ggplot(loan, aes(emp_length)) + geom_bar()  + 
  ggtitle("Employment time length analysis") +
  labs(x="Employment length", y="Applicants count")


# 
# Data - emp_title: too many unique values
# maybe insignificant - Hence remove the column
length(unique(loan$emp_title))
# dropping the field
loan$emp_title <- NULL


# 
# Data - funded_amnt
# The total amount committed to that loan at that point in time.
# significant, with a good distribution
summary(loan$funded_amnt)
hist(loan$funded_amnt, main="Histogram for Funded Amount", xlab="Funded Amount")


# 
# Data - funded_amnt_inv
# The total amount committed by investors for that loan at that point in time.
# significant, with a good distribution
summary(loan$funded_amnt_inv)
hist(loan$funded_amnt_inv, main="Histogram for Funded Amount - Investor", xlab="Funded Amount")
# may not be useful for analysis as this comes into picture only after the loan has been approved.
# removing
loan$funded_amnt_inv <- NULL


#
# Data - grade
# LC assigned loan grade
# good distribution, significant
summary(as.factor(loan$grade))
ggplot(loan, aes(as.factor(loan$grade))) + geom_bar() +
  ggtitle("Grade Analysis") +
  labs(x="Grade", y="Applicants count")


#
# Data - home_ownership
# good distribution
summary(as.factor(loan$home_ownership))
ggplot(loan, aes(as.factor(loan$home_ownership))) + geom_bar() +
  ggtitle("Home Ownership Analysis") +
  labs(x="Home Ownership Status", y="Applicants count")


#
# Data - id
# unique id, but insignificant for EDA
summary(loan$id)
# retaining the field for scatter plots


#
# Data - inq_last_6mths
# The number of inquiries in past 6 months (excluding auto and mortgage inquiries)
# significant, good distribution
summary(loan$inq_last_6mths)
ggplot(loan, aes(inq_last_6mths)) + geom_bar() +
  ggtitle("Inquiries Analysis") +
  labs(x="Inquiries in last 6 mnths", y="Applicants count")



#
# Data - installment
# The monthly payment owed by the borrower if the loan originates.
# significant, good distribution
summary(loan$installment)
hist(loan$installment, main="Histogram for Loan Installment", xlab="Loan Installment Amount")


#
# Data - int_rate
# Interest Rate on the loan
# significant, good distribution - may need binning?
summary(loan$int_rate)
ggplot(loan, aes(loan$int_rate)) + geom_histogram(binwidth = 1)  +
  ggtitle("Interest Rate Analysis") +
  labs(x="Loan Interest Rate", y="Applicants count")



#
# Data - issue_d
# The month which the loan was funded
# maybe significant to calculate duration of payment?
summary(loan$issue_d)


#
# Data - last_credit_pull_d
# The most recent month LC pulled credit for this loan
summary(loan$last_credit_pull_d)
# unnecessary, because this information comes up only after loan is approved.
loan$last_credit_pull_d <- NULL


# 
# Data - last_pymnt_amnt
# maybe significant, cannot say.
summary(loan$last_pymnt_amnt)


#
# Data - last_pymnt_d
# maybe significant, cannot say at this time.
summary(loan$last_pymnt_d)
# this information is not available when a LC comes asking for a loan. removing column
loan$last_pymnt_d <- NULL


# 
# Data - loan_amnt
# well distributed, 
# very significant
summary(loan$loan_amnt)


# 
# Data - loan_status
# end result
# needed for final assessment
summary(as.factor(loan$loan_status))/length(loan$loan_status)
ggplot(loan, aes(as.factor(loan_status))) + 
  geom_bar() + 
  # theme_wsj()
  ggtitle("Loan Status Analysis") +
  labs(x="Loan Status", y="Applicants count")


#
# Data - mths_since_last_delinq (months since last default)
# The number of months since the borrower's last delinquency.
# very significant
summary(loan$mths_since_last_delinq)
hist(loan$mths_since_last_delinq, main="Histogram for Months since last default", xlab="Months since last default")


# 
# Data - mths_since_last_record
# The number of months since the last public record.
# significant field
summary(loan$mths_since_last_record)
hist(loan$mths_since_last_record, main="Histogram for Months since last record", xlab="Months since last record")


#
# Data - open_acc
# The number of open credit lines in the borrower's credit file.
# significant
summary(loan$open_acc)
hist(loan$open_acc, main="Histogram for Open Credit Lines", xlab="Open Credit Lines Count")


#
# Data - pub_rec
# Number of derogatory public records
# significant
summary(loan$pub_rec)


#
# Data - pub_rec_bankruptcies
# Number of public record bankruptcies
# maybe significant
summary(as.factor(loan$pub_rec_bankruptcies))
ggplot(loan, aes(pub_rec_bankruptcies)) + geom_bar() +
  ggtitle("Public record bankruptcies Analysis") +
  labs(x="Public record bankruptcies count", y="Applicants count")


#
# Data - purpose
# A category provided by the borrower for the loan request. 
# significant, can be used for segmentation?
summary(as.factor(loan$purpose))


# 
# Data - recoveries
# post charge off gross recovery
# does not seem significant for our analysis, as this is after charge off
summary(loan$recoveries)
summary(as.factor(loan$recoveries))
# removing field
loan$recoveries <- NULL


#
# Data - revol_bal
# Total credit revolving balance
# may be useful in continuous variables bivariate analysis
summary(loan$revol_bal)


#
# Data - revol_util
# Revolving line utilization rate, or the amount of credit the borrower 
# is using relative to all available revolving credit.
# significant
summary(loan$revol_util)
hist(loan$revol_util, main="Histogram for Revolving line utilization rate", xlab="Rate")


#
# Data - sub_grade
# LC assigned loan subgrade
# significant
summary(loan$sub_grade)
summary(as.factor(loan$sub_grade))


#
# Data - term
# The number of payments on the loan. 
# Values are in months and can be either 36 or 60.
# maybe significant, but only 2 levels 36/60 months
summary(as.factor(loan$term))


#
# Data - total_acc
# The total number of credit lines currently in the borrower's credit file
# significant
summary(loan$total_acc)
hist(loan$total_acc, main="Histogram for Total Credit Lines ", xlab="Credit Lines Count")


#
# Data - total_pymnt
# Payments received to date for total amount funded
# this would be in proportion to the loan amount, so maybe insignificant as is
# percentage paid back may be a better field for analysis(this should be derivable)
summary(loan$total_pymnt)
hist(loan$total_pymnt, main="Histogram for Total Payment", xlab="Total Payment Amount")
# this comes into the picture only after loan has been granted.
# for our purposes, this is not required. removing
loan$total_pymnt <- NULL


#
# Data - total_pymnt_inv
# Payments received to date for portion of total amount funded by investors
# not sure if this is significant or same as total_pymnt
summary(loan$total_pymnt_inv)
# removing as it's not required
loan$total_pymnt_inv <- NULL


#
# Data - total_rec_int
# Interest received to date
# may not be significant
summary(loan$total_rec_int)
# removing
loan$total_rec_int <- NULL


# 
# Data - total_rec_late_fee
# Interest received to date
# may not be significant
summary(loan$total_rec_late_fee)
# removing
loan$total_rec_late_fee <- NULL


#
# Data - total_rec_prncp
# Principal received to date
# may not be significant
summary(loan$total_rec_prncp)
# removing
loan$total_rec_prncp <- NULL


#
# Data - verification_status
# Indicates if income was verified by LC, not verified, 
# or if the income source was verified
# significant
summary(loan$verification_status)
summary(as.factor(loan$verification_status))


# 
# Data - zip_code
# The first 3 numbers of the zip code 
# provided by the borrower in the loan application.
# may/may not be significant
summary(as.factor(loan$zip_code))

#
# Univariate Analysis Complete


#
# Segment Data into ChargedOff and FullyPaid
Loan_ChargedOff <-loan[which(toupper(loan$loan_status) == "CHARGED OFF"), ]
Loan_FullyPaid  <-loan[which(toupper(loan$loan_status) == "FULLY PAID"), ]

Loan_ChargedOff$emp_length <- ordered(Loan_ChargedOff$emp_length, levels = c("N/A","< 1 YEAR", "1 YEAR","2 YEARS","3 YEARS","4 YEARS","5 YEARS","6 YEARS","7 YEARS","8 YEARS","9 YEARS","10+ YEARS"))
Loan_FullyPaid$emp_length <- ordered(Loan_FullyPaid$emp_length, levels = c("N/A","< 1 YEAR", "1 YEAR","2 YEARS","3 YEARS","4 YEARS","5 YEARS","6 YEARS","7 YEARS","8 YEARS","9 YEARS","10+ YEARS"))

loan %>%
  group_by(loan_status, emp_length) %>%
  ggplot(aes(x=emp_length)) + geom_bar(position = "dodge") +
  facet_wrap(~loan_status) +
  ggtitle("Employee Working Time Length vs Loan Status") +
  labs(x="Emp Time", y="Applicants count")


#
# Order columns to map with data dictionary for ease of understanding
loan <- loan[c(sort(colnames(loan)))]

#--------------------------------------------------------------------------------------------------#

#
# Bivariate Anlaysis
#


#
# addr_state with respect to loan status
# the graph shows similar trends for charged off across most of the states
# states IA, ID and ME have no charged off loans but the total loans count is very less compared 
# to other states
ggplot(loan,aes(x=factor(loan$addr_state),fill=factor(loan$loan_status)))+
  geom_bar(stat='count',position = "dodge") + scale_y_continuous(trans='log2') +
  ggtitle("Loan Status per Address State Analysis") +
  labs(x="Address State", y="Applicants Count", fill="Loan Status")


#
# annual_inc with respect to loan status
# income range from 30000-70000 has maximum charged off counts.
# income range greater than 100000 has less charged off to total ratio compared to other income 
# ranges.
# Creating income brackets
loan$income_bucket = cut(as.numeric(loan$annual_inc), breaks=c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,1000000)
,labels=c("0-10000","10000-20000","20000-30000","30000-40000","40000-50000","50000-60000","60000-70000","70000-80000","80000-90000","90000-100000",">100000")
,include.lowest=TRUE,right=FALSE)

ggplot(loan,aes(x=factor(income_bucket),fill=factor(loan_status)))+geom_bar(stat='count') +
  ggtitle("Loan Status per Income Bucket Analysis") +
  labs(x="Income Bucket", y="Applicants Count", fill="Loan Status")

loan %>%
  filter(loan_status=="CHARGED OFF") %>%
ggplot(aes(x=factor(income_bucket),fill=factor(loan_status)))+geom_bar(stat='count') + facet_wrap(~loan_status) +
  ggtitle("Income Bucket Analysis for Charged Off Loans") +
  labs(x="Income Bucket", y="Applicants Count", fill="Loan Status")


#
# collection_recovery_fee with respect to loan status
# it is applicable only for charged off , hence not significant

# delinq_2yrs with respect to loan status 
# no significant insights from the graph between the two
# delinq_2yrs >= 6 has almost zero charged off counts but the loans count for it is just 0.05% of 
# the total.
ggplot(loan,aes(x=factor(delinq_2yrs),fill=factor(loan_status)))+geom_bar(stat='count',position='dodge')+scale_y_continuous(trans='log2') +
  ggtitle("Delinquency incident count Analysis w.r.t Loan Status") +
  labs(x="Delinquency incident count", y="Applicants Count", fill="Loan Status")

# delinq_2yrs >= 6' percent of total
length(loan$id[which(loan$delinq_2yrs>=6)])*100/length(loan$id)


#
# earliest_cr_line with respect to loan status
# no significance as such.charged off and fully paid have similar trend based on 
# earliest_cr_line_year.
# extracting year from earliest_cr_line 
loan$earliest_cr_line_year <-year(loan$earliest_cr_line)
ggplot(loan,aes(x=factor(loan_status),fill=factor(earliest_cr_line_year)))+geom_bar(stat='count',position='dodge') +
  ggtitle(" Earliest credit line time Analysis w.r.t Loan Status") +
  labs(x="Loan Status", y="Applicants Count", fill="Year")



#
# calculate number of years since first credit
# checking if older loan takers are less likely to default
loan$years_since_first_credit <- max(year(loan$issue_d)) - year(loan$earliest_cr_line)
Loan_ChargedOff$years_since_first_credit <- max(year(Loan_ChargedOff$issue_d)) - year(Loan_ChargedOff$earliest_cr_line)
Loan_FullyPaid$years_since_first_credit <- max(year(Loan_FullyPaid$issue_d)) - year(Loan_FullyPaid$earliest_cr_line)

Loan_ChargedOff %>%
  ggplot(aes(x=years_since_first_credit))+
  geom_bar() +
  ggtitle("Years since first credit analysis") +
  labs(x="Years since first credit", y="Applicants Count")

# those between 10-14 years since first loan seem like the most defaulters
# the ones over 15 years of taking loans are less likely to default

Loan_ChargedOff %>%
  ggplot(aes(x=years_since_first_credit, fill=purpose))+
  geom_bar() +
  ggtitle("Years since first credit Analysis w.r.t Loan purpose") +
  labs(x="Years since first credit", y="Applicants Count", fill="Loan Purpose")

# debt consolidation is highlighted as the most risky loan


#
# emp_length with respect to loan status
# emp_length > 10 has more charged off to total loan ratio compared to other ranges.
ggplot(loan,aes(x=factor(emp_length),fill=factor(loan_status)))+geom_bar(stat='count',position='dodge') +
  ggtitle("Employee time length analysis w.r.t Loan Status") +
  labs(x="Employee Time Length", y="Applicants Count", fill="Loan Status")



#
# grade with respect to loan status
# grade A has very less charged off loan count inspite of having high total loans count.
# ratio of charged off loans to total loans count is showing increasing behaviour with grade.
ggplot(loan,aes(x=factor(grade),fill=factor(loan_status)))+geom_bar(stat='count',position='dodge') +
  ggtitle("Grade analysis w.r.t Loan Status") +
  labs(x="Grade", y="Applicants Count", fill="Loan Status")



#
# home_ownership with respect to loan status
# Mortgage and rent follow almost similar trend.
# home_ownership=OWN has less charged off count to total loans count ratio compared to mortgage and 
# rent.
ggplot(loan,aes(x=factor(home_ownership),fill=factor(loan_status)))+geom_bar(stat='count',position='dodge') +
  ggtitle("Home ownership analysis w.r.t Loan Status") +
  labs(x="Home Ownership analysis", y="Applicants Count", fill="Loan Status")



#
# purpose with respect to loan status
# debt consolidation has highest number of charged off loan.However,since debt_consolidation can 
# comprise of multiple other purposes, it does not give a clear idea.
# small_business has high charged off count to total loans count ratio compared to other purposes.
# wedding and car has less charged off count to total loans count ratio compared to other purposes.
ggplot(loan,aes(x=factor(purpose),fill=factor(loan_status)))+geom_bar(stat='count',position='dodge') + facet_wrap(~loan_status) +
  ggtitle("Loan Purpose analysis w.r.t Loan Status") +
  labs(x="Loan purpose", y="Applicants Count", fill="Loan Status")


loan %>%
  filter(loan_status == "CHARGED OFF") %>%
ggplot(aes(x=factor(purpose),fill=factor(emp_length)))+geom_bar(stat='count',position='fill') + facet_wrap(~loan_status) +
  ggtitle("Loan Purpose analysis w.r.t Employee Time length for Charged off Loans") +
  labs(x="Loan purpose", y="Applicants Count", fill="Time lenth")



#
# focusing on >10 years experienced LCs
Loan_ChargedOff %>%
  filter(emp_length=="10+ YEARS") %>%
ggplot(aes(x=factor(verification_status),fill=factor(emp_length)))+
  geom_bar(stat='count',position='stack') +
  facet_wrap(~purpose) +
  ggtitle("Verification Status analysis w.r.t Loan purpose") +
  labs(x="Verification Status", y="Applicants Count", fill="Time lenth")

# main loan for debt consolidation.
# other loan requests are for 'moving', 'small businesses', 'house' and 'other'.
# this is the same across all verification statuses


#
# term with respect to loan status
# Loan with 36 Months of Term is more popular 
# Even though the count of people defaulted in 36 months is higher. the Ratio is very much higher 
# for the 60 Month term
# The Loans with 60 Months term has high charged off count to total loans count ratio
ggplot(loan,aes(x=factor(term),fill=factor(loan_status)))+geom_bar(stat='count',position='dodge') +
  ggtitle("Loan Status analysis w.r.t Loan Term") +
  labs(x="Loan term", y="Applicants Count", fill="Loan Status")

# 3227 defaulted aganist 36 months term to that of the 2400 defaulters from 60 month loan
ggplot(data = Loan_ChargedOff, aes(term)) + geom_bar(stat="count") + geom_text(stat='count',aes(label=..count..), vjust=-0.25)+xlab("Term of Payments") + ylab("Count of Applicants") + ggtitle("No of payments on the loan (Values are in months and can be either 36 or 60)")


#
# sub_grade with respect to loan status
# grades B,C and D have the majority of the people defaulted
ggplot (loan,aes(x=loan_status)) +
  geom_bar(aes(y=(..count..)/sum(..count..),fill=loan_status)) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),y= (..count..)/sum(..count..) ),
            stat= "count",vjust = -0.5)  + labs(x="Grade",y="Percentage") + facet_grid(~grade) +
  scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels=abbreviate) +
  ggtitle("Loan sub grade analysis w.r.t Loan Status")

# the default ratio is high with the D, E, F and G subsets
# B3, B5 and C1 are the top 3 subsets wiht the highest defaulters
ggplot(loan,aes(x=factor(sub_grade),fill=factor(loan_status)))+geom_bar(stat='count',position='dodge') +
  ggtitle("Loan sub grade analysis w.r.t Loan Status") +
  labs(x="Sub Grade", y="Applicants Count", fill="Loan Status")



#
# verification with respect to loan status
# The accounts which are not verified are defaulted the most
ggplot (loan,aes(x=loan_status)) +
  geom_bar(aes(y=(..count..)/sum(..count..),fill=loan_status)) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),y= (..count..)/sum(..count..) ),
            stat= "count",vjust = -0.5)  +
  labs(x="Grade",y="Percentage") +
  facet_grid(~verification_status) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Loan verification status analysis w.r.t Loan Status per grade")

# The Non Verified accounts are the most number of charged off accounts
ggplot(Loan_ChargedOff,aes(x=factor(verification_status),fill=factor(loan_status)))+geom_bar(stat='count',position='dodge') +
  ggtitle("Verification Status Analysis for Charged Off loans") +
  labs(x="Verification Status", y="Applicants Count", fill="Loan Status")



#
# charged off accounts : verification status vs purpose of loan
Loan_ChargedOff %>%
  ggplot(aes(y=length(loan_amnt), x=verification_status, fill=purpose))+
  geom_col() +
  ggtitle("Verification Status Analysis for Charged Off loans") +
  labs(x="Verification Status", y="Loan Amount", fill="Loan purpose")



#
# Why do verfied accounts default?
# Plotting verified but defaulted accounts w.r.t. home ownership
Loan_ChargedOff %>%
  filter(verification_status=="VERIFIED") %>%
  ggplot(aes(x=factor(home_ownership)))+
  geom_bar(stat='count',position='stack') +
  ggtitle("Home Ownership Analysis for verified status loans") +
  labs(x="Home Ownership Status", y="Loan Amount")



#
# Employee with respect to the loan status
# Customers with more than 10+ years have defaulted more than any other customers
# customers who have taken it while havng a rent have defaulted the most
# Customers who have been with a company for more than 10+ years and have taken the mortagage have 
# successfully paid the loan
p1 <-ggplot(loan, aes(x=loan$home_ownership, y = (..count..)/sum(..count..), fill=factor(loan_status))) + geom_bar(position="fill") + scale_y_continuous(labels = scales::percent) + xlab("Home Ownership") + ylab("Applicants  Percentage")+ ggtitle("Status of the loan on the Basis of the type of Home") + geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) 
p2 <- ggplot(loan, aes(x=loan$emp_length, fill=factor(loan_status))) + geom_bar(position="dodge",stat="count") + geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25)+ xlab("Work Experience") + ylab("Applicants Count") + ggtitle("Status of the loan on the Basis of the type of Home") 
grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)


#
# Address State with respect to loan status
# neveda is the riskiest state to lend as the maximum defaulters are from this state
p1 <- ggplot(loan, aes(x=loan$addr_state, y = (..count..)/sum(..count..), fill=factor(loan_status))) + geom_bar(position="fill") + scale_y_continuous(labels = scales::percent) + xlab("Address State") + ylab("Applicants  Percentage")+ ggtitle("Status of Loan on basis of State") + geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) 
p2 <- ggplot(loan, aes(x=loan$purpose, fill=factor(loan_status))) + geom_bar(position="dodge",stat="count") + geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25)+ xlab("Home Ownership") + ylab("Applicants Count") + ggtitle("Status of the loan on the Basis of the type of Home") 
grid.arrange(p1, p2, ncol=2)
p1
p2
rm(p1)
rm(p2)


#
# Multivariate Scatter plot
# The Small Business and Others category has the highest defaulters ratio to that of the paid
# The debt_consolidation and Credit_card are the highest in terms of volume in terms of purpose 
# of the customers
ggplot(loan, aes(x = loan_amnt, y = purpose, col = loan_status )) + geom_point( alpha = 0.2 ) + geom_jitter() + facet_wrap(~loan_status) +
  labs(x="Loan AMount", y="Purpose")



# rm(Loan_ChargedOff)
# rm(Loan_FullyPaid)
# rm(loan)


#
# correlation between numeric variables
# loan_amnt and fund_amnt are highly correlated for both overall loans and charged off loans.
# loan_amnt and installment are highly correlated for both overall loans and charged off loans.
# loan_amnt and last_pymnt_amnt has a correlation of 0.47 for overall loan but only 0.33 for 
# charged off loans.
# toal_accnt and open_accnt has moderate correlation for both overall loans and charged off loans.

loan_numeric_variables <- subset(loan,select=c(loan_amnt,funded_amnt,dti,inq_last_6mths,installment,int_rate,last_pymnt_amnt,open_acc,revol_bal,total_acc))
loan_charged_off_numeric_variables <- subset(Loan_ChargedOff,select=c(loan_amnt,funded_amnt,dti,inq_last_6mths,installment,int_rate,last_pymnt_amnt,open_acc,revol_bal,total_acc))
loan_numeric_variables_cor <-cor(loan_numeric_variables)
loan_charged_off_numeric_variables_cor <-cor(loan_charged_off_numeric_variables)
corrplot(loan_numeric_variables_cor)
corrplot(loan_charged_off_numeric_variables_cor)

#--------------------------------------------------------------------------------------------------#
