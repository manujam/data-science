############# EDA: LOAN CASE STUDY ###############################
# Submitted by: Team Fantastic Four - DDA1730283

################# ASSUMPTIONS ####################################
# 1. This code is developed & run on Windows 10 platform
# 2. Working directory is correctly set
# 3. All files are present in the working directory
# 4. All file names are maintained as per UpGrad website

################# Business Objective #############################
# 1. Identify main factors for credit loss / default through EDA
# 2. Identify customer profiles (credit score) who turn defaulters
# 3. Identify loan profiles that end in defaults
# 4. Identify patterns/trends on defaults
# 5. Make recommendations: 
#   a. Deny Loan
#   b. Reduce loan amount
#   c. Increase lending (interest) rate

install.packages("OneR")
library(OneR)
library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

loans <- read.csv("loan.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
str(loans)

##################### Data Cleaning #################################

# Null check
is.na(loans)

# Blank check
sapply(loans, function(x) length(which(x == ""))) # no blanks

# Duplicates check
sum(duplicated(loans$id)) # no duplicates for loans
sum(duplicated(loans$member_id)) # no duplicates for members

# Formatting for columns with "%"
loans$int_rate <- as.numeric(sub('[%]', '', trimws(loans$int_rate)))
loans$revol_util <- as.numeric(sub('[%]', '', trimws(loans$revol_util)))

# Remove whitespaces
loans$term <- trimws(loans$term)

# Date-time formatting
loans$issue_d = as.Date(paste0(trimws(loans$issue_d),"-18"), format="%b-%d-%y")
loans$last_pymnt_d = as.Date(paste0(trimws(loans$last_pymnt_d),"-18"), format="%b-%d-%y")
loans$last_credit_pull_d = as.Date(paste0(trimws(loans$last_credit_pull_d),"-18"), format="%b-%d-%y")

# Down-select to only defaulters & remove redundant columns
defaulters <- subset(loans, loan_status=="Charged Off") # create subset of only defaulters
defaulters_clean <- defaulters[c(1:10, 12:17, 21, 23:35, 39:47, 49, 106)] # remove columns with only NA
# remove(loans)
remove(defaulters)

##################### Data Preparation ###############################
loans$loan_amnt_bins <- bin(loans$loan_amnt, nbins = 5)
loans$annual_inc_bins <- bin(loans$annual_inc, nbins = 5)
loans$int_rate_bins <- bin(loans$int_rate, nbins = 5)
loans$revol_bal_bins <- bin(loans$revol_bal, nbins = 5)
loans$dti_bins <- bin(loans$dti, nbins = 5)
loans$op_acc_bins <- bin(loans$open_acc, nbins = 5)
loans$t_acc_bins <- bin(loans$total_acc, nbins = 5)

defaulters_clean$loan_amnt_bins <- bin(defaulters_clean$loan_amnt, nbins = 5)
defaulters_clean$annual_inc_bins <- bin(defaulters_clean$annual_inc, nbins = 5)
defaulters_clean$int_rate_bins <- bin(defaulters_clean$int_rate, nbins = 5)
defaulters_clean$revol_bal_bins <- bin(defaulters_clean$revol_bal, nbins = 5)
defaulters_clean$dti_bins <- bin(defaulters_clean$dti, nbins = 5)
defaulters_clean$op_acc_bins <- bin(defaulters_clean$open_acc, nbins = 5)
defaulters_clean$t_acc_bins <- bin(defaulters_clean$total_acc, nbins = 5)
#defaulters_clean$revol_util_bins <- bin(defaulters_clean$revol_util, nbins = 5)

##################### Univariate Analysis ############################

# Overall Loans portfolio
loan_status_plot <- ggplot(loans, aes(x=loan_status, y=prop.table(..count..)* 100, fill=..count..)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            vjust = -0.5,
            size = 4) + 
  labs(title = "Overall Loan Portfolio", x = "Loan Status", y = "% of total") + theme(legend.position="none")

loan_status_plot

# Analysis by state
state_perc_plot <- ggplot(loans, aes(x=addr_state, y=..count.., fill=loan_status)) + 
  geom_bar(stat = "count", position = "fill") +
  geom_text(aes(y =..count.., label = ..count..), 
            stat = 'count',
            position=position_fill(vjust=0.5),
            size = 2) + 
  labs(title = "Loans by State", x = "State", y = "Number of Loans")

# Analysis by state - percentage
state_def_plot <- ggplot(defaulters_clean, aes(x=addr_state, y=prop.table(..count..)* 100, fill=..count..)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            vjust = -0.5,
            size = 3) + 
  labs(title = "Defaulters by State", x = "State", y = "Defaulter %") + theme(axis.text.x = element_text(size=6, angle=45))

# Plot to summarize state-wise trend
grid.newpage()
grid.arrange(state_perc_plot, state_def_plot, ncol = 1, heights = c(1,1))

# boxplot(defaulters_clean$annual_inc) # Too many outliers
# boxplot(defaulters_clean$loan_amnt) # Outliers
# boxplot(defaulters_clean$funded_amnt) # Outliers
# boxplot(defaulters_clean$int_rate) # Outliers beyond 16%
# boxplot(defaulters_clean$revol_util) # No outliers
# boxplot(defaulters_clean$dti) # No outliers

#boxplot(annual_inc ~ loan_status, data=loans, main="Annual Income Distribution", col=c("#F1A59D","#9DC6F1","#9DF1B8"), xlab="Loan Status", ylab="Annual Income ($)")
#boxplot(loan_amnt ~ loan_status, data=loans, main="Loan Amount Distribution", col=c("#F1A59D","#9DC6F1","#9DF1B8"), xlab="Loan Status", ylab="Loan Amount ($)")
#boxplot(funded_amnt ~ loan_status, data=loans, main="Funded Amount Distribution", col=c("#F1A59D","#9DC6F1","#9DF1B8"), xlab="Loan Status", ylab="Funded Amount ($)")
#boxplot(installment ~ loan_status, data=loans, main="Installment Distribution", col=c("#F1A59D","#9DC6F1","#9DF1B8"), xlab="Loan Status", ylab="Installment ($)")
#boxplot(total_pymnt ~ loan_status, data=loans, main="Total Payment Distribution", col=c("#F1A59D","#9DC6F1","#9DF1B8"), xlab="Loan Status", ylab="Total Repayment ($)")
#boxplot(revol_bal ~ loan_status, data=loans, main="Revolving Balance Distribution", col=c("#F1A59D","#9DC6F1","#9DF1B8"), xlab="Loan Status", ylab="Revolving Balance ($)")

# Box plot to understand various distributions: Annual Income, Loan Amount, Funded Amount, Installment, Total Payments Received, Revolving Balance
ai_box <- ggplot(loans, aes(x=loan_status, y=annual_inc, color=loan_status)) + 
  geom_boxplot() +
  labs(title="Annual Income Distribution", y = "Annual Income ($)") + theme(legend.position="none")
la_box <- ggplot(loans, aes(x=loan_status, y=loan_amnt, color=loan_status)) + 
  geom_boxplot() +
  labs(title="Loan Amount Distribution", y = "Loan Amount ($)") + theme(legend.position="none")
fa_box <- ggplot(loans, aes(x=loan_status, y=funded_amnt, color=loan_status)) + 
  geom_boxplot() +
  labs(title="Funded Amount Distribution", y = "Funded Amount ($)") + theme(legend.position="none")
in_box <- ggplot(loans, aes(x=loan_status, y=installment, color=loan_status)) + 
  geom_boxplot() +
  labs(title="Installment Distribution", y = "Installment ($)") + theme(legend.position="none")
tp_box <- ggplot(loans, aes(x=loan_status, y=total_pymnt, color=loan_status)) + 
  geom_boxplot() +
  labs(title="Total Payments Received to date", y = "Total Repayment ($)") + theme(legend.position="none")
rb_box <- ggplot(loans, aes(x=loan_status, y=revol_bal, color=loan_status)) + 
  geom_boxplot() +
  labs(title="Revolving Balance Distribution", y = "Revolving Balance ($)") + theme(legend.position="none")

# Summarize various distrubitions
grid.newpage()
grid.arrange(ai_box, la_box, fa_box, rb_box, tp_box, in_box, ncol = 3, heights = c(3, 2))

# Box plot to understand various distributions: Annual Income, Loan Amount, Funded Amount, Installment, Total Payments Received, Revolving Balance
ir_box <- ggplot(loans, aes(x=loan_status, y=int_rate, color=loan_status)) + 
  geom_boxplot() +
  labs(title="Interest Rate Distribution", y = "Interest Rate (%)") + theme(legend.position="none")
ru_box <- ggplot(loans, aes(x=loan_status, y=revol_util, color=loan_status)) + 
  geom_boxplot() +
  labs(title="Revolving Line Utilization Rate Distribution", y = "Rate (%)") + theme(legend.position="none")
dt_box <- ggplot(loans, aes(x=loan_status, y=dti, color=loan_status)) + 
  geom_boxplot() +
  labs(title="Debt-to-income Distribution", y = "DTI (%)") + theme(legend.position="none")

# Summarize various distrubitions
grid.newpage()
grid.arrange(ir_box, ru_box, dt_box, ncol = 3, heights = c(3,1))

# Analysis by loan term
loan_term_plot <- ggplot(defaulters_clean, aes(x=term, y=prop.table(..count..)* 100, fill=..count..)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            vjust = -0.2,
            size = 3) + 
  labs(title = "% Defaulters by Loan term", x = "Loan Term", y = "Defaulter %")+ theme(legend.position="none")

# Analysis by home ownership
home_plot <- ggplot(defaulters_clean, aes(x=home_ownership, y=prop.table(..count..)* 100, fill=..count..)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            vjust = -0.2,
            size = 3) + 
  labs(title = "% Defaulters by Home Ownership", x = "Home Ownership", y = "Defaulter %") + theme(legend.position="none")

# Analysis by loan purpose
loan_purpose_plot <- ggplot(defaulters_clean, aes(x=purpose, y=prop.table(..count..)* 100, fill=..count..)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            vjust = -0.2,
            size = 3) + 
  labs(title = "% Defaulters by Purpose", x = "Loan Purpose", y = "Defaulter %") + theme(axis.text.x = element_text(size=6, angle=45), legend.position="none")

# Analysis by employment length
emp_length_plot <- ggplot(defaulters_clean, aes(x=emp_length, y=prop.table(..count..)* 100, fill=..count..)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            vjust = -0.2,
            size = 3) + 
  labs(title = "% Defaulters by Employment Length", x = "Employment Length", y = "Defaulter %") + theme(axis.text.x = element_text(size=6, angle=45), legend.position="none")

# Analysis by loan grade
grade_plot <- ggplot(defaulters_clean, aes(x=grade, y=prop.table(..count..)* 100, fill=..count..)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            vjust = -0.2,
            size = 3) + 
  labs(title = "% Defaulters by Grade", x = "Grade", y = "Defaulter %") + theme(legend.position="none")

# Analysis by loan verification status
ver_status_plot <- ggplot(defaulters_clean, aes(x=verification_status, y=prop.table(..count..)* 100, fill=..count..)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            vjust = -0.2,
            size = 3) + 
  labs(title = "% Defaulters by Verification Status", x = "Verification Status", y = "Defaulter %") + theme(legend.position="none")

# Summarize plots for defaulter univariate analysis-1
require(gridExtra)
grid.newpage()
grid.arrange(loan_term_plot, ver_status_plot, grade_plot, home_plot, ncol = 2, heights = c(2, 2))

# Summarize plots for defaulter univariate analysis-2
grid.newpage()
grid.arrange(loan_purpose_plot, emp_length_plot, ncol = 1, heights = c(1, 1))


##################### Bivariate Analysis #############################
# Analysis by term & home ownership
owner_by_term_plot <- ggplot(defaulters_clean, aes(x=term, y=prop.table(..count..)* 100, fill=home_ownership)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            position=position_stack(vjust=0.5),
            size = 3) + 
  labs(title = "Defaulters by Term & Home Ownership", x = "Term", y = "Defaulter %")

# Analysis by term & grade
grade_by_term_plot <- ggplot(defaulters_clean, aes(x=term, y=prop.table(..count..)* 100, fill=grade)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            position=position_stack(vjust=0.5),
            size = 3) + 
  labs(title = "Defaulters by Term & Grade", x = "Term", y = "Defaulter %")

# Analysis by term & loan purpose
purpose_by_term_plot <- ggplot(defaulters_clean, aes(x=purpose, y=prop.table(..count..)* 100, fill=term)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            position=position_stack(vjust=0.5),
            size = 3) + 
  labs(title = "Defaulters by Term & Loan Purpose", x = "Loan Purpose", y = "Defaulter %") + theme(axis.text.x = element_text(size=7, angle=35))

# Analysis by term & employment length
empl_by_term_plot <- ggplot(defaulters_clean, aes(x=emp_length, y=prop.table(..count..)* 100, fill=term)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            position=position_stack(vjust=0.5),
            size = 3) + 
  labs(title = "Defaulters by Term & Emp-Length", x = "Employment Length", y = "Defaulter %")+ theme(axis.text.x = element_text(size=7, angle=35))

# Summarize defaulter analysis
grid.newpage()
grid.arrange(owner_by_term_plot, purpose_by_term_plot, grade_by_term_plot, empl_by_term_plot, ncol = 2, widths = 1:2, heights = c(2, 2))

grade_dti_plot <- ggplot(defaulters_clean, aes(x=grade, y=prop.table(..count..)* 100, fill=dti_bins)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            position=position_stack(vjust=0.5),
            size = 4) + 
  labs(title = "% Defaulters by Grade & DTI", x = "Grades", y = "Defaulter %")

# Create subgroup of defaulters: Term=36 months, Grade=B or C, for Debt consolidation & 10+ years work exp. 
defaulters_sub <- subset(defaulters_clean, term=="36 months" & (grade=='B'|grade=='C') & purpose=="debt_consolidation" & emp_length=="10+ years")

# Analysis by defaulter subgroup and open-accounts
defaulters_by_op_acc <- ggplot(defaulters_sub, aes(x=open_acc, y=prop.table(..count..)* 100, fill=(..count..))) + 
  geom_bar(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            vjust = -0.5,
            size = 3) + 
  labs(title = "Defaulters (sub group) with # Open Account", x = "Number of Open Acc", y = "Defaulter %") + theme(legend.position="none")

# Analysis by defaulter subgroup and inquiries
defaulters_by_inq <- ggplot(defaulters_sub, aes(x=inq_last_6mths, y=prop.table(..count..)* 100, fill=(..count..))) + 
  geom_bar(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            vjust = -0.5,
            size = 3) + 
  labs(title = "Defaulters (sub group) by Inquiries", x = "Number of Inquiries", y = "Defaulter %") + theme(legend.position="none")

# Summarize defaulter subgroups
grid.newpage()
grid.arrange(defaulters_by_op_acc, defaulters_by_inq, ncol = 1, heights = c(1, 1))

#Method to convert time into timeslot, which wil be used for grouping.
DTIBinFinder <- function(dti)
{
  if((dti>=0 & dti<5) )
  {
    bin <- "Bin (0 - 4.9)"
  } else if(dti>= 5 & dti<10)
  {
    bin <- "Bin (5 - 9.9)"
  }else if(dti>=10 & dti< 15)
  {
    bin <- "Bin (10 - 15)"
  }else if(dti>= 15 & dti<20)
  {
    bin <- "Bin (15 -20)"
  }else
  {
    bin <- "Bin ( > 20)"
  }
  return(bin)
}


DTIBinFinders <- function(dtis)
{
  bins<- c()
  for(i in 1:length(dtis))
  {
    bins[i]<- DTIBinFinder(dtis[i])
  }
  return(bins)
}

defaulters_clean$DtiBins <- DTIBinFinders(defaulters_clean$dti)

plot_by_Dti_Bins_Verification_status__with_defaulted_loan_amount_sum <-
  ggplot(defaulters_clean, aes(x=DtiBins, y=prop.table(..count..)* 100, fill=verification_status)) + 
  geom_histogram(stat = "count") +
  geom_text(aes(y =prop.table(..count..)* 100,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            position=position_stack(vjust=0.5),
            size = 4) + 
  labs(title = "% Defaulters by DTI & Verification Status", x = "DTI bins", y = "Defaulter %")

plot_by_Dti_Bins_Verification_status__with_defaulted_loan_amount_sum

defaulters_meas <- defaulters_clean[c(3:5, 7:8, 13, 20, 23, 26:37)]
defaulters_corr <- cor(defaulters_meas)
melted_cor <- melt(defaulters_corr, na.rm = TRUE)

corr_plot <- ggplot(data = melted_cor, aes(Var2, Var1, fill = value)) + 
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "yellow", high = "red", mid = "orange", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  coord_fixed()

corr_plot 