# ---- Understanding Sampling Distributions ----

# reading the bank demographic data 
# bank <- read.csv("https://raw.githubusercontent.com/kshitijjain91/Credit-Risk-Capstone/master/datasets/Demogs_v1.csv")

str(bank)
head(bank$Income, 25)

# population
income <- bank$Income
length(income)


# TODO:
# 1. Choose a sample size (n) and number of samples (K)
# 2. Take K samples each of size n and store the sample means in a vector 
# 3. Compare population mean with the mean of sample means
# 4. Compare population st_dev/sqrt(n) with the standard error
# 5. Plot the sampling distribution





# TODO:
# 1. Choose a sample size (n) and number of samples (K)
n = 5
K = 400


# 2. Take K samples each of size n and store the sample means in a vector 
samp_means = vector(mode="numeric", length = K)

for (sample_num in 1:K){
  
  s <- sample(income, n, replace=F)
  samp_means[sample_num] <- mean(s, na.rm=T)
}

head(samp_means, 10)


# 3. Compare population mean with the mean of sample means
pop_mean <- mean(income, na.rm = T)
mean_of_samp_means <- mean(samp_means, na.rm =T)
pop_mean
mean_of_samp_means


# 4. Compare population st_dev/sqrt(n) with the standard error
pop_sd <- sd(income, na.rm=T)
pop_sd_by_root_n <- pop_sd/sqrt(n)

stand_err <- sd(samp_means, na.rm = T)

pop_sd_by_root_n
stand_err


# 5. Plot the sampling distribution
samp_means <- data.frame(samp_means)
str(samp_means)

ggplot(samp_means, aes(samp_means)) + geom_density()