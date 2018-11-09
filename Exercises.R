###
###EXERCISE 
###
 
# Define new spinner: regions
regions<-c(2,2,4)
 
# Simulation 1000 spins: spins
spins<-spinner_data(regions,1000)
 
# Graph the spin data using bar_plot()
bar_plot(spins)
 
# Construct frequency table of spins
table(spins)
 
# Find fraction of spins equal to 2
mean(spins == 2)
 
# Find mean spin value
mean(spins)
 
 
###
###EXERCISE 
###
# Create the vector of models: Model
Model <- c("Spinner A", "Spinner B")
 
# Define the vector of prior probabilities: Prior
Prior <- c(0.5,0.5)
 
# Define the vector of likelihoods: Likelihood
Likelihood <- c(0.5,1/6)
 
# Make a data frame with variables Model, Prior, Likelihood: bayes_df
(bayes_df<-data.frame(Model,Prior,Likelihood))
 
# Compute the posterior probabilities
bayesian_crank(bayes_df)
 
prior_post_plot(bayes_df)
 
 
###
###EXERCISE 
###
# Display the vector of models: Model
Model <- c("Spinner A", "Spinner B")
 
# Define the vector of prior probabilities: Prior
Prior <- c(0.75, 0.25)
 
# Define the vector of likelihoods: Likelihood
Likelihood <- c(1/2, 1/6)
 
# Make a data frame with variables Model, Prior, Likelihood: bayes_df
(bayes_df<-data.frame(Model,Prior,Likelihood))
 
 
###
###EXERCISE 
###
# Define the values of the proportion: P
P <- c(0.5, 0.6, 0.7, 0.8, 0.9)
 
# Define Madison's prior: Prior
Prior <- c(0.3, 0.3, 0.2, 0.1, 0.1)
 
# Compute the likelihoods: Likelihood
Likelihood<-dbinom(16, size = 20, prob = P)
 
# Create Bayes data frame: bayes_df
bayes_df<-data.frame(P,Prior,Likelihood)
 
# Compute and print the posterior probabilities: bayes_df
bayes_df<-bayesian_crank(bayes_df)
 
# Graphically compare the prior and posterior
prior_post_plot(bayes_df)
 
 
###
###EXERCISE 
###
# Find the probability that P is smaller than 0.85
pbeta(0.85, 8.13, 3.67)
 
# Find the probability that P is larger than 0.85
1-pbeta(0.85, 8.13, 3.67)
 
# Find the 0.75 quantile of P
qbeta(0.75,8.13, 3.67)
 
# Specify that the 0.25 quantile of P is equal to 0.7: quantile1
quantile1 <- list(p = 0.25, x = 0.70)
 
# Specify that the 0.75 quantile of P is equal to 0.85: quantile2
quantile2 <- list(p = 0.75, x = 0.85)
 
# Find the beta shape parameters matching the two quantiles: ab
ab<-beta.select(quantile1, quantile2)
 
# Plot the beta curve using the beta_draw() function
beta_draw(ab)
 
 
###
### EXERCISE
###
# Harry's shape parameters for his prior: ab
ab<-c(3,3)
 
# Vector of successes and failures: sf
sf<-c(16,4)
 
# Harry's shape parameters for his posterior: ab_new
ab_new <- ab + sf
 
# Graph Harry's posterior
beta_draw(ab_new)
 
# Vector of beta parameters for Harry: ab
ab <- c(19, 7)
 
# Compute probability that P is smaller than 0.70
pbeta(0.70, ab[1], ab[2])
 
# Show the area that is computed
beta_area(0, 0.70, ab)
 
 
 
 
 
# Compute the posterior probabilities
bayesian_crank(bayes_df)
