###
### BAYESIAN THINKING
### IN R
###
 
setwd('/Users/andreacosta/Desktop/Bayes_R/') 

#install.packages("R.utils")
#library(R.utils)
sourceDirectory("/Users/andreacosta/Desktop/Bayes_R/library")


#starting
areas<-c(2,1,2,1,2)
spinner_plot(areas)
 
(df<-data.frame(Region=1:5,areas, Probability=areas/sum(areas)))
 
#sanity check
sum(df$Probability)
 
library(dplyr)
filter(df,Region %in% c(1,3,5)) #proba of odd
 
filter(df,Region > 3)
 
 
#simulate 10000 spins
many_spins <- spinner_data(areas,10000)
 
bar_plot(many_spins)
 
#frequency table
(S<-summarize(group_by(data.frame(Region=many_spins), Region), N=n()))
 
#probability of ones
(Freq_1<-sum(S$N[S$Region==1]))/10000
 
 
###
### BAYES' RULE
###
 
(bayes_df<-data.frame(Model=paste("Spinner",c("A","B","C","D"))))
 
#define unifrom prior
bayes_df$Prior<-rep(0.25,4)
bayes_df
 
#add likelyhoods to table
bayes_df$Likelihood<-round(c(1/3,1/2,1/4,1/6),2)
bayes_df
 
bayesian_crank(bayes_df)
 
 
###
### SEQUENTIAL BAYES
###
 
#old osterior is my new prior
bayes_df$Likelihood<-c(1/3,1/4,1/2,2/3)
 
bayesian_crank(bayes_df)
 
 
###
### BAYES WITH DISCRETE MODELS
###
 
bayes_df<-data.frame(P=seq(0.3,0.8,by=0.1),Weight=c(1,1,2,2,1,1),
Prior=c(1,1,2,2,1,1)/8)
 
bayes_df
 
#binomal experiment
bayes_df$Likelihood<-dbinom(12,size=20,prob=bayes_df$P)
 
bayes_df <- bayes_df[,c(1,3,4)]
round(bayes_df,3)
 
bayes_df<-bayesian_crank(bayes_df)
round(bayes_df,3)
 
prior_post_plot(bayes_df)
print(bayes_df)
 
#statistical inference
round(bayes_df[,c("P","Posterior")],3)
#so prob(p>0.5)=0.639
 
 
###
### CONTINOUS PRIOR
###
 
#beta curve
beta_area(0.4,0.8,c(7,10))
beta_quantile(0.5,c(7,10))
 
#find a and b that matches quantiles
p50<-list(x=0.55,p=0.50) #median
p90<-list(x=0.80,p=0.90)
beta.select(p50,p90)
beta_draw(c(4.91,3.38))
 
beta_interval(0.5,c(4.91,3.38))
 
beta_interval(0,0.4,c(4.91,3.38))
 
 
###
### BETA PRIOR TO BETA POSTERIOR
###
#calc shape aprameters for beta posterior:
prior_par<-c(4.91,3.38)
data<-c(12,8)
post_par<-prior_par+data
post_par
 
#overlay prior ad posterior curve:
beta_prior_post(prior_par,post_par)
