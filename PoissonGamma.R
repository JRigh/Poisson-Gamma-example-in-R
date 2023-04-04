#--------------------------------------------
# Poisson (likelihood) - Gamma (prior) models 
# in R
#--------------------------------------------

# 1. Histogram of Poisson distributed observations

library(tidyverse)
set.seed(2023)
s1 <- data.frame('data' = rpois(n = 10000, lambda = 1/10))
s2 <- data.frame('data' = rpois(n = 10000, lambda = 1/2))
s3 <- data.frame('data' = rpois(n = 10000, lambda = 2))
s4 <- data.frame('data' = rpois(n = 10000, lambda = 5))
s5 <- data.frame('data' = rpois(n = 10000, lambda = 10))
s6 <- data.frame('data' = rpois(n = 10000, lambda = 50))

p1 <- s1 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'darkred') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 0.1') + theme_classic()
p2 <- s2 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'darkred') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 0.5')  + theme_classic()
p3 <- s3 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'darkred') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 2') + theme_classic()
p4 <- s4 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'darkred') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 5') + theme_classic()
p5 <- s5 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'darkred') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 10')  + theme_classic()
p6 <- s6 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'darkred') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 50') + theme_classic()

library(gridExtra)

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)


# 2. Density plots of examples of Gamma family

par(mfrow = c(2,3))
p = seq(0, 20, length = 500)
plot(p, dgamma(p, 1/2, 1/2), col = 2, lwd = 2, main = 'Ga(1/2, 1/2)', type = 'l')
plot(p, dgamma(p, 1, 1), col = 2, lwd = 2, main = 'Ga(1, 1)', type = 'l')
plot(p, dgamma(p, 2, 2), col = 2, lwd = 2, main = 'Ga(2, 2)', type = 'l')
plot(p, dgamma(p, 80, 15), col = 2, lwd = 2, main = 'Ga(80, 15)', type = 'l')
plot(p, dgamma(p, 50, 5), col = 2, lwd = 2, main = 'Ga(50, 5)', type = 'l')
plot(p, dgamma(p, 10, 1), col = 2, lwd = 2, main = 'Ga(11.27, 1)', type = 'l')

# 3. Density plots of posteriors

# data
n = 20; lambda1 = 1 # Poisson likelihood parameters
lambda <- seq(0,10,length=1000)

# priors
alpha1 = 2; beta1 = 2
prior1 = lambda^(alpha1 - 1) * exp(- lambda * beta1) ; prior1 = prior1 / sum(prior1)

# likelihood
likelihood1 = dpois(lambda1, lambda)

# posterior
lp1 = likelihood1 * prior1 ; posterior1 = lp1 / sum(lp1)
set.seed(2023)
data1 = rpois(n = n, lambda = lambda1)

# save a copy of entire dataset, training and testing datasets in .csv
write.csv(data1, 
          "C:/Users/julia/OneDrive/Desktop/github/3. PoissonGamma/Poisson_data.csv",
          row.names = FALSE)

# posterior mean and parameters
meandata1 = mean(data1) # [1] 0.85
alpha_posterior = round(alpha1 + n*mean(data1), 2) # 19
beta_posterior = n + beta1 # 22

# plots
ylim = c(0, max(c(prior1, posterior1, likelihood1 / sum(likelihood1))))
xlim = range(lambda)
plot(lambda, prior1, type='l', xlim=xlim, ylim=ylim, col="darkred", xlab='lambda', ylab='', yaxt='n',
     main = 'Posterior distribution in blue - Ga(19, 22)')
par(new=T) 
plot(lambda, likelihood1/sum(likelihood1), type='l', xlim=xlim, ylim=ylim, col="black", xlab='', ylab='', yaxt='n')
par(new=T)
plot(lambda, posterior1, type='l', xlim=xlim, ylim=ylim, col="blue", xlab='', ylab='', yaxt='n')
legend("topright", c("Ga(2,2) prior", "Example of a scaled likelihood P(1)", "Ga(19,22) posterior"), lty=1, col=c("darkred", "black", "blue"))

# ggplot

# priors
alpha1 = 2; beta1 = 2
lambda <- seq(0,10,length=1000)
prior1 = lambda^(alpha1 - 1) * exp(- lambda * beta1) ; prior1 = prior1 / sum(prior1)

# data
n = 20; lambda1 = 1 # Poisson likelihood parameters

# likelihood
likelihood1 = dpois(lambda1, lambda)

# posterior
lp1 = likelihood1 * prior1 ; posterior1 = lp1 / sum(lp1)

set.seed(2023)
data1 = rpois(n = n, lambda = lambda1)
meandata1 = mean(data1) # [1] 0.85
alpha_posterior = round(alpha1 + n*mean(data1), 2) # 19
beta_posterior = n + beta1 # 22

# dataframe
data_frame <- data.frame('lambda' = lambda, 'prior' = prior1, 'likelihood' = likelihood1, 'posterior' = posterior1)

ggplot(data=data_frame) +
  geom_line(aes(x = lambda, y = prior1, color = 'Ga(2,2) prior'), lwd = 1.2) +
  geom_line(aes(x = lambda, y = likelihood1/sum(likelihood1), 
                color = 'Scaled likelihood P(1)'), lwd = 1.2) +
  geom_line(aes(x = lambda, y = posterior1, color = 'Ga(19,22) posterior'), lwd = 1.2) +
  xlim(0, 10) + ylim(0, max(c(prior1, posterior1, likelihood1 / sum(likelihood1)))) +
  scale_color_manual(name = "Distributions", values = c("Ga(2,2) prior" = "darkdarkred", 
                                                        "Scaled likelihood P(1)" = "black",
                                                        'Ga(19,22) posterior' = 'darkblue')) +
  labs(title = 'Posterior distribution in blue - Ga(19, 22)',
       subtitle = 'Working example data',
       y="density", x="lambda") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkdarkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# Posterior mean, posterior variance and 95% Cdarkredible Interval including the sample median
set.seed(2023)
data1 = rpois(n = n, lambda = lambda1)
alpha_posterior = round(alpha1 + n*mean(data1), 2) # 19
beta_posterior = n + beta1 # 22

pmean = alpha_posterior / beta_posterior
pmean
# [1] 0.8636364

pvariance = alpha_posterior / beta_posterior^2
pvariance
# [1] 0.0392562

# 95% Cdarkredible Interval obtained by direct sampling (simulation)
set.seed(2023)
round(quantile(rgamma(n = 10^8, alpha_posterior, beta_posterior), probs = c(0.025, 0.5, 0.975)),4)
#   2.5%    50%  97.5% 
# 0.5200 0.8486 1.2931 

# Posterior mean obtained from direct sampling
set.seed(2023)
mean(rgamma(n = 10^8, alpha_posterior, beta_posterior))
# [1] 0.8928863

# Jeffrey's prior
lambda <- seq(0,10,length=1000)
density <- dgamma(lambda, shape = 1/2, scale = 0)

# dataframe <- data.frame()
data_frame <- data.frame(density, lambda)

# plot of the Jeffreys prior Gamma(1/2, 0)
ggplot(data=data_frame,aes(x=lambda))+
  stat_function(fun=dgamma, args=list(shape=1/2, scale=0.01), 
                colour = "darkdarkred", linetype = "solid", lwd = 1.2) +
  stat_function(fun=dgamma, linetype="dashed", args=list(shape=1/2, scale=1/2),
                colour = "black", linetype = "dashed", lwd = 1.2) +
  xlim(0, 1) + ylim(0, 5) +
  labs(title = 'Jeffreys Gamma prior',
       subtitle = 'in darkred Gamma(1/2, 0.01) because Gamma(1/2, 0) is impossible to plot. Gamma(1/2, 1/2) prior in dashed black',
       y="density", x="lambda") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkdarkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# Posterior mean, posterior variance and 95% Cdarkredible Interval including the sample median, with prior Ga(1/2, 1/2)
set.seed(2023)
data1 = rpois(n = n, lambda = lambda1)
alpha_posterior = round(alpha1 + n*mean(data1), 2) # 17.5
beta_posterior = n + beta1 # 20.5

pmean = alpha_posterior / beta_posterior
pmean
# [1] 0.8536585

pvariance = alpha_posterior / beta_posterior^2
pvariance
# [1] 0.04164188

# 95% Cdarkredible Interval obtained by direct sampling (simulation)
set.seed(2023)
round(quantile(rgamma(n = 10^8, alpha_posterior, beta_posterior), probs = c(0.025, 0.5, 0.975)),4)
#   2.5%    50%  97.5% 
# 0.5200 0.8486 1.2931 

# Posterior mean obtained from direct sampling
set.seed(2023)
mean(rgamma(n = 10^8, alpha_posterior, beta_posterior))
# [1] 0.8928863

#-----------------------------------------
# Multi-stage Poisson-Gamma Gibbs sampling
# in R
# Robert & Casella, 10.17
#-----------------------------------------

library(tidyverse) 
library(xtable)

setwd("path")

# write the data and export in LaTeX table 
dataset = data.frame('Pump' = 1:10,
                     'Failures' = c(5,1,5,14,3,19,1,1,4,22),
                     'Time' = c(94.32, 15.72, 62.88, 125.76, 5.24, 31.44, 1.05, 1.05, 2.10, 10.48))
print(xtable(dataset, type = "latex"), file = "PGMSGS_tables.tex")

# 1 Gibbs sampler function and draws from the posterior
Gibbs_sampler_PG <- function(nsim, beta, alpha, gamma, delta, y, t, burnin) {
  
  X = matrix(0, nrow = nsim, ncol = length(y)+1) # empty matrix to record the simulated values
  X[1,1] = beta  # beta prior parameter
  X[1,c(2:(length(y)+1))] = rgamma(length(y), y + alpha, t + X[1,1]) # initial lambda
  
  for(i in 2:nsim) {
    
    X[i,c(2:(length(y)+1))] = rgamma(length(y), y + alpha, t + X[i-1,1]) # update lambda
    X[i,1] = rgamma(1, length(y) * alpha + gamma, delta + sum(X[i-1,c(2:(length(y)+1))])) # update beta
  }
  
  b <- burnin + 1 # record the burn in period (observations to be discarded)
  x <- X[b:nsim, ] 
  
  return(list('lambda' = as.numeric(x[,c(2:(length(y)+1))]), 'beta' = x[,1] ))
}

# posterior
set.seed(2023)
posterior <- Gibbs_sampler_PG(nsim = 10000, beta = 1, alpha = 1.8, gamma = 0.01,
                              delta = 1, y = dataset[,2], t = dataset[,3], burnin = 1000)

# 2. posterior quantities
mean_lambda = mean(posterior$lambda)
mean_beta = mean(posterior$beta)
sd_lambda = sd(posterior$lambda)
sd_beta = sd(posterior$beta)
df1 <- matrix(c(mean_lambda, mean_beta, sd_lambda, sd_beta), byrow = TRUE, ncol =2)
colnames(df1) = c('lambda', 'beta')
rownames(df1) = c('post mean', 'post sd')

quantiles_lambda = quantile(posterior$lambda, probs = c(0.025, 0.5, 0.975))
quantiles_beta = quantile(posterior$beta, probs = c(0.025, 0.5, 0.975))
df2 <- matrix(c(quantiles_lambda, quantiles_beta), byrow = FALSE, ncol =2)
colnames(df2) = c('lambda', 'beta')
rownames(df2) = c('2.5%', '50%', '97.5%')

post <- rbind(df1, df2)
print(xtable(post, type = "latex", digits=4), file = "tables.tex")

# 3. plot
p1 <- ggplot(data.frame('x' = matrix(posterior$lambda, ncol = 1)), aes(x = x)) +
  geom_histogram( color = 'black', fill = 'darkred', binwidth = 0.1, aes(y = ..density..)) +
  geom_density(color = 'black', lwd = 1.4) +
  labs(title = 'Histogram of posterior distribution of lambda parameter',
       y="count", x="lambda") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

p2 <- ggplot(data.frame('x' = matrix(posterior$beta, ncol = 1)), aes(x = x)) +
  geom_histogram( color = 'black', fill = 'darkred', binwidth = 0.1, aes(y = ..density..)) +
  geom_density(color = 'black', lwd = 1.4) +
  labs(title = 'Histogram of posterior distribution of beta hyperparameter',
       y="count", x="beta") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

grid.arrange(p1, p2, nrow = 1)

#----
# end
#----


