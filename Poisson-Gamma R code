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
           fill = 'red') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 0.1') + theme_classic()
scale_color_gradient(low="firebrick1", high="firebrick4")
p2 <- s2 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'red') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 0.5')  + theme_classic()
p3 <- s3 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'red') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 2') + theme_classic()
p4 <- s4 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'red') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 5') + theme_classic()
scale_color_gradient(low="firebrick1", high="firebrick4")
p5 <- s5 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'red') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 10')  + theme_classic()
p6 <- s6 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'red') +
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

# priors
alpha1 = 2; beta1 = 2
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

# plots
ylim = c(0, max(c(prior1, posterior1, likelihood1 / sum(likelihood1))))
xlim = range(lambda)
plot(lambda, prior1, type='l', xlim=xlim, ylim=ylim, col="red", xlab='lambda', ylab='', yaxt='n',
     main = 'Posterior distribution in blue - Ga(19, 22)')
par(new=T) 
plot(lambda, likelihood1/sum(likelihood1), type='l', xlim=xlim, ylim=ylim, col="black", xlab='', ylab='', yaxt='n')
par(new=T)
plot(lambda, posterior1, type='l', xlim=xlim, ylim=ylim, col="blue", xlab='', ylab='', yaxt='n')
legend("topright", c("Ga(2,2) prior", "Example of a scaled likelihood P(1)", "Ga(19,22) posterior"), lty=1, col=c("red", "black", "blue"))

# Posterior mean, posterior variance and 95% Credible Interval including the sample median
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

# 95% Credible Interval obtained by direct sampling (simulation)
set.seed(2023)
round(quantile(rgamma(n = 10^8, alpha_posterior, beta_posterior), probs = c(0.025, 0.5, 0.975)),4)
#   2.5%    50%  97.5% 
# 0.5200 0.8486 1.2931 

# Posterior mean obtained from direct sampling
set.seed(2023)
mean(rgamma(n = 10^8, alpha_posterior, beta_posterior))
# [1] 0.8928863

#----
# end
#----
