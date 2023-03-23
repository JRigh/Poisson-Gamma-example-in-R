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
plot(lambda, prior1, type='l', xlim=xlim, ylim=ylim, col="red", xlab='lambda', ylab='', yaxt='n',
     main = 'Posterior distribution in blue - Ga(19, 22)')
par(new=T) 
plot(lambda, likelihood1/sum(likelihood1), type='l', xlim=xlim, ylim=ylim, col="black", xlab='', ylab='', yaxt='n')
par(new=T)
plot(lambda, posterior1, type='l', xlim=xlim, ylim=ylim, col="blue", xlab='', ylab='', yaxt='n')
legend("topright", c("Ga(2,2) prior", "Example of a scaled likelihood P(1)", "Ga(19,22) posterior"), lty=1, col=c("red", "black", "blue"))

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
  scale_color_manual(name = "Distributions", values = c("Ga(2,2) prior" = "darkred", 
                                                        "Scaled likelihood P(1)" = "black",
                                                        'Ga(19,22) posterior' = 'darkblue')) +
  labs(title = 'Posterior distribution in blue - Ga(19, 22)',
       subtitle = 'Working example data',
       y="density", x="lambda") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

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

# Jeffrey's prior
lambda <- seq(0,10,length=1000)
density <- dgamma(lambda, shape = 1/2, scale = 0)

# dataframe <- data.frame()
data_frame <- data.frame(density, lambda)

# plot of the Jeffreys prior Gamma(1/2, 0)
ggplot(data=data_frame,aes(x=lambda))+
  stat_function(fun=dgamma, args=list(shape=1/2, scale=0.01), 
                colour = "darkred", linetype = "solid", lwd = 1.2) +
  stat_function(fun=dgamma, linetype="dashed", args=list(shape=1/2, scale=1/2),
                colour = "black", linetype = "dashed", lwd = 1.2) +
  xlim(0, 1) + ylim(0, 5) +
  labs(title = 'Jeffreys Gamma prior',
       subtitle = 'in red Gamma(1/2, 0.01) because Gamma(1/2, 0) is impossible to plot. Gamma(1/2, 1/2) prior in dashed black',
       y="density", x="lambda") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# Posterior mean, posterior variance and 95% Credible Interval including the sample median, with prior Ga(1/2, 1/2)
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