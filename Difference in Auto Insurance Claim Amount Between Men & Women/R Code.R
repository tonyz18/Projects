# First, read in and explore the data:
raw_data <- read.csv("C:\\Users\\tzhan\\Google Drive\\GSU Graduate School\\STAT 8670 Computational Methods in Statistics\\Project\\Auto_Insurance_Claims_Sample.csv",header=T)
data <- data.frame(raw_data$Claim.Amount,raw_data$Gender,raw_data$Coverage)
names(data) <- c("Claim Amount","Gender","Coverage")
plot(data$Coverage,data$`Claim Amount`)
Basic_Coverage <- data[data$Coverage=='Basic',]
Extended_Coverage <- data[data$Coverage=='Extended',]
Premium_Coverage <- data[data$Coverage=='Premium',]
table(data$Gender)
table(Basic_Coverage$Gender)
table(Extended_Coverage$Gender)
table(Premium_Coverage$Gender)

Basic_Coverage_M <- subset(Basic_Coverage, Gender=='M')
Basic_Coverage_F <- subset(Basic_Coverage, Gender=='F')
Extended_Coverage_M <- subset(Extended_Coverage, Gender=='M')
Extended_Coverage_F <- subset(Extended_Coverage, Gender=='F')
Premium_Coverage_M <- subset(Premium_Coverage, Gender=='M')
Premium_Coverage_F <- subset(Premium_Coverage, Gender=='F')

# Basic Coverage:
M_sample_Basic <- sample(nrow(Basic_Coverage_M), 100)
F_sample_Basic <- sample(nrow(Basic_Coverage_F), 100)
Amt_Male_Basic <- Basic_Coverage_M[c(M_sample_Basic),1]/100
Amt_Female_Basic <- Basic_Coverage_F[c(F_sample_Basic),1]/100

mean(Amt_Male_Basic) # 6.435025
mean(Amt_Female_Basic) # 6.322932

# draw a picture
xlim = c(min(Basic_Coverage[,1]/100),max(Basic_Coverage[,1]/100))
hist(Amt_Male_Basic,100,col="red",xlim=xlim,main='Mean Claim Amount for Men Under Basic Coverage',xlab='Claim Amount/100')
hist(Amt_Female_Basic,100,col="red",xlim=xlim,main='Mean Claim Amount for Women Under Basic Coverage',xlab='Claim Amount/100')

# likelihood function
lik = function(th){
  mu1 <- th[1]; sig1 <- th[2]; mu2 <- th[3]; sig2 <- th[4]
  prod(dnorm(Amt_Male_Basic,mean=mu1,sd=sig1))*prod(dnorm(Amt_Female_Basic,mean=mu2,sd=sig2))
}

# prior function
prior <-  function(th){
  mu1 <- th[1]; sig1 <- th[2]; mu2 <- th[3]; sig2 <- th[4]
  if(sig1 <= 0 | sig2 <= 0) return(0)
  dnorm(mu1,6.435025,6.435025)*dnorm(mu2,6.322932,6.322932)*dexp(sig1,rate=1/6.435025)*dexp(sig2,rate=1/6.322932)
}

# posterior function
post <-  function(th){prior(th) * lik(th)}

# Starting values
mu1 <-  6.435025; sig1 <-  6.435025; mu2 <-  6.322932; sig2 <-  6.322932
th0 <- c(mu1,sig1,mu2,sig2)
# Here is what does the MCMC (Metropolis method):
nit <- 10000
results <-  matrix(0, nrow=nit, ncol=4)
th <-  th0
results[1,] <-  th0
for(it in 2:nit){
  cand <-  th + rnorm(4,sd=.5)
  ratio <-  post(cand)/post(th)
  if(runif(1)<ratio) th <- cand
  results[it,] <-  th
}
# Take a peek at what we got
edit(results)

plot(results[,1], main=expression(mu[1]))
plot(results[,2], main=expression(sigma[1]))
plot(results[,3], main=expression(mu[2]))
plot(results[,4], main=expression(sigma[2]))

mu1s <-  results[,1]
sig1s <-  results[,2]
mu2s <- results[,3]
sig2s <- results[,4]
hist(mu1s-mu2s, xlab=expression(mu[1]-mu[2]), main=expression(mu[1]-mu[2]), col = 'gray')
mean(mu1s-mu2s > 0) # 0.5456

# Extended Coverage:
M_sample_Ext <- sample(nrow(Extended_Coverage_M), 100)
F_sample_Ext <- sample(nrow(Extended_Coverage_F), 100)
Amt_Male_Ext <- Extended_Coverage_M[c(M_sample_Ext),1]/100
Amt_Female_Ext <- Extended_Coverage_F[c(F_sample_Ext),1]/100

mean(Amt_Male_Ext) # 7.226277
mean(Amt_Female_Ext) # 7.822649

# draw a picture
xlim = c(min(Extended_Coverage[,1]/100),max(Extended_Coverage[,1]/100))
hist(Amt_Male_Ext,100,col="red",xlim=xlim,main='Mean Claim Amount for Men Under Extended Coverage',xlab='Claim Amount/100')
hist(Amt_Female_Ext,100,col="red",xlim=xlim,main='Mean Claim Amount for Women Under Extended Coverage',xlab='Claim Amount/100')

# likelihood function
lik = function(th){
  mu1 <- th[1]; sig1 <- th[2]; mu2 <- th[3]; sig2 <- th[4]
  prod(dnorm(Amt_Male_Ext,mean=mu1,sd=sig1))*prod(dnorm(Amt_Female_Ext,mean=mu2,sd=sig2))
}

# prior function
prior <-  function(th){
  mu1 <- th[1]; sig1 <- th[2]; mu2 <- th[3]; sig2 <- th[4]
  if(sig1 <= 0 | sig2 <= 0) return(0)
  dnorm(mu1,7.226277,7.226277)*dnorm(mu2,7.822649,7.822649)*dexp(sig1,rate=1/7.226277)*dexp(sig2,rate=1/7.822649)
}

# posterior function
post <-  function(th){prior(th) * lik(th)}

# Starting values
mu1 <-  7.226277; sig1 <-  7.226277; mu2 <-  7.822649; sig2 <-  7.822649
th0 <- c(mu1,sig1,mu2,sig2)
# Here is what does the MCMC (Metropolis method):
nit <- 10000
results <-  matrix(0, nrow=nit, ncol=4)
th <-  th0
results[1,] <-  th0
for(it in 2:nit){
  cand <-  th + rnorm(4,sd=.5)
  ratio <-  post(cand)/post(th)
  if(runif(1)<ratio) th <- cand
  results[it,] <-  th
}
# Take a peek at what we got
edit(results)

plot(results[,1], main=expression(mu[1]))
plot(results[,2], main=expression(sigma[1]))
plot(results[,3], main=expression(mu[2]))
plot(results[,4], main=expression(sigma[2]))

mu1s <-  results[,1]
sig1s <-  results[,2]
mu2s <- results[,3]
sig2s <- results[,4]
hist(mu1s-mu2s, xlab=expression(mu[1]-mu[2]), main=expression(mu[1]-mu[2]), col = 'gray')
mean(mu1s-mu2s > 0) # 0.2242

# Premium Coverage:
M_sample_Prem <- sample(nrow(Premium_Coverage_M), 100)
F_sample_Prem <- sample(nrow(Premium_Coverage_F), 100)
Amt_Male_Prem <- Premium_Coverage_M[c(M_sample_Prem),1]/100
Amt_Female_Prem <- Premium_Coverage_F[c(F_sample_Prem),1]/100

mean(Amt_Male_Prem) # 9.922662
mean(Amt_Female_Prem) # 10.70696

# draw a picture
xlim = c(min(Premium_Coverage[,1]/100),max(Premium_Coverage[,1]/100))
hist(Amt_Male_Prem,100,col="red",xlim=xlim,main='Mean Claim Amount for Men Under Premium Coverage',xlab='Claim Amount/100')
hist(Amt_Female_Prem,100,col="red",xlim=xlim,main='Mean Claim Amount for Women Under Premium Coverage',xlab='Claim Amount/100')

# likelihood function
lik = function(th){
  mu1 <- th[1]; sig1 <- th[2]; mu2 <- th[3]; sig2 <- th[4]
  prod(dnorm(Amt_Male_Prem,mean=mu1,sd=sig1))*prod(dnorm(Amt_Female_Prem,mean=mu2,sd=sig2))
}

# prior function
prior <-  function(th){
  mu1 <- th[1]; sig1 <- th[2]; mu2 <- th[3]; sig2 <- th[4]
  if(sig1 <= 0 | sig2 <= 0) return(0)
  dnorm(mu1,9.922662,9.922662)*dnorm(mu2,10.70696,10.70696)*dexp(sig1,rate=1/9.922662)*dexp(sig2,rate=1/10.70696)
}

# posterior function
post <-  function(th){prior(th) * lik(th)}

# Starting values
mu1 <-  9.922662; sig1 <-  9.922662; mu2 <-  10.70696; sig2 <-  10.70696
th0 <- c(mu1,sig1,mu2,sig2)
# Here is what does the MCMC (Metropolis method):
nit <- 10000
results <-  matrix(0, nrow=nit, ncol=4)
th <-  th0
results[1,] <-  th0
for(it in 2:nit){
  cand <-  th + rnorm(4,sd=.5)
  ratio <-  post(cand)/post(th)
  if(runif(1)<ratio) th <- cand
  results[it,] <-  th
}
# Take a peek at what we got
edit(results)

plot(results[,1], main=expression(mu[1]))
plot(results[,2], main=expression(sigma[1]))
plot(results[,3], main=expression(mu[2]))
plot(results[,4], main=expression(sigma[2]))

mu1s <-  results[,1]
sig1s <-  results[,2]
mu2s <- results[,3]
sig2s <- results[,4]
hist(mu1s-mu2s, xlab=expression(mu[1]-mu[2]), main=expression(mu[1]-mu[2]), col = 'gray')
mean(mu1s-mu2s > 0) # 0.2572
