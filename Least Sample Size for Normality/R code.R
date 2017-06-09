###     Choose the distribution

#####################################################################

## Exponential(rate=m)

m=1                    # Set the parameters
alpha= 0.05                     # Set the level of significance

# Median

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  med=vector()
  for(i in 1:r)
    med[i]=median(rexp(n,m))
  pv=as.numeric(shapiro.test(med)[2])
  hist((med-mean(med))/sd(med),freq=F,main=paste("median,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Quartile Deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  qd=vector()
  for(i in 1:r)
  {	
    x=rexp(n,m)
    qd[i]=(quantile(x,0.75)-quantile(x,0.25))/2
  }
  pv=as.numeric(shapiro.test(qd)[2])
  hist((qd-mean(qd))/sd(qd),freq=F,main=paste("QD,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Coefficient of quarile deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  cqd=vector()
  for(i in 1:r)
  {	
    x=rexp(n,m)
    cqd[i]=as.numeric(((quantile(x,0.75)-quantile(x,0.25))/2)/median(x))
  }
  pv=as.numeric(shapiro.test(cqd)[2])
  hist((cqd-mean(cqd))/sd(cqd),freq=F,main=paste("Coeff. of QD,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Bowley's measure of skewness

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  sk=vector()
  for(i in 1:r)
  {	
    x=rexp(n,m)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    sk[i]=as.numeric((quantile(x,0.75)+quantile(x,0.25)-2*median(x))/(2*qd))
  }
  pv=as.numeric(shapiro.test(sk)[2])
  hist((sk-mean(sk))/sd(sk),freq=F,main=paste("SK,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Percentile measure of kurtosis

r=1000
n=2
pv=0
while(pv<alpha)
{
  kp=vector()
  for(i in 1:r)
  {
    x=rexp(n,m)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    kp[i]=as.numeric(qd/(quantile(x,0.90)-quantile(x,0.10)))
  }
  pv=as.numeric(shapiro.test(kp)[2])
  hist((kp-mean(kp))/sd(kp),freq=F,main=paste("KP,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1


##############################################################################

## Lognormal(0,1)

alpha= 0.05                     # Set the level of significance

# Median

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  med=vector()
  for(i in 1:r)
    med[i]=median(rlnorm(n))
  pv=as.numeric(shapiro.test(med)[2])
  hist((med-mean(med))/sd(med),freq=F,main=paste("med,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Quartile Deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  qd=vector()
  for(i in 1:r)
  {	
    x=rlnorm(n)
    qd[i]=(quantile(x,0.75)-quantile(x,0.25))/2
  }
  pv=as.numeric(shapiro.test(qd)[2])
  hist((qd-mean(qd))/sd(qd),freq=F,main=paste("qd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Coefficient of quarile deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  cqd=vector()
  for(i in 1:r)
  {	
    x=rlnorm(n)
    cqd[i]=as.numeric(((quantile(x,0.75)-quantile(x,0.25))/2)/median(x))
  }
  pv=as.numeric(shapiro.test(cqd)[2])
  hist((cqd-mean(cqd))/sd(cqd),freq=F,main=paste("cqd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Bowley's measure of skewness

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  sk=vector()
  for(i in 1:r)
  {	
    x=rlnorm(n)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    sk[i]=as.numeric((quantile(x,0.75)+quantile(x,0.25)-2*median(x))/(2*qd))
  }
  pv=as.numeric(shapiro.test(sk)[2])
  hist((sk-mean(sk))/sd(sk),freq=F,main=paste("sk,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Percentile measure of kurtosis

r=1000
n=2
pv=0
while(pv<alpha)
{
  kp=vector()
  for(i in 1:r)
  {
    x=rlnorm(n)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    kp[i]=as.numeric(qd/(quantile(x,0.90)-quantile(x,0.10)))
  }
  pv=as.numeric(shapiro.test(kp)[2])
  hist((kp-mean(kp))/sd(kp),freq=F,main=paste("kp,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1


##############################################################################

## Cauchy distribution

alpha= 0.05                     # Set the level of significance

# Median

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  med=vector()
  for(i in 1:r)
    med[i]=median(rcauchy(n))
  pv=as.numeric(shapiro.test(med)[2])
  hist((med-mean(med))/sd(med),freq=F,main=paste("med,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Quartile Deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  qd=vector()
  for(i in 1:r)
  {	
    x=rcauchy(n)
    qd[i]=(quantile(x,0.75)-quantile(x,0.25))/2
  }
  pv=as.numeric(shapiro.test(qd)[2])
  hist((qd-mean(qd))/sd(qd),freq=F,main=paste("qd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Coefficient of quarile deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  cqd=vector()
  for(i in 1:r)
  {	
    x=rcauchy(n)
    cqd[i]=IQR(x)/(2*median(x))
  }
  pv=as.numeric(shapiro.test(cqd)[2])
  hist((cqd-mean(cqd))/sd(cqd),freq=F,main=paste("cqd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Bowley's measure of skewness

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  sk=vector()
  for(i in 1:r)
  {	
    x=rcauchy(n)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    sk[i]=as.numeric((quantile(x,0.75)+quantile(x,0.25)-2*median(x))/(2*qd))
  }
  pv=as.numeric(shapiro.test(sk)[2])
  hist((sk-mean(sk))/sd(sk),freq=F,main=paste("sk,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Percentile measure of kurtosis

r=1000
n=2
pv=0
while(pv<alpha)
{
  kp=vector()
  for(i in 1:r)
  {
    x=rcauchy(n)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    kp[i]=as.numeric(qd/(quantile(x,0.90)-quantile(x,0.10)))
  }
  pv=as.numeric(shapiro.test(kp)[2])
  hist((kp-mean(kp))/sd(kp),freq=F,main=paste("kp,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

##############################################################################

## Normal distribution

alpha= 0.05                     # Set the level of significance

# Median

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  med=vector()
  for(i in 1:r)
    med[i]=median(rnorm(n))
  pv=as.numeric(shapiro.test(med)[2])
  hist((med-mean(med))/sd(med),freq=F,main=paste("med,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Quartile Deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  qd=vector()
  for(i in 1:r)
  {	
    x=rnorm(n)
    qd[i]=(quantile(x,0.75)-quantile(x,0.25))/2
  }
  pv=as.numeric(shapiro.test(qd)[2])
  hist((qd-mean(qd))/sd(qd),freq=F,main=paste("qd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Coefficient of quarile deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  cqd=vector()
  for(i in 1:r)
  {	
    x=rnorm(n)
    cqd[i]=IQR(x)/(2*median(x))
  }
  pv=as.numeric(shapiro.test(cqd)[2])
  hist((cqd-mean(cqd))/sd(cqd),freq=F,main=paste("cqd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Bowley's measure of skewness

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  sk=vector()
  for(i in 1:r)
  {	
    x=rnorm(n)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    sk[i]=as.numeric((quantile(x,0.75)+quantile(x,0.25)-2*median(x))/(2*qd))
  }
  pv=as.numeric(shapiro.test(sk)[2])
  hist((sk-mean(sk))/sd(sk),freq=F,main=paste("sk,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Percentile measure of kurtosis

r=1000
n=2
pv=0
while(pv<alpha)
{
  kp=vector()
  for(i in 1:r)
  {
    x=rnorm(n)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    kp[i]=as.numeric(qd/(quantile(x,0.90)-quantile(x,0.10)))
  }
  pv=as.numeric(shapiro.test(kp)[2])
  hist((kp-mean(kp))/sd(kp),freq=F,main=paste("kp,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

##############################################################################

## Beta distribution

alpha= 0.05                     # Set the level of significance

# Median

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  med=vector()
  for(i in 1:r)
    med[i]=median(rbeta(n,2,5))
  pv=as.numeric(shapiro.test(med)[2])
  hist((med-mean(med))/sd(med),freq=F,main=paste("med,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Quartile Deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  qd=vector()
  for(i in 1:r)
  {	
    x=rbeta(n,2,5)
    qd[i]=(quantile(x,0.75)-quantile(x,0.25))/2
  }
  pv=as.numeric(shapiro.test(qd)[2])
  hist((qd-mean(qd))/sd(qd),freq=F,main=paste("qd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Coefficient of quarile deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  cqd=vector()
  for(i in 1:r)
  {	
    x=rbeta(n,2,5)
    cqd[i]=IQR(x)/(2*median(x))
  }
  pv=as.numeric(shapiro.test(cqd)[2])
  hist((cqd-mean(cqd))/sd(cqd),freq=F,main=paste("cqd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Bowley's measure of skewness

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  sk=vector()
  for(i in 1:r)
  {	
    x=rbeta(n,2,5)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    sk[i]=as.numeric((quantile(x,0.75)+quantile(x,0.25)-2*median(x))/(2*qd))
  }
  pv=as.numeric(shapiro.test(sk)[2])
  hist((sk-mean(sk))/sd(sk),freq=F,main=paste("sk,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Percentile measure of kurtosis

r=1000
n=2
pv=0
while(pv<alpha)
{
  kp=vector()
  for(i in 1:r)
  {
    x=rbeta(n,2,5)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    kp[i]=as.numeric(qd/(quantile(x,0.90)-quantile(x,0.10)))
  }
  pv=as.numeric(shapiro.test(kp)[2])
  hist((kp-mean(kp))/sd(kp),freq=F,main=paste("kp,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

##############################################################################

## Beta distribution

alpha= 0.05                     # Set the level of significance

# Median

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  med=vector()
  for(i in 1:r)
    med[i]=median(rbeta(n,0.5,0.5))
  pv=as.numeric(shapiro.test(med)[2])
  hist((med-mean(med))/sd(med),freq=F,main=paste("med,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Quartile Deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  qd=vector()
  for(i in 1:r)
  {	
    x=rbeta(n,0.5,0.5)
    qd[i]=(quantile(x,0.75)-quantile(x,0.25))/2
  }
  pv=as.numeric(shapiro.test(qd)[2])
  hist((qd-mean(qd))/sd(qd),freq=F,main=paste("qd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Coefficient of quarile deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  cqd=vector()
  for(i in 1:r)
  {	
    x=rbeta(n,0.5,0.5)
    cqd[i]=IQR(x)/(2*median(x))
  }
  pv=as.numeric(shapiro.test(cqd)[2])
  hist((cqd-mean(cqd))/sd(cqd),freq=F,main=paste("cqd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Bowley's measure of skewness

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  sk=vector()
  for(i in 1:r)
  {	
    x=rbeta(n,0.5,0.5)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    sk[i]=as.numeric((quantile(x,0.75)+quantile(x,0.25)-2*median(x))/(2*qd))
  }
  pv=as.numeric(shapiro.test(sk)[2])
  hist((sk-mean(sk))/sd(sk),freq=F,main=paste("sk,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Percentile measure of kurtosis

r=1000
n=2
pv=0
while(pv<alpha)
{
  kp=vector()
  for(i in 1:r)
  {
    x=rbeta(n,0.5,0.5)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    kp[i]=as.numeric(qd/(quantile(x,0.90)-quantile(x,0.10)))
  }
  pv=as.numeric(shapiro.test(kp)[2])
  hist((kp-mean(kp))/sd(kp),freq=F,main=paste("kp,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

##############################################################################

## Beta distribution

alpha= 0.05                     # Set the level of significance

# Median

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  med=vector()
  for(i in 1:r)
    med[i]=median(rbeta(n,2,2))
  pv=as.numeric(shapiro.test(med)[2])
  hist((med-mean(med))/sd(med),freq=F,main=paste("med,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Quartile Deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  qd=vector()
  for(i in 1:r)
  {	
    x=rbeta(n,2,2)
    qd[i]=(quantile(x,0.75)-quantile(x,0.25))/2
  }
  pv=as.numeric(shapiro.test(qd)[2])
  hist((qd-mean(qd))/sd(qd),freq=F,main=paste("qd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Coefficient of quarile deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  cqd=vector()
  for(i in 1:r)
  {	
    x=rbeta(n,2,2)
    cqd[i]=IQR(x)/(2*median(x))
  }
  pv=as.numeric(shapiro.test(cqd)[2])
  hist((cqd-mean(cqd))/sd(cqd),freq=F,main=paste("cqd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Bowley's measure of skewness

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  sk=vector()
  for(i in 1:r)
  {	
    x=rbeta(n,2,2)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    sk[i]=as.numeric((quantile(x,0.75)+quantile(x,0.25)-2*median(x))/(2*qd))
  }
  pv=as.numeric(shapiro.test(sk)[2])
  hist((sk-mean(sk))/sd(sk),freq=F,main=paste("sk,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Percentile measure of kurtosis

r=1000
n=2
pv=0
while(pv<alpha)
{
  kp=vector()
  for(i in 1:r)
  {
    x=rbeta(n,2,2)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    kp[i]=as.numeric(qd/(quantile(x,0.90)-quantile(x,0.10)))
  }
  pv=as.numeric(shapiro.test(kp)[2])
  hist((kp-mean(kp))/sd(kp),freq=F,main=paste("kp,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1


##############################################################################

## Uniform distribution

alpha= 0.05                     # Set the level of significance

# Median

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  med=vector()
  for(i in 1:r)
    med[i]=median(runif(n))
  pv=as.numeric(shapiro.test(med)[2])
  hist((med-mean(med))/sd(med),freq=F,main=paste("med,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Quartile Deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  qd=vector()
  for(i in 1:r)
  {	
    x=runif(n)
    qd[i]=(quantile(x,0.75)-quantile(x,0.25))/2
  }
  pv=as.numeric(shapiro.test(qd)[2])
  hist((qd-mean(qd))/sd(qd),freq=F,main=paste("qd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Coefficient of quarile deviation

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  cqd=vector()
  for(i in 1:r)
  {	
    x=runif(n)
    cqd[i]=IQR(x)/(2*median(x))
  }
  pv=as.numeric(shapiro.test(cqd)[2])
  hist((cqd-mean(cqd))/sd(cqd),freq=F,main=paste("cqd,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Bowley's measure of skewness

r= 1000                         # Set number of samples to be taken
n=2
pv=0
while(pv<alpha)
{
  sk=vector()
  for(i in 1:r)
  {	
    x=runif(n)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    sk[i]=as.numeric((quantile(x,0.75)+quantile(x,0.25)-2*median(x))/(2*qd))
  }
  pv=as.numeric(shapiro.test(sk)[2])
  hist((sk-mean(sk))/sd(sk),freq=F,main=paste("sk,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1

# Percentile measure of kurtosis

r=1000
n=2
pv=0
while(pv<alpha)
{
  kp=vector()
  for(i in 1:r)
  {
    x=runif(n)
    qd=as.numeric((quantile(x,0.75)-quantile(x,0.25))/2)
    kp[i]=as.numeric(qd/(quantile(x,0.90)-quantile(x,0.10)))
  }
  pv=as.numeric(shapiro.test(kp)[2])
  hist((kp-mean(kp))/sd(kp),freq=F,main=paste("kp,n=",n))
  curve(dnorm,add=T)
  n=n+1
}
n-1