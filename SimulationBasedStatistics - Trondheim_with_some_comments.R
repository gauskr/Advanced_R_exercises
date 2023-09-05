
library(dplyr) # library for basic data management
library(ggplot2) # graphic libraries
library(ggthemes)
library(effsize) # to compute effect sizes
install.packages("pwr")
library(faux) # for multivariate random generation
library(pwr) # for analytical statistical power computations
library(metafor) # for meta-analysis

options(digits=3,scipen=999)
1:6
c(4:6,4:2)

a <- cor(1:6, c(4:6, 4:2))
plot(1:6, c(4:6,4:2))

?geom_line

tibble(x = 1:6, y = c(4:6, 4:2)) |>
  ggplot(aes(x = x, y = y)) + geom_smooth() + geom_abline(color = "red", slope = a)

##### BASIC CONCEPTS: DESCRIPTIVE STATISTICS #####

# Imagine we measure the intelligence (IQ) of a sample of people.

n=10 # We set sample size.
M=100 # We set poblational mean (the mean of the whole population if we could assess everybody).
SD=15 # We set populational variability (measured by the Standard Deviation).

datosx<-rnorm(n = n, mean = M, sd = SD) # We extract data randomly from the population,
# assuming a normal distribution and the mean and SD that we have set.
datosy<-rep(0,n) # Do not pay attention to this instruction. It only helps to
# produce the chart later on.
datos<-data.frame(datosx,datosy)
datos
# plot data points

ggplot(datos, aes(x = datosx, y = datosy))  +
  geom_point(colour="black") +
  xlab("Intelligence") + ylab("number of participants")  + theme_base() +
  geom_vline(xintercept=mean(datosx), colour="gray20", linetype="dashed") +
  coord_cartesian(xlim=c(50,150), ylim=c(0,8)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8)) +
  annotate("text", x = 150, y = 7, hjust = "right", label = paste("NExps=1","\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 150, y = 2, hjust = "right", label = paste("m=",round(mean(datosx),2),"\n","sd=",round(sd(datosx),2),sep = ""))

#plot data histogram
ggplot(datos, aes(datosx))  +
  geom_histogram(colour="black", fill="grey", binwidth=10) +
  geom_vline(xintercept=mean(datosx), colour="gray20", linetype="dashed") +
  xlab("Intelligence") + ylab("number of participants")  + theme_base() +
  coord_cartesian(xlim=c(50,150), ylim=c(0,8)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8)) +
  annotate("text", x = 150, y = 7, hjust = "right", label = paste("NExps=1","\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 150, y = 2, hjust = "right", label = paste("m=",round(mean(datosx),2),"\n","sd=",round(sd(datosx),2),sep = ""))

# plot data density
ggplot(datos, aes(datosx))  +
  geom_histogram(colour="black", fill="grey", binwidth=10, aes(y=..density..)) +
  geom_density(fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(datosx), colour="gray20", linetype="dashed") +
  xlab("Intelligence") + ylab("number of participants")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.08)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08)) +
  annotate("text", x = 150, y = 0.07, hjust = "right", label = paste("NExps=1","\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 150, y = 0.02, hjust = "right", label = paste("m=",round(mean(datosx),2),"\n","sd=",round(sd(datosx),2),sep = ""))

# Let's see how well these data adjust to the underlying reality (which I know is a normal distribution with mean 100 and SD 15).

ggplot(datos, aes(datosx))  +
  geom_histogram(colour="black", fill="grey", binwidth=10, aes(y=..density..)) +
  geom_density(fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(datosx), colour="gray20", linetype="dashed") +
  stat_function(fun=dnorm, args=c(mean=M,sd=SD), size=1, color="red", lty=2) +
  geom_vline(xintercept = M, colour="red", linetype="dashed") +
  xlab("Intelligence") + ylab("number of participants")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.08)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08)) +
  annotate("text", x = 150, y = 0.07, hjust = "right", label = paste("NExps=1","\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 150, y = 0.02, hjust = "right", label = paste("m=",round(mean(datosx),2),"\n","sd=",round(sd(datosx),2),sep = ""))

# To see that the density is the same as a continuous histogram, and the sampled distribution ultimately comes from the populational distribution,
# we repeat the experiment with a much larger sample.

n=1000000
M=100
SD=15

datosx<-rnorm(n = n, mean = M, sd = SD)
datos<-data.frame(datosx)

ggplot(datos, aes(datosx))  +
  geom_histogram(colour="black", fill="grey", binwidth=10, aes(y=..density..)) +
  geom_density(fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(datosx), colour="gray20", linetype="dashed") +
  stat_function(fun=dnorm, args=c(mean=M,sd=SD), size=1, color="red", lty=2) +
  geom_vline(xintercept = M, colour="red", linetype="dashed") +
  xlab("Intelligence") + ylab("number of participants")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.08)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08)) +
  annotate("text", x = 150, y = 0.07, hjust = "right", label = paste("NExps=1","\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 150, y = 0.02, hjust = "right", label = paste("m=",round(mean(datosx),2),"\n","sd=",round(sd(datosx),2),sep = ""))

# We can see the fit even better if we make the column width smaller.

ggplot(datos, aes(datosx))  +
  geom_histogram(colour="black", fill="grey", binwidth=1, aes(y=..density..)) +
  geom_density(fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(datosx), colour="gray20", linetype="dashed") +
  stat_function(fun=dnorm, args=c(mean=M,sd=SD), size=1, color="red", lty=2) +
  geom_vline(xintercept = M, colour="red", linetype="dashed") +
  xlab("Intelligence") + ylab("number of participants")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.08)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08)) +
  annotate("text", x = 150, y = 0.07, hjust = "right", label = paste("NExps=1","\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 150, y = 0.02, hjust = "right", label = paste("m=",round(mean(datosx)),"\n","sd=",round(sd(datosx)),sep = ""))

# This is a validation of our random number generator: the produced numbers do come from a normal distribution with the set mean and SD.


##### INFERENTIAL STATISTICS ######

# Usually, we are not interested in the sample mean, but we want to know the
# populational mean
# (or sometimes other parameters, such as the standard deviation).
# But for practical reasons we cannot assess the whole population. Samples are
# almost always a fraction of the population.
# This produces a problem: each time we repeat the study (that is, each time
# we extract a new sample and measure its intelligence)
# the data are different and their means and sds differ.

# The difference in sample parameters from population parameters is due to
# many factors. There is the basic fact that because of random extraction,
# the parameters of the sample will not usually coincide with the parameters
# of the population. The greater the "real" (populational) variability
# the greater the variations between samples. But it can also be due to factors that affect the study (perhaps our measure is not very precise,
# or some of our participants did not sleep well, or a myriad other factors).

# The key question is: from sampled data, how can we estimate the real,
# populational value of the mean (or any other parameter of interest)?
# The esential problema is the NOISE, the degree of variability that there is
# in sampled data. We will call this noise "sampling error".

# The main factor that affects sampling error is sample size. As we showed
# above, when sample size is huge, the sample mean coincides with the
# populational mean.
# Let simulate that we repeat the prior study many times without changing
# populational parameters (mean and SD constant), and using samples of
# different sizes.

nSims = 1000 # Set the number of repetitions of the study.
n=10 # Set sample size of each study.
M=100 # Populational mean.
SD=15 # Populational variability.

means <-numeric(nSims) # empty container to store all sample means.
SDs <- numeric(nSims) # empty container to store all sample SDs.

for(i in 1:nSims){ # in each simulated study
  x<-rnorm(n = n, mean = M, sd = SD) # we randomly take a sample from the population.
  means[i]=mean(x) # we compute the mean and store it.
  SDs[i]=sd(x) # we compute the SD and store it.
}

meansSDs <- data.frame(means,SDs) # we join the two containers in a dataframe
View(meansSDs)
# Distribution of sample means

ggplot(meansSDs, aes(x=means))  +
  geom_density(fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(means), colour="gray20", linetype="dashed") +
  geom_vline(xintercept = M, colour="red", linetype="dashed") +
  xlab("Intelligence") + ylab("number of MEANS")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.2)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  annotate("text", x = 150, y = 0.15, hjust = "right", label = paste("NExps=",nSims,"\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 150, y = 0.02, hjust = "right", label = paste("m=",round(mean(means),2),"\n","sd=",round(sd(means),2),sep = ""))

# We see that: 1) the mean of sample means coincides with the populational mean.
# That is, sample mean is an unbiased estimator of populational mean (half of
# the sample means are above and half below the populational mean);
# 2) sample means distribute around the populational mean, that is, there is
# noise.
# We repeat the simulation increasing sample size in the studies.

nSims = 1000
n=30 # This is the only change.
M=100
SD=15

means <-numeric(nSims)
SDs <- numeric(nSims)

for(i in 1:nSims){
  x<-rnorm(n = n, mean = M, sd = SD)
  means[i]=mean(x)
  SDs[i]=sd(x)
}

meansSDs <- data.frame(means,SDs)

# Distribution of sample means

ggplot(meansSDs, aes(x=means))  +
  geom_density(fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(means), colour="gray20", linetype="dashed") +
  geom_vline(xintercept = M, colour="red", linetype="dashed") +
  xlab("Intelligence") + ylab("number of MEANS")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.2)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  annotate("text", x = 150, y = 0.15, hjust = "right", label = paste("NExps=",nSims,"\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 150, y = 0.02, hjust = "right", label = paste("m=",round(mean(means),2),"\n","sd=",round(sd(means),2),sep = ""))

# As sample size increases, sample means differ less from the populational
# mean, being more closely grouped around
# the populational mean (there is a reduction in noise).
# That is, as sample size increases, sample means become better (more precise)
# estimators of the populational mean.
rnorm(n = 100, mean = 100, sd = 15)
# How does sample mean precision varies as a function of sample size?
# Let's simulate 1000 studies for each sample size between 2 and 500.

nSims = 1000
nMin = 2 # Minimum sample size.
nMax = 500 # Maximum sample size.
M = 100
SD = 15

means <-numeric(nSims) # containers for sample means and SDs.
SDs <- numeric(nSims)
meansofmeans <- numeric(nMax-nMin) # container for the mean of sample means with each sample size.
SDsofmeans <- numeric(nMax-nMin) # container for the SD of sample means with each sample size.

for (j in (nMin+1):nMax) { # for each sample size
  for(i in 1:nSims){ # for each simulated study
    x<-rnorm(n = j, mean = M, sd = SD) # draw random sample
    means[i]=mean(x) # store sample mean
    SDs[i]=sd(x) # store sample SD
  }
meansofmeans[j-nMin]=mean(means) # store the mean of the sample means of all the studies with a given sample size
SDsofmeans[j-nMin]=sd(means) # store the SD of the sample means of all the studies with a given sample size
}

n <- c((nMin+1):nMax) # vector for the x axis in the plot
meansSDs <- data.frame(meansofmeans,SDsofmeans,n)

#plot of the decrease in variability (SD) of sample means as sample size increases.

ggplot(meansSDs, aes(x=n, y=SDsofmeans))  +
  geom_line() +
  coord_cartesian(ylim=c(0,30)) +
  xlab("Sample size") + ylab("SD of the sample means")  + theme_base() +
  annotate("text", x = nMax, y = 7, hjust = "right", label = paste("NExps=",nSims,"\n","M=",M,"\n","SD=",SD,sep = ""))

# We can see that the dispersion of the means of the studies is larger when the studies have small sample sizes
# and decreases as sample size increases.

# As sample size approaches infinite, the dispersion of the means approaches zero
# (that is, sample means approach the populational mean).

# Another way to show the dispersion of the sample means around the overall mean is to show a confidence interval (CI) around it.
# A 95% confidence interval shows where 95% of sample means will be found if the study is repeated many times.
# In order to compute it, it is necessary to multiply the SD by the z-value that corresponds to 97.5% in the normal
# distribution, and then add it to the mean (to get the upper whisker) and subtract it from the mean (to get the lower whisker).
# This value is 1.96.

nSims = 1000
n=10
M = 100
SD = 15

means <-numeric(nSims)
SDs <- numeric(nSims)

for(i in 1:nSims){
  x<-rnorm(n = n, mean = M, sd = SD)
  means[i]=mean(x)
  SDs[i]=sd(x)
}

meansSDs <- data.frame(means,SDs)

# Distribution of the sample means

ggplot(meansSDs, aes(x=means))  +
  geom_density(fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(means), colour="gray20", linetype="dashed") +
  xlab("Intelligence") + ylab("number of MEANS")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.1)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  annotate("text", x = 150, y = 0.06, hjust = "right", label = paste("NExps=",nSims,"\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 150, y = 0.02, hjust = "right", label = paste("m=",round(mean(means),2),"\n","sd=",round(sd(means),2),sep = "")) +
  annotate("point", x = mean(means), y = 0, size = 3) +
  annotate("errorbarh", x = mean(means), y = 0, height = 0.01,
          xmin = (mean(means)-(sd(means)*1.96)), # We compute the confidence interval by multiplying the SD of the sample means by 1.96 and both add it
                                                 # and subtract it from the mean of the sample means.
          xmax = (mean(means)+(sd(means)*1.96)))

# As the mean of the sample means (thus, the populational mean) can be estimated from each sample mean (with a precision that depends on
# the dispersion of sample means), the SD of the distribution of sample means can be also be estimated from a single sample.

# This estimator is called the Standard Error of the Mean (or SEM) and is the SD of the sample divided by the
# square root of sample size.

# If we use SEM to compute a CI around the sample mean (multiplying SEM by 1.96),
# it gives us an estimation of where 95% of the sample means will fall if the experiment is repeated many times.

# We now repeat the prior simulation, but adding to the plot the means and SEMs of five randomly chosen samples.

nSims = 1000
n=10
M = 100
SD = 15

means <-numeric(nSims) # Containers
SDs <- numeric(nSims)
SEMs <- numeric(nSims)
lowCIs <- numeric(nSims)
highCIs <- numeric(nSims)

for(i in 1:nSims){
  x<-rnorm(n = n, mean = M, sd = SD)
  means[i]=mean(x)
  SDs[i]=sd(x)
  SEMs[i]=sd(x)/sqrt(n) # SEMs
  lowCIs[i]=means[i]-(SEMs[i]*1.96) # lower limit of the CI based on sample SEM
  highCIs[i]=means[i]+(SEMs[i]*1.96) # upper limit of the CI based on sample SEM
}

meansSDs <- data.frame(means,SDs,SEMs,lowCIs,highCIs)

# Distribution of means plus the means and SEM-based CI from five randomly chosen samples

ggplot(meansSDs, aes(x=means))  +
  geom_density(fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(means), colour="gray20", linetype="dashed") +
  xlab("Intelligence") + ylab("number of MEANS")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.1)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  annotate("text", x = 150, y = 0.07, hjust = "right", label = paste("NExps=",nSims,"\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 150, y = 0.02, hjust = "right", label = paste("m=",round(mean(means),2),"\n","sd=",round(sd(means),2),sep = "")) +
  annotate("point", x = mean(means), y = 0, size = 3) +
  annotate("errorbarh", x = mean(means), y = 0, height = 0.01,
           xmin = (mean(means)-(sd(means)*1.96)),
           xmax = (mean(means)+(sd(means)*1.96))) +
  annotate("point", x = means[10], y = 0.01, size = 3, color = "blue") +
  annotate("errorbarh", x = means[10], y = 0.01, height = 0.01,
           xmin = lowCIs[10],
           xmax = highCIs[10], color = "blue") +
  annotate("point", x = means[20], y = 0.02, size = 3, color = "blue") +
  annotate("errorbarh", x = means[20], y = 0.02, height = 0.01,
           xmin = lowCIs[20],
           xmax = highCIs[20], color = "blue") +
  annotate("point", x = means[30], y = 0.03, size = 3, color = "blue") +
  annotate("errorbarh", x = means[30], y = 0.03, height = 0.01,
           xmin = lowCIs[30],
           xmax = highCIs[30], color = "blue") +
  annotate("point", x = means[40], y = 0.04, size = 3, color = "blue") +
  annotate("errorbarh", x = means[40], y = 0.04, height = 0.01,
           xmin = lowCIs[40],
           xmax = highCIs[40], color = "blue") +
  annotate("point", x = means[50], y = 0.05, size = 3, color = "blue") +
  annotate("errorbarh", x = means[50], y = 0.05, height = 0.01,
           xmin = lowCIs[50],
           xmax = highCIs[50], color = "blue")

# This plot shows the mean of means and its 95% CI (in black), as well as the
# means and SEM-based CI from five randomly chosen samples. We can see that they are
# estimations of where 95% of sample means will fall.
# Moreover, 95% of these CIs will contain the population mean.

# Very often, in published studies researchers depict means plus and minus the SEM.
# It is important to be aware that this interval DOES NOT estimate where the 95% of
# sample means will fall, as the SEM has not been multiplied by 1.96.
# Using only the SEM corresponds to a CI of 68%.

# If we repeat the simulation using larger sample sizes, we see that the SEM-based CIs
# become smaller, as they become more precise estimators of the SD of the sample means distribution
# (eventually, with very large sample sizes they will match it).
# Go up and try by yourself changing the sample size and repeating the simulation.

# Both the interval based on SEM (68% CI) and the 95% CI provide us with information about the
# precision of the study: the wider the intervals are, the less precision the study has.

# However, only the 95% CI will contain 95% of the sample means and will be useful to do
# "statistics by eye", as we will see later.

# A frequent mistake when we see a plot with a mean and a 95% CI around is to think that
# the interval encompasses 95% of all data points in that sample. This is wrong.
# The interval estimates where 95% of sample means will fall if the study were to be repeated many times.
# We can see this difference in the following simulation of a single study.

n=100
M = 100
SD = 15

datosx<-rnorm(n = n, mean = M, sd = SD)
meanx=mean(datosx)
SDx=sd(datosx)
SEMx=SDx/sqrt(n)
lowCI95=meanx-(SDx*1.96) # lower limit of the 95% CI of the sample data
highCI95=meanx+(SDx*1.96) # upper limit of the 95% CI of the sample data
lowCIsem=meanx-(SEMx*1.96) # lower limit of the 95% CI around the mean (based on SEM)
highCIsem=meanx+(SEMx*1.96) # upper limit of the 95% CI around the mean (based on SEM)

x <- data.frame(datosx)

# The blue CI encompasses 95% of DATA around the sample mean.
# The black CI estimates where 95% of the MEANS of future identical studies would fall.

ggplot(x, aes(x=datosx))  +
  geom_density(fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=meanx, colour="gray20", linetype="dashed") +
  xlab("Intelligence") + ylab("number of PARTICIPANTS")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.08)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  annotate("text", x = 150, y = 0.07, hjust = "right", label = paste("NExps=1","\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 150, y = 0.02, hjust = "right", label = paste("m=",round(meanx,2),"\n","sd=",round(SDx,2),sep = "")) +
  annotate("point", x = meanx, y = 0.04, size = 3) +
  annotate("errorbarh", x = meanx, y = 0.04, height = 0.005,
           xmin = lowCIsem,
           xmax = highCIsem) +
  annotate("point", x = meanx, y = 0.03, size = 3, color = "blue") +
  annotate("errorbarh", x = meanx, y = 0.03, height = 0.005,
           xmin = lowCI95,
           xmax = highCI95, color = "blue")

# Repeat the simulation using a much larger sample size (say, 1000). You will see
# that the size of the data-based CI does not change, whereas the SEM-based CI
# is much smaller.


##### SAMPLE SD AS ESTIMATOR OF POPULATIONAL SD (THIS IS NOT ESSENTIAL FOR THE REST OF THE CONTENTS, SO IT CAN BE SAFELY SKIPPED) #####

# As we saw, a sample mean is an unbiased estimator of the populational mean. That is why the mean of many sample means
# matches the populational mean. But it is different with the sample SD, which has a bias (which approaches zero as sample
# mean approaches infinite). The best way to correct this bias is to divide by n-1, instead of n
# (R computes unbiased SD by default). Even with this correction, the unbiased SD tends to slightly subestimate the populational SD.
# We can see it in the following simulation. You can play with sample size to see how the bias decreases as sample size increases.

nSims = 1000
M = 100
SD = 15
n=10

means <-numeric(nSims)
SDs <- numeric(nSims) # container for biased SDs (divided by n)
SDsu <- numeric(nSims) # container for unbiased SDs (divided by n-1)


for(i in 1:nSims){
  x<-rnorm(n = n, mean = M, sd = SD)
  means[i]=mean(x)
  SDs[i]=sqrt(mean((x-mean(x))^2))
  SDsu[i]=sd(x)
}

meansSDs <- data.frame(means,SDs,SDsu)

# Distribution of biased SDs

ggplot(meansSDs, aes(x=SDs))  +
  geom_density(fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(SDs), colour="gray20", linetype="dashed") +
  geom_vline(xintercept = SD, colour="red", linetype="dashed") +
  xlab("Intelligence") + ylab("number of SDs")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(0,30), ylim=c(0,0.12)) +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30)) +
  annotate("text", x = 30, y = 0.07, hjust = "right", label = paste("NExps=",nSims,"\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 30, y = 0.02, hjust = "right", label = paste("m=",round(mean(SDs),2),"\n","sd=",round(sd(SDs),2),sep = ""))

# Distribution of unbiased SDs

ggplot(meansSDs, aes(x=SDsu))  +
  geom_density(fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(SDsu), colour="gray20", linetype="dashed") +
  geom_vline(xintercept = SD, colour="red", linetype="dashed") +
  xlab("Intelligence") + ylab("number of unbiased SDs")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(0,30), ylim=c(0,0.12)) +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30)) +
  annotate("text", x = 30, y = 0.07, hjust = "right", label = paste("NExps=",nSims,"\n","M=",M,"\n","SD=",SD,"\n","n=",n,sep = "")) +
  annotate("text", x = 30, y = 0.02, hjust = "right", label = paste("m=",round(mean(SDsu),2),"\n","sd=",round(sd(SDsu),2),sep = ""))


##### DOING STATISTICS BY EYE #####

# Now we are going to see how 95% CIs let us do statistics by eye.
# Let's simulate two samples with the same mean but different variability.

n=100
M1 = 100
M2 = 100
SD1 = 13
SD2 = 20
datosxsd1<-rnorm(n = n, mean = M1, sd = SD1)
datosxsd2<-rnorm(n = n, mean = M2, sd = SD2)

datos<-data.frame(datosxsd1,datosxsd2)

ggplot(datos)  +
  geom_density(aes(x=datosxsd1), fill=NA, colour="black", size = 1) +
  geom_density(aes(x=datosxsd2), fill=NA, colour="blue", size = 1) +
  xlab("Intelligence") + ylab("number of participants")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.04)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  annotate("point", x = mean(datosxsd1), y = 0.005, size = 3) +
  annotate("errorbarh", x = mean(datosxsd1), y = 0.005, height = 0.003,
           xmin = mean(datosxsd1)-(sd(datosxsd1)*1.96),
           xmax = mean(datosxsd1)+(sd(datosxsd1)*1.96)) +
  annotate("point", x = mean(datosxsd2), y = 0.01, size = 3, color = "blue") +
  annotate("errorbarh", x = mean(datosxsd2), y = 0.01, height = 0.003, color = "blue",
    xmin = mean(datosxsd2)-(sd(datosxsd2)*1.96),
    xmax = mean(datosxsd2)+(sd(datosxsd2)*1.96)) +
  annotate("text", x = 140, y = 0.035, label = "95% CI")

# Only by watching the plot we can say that we have two distributions that have
# nearly the same mean, that one has less noise (and therefore allows a more
# precise estimation of the populational mean) and is practically
# subsumed within the other.

# It also allows other type of inferences: what is the probability that the participant
# added in the following chart comes from one or the other sample?

ggplot(datos)  +
  geom_density(aes(x=datosxsd1), fill=NA, colour="black", size = 1) +
  geom_density(aes(x=datosxsd2), fill=NA, colour="blue", size = 1) +
  geom_vline(xintercept=65, colour="red", linetype="dashed") +
  xlab("Intelligence") + ylab("number of participants")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.04)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  annotate("point", x = mean(datosxsd1), y = 0.005, size = 3) +
  annotate("errorbarh", x = mean(datosxsd1), y = 0.005, height = 0.003,
           xmin = mean(datosxsd1)-(sd(datosxsd1)*1.96),
           xmax = mean(datosxsd1)+(sd(datosxsd1)*1.96)) +
  annotate("point", x = mean(datosxsd2), y = 0.01, size = 3, color = "blue") +
  annotate("errorbarh", x = mean(datosxsd2), y = 0.01, height = 0.003, color = "blue",
           xmin = mean(datosxsd2)-(sd(datosxsd2)*1.96),
           xmax = mean(datosxsd2)+(sd(datosxsd2)*1.96)) +
  annotate("text", x = 140, y = 0.035, label = "95% CI")

# There is less than 5% probability that the participant comes from one of the two distributions,
# but more than 5% that it comes from the other.
# If we apply this logic to a distribution of sample MEANS, by using a SEM-based CI,
# we will be able to estimate whether it is
# unlikely that the data point coincides with the populational mean.

ggplot(datos)  +
  geom_density(aes(x=datosxsd1), fill=NA, colour="black", size = 1) +
  geom_density(aes(x=datosxsd2), fill=NA, colour="blue", size = 1) +
  geom_vline(xintercept=65, colour="red", linetype="dashed") +
  xlab("Intelligence") + ylab("number of participants")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.04)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  annotate("point", x = mean(datosxsd1), y = 0.005, size = 3) +
  annotate("errorbarh", x = mean(datosxsd1), y = 0.005, height = 0.002,
           xmin = mean(datosxsd1)-((sd(datosxsd1)/sqrt(n))*1.96),
           xmax = mean(datosxsd1)+((sd(datosxsd1)/sqrt(n))*1.96)) +
  annotate("point", x = mean(datosxsd2), y = 0.01, size = 3, color = "blue") +
  annotate("errorbarh", x = mean(datosxsd2), y = 0.01, height = 0.002, color = "blue",
           xmin = mean(datosxsd2)-((sd(datosxsd2)/sqrt(n))*1.96),
           xmax = mean(datosxsd2)+((sd(datosxsd2)/sqrt(n))*1.96)) +
  annotate("text", x = 140, y = 0.035, label = "95% CI SEM")

# Using the same logic, we can also judge by eye whether two means are "significantly different",
# that is, whether there is a probability smaller than 5% that one mean comes from the distribution of
# the other mean: if the 95% SEM-based CI do not overlap, the probability that the two
# means come from the same distribution is smaller than 5%.

# A t-test confirms this impression.

t.test(datosxsd1, datosxsd2,
       alternative = "two.sided")

# Let's repeat the simulation using different means and the same variability
# in the two samples.

n=100
M1 = 100
M2 = 110
SD1 = 20
SD2 = 20
datosxsd1<-rnorm(n = n, mean = M1, sd = SD1)
datosxsd2<-rnorm(n = n, mean = M2, sd = SD2)

datos<-data.frame(datosxsd1,datosxsd2)

ggplot(datos)  +
  geom_density(aes(x=datosxsd1), fill=NA, colour="black", size = 1) +
  geom_density(aes(x=datosxsd2), fill=NA, colour="blue", size = 1) +
  xlab("Intelligence") + ylab("number of participants")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(50,150), ylim=c(0,0.04)) +
  scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  annotate("point", x = mean(datosxsd1), y = 0.005, size = 3) +
  annotate("errorbarh", x = mean(datosxsd1), y = 0.005, height = 0.002,
           xmin = mean(datosxsd1)-((sd(datosxsd1)/sqrt(n))*1.96),
           xmax = mean(datosxsd1)+((sd(datosxsd1)/sqrt(n))*1.96)) +
  annotate("point", x = mean(datosxsd2), y = 0.01, size = 3, color = "blue") +
  annotate("errorbarh", x = mean(datosxsd2), y = 0.01, height = 0.002, color = "blue",
           xmin = mean(datosxsd2)-((sd(datosxsd2)/sqrt(n))*1.96),
           xmax = mean(datosxsd2)+((sd(datosxsd2)/sqrt(n))*1.96)) +
  annotate("text", x = 140, y = 0.035, label = "95% CI SEM")

t.test(datosxsd1, datosxsd2,
       alternative = "two.sided")

# We can see that, sometimes, the CIs overlap a little, but the t-test is significant.
# This teaches us that:
# 1) Statistics by eye is a bit conservative (it tends to produce more false negatives when there is
# an effect, and more true negatives when there is no effect).
# 2) The fact that two means are significantly different doesn't mean that their distributions do not overlap.


##### THE LOGIC OF NULL HYPOTHESIS TESTING #####

# A central goal of inferential statistics is to establish whether there is a
# populational difference between two (or more) conditions.

# The visual tests of significance work by (visually) comparing the difference
# between the means with the
# degree of noise (variability) that there is around them. If the variability is big compared to the
# difference in means, the CIs will overlap and this will tell us that the probability that one
# mean belongs to the distribution of the other means is not greater than a critical value
# called alpha, and usually set at 5%.

# A t-test also compares the difference between means with the internal variability in each sample.
# The strategy is to divide the difference between means by the variability such that we obtain
# a t value. We then look up the t value in tables that take into account the size of the samples,
# retrieving the probability of obtaining that t value when the two means come from the same
# distribution. If that probability is smaller than 5%, we will claim that the two means are
# "significantly different" with a 5% probability of making a false positive claim, that is,
# with a 5% probability of claiming that there is a difference when there is none.
# alpha is the probability of making a false positive error.

# Let's repeat the study many times, compute a t-value and its corresponding p-value,
# and see how their distributions look like.

nSims = 1000
n=30
M1=110
M2=100
SD1=30
SD2=30

tvalues <-numeric(nSims) # containers
tvaluesp <-numeric(nSims)

for(i in 1:nSims){
  datosx1<-rnorm(n = n, mean = M1, sd = SD1)
  datosx2<-rnorm(n = n, mean = M2, sd = SD2)
  tvalues[i]=t.test(datosx1, datosx2, alternative = "two.sided")$statistic # compute and store the t-value
  tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value # compute and storethe p-value
}

# plot of t-values
ggplot()  +
  geom_density(aes(x=tvalues), fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(tvalues), colour="black", linetype="dashed") +
  geom_vline(xintercept=0, colour="blue", linetype="dashed") +
  xlab("t-values") + ylab("number of t-values")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(-6,6), ylim = c(0,0.8)) +
  annotate("text", x = 0, y = 0.6, label = "Null difference") +
  annotate("text", x = 6, vjust = "top", hjust = "right", y = 0.8, label =
             paste("nExps=",nSims,"\n","n=",n,"\n"," M1=",M1," M2=",M2,"\n","SD1=",SD1," SD2=",SD2))

# plot of the p-values associated to the t-values
ggplot()  +
  geom_density(aes(x=tvaluesp), fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=0.05, colour="blue", linetype="dashed") +
  xlab("p-value associated to the t-value") + ylab("number of p-values")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(0,1), ylim = c(0,15)) +
  annotate("text", x = 0.06, hjust = "left", y = 15, label = "p= 0.05") +
  annotate("text", x = 1, vjust = "top", hjust = "right", y = 15, label =
             paste("nExps=",nSims,"\n","n=",n,"\n"," M1=",M1," M2=",M2,"\n","SD1=",SD1," SD2=",SD2))

# The value of t and its associated p (and, therefore, whether the result will be significant) depends on
# three factors: 1) the difference between the means, that is, the size of the effect;
# 2) the sampling error, that is, the noise in the samples, which depends crucially on sample size
# (but also on populational variability, which we may improve in several ways, for example, improving the
# precision with which we measure the dependent variable);
# and 3) the value of alpha, the percent of false positives that we are willing to tolerate.

# Playing with the following simulation we can see how these three factors affect the distributions of
# t and p values. We keep alpha at 5%, and play with effect size, sample size, and populational variability.
# Let's start with an effect of zero size (a null difference between the means). What is the shape of the
# t-value and p-value distribution? Then, proceed by increasing the difference between the means. Then play with sample sizes.

nSims = 1000
n=30
M1=100
M2=100
SD1=30
SD2=30

tvalues <-numeric(nSims)
tvaluesp <-numeric(nSims)

for(i in 1:nSims){
  datosx1<-rnorm(n = n, mean = M1, sd = SD1)
  datosx2<-rnorm(n = n, mean = M2, sd = SD2)
  tvalues[i]=t.test(datosx1, datosx2, alternative = "two.sided")$statistic
  tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value
}

# plot of t-values
ggplot()  +
  geom_density(aes(x=tvalues), fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(tvalues), colour="black", linetype="dashed") +
  geom_vline(xintercept=0, colour="blue", linetype="dashed") +
  xlab("t-values") + ylab("number of t-values")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(-6,6), ylim = c(0,0.8)) +
  annotate("text", x = 0, y = 0.8, label = "Null difference") +
  annotate("text", x = 6, vjust = "top", hjust = "right", y = 0.8, label =
             paste("nExps=",nSims,"\n","n=",n,"\n"," M1=",M1," M2=",M2,"\n","SD1=",SD1," SD2=",SD2))

# plot of associated p-values
ggplot()  +
  geom_density(aes(x=tvaluesp), fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=0.05, colour="blue", linetype="dashed") +
  xlab("p-values associated to the t-values") + ylab("number of p-values")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(0,1), ylim = c(0,15)) +
  annotate("text", x = 0.06, hjust = "left", y = 15, label = "p= 0.05") +
  annotate("text", x = 1, vjust = "top", hjust = "right", y = 15, label =
             paste("nExps=",nSims,"\n","n=",n,"\n"," M1=",M1," M2=",M2,"\n","SD1=",SD1," SD2=",SD2))

# We see several important things:
# 1) When the difference is null, the distribution of p is flat. That is, we will find 5% of significant p-values
# when there is no effect. We can always claim a difference when there is none (exactly in 5% of the repetitions
# of the same study). This means that it is not possible to interpret a single p-value. This leads to the
# necessity of thinking meta-analytically (as we will see below).

# 2) As the effect size increases, or sampling error decreases (i.e., by using greater sample sizes),
# the distribution of p becomes more asymmetric toward the right. This is the foundation of a meta-analytic
# technique called the p-curve analysis (and its more recent derivation, the z-curve analysis).

# (Here we change to the associated Powerpoint).

# This is the logic of Hypothesis Testing: estimate the probability that the two means are equal in the population
# from computing the probability of finding a difference between the means as big as the observed difference
# if there is no real difference in the population. If that probability is as low as to be below a given
# criterium (called alpha, usually 5%), we then claim that it is unlikely that the two means come from the same
# distribution (i.e., that they are equal in the population) and, therefore, that the difference between means is
# "significant". This claim has an associate probability of being a false positive error of 5% (also called a Type I error).
# As the false positive error is low enough, we can act as if the difference is real.

# In Hypothesis Testing, a non-significant p-value does not have much informative value, as it only allows
# to claim that the two means are not significantly different, but this does not entail that they are the same.
# It could just be the case that there was too much noise in the data, due to a myriad of potential causes.

# However, in its use by the scientists, the logic of HT was soon perverted in several ways:
# all the emphasis was put on finding a significant p-value in a specific study, assumming that
# a real populational difference follows from it. Scientists did not pay attention to making
# sure that there was a real populational difference nor what would be the real size of that difference.
# The p-value became a magic value, indexing the making of a discovery, its publication, and therefore,
# the gaining of scientific reputation and promotion for the scientist.

# The reasoning about the populational effect followed this (wrong) logic:
# 1) the smaller the p-value, the greater the populational effect size.
# 2) the smaller the p-value, the greater the probability of replicating the significant finding in future studies.

# The main problem with this logic follows from the simulations above: a null populational effect
# will produce a significant p-value in 5% of studies. Moreover, this p-value can take any value in the
# range 0-0.05, so it can be very small.

# The importance attributed to significant p-values led to the use of several Questionable Research Practices (QRPs),
# including p-hacking and HARKing, and also to a strong publication bias (only significant p-values get published).
# These practices have thrown psychology into a Replicability Crisis.

# Now we return from the Powerpoint and go back to the simulations.


##### ALPHA (FALSE POSITIVES), BETA (FALSE NEGATIVES) AND STATISTICAL POWER (TRUE POSITIVES) #####

# The statistical power of a given study is the percentage of repetitions of the study in which
# we will find a significant result, given that a real difference exists in the population.

# The complementary of statistical power is the false negative rate, that is, how many times
# we will not find a significant result, given that a real difference exists in the population.
# This is called beta, or Type II Error.

# The false alarm rate is how many times we will find a significant finding when there is no
# real effect in the population. This is alpha, or Type I Error.

# Let's repeat the prior simulations varying populational effect size and sample size, focusing
# on the distribution of p-values and counting how many of those p-values are significant.

# To compute the false alarm rate and the statistical power, we just count how many significant results
# we observe. The only difference is that, in the first case, there is no real effect, whereas
# in the second there is a real effect in the population.

# We start with a null effect and proceed by increasing effect size. Then we fix
# effect size and increase sample size.

nSims = 1000
n=30
M1=100
M2=100
sd1=30
sd2=30

tvaluesp <-numeric(nSims)

for(i in 1:nSims){
  datosx1<-rnorm(n = n, mean = M1, sd = sd1)
  datosx2<-rnorm(n = n, mean = M2, sd = sd2)
  tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value
}

significantp <- sum(tvaluesp < 0.05)*100/nSims # compute percentage of significant p-values

# plot of p-values
ggplot()  +
  geom_density(aes(x=tvaluesp), fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=0.05, colour="blue", linetype="dashed") +
  xlab("p-value") + ylab("number of p-values")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(0,1), ylim = c(0,15)) +
  annotate("text", x = 0.06, hjust = "left", y = 15, label = "p= 0.05") +
  annotate("text", x = 1, vjust = "top", hjust = "right", y = 15, label =
             paste("nExps=",nSims,"\n","n=",n,"\n"," M1=",M1," M2=",M2,"\n","SD1=",sd1," SD2=",sd2,"\n","\n",
                   "p<.05=",significantp,"%"))

# Now, let's see how the false alarm rate varies with sample size (there is no effect)

nSims = 10000 # number of experiments for each sample size
nSampleSizes = 20 # number of sample sizes to be evaluated
sampleSizeIncrement = 10 # increment in sample size between each two consecutive sample sizes
n=10 # starting sample size
M1=100
M2=100
SD1=30
SD2=30

samplesize <- seq(n,(nSampleSizes*sampleSizeIncrement),by=sampleSizeIncrement) # establish the sequence of sample sizes to be evaluated

tvaluesp <-numeric(nSims)
significantpvalues <- numeric(nSampleSizes)

for (s in 1:nSampleSizes) { # for each sample size
  for(i in 1:nSims){ # for each simulated experiment
    datosx1<-rnorm(n = n, mean = M1, sd = SD1) # randomly draw the data for each condition
    datosx2<-rnorm(n = n, mean = M2, sd = SD2)
    tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value # compute t-test and extract p-value
  }
  significantpvalues[s]=sum(tvaluesp < 0.05)*100/nSims     # compute percentage of significant p-values obtained with this sample size
  n = n + sampleSizeIncrement # increase sample size
}

p05datos <- data.frame(samplesize,significantpvalues)

# plot of significant p-values as a function of sample size
ggplot(data = p05datos, aes(x=samplesize, y=significantpvalues))  +
  geom_point(colour="black", size = 1) +
  geom_line() +
  xlab("sample size per group") + ylab("% p<.05")  + theme_base() +
  coord_cartesian(ylim = c(0,100)) +
  geom_hline(yintercept=5, colour="black", linetype="dashed") +
  annotate("text", x = (nSampleSizes*sampleSizeIncrement), hjust = "right", y = 10, label = "alpha= 5%")

# We can see that the false alarm rate stays at 5% independently of sample size.

# Let's see now how statistical power varies with sample size.

# This type of simulations let us see what sample size we will need to collect in order to have
# a given probability (say, 80%) of detecting a real effect (a difference of means) of a given size.
# In other words, what sample size we need to attain a given statistical power.
# In this simulation we will set desired power at 80% (a common convention) and draw a line in the plot
# when power passes this threshold.

# Note that we use exactly the same code as above. The only difference is that now there is
# a real effect in the population. We will use an effect size that is frequently found in
# psychological studies (more on effect sizes below).

desiredpower = 80
nSims = 10000 # number of experiments for each sample size
nSampleSizes = 20 # number of sample sizes to be evaluated
sampleSizeIncrement = 10 # increment in sample size between each two consecutive sample sizes
n=10 # starting sample size
M1=100
M2=112
SD1=30
SD2=30

samplesize <- seq(n,(nSampleSizes*sampleSizeIncrement),by=sampleSizeIncrement) # establish the sequence of sample sizes to be evaluated

tvaluesp <-numeric(nSims)
significantpvalues <- numeric(nSampleSizes)

for (s in 1:nSampleSizes) { # for each sample size
  for(i in 1:nSims){ # for each simulated experiment
    datosx1<-rnorm(n = n, mean = M1, sd = SD1) # randomly draw the data for each condition
    datosx2<-rnorm(n = n, mean = M2, sd = SD2)
    tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value # compute t-test and extract p-value
  }
  significantpvalues[s]=sum(tvaluesp < 0.05)*100/nSims     # compute percentage of significant p-values obtained with this sample size
  n = n + sampleSizeIncrement # increase sample size
}

p05datos <- data.frame(samplesize,significantpvalues)

# We find the sample size that passes the desired power in order to show it in the plot

p05datos <- p05datos %>%
  mutate(criticalsamplesize=if_else(significantpvalues<desiredpower,100000,samplesize))

targetsample = min(p05datos$criticalsamplesize)

# plot of significant p-values as a function of sample size
ggplot(data = p05datos, aes(x=samplesize, y=significantpvalues))  +
  geom_point(colour="black", size = 1) +
  geom_line() +
  xlab("sample size per group") + ylab("% p<.05")  + theme_base() +
  coord_cartesian(ylim = c(0,100)) +
#  geom_hline(yintercept=5, colour="black", linetype="dashed") +
#  annotate("text", x = (nSampleSizes*sampleSizeIncrement), hjust = "right", y = 10, label = "alpha= 5%") +
  geom_hline(yintercept=desiredpower, colour="black", linetype="dashed") +
  annotate("text", x = (nSampleSizes*sampleSizeIncrement), hjust = "right", y = 85, label = "power= 80%") +
  geom_vline(xintercept=targetsample, colour="red", linetype="dashed") +
  annotate("text", x = targetsample+5, y = 15, hjust = "left", label = paste("n=",as.character(targetsample)))

# This is the same result as obtained from an analytical computation of power.

p  <- pwr.t.test(d = 0.4,
                 sig.level = 0.05,
                 power = 0.80,
                 alternative = "two.sided")

plot(p) + theme_base()

# Note that the required sample size to have 80% chance to detect a medium-to-small effect
# is 100 participants per group (200 in total).
# This is much more than usually used in psychological studies.
# Thus, most psychological research is underpowered. This means that:
# 1) many attempted studies fail to observe the expected effect (high false negative rate).
# 2) those who observe a significant difference is probably because their samples showed, by chance,
# a difference of means much larger than the real one (that is, they overestimate effect size; more on this below).
# 3) it is unlikely that a repetition of the study will find the same difference (low replicability).
# 4) (to be shown below) underpowered studies are more strongly affected by questionable research practices, thus
# leading to significant findings that do not really exist in the population.

# If we can compute power easily using analytical strategies, why use simulations? Mainly because
# simulation can be applied to more complex designs and any kind of statistical test.

# Now, we are going to see how the design of a study affects its power. In particular, we are going to see
# how the use of a within-participant design can increase power substantially (requiring smaller samples
# to detect an effect of the same size).

# Instead of a between-groups t-test, we will use a within-participants t-test.

# The main reason is because, in a within-participant design, the measures are correlated, due to
# between-participants variation. The greater this correlation, the larger the amount of noise that
# the statistical test is able to discount, and therefore, the easier it is to detect a significant
# difference of means.

desiredpower = 80
nSims = 10000
nSampleSizes = 20
sampleSizeIncrement = 10
n=10
M1=100
M2=112
SD1=30
SD2=30
R=0.8 # correlation between the two measures

samplesize <- seq(n,(nSampleSizes*sampleSizeIncrement),by=sampleSizeIncrement)

tvaluesp <-numeric(nSims)
significantpvalues <- numeric(nSampleSizes)

for (s in 1:nSampleSizes) {
  for(i in 1:nSims){
    experiment <- rnorm_multi(n=n, mu=c(M1,M2), sd=c(SD1,SD2), r=R, varnames=c("datosx1","datosx2"), empirical=FALSE) # draw from two correlated distributions
    tvaluesp[i]=t.test(experiment$datosx1, experiment$datosx2, alternative = "two.sided", paired = TRUE)$p.value
  }
  significantpvalues[s]=sum(tvaluesp < 0.05)*100/nSims
  n = n + sampleSizeIncrement
}

p05datos <- data.frame(samplesize,significantpvalues)

# We find the sample size that passes the desired power in order to show it in the plot

p05datos <- p05datos %>%
  mutate(criticalsamplesize=if_else(significantpvalues<desiredpower,100000,samplesize))

targetsample = min(p05datos$criticalsamplesize)

ggplot(data = p05datos, aes(x=samplesize, y=significantpvalues))  +
  geom_point(colour="black", size = 1) +
  geom_line() +
  xlab("sample size per group") + ylab("p<.05 rate")  + theme_base() +
  coord_cartesian(ylim = c(0,100)) +
#  geom_hline(yintercept=5, colour="black", linetype="dashed") +
#  annotate("text", x = (nSampleSizes*sampleSizeIncrement), hjust = "right", y = 10, label = "alpha= 5%") +
  geom_hline(yintercept=desiredpower, colour="black", linetype="dashed") +
  annotate("text", x = (nSampleSizes*sampleSizeIncrement), hjust = "right", y = 85, label = "power= 80%") +
  geom_vline(xintercept=targetsample, colour="red", linetype="dashed") +
  annotate("text", x = targetsample+5, y = 15, hjust = "left", label = paste("n=",as.character(targetsample)))

# This simulation can be used to find how power changes depending on the degree
# of correlation between repeated measures. For more complex designs, see
# Brysbaert (2019, Journal of Cognition). For the application of simulations to
# linear mixed models, see Brysbaert y Stevens (2018, Journal of Cognition).

##### EFFECT SIZE AND META-ANALYSIS #####

# So far we have spoken of effect size in absolute terms, as the difference
# between the means. To ease comparisons across studies, it is better to use a
# standardized effect size, that is, to put the difference between the means in
# terms of the variability observed in the study.

# For a difference between means, a popular way to do this is Cohen's d: the
# difference between the means divided by global variability. It is possible to
# compute a CI surrounding d, the interval that
# will contain 95% of estimated d, including the populational d value. As we already know, the CI
# is sensitive to sample size and its width provides an indication of how precisely d has been estimated.

# d has a small bias, and a correction called Hedges' g is often used.

# Playing with the following simulation we can see how g changes as a function
# of the difference between means and sampling error.

nSims = 1000
n=100
M1=112
M2=100
SD1=30
SD2=30

hedgesgs <- numeric(nSims) # container for Hedge's gs
f <- rep(c("datosx1","datosx2"),each=n) # factor for contrast in the computation of Hedges' g

for(i in 1:nSims){
  datosx1<-rnorm(n = n, mean = M1, sd = SD1) # draw data
  datosx2<-rnorm(n = n, mean = M2, sd = SD2)
  d=c(datosx1,datosx2)
  hedgesgs[i]=cohen.d(d,f,hedges.correction = TRUE)$estimate # compute g
}

# plot of g-values
ggplot()  +
  geom_density(aes(x=hedgesgs), fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=mean(hedgesgs), colour="black", linetype="dashed") +
  geom_vline(xintercept=0, colour="blue", linetype="dashed") +
  xlab("g-values") + ylab("number of g-values")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(-1,2), ylim = c(0,4)) +
  annotate("text", x = 0, y = 4, label = "Null difference") +
  annotate("text", x = 2, vjust = "top", hjust = "right", y = 4, label =
             paste("nExps=",nSims,"\n","n=",n,"\n"," M1=",M1," M2=",M2,"\n","SD1=",SD1," SD2=",SD2, "\n", "g=",round(mean(hedgesgs),2))) +
  annotate("point", x = mean(hedgesgs), y = 0, size = 3) +
  annotate("errorbarh", x = mean(hedgesgs), y = 0, height = 0.3,
           xmin = (mean(hedgesgs)-(sd(hedgesgs)*1.96)),
           xmax = (mean(hedgesgs)+(sd(hedgesgs)*1.96)))

# We are going to see how a real effect of a fixed size can give rise to both
# significant and non-significant p-values. Let's simulate 10 experiments. We
# will use a common sample size of n=30 and common effect size of d=0.4.

# My comm. the dance of the p-value, from meta analysis movement.

nSims = 10
n=30
M1=112
M2=100
SD1=30
SD2=30

hedgesgs <- numeric(nSims) # container for Hedges' g
hedgesgSDs <- numeric(nSims) # container for their SDs
hedgesglowerCI <- numeric(nSims) # container for lower limit of the CI around Hedges' g
hedgesgupperCI <- numeric(nSims) # container for upper limit of the CI around Hedges' g
tvaluesp <-numeric(nSims)
varef <- numeric(nSims) # container for variances of g
f <- rep(c("datosx1","datosx2"),each=n) # factor for contrast in the computation of Hedges' g

for(i in 1:nSims){
  datosx1<-rnorm(n = n, mean = M1, sd = SD1)
  datosx2<-rnorm(n = n, mean = M2, sd = SD2)
  d=c(datosx1,datosx2)
  hedgesgs[i]=cohen.d(d,f,hedges.correction = TRUE)$estimate
  hedgesgSDs[i]=cohen.d(d,f,hedges.correction = TRUE)$sd
  hedgesglowerCI[i]=cohen.d(d,f,hedges.correction = TRUE)$conf.int["lower"]
  hedgesgupperCI[i]=cohen.d(d,f,hedges.correction = TRUE)$conf.int["upper"]
  tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value
  m1i <- mean(datosx1)
  m2i <- mean(datosx2)
  sd1i <- sd(datosx1)
  sd2i <- sd(datosx2)
  varef[i]=escalc(measure = "SMD", m1i = m1i,m2i = m2i, sd1i = sd1i, sd2i = sd2i, n1i = n, n2i = n)$vi
}

# plot of g-values, CIs, and p-values of the t-test

ggplot()  +
  geom_vline(xintercept=0, colour="blue", linetype="dashed") +
  xlab("effect size (Hedges' g)") + ylab("experiments")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(-0.5,3.5), ylim = c(0,10)) +
  annotate("text", x = 6, vjust = "top", hjust = "right", y = 0.8, label =
             paste("nExps=",nSims,"\n","n=",n,"\n"," M1=",M1," M2=",M2,"\n","SD1=",SD1," SD2=",SD2)) +
  annotate("point", x = hedgesgs[1], y = 10, size = 3) +
  annotate("errorbarh", x = hedgesgs[1], y = 10, height = 0.1,
           xmin = hedgesglowerCI[1],
           xmax = hedgesgupperCI[1]) +
  annotate("text", x = (hedgesgs[1]+1), hjust = "left", y = 10,
           label = paste("p=",round(tvaluesp[1],3))) +
  annotate("point", x = hedgesgs[2], y = 9, size = 3) +
  annotate("errorbarh", x = hedgesgs[2], y = 9, height = 0.1,
           xmin = hedgesglowerCI[2],
           xmax = hedgesgupperCI[2]) +
  annotate("text", x = (hedgesgs[2]+1), hjust = "left", y = 9,
           label = paste("p=",round(tvaluesp[2],3))) +
  annotate("point", x = hedgesgs[3], y = 8, size = 3) +
  annotate("errorbarh", x = hedgesgs[3], y = 8, height = 0.1,
           xmin = hedgesglowerCI[3],
           xmax = hedgesgupperCI[3]) +
  annotate("text", x = (hedgesgs[3]+1), hjust = "left", y = 8,
           label = paste("p=",round(tvaluesp[3],3))) +
  annotate("point", x = hedgesgs[4], y = 7, size = 3) +
  annotate("errorbarh", x = hedgesgs[4], y = 7, height = 0.1,
           xmin = hedgesglowerCI[4],
           xmax = hedgesgupperCI[4]) +
  annotate("text", x = (hedgesgs[4]+1), hjust = "left", y = 7,
           label = paste("p=",round(tvaluesp[4],3))) +
  annotate("point", x = hedgesgs[5], y = 6, size = 3) +
  annotate("errorbarh", x = hedgesgs[5], y = 6, height = 0.1,
           xmin = hedgesglowerCI[5],
           xmax = hedgesgupperCI[5]) +
  annotate("text", x = (hedgesgs[5]+1), hjust = "left", y = 6,
           label = paste("p=",round(tvaluesp[5],3))) +
  annotate("point", x = hedgesgs[6], y = 5, size = 3) +
  annotate("errorbarh", x = hedgesgs[6], y = 5, height = 0.1,
           xmin = hedgesglowerCI[6],
           xmax = hedgesgupperCI[6]) +
  annotate("text", x = (hedgesgs[6]+1), hjust = "left", y = 5,
           label = paste("p=",round(tvaluesp[6],3))) +
  annotate("point", x = hedgesgs[7], y = 4, size = 3) +
  annotate("errorbarh", x = hedgesgs[7], y = 4, height = 0.1,
           xmin = hedgesglowerCI[7],
           xmax = hedgesgupperCI[7]) +
  annotate("text", x = (hedgesgs[7]+1), hjust = "left", y = 4,
           label = paste("p=",round(tvaluesp[7],3))) +
  annotate("point", x = hedgesgs[8], y = 3, size = 3) +
  annotate("errorbarh", x = hedgesgs[8], y = 3, height = 0.1,
           xmin = hedgesglowerCI[8],
           xmax = hedgesgupperCI[8]) +
  annotate("text", x = (hedgesgs[8]+1), hjust = "left", y = 3,
           label = paste("p=",round(tvaluesp[8],3))) +
  annotate("point", x = hedgesgs[9], y = 2, size = 3) +
  annotate("errorbarh", x = hedgesgs[9], y = 2, height = 0.1,
           xmin = hedgesglowerCI[9],
           xmax = hedgesgupperCI[9]) +
  annotate("text", x = (hedgesgs[9]+1), hjust = "left", y = 2,
           label = paste("p=",round(tvaluesp[9],3))) +
  annotate("point", x = hedgesgs[10], y = 1, size = 3) +
  annotate("errorbarh", x = hedgesgs[10], y = 1, height = 0.1,
           xmin = hedgesglowerCI[10],
           xmax = hedgesgupperCI[10]) +
  annotate("text", x = (hedgesgs[10]+1), hjust = "left", y = 1,
           label = paste("p=",round(tvaluesp[10],3)))

# This is input data for a meta-analysis. In a meta-analysis, the effect sizes
# of the experiments are weighted by their precision (variability), and a
# meta-analytic effect size is computed.
# Let's see how a meta-analysis of these same 10 experiments looks like using
# the specific library.

meta <- rma(hedgesgs,vi = varef[i])

forest(meta)

# The "forest plot" lets us see the input effect sizes and CIs, and the output
# (meta-analytic) effect size and CI. We can see that it perfectly matches the populational
# effect size and tells us that it is significant (the CI does not cross zero).

# Let's now run a more realistic meta-analysis, with variation in sample sizes (from 20 to 200 participants).
# We will do it first with a real effect and then without any effect in the population.
# In a meta-analysis, studies with better precision (usually because of larger sample sizes) receive a greater weight.

nSims = 20
M1=112
M2=100
SD=30
samplemin=10 # minimum sample size
samplemax=200 # maximum sample size

hedgesgs <- numeric(nSims) # containers
esvars <- numeric(nSims)
tvaluesp <-numeric(nSims)

for(i in 1:nSims){
  n1=sample(samplemin:samplemax,1) # we randomly select a sample size between 20 and 200
  n2=n1 # the two groups have the same sample size
  datosx1<-rnorm(n = n1, mean = M1, sd = SD)
  datosx2<-rnorm(n = n2, mean = M2, sd = SD)
  m1 <- mean(datosx1)
  m2 <- mean(datosx2)
  sd1 <- sd(datosx1)
  sd2 <- sd(datosx2)
  es <- escalc(measure = "SMD", m1i = m1,m2i = m2, sd1i = sd1, sd2i = sd2, n1i = n1, n2i = n2)
  hedgesgs[i]=es$yi
  esvars[i]=es$vi
  tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value
}

study <- seq(1,nSims)
metadata <- data.frame(study,hedgesgs,esvars,tvaluesp)

meta <- rma(metadata$hedgesgs,vi = metadata$esvars)

forest(meta)

# As we can see, the meta-analytic effect size coincides with the real (populational) effect size.
# A meta-analysis can uncover a real effect even when several studies have thrown non-significant p-values.
# However, meta-analysis is only precise if all studies are included. In reality,
# studies with significant p-values have better chances of being published than non-significant studies
# (publication bias). Under conditions of publication bias, the meta-analytic effect size will also be a biased estimation
# of the real effect size.

# In the prior meta-analysis, let's select only the significant studies and re-run the meta-analysis:

metadata <- metadata %>%
  mutate(significant = ifelse(tvaluesp < 0.05,hedgesgs,NA))

meta <- rma(metadata$significant,vi = metadata$esvars)

forest(meta)
?forest
vignette(package = "metafor")

##### QUESTIONABLE RESEARCH PRACTICES AND PUBLICATION BIAS #####

# Psychology suffers nowadays of an important replicability crisis. The causes are multiple.
# At a methodological level, the more important causes are: 1) use of small sample sizes,
# without prior consideration of the required sample size to attain enough statistical power;
# 2) p-hacking, to increase the probability of obtaining a significant result;
# 3) HARKing, or hypothesizing after results are known.
# 3) publication bias in favour of studies with significant findings.
# As a result, there is an excess of false positive findings in the literature, well above
# the accepted 5%. Moreover, this implies an overestimation of effect sizes, as we saw above.

# The fact is that the use of small sample sizes, by itself, does not lead to a higher false alarm rate
# (as we showed above). However, studies with smaller samples are more susceptible to bias due to
# questionable research practices.


## INFLATION OF EFFECT SIZES DUE TO PUBLICATION BIAS AS A FUNCTION OF SAMPLE SIZE ##

# We saw above that publication bias (publishing only or mostly studies with significant findings)
# inflates meta-analytic estimates of effect size. Let's see how this happens more strongly
# when the studies have small than large sample sizes.

# Let's simulate 1000 studies using a large sample size (100 participants) and a null populational difference between conditions.

nSims = 1000
n=100
M1=100
M2=100
SD1=30
SD2=30

hedgesgs <- numeric(nSims)
tvalues <-numeric(nSims)
tvaluesp <-numeric(nSims)
esvars <- numeric(nSims)
study <- seq(1,nSims)

for(i in 1:nSims){
  datosx1<-rnorm(n = n, mean = M1, sd = SD1)
  datosx2<-rnorm(n = n, mean = M2, sd = SD2)
  tvalues[i]=t.test(datosx1, datosx2, alternative = "two.sided")$statistic
  tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value
  m1 <- mean(datosx1)
  m2 <- mean(datosx2)
  sd1 <- sd(datosx1)
  sd2 <- sd(datosx2)
  es <- escalc(measure = "SMD", m1i = m1,m2i = m2, sd1i = sd1, sd2i = sd2, n1i = n, n2i = n)
  hedgesgs[i]=es$yi
  esvars[i]=es$vi
}

datos <- data.frame(study,hedgesgs,esvars,tvalues,tvaluesp)

# We mark the studies with significant values, both taking and without taking into account the
# direction of the effect.

datos <- datos %>%
  mutate(significant = ifelse(tvaluesp < 0.05,hedgesgs,NA)) %>%
  mutate(significantneg = ifelse(tvaluesp < 0.05 & tvalues < 0,hedgesgs,NA)) %>%
  mutate(significantpos = ifelse(tvaluesp < 0.05 & tvalues > 0,hedgesgs,NA))

# plot of g-values without publication bias (blue) and with publication bias (black)

ggplot(data = datos)  +
  geom_density(aes(x = hedgesgs), fill=NA, colour="blue", size = 1) +
  annotate("point", x = mean(hedgesgs), y = -0.5, size = 5, colour = "blue") +
  annotate("errorbarh", x = mean(hedgesgs), y = -0.5, height = 0.3, colour = "blue",
           xmin = (mean(hedgesgs)-(sd(hedgesgs)*1.96)),
           xmax = (mean(hedgesgs)+(sd(hedgesgs)*1.96))) +

  geom_density(aes(x = significant), fill=NA, colour="black", size = 1) +

  annotate("point", x = mean(datos$significantneg, na.rm = TRUE), y = 0, size = 5) +
  annotate("errorbarh", x = mean(datos$significantneg, na.rm = TRUE), y = 0, height = 0.3,
           xmin = (mean(datos$significantneg, na.rm = TRUE)-(sd(datos$significantneg, na.rm = TRUE)*1.96)),
           xmax = (mean(datos$significantneg, na.rm = TRUE)+(sd(datos$significantneg, na.rm = TRUE)*1.96))) +

  annotate("point", x = mean(datos$significantpos, na.rm = TRUE), y = 0, size = 5) +
  annotate("errorbarh", x = mean(datos$significantpos, na.rm = TRUE), y = 0, height = 0.3,
           xmin = (mean(datos$significantpos, na.rm = TRUE)-(sd(datos$significantpos, na.rm = TRUE)*1.96)),
           xmax = (mean(datos$significantpos, na.rm = TRUE)+(sd(datos$significantpos, na.rm = TRUE)*1.96))) +

  geom_vline(xintercept=mean(hedgesgs), colour="black", linetype="dashed") +
  geom_vline(xintercept=0, colour="blue", linetype="dashed") +
  xlab("g-values") + ylab("number of g-values")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(-2,2), ylim = c(-0.5,5)) +
  annotate("text", x = 0, y = 4, label = "Null difference") +
  annotate("text", x = 2, vjust = "top", hjust = "right", y = 4, label =
             paste("nExps=",nSims,"\n","n=",n,"\n"," M1=",M1," M2=",M2,"\n","SD1=",SD1," SD2=",SD2))

# We now run a meta-analysis of only the significant results in the positive direction (M2 larger than M1)

meta <- rma(datos$significantpos,vi = datos$esvars)

forest(meta)

# We do the same with small samples (n=30)

nSims = 1000
n=30 # This is the only change
M1=100
M2=100
SD1=30
SD2=30

hedgesgs <- numeric(nSims)
tvalues <-numeric(nSims)
tvaluesp <-numeric(nSims)
esvars <- numeric(nSims)
study <- seq(1,nSims)

for(i in 1:nSims){
  datosx1<-rnorm(n = n, mean = M1, sd = SD1)
  datosx2<-rnorm(n = n, mean = M2, sd = SD2)
  tvalues[i]=t.test(datosx1, datosx2, alternative = "two.sided")$statistic
  tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value
  m1 <- mean(datosx1)
  m2 <- mean(datosx2)
  sd1 <- sd(datosx1)
  sd2 <- sd(datosx2)
  es <- escalc(measure = "SMD", m1i = m1,m2i = m2, sd1i = sd1, sd2i = sd2, n1i = n, n2i = n)
  hedgesgs[i]=es$yi
  esvars[i]=es$vi
}

datos <- data.frame(study,hedgesgs,esvars,tvalues,tvaluesp)

# # We mark the studies with significant values, both taking and without taking into account the
# direction of the effect.

datos <- datos %>%
  mutate(significant = ifelse(tvaluesp < 0.05,hedgesgs,NA)) %>%
  mutate(significantneg = ifelse(tvaluesp < 0.05 & tvalues < 0,hedgesgs,NA)) %>%
  mutate(significantpos = ifelse(tvaluesp < 0.05 & tvalues > 0,hedgesgs,NA))

# plot of g-values without publication bias (blue) and with publication bias (black)

ggplot(data = datos)  +
  geom_density(aes(x = hedgesgs), fill=NA, colour="blue", size = 1) +
  annotate("point", x = mean(hedgesgs), y = -0.5, size = 5, colour = "blue") +
  annotate("errorbarh", x = mean(hedgesgs), y = -0.5, height = 0.3, colour = "blue",
           xmin = (mean(hedgesgs)-(sd(hedgesgs)*1.96)),
           xmax = (mean(hedgesgs)+(sd(hedgesgs)*1.96))) +

  geom_density(aes(x = significant), fill=NA, colour="black", size = 1) +

  annotate("point", x = mean(datos$significantneg, na.rm = TRUE), y = 0, size = 5) +
  annotate("errorbarh", x = mean(datos$significantneg, na.rm = TRUE), y = 0, height = 0.3,
           xmin = (mean(datos$significantneg, na.rm = TRUE)-(sd(datos$significantneg, na.rm = TRUE)*1.96)),
           xmax = (mean(datos$significantneg, na.rm = TRUE)+(sd(datos$significantneg, na.rm = TRUE)*1.96))) +

  annotate("point", x = mean(datos$significantpos, na.rm = TRUE), y = 0, size = 5) +
  annotate("errorbarh", x = mean(datos$significantpos, na.rm = TRUE), y = 0, height = 0.3,
           xmin = (mean(datos$significantpos, na.rm = TRUE)-(sd(datos$significantpos, na.rm = TRUE)*1.96)),
           xmax = (mean(datos$significantpos, na.rm = TRUE)+(sd(datos$significantpos, na.rm = TRUE)*1.96))) +

  geom_vline(xintercept=mean(hedgesgs), colour="black", linetype="dashed") +
  geom_vline(xintercept=0, colour="blue", linetype="dashed") +
  xlab("g-values") + ylab("number of g-values")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim=c(-2,2), ylim = c(-0.5,5)) +
  annotate("text", x = 0, y = 4, label = "Null difference") +
  annotate("text", x = 2, vjust = "top", hjust = "right", y = 4, label =
             paste("nExps=",nSims,"\n","n=",n,"\n"," M1=",M1," M2=",M2,"\n","SD1=",SD1," SD2=",SD2))

# We now run a meta-analysis with only significant results in the positive direction (M2 > M1)

meta <- rma(datos$significantpos,vi = datos$esvars)

forest(meta)

# We can see that when the samples are small, the estimated effect size is
# larger (and remember that there is no real effect whatsoever in the
# population in these simulations).


## OPTIONAL STOPPING IN DATA COLLECTION ##

# Another questionable practice is called "optional stopping": collect a
# sample, check whether p is significant, if not, collect another batch of
# data, check p again, stop if p is significant or carry on with data
# collection.

# Let's see how this practice increases the false alarm rate.

sampleSizeIncrement = 5
nSampleSizes = 10 # Number of sample size increments that we are going to evaluate.
n = 4 # Starting sample size
M1=100 # There is no real effect in the population
M2=100
SD1=30
SD2=30

tvaluesp <-numeric(nSampleSizes)
samplesize <- numeric(nSampleSizes)

datosx1<-rnorm(n = n, mean = M1, sd = SD1) # we draw the data of the starting sample
datosx2<-rnorm(n = n, mean = M2, sd = SD2)
tvaluesp[1]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value
samplesize[1]=n

for(i in 2:nSampleSizes){ # for each simulated experiment
    datosx1mas<-rnorm(n = sampleSizeIncrement, mean = M1, sd = SD1) # draw new data
    datosx2mas<-rnorm(n = sampleSizeIncrement, mean = M2, sd = SD2)
    datosx1 <- c(datosx1,datosx1mas)  # add the new data to the prior data
    datosx2 <- c(datosx2,datosx2mas)
    tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value # compute p-value
    samplesize[i]=samplesize[i-1]+sampleSizeIncrement
  }

datos <- data.frame(samplesize,tvaluesp)

ggplot(data = datos, aes(x=samplesize, y=tvaluesp)) +
  geom_line() +
  theme_base() +
  geom_hline(yintercept=0.05, linetype="dashed") +
  xlab("sample size") + ylab("p-value")  + theme_base() +
  coord_cartesian(ylim = c(0,1)) +
  annotate("text", x = samplesize[nSampleSizes], y = 0.10, label = "alpha", hjust = "right")

# How much does optional stopping inflate the false alarm rate? We repeat the prior simulation
# many times and count the number of significant findings (remember that there is no real effect).

nSims = 10000
sampleSizeIncrement = 5
nSampleSizes = 10
n = 30
M1=100
M2=100
SD1=30
SD2=30

tvaluesp <-numeric(nSampleSizes)
samplesize <- numeric(nSampleSizes)
finaltvaluesp <- numeric(nSims) # container for final p-values of each simulation

for (s in 1:nSims){
  datosx1<-rnorm(n = n, mean = M1, sd = SD1)
  datosx2<-rnorm(n = n, mean = M2, sd = SD2)
  tvaluesp[1]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value
  samplesize[1]=n
  for(i in 2:nSampleSizes){
    datosx1mas<-rnorm(n = sampleSizeIncrement, mean = M1, sd = SD1)
    datosx2mas<-rnorm(n = sampleSizeIncrement, mean = M2, sd = SD2)
    datosx1 <- c(datosx1,datosx1mas)
    datosx2 <- c(datosx2,datosx2mas)
    tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value
    samplesize[i]=samplesize[i-1]+sampleSizeIncrement
  }
  finaltvaluesp[s] = min(tvaluesp)
}

# plot of p-values
# Remember: if there is no real effect, we should expect a flat distribution
# of p-values, with exactly 5% of them below the significance threshold (5%).

ggplot()  +
  geom_density(aes(x=finaltvaluesp), fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=0.05, colour="blue", linetype="dashed") +
  xlab("p-values") + ylab("number of p-values")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  annotate("text", x = 0.06, hjust = "left", y = 15, label = "p= 0.05") +
  annotate("text", x = 1, vjust = "top", hjust = "right", y = 15, label =
             paste("nExps=",nSims,"\n","n=",n,"\n"," M1=",M1," M2=",M2,"\n","SD1=",SD1," SD2=",SD2,"\n",
                   "p<.05=",sum(finaltvaluesp < 0.05)*100/nSims))

# There is an important increase in false alarms. Playing with the prior simulation
# you can see that the main factor that affects the false alarm rate is the number of times
# that the sample is increased and the p checked: the more you check the p, the more
# likely it is that there will be a significant finding just by chance.

# We can also see that the greater the initial sample size, the smaller the effect of this
# strategy, as larger samples provide more precise estimations and are less
# affected by optional stopping. But even in the case of very large initial
# samples (200 per group), optional stopping increases false alarms beyond 5%.


## GREATER SENSITIVITY OF SMALL SAMPLES TO P-HACKING ##

# One way to p-hack is to tamper with outliers.
# In this simulation we will filter out only one participant from each group. This "outlier" will
# be the participant with the most extreme value in the direction that favours a greater difference
# between the means. Its value will be replaced by the mean value of the group (a common practice).

# Let's start using large samples. In these simulations there is no real
# effect in the population.

nSims = 10000
n=100
M1=100
M2=100
SD1=30
SD2=30

tvaluesp <-numeric(nSims)

for(i in 1:nSims){
  datosx1<-rnorm(n = n, mean = M1, sd = SD1)
  datosx2<-rnorm(n = n, mean = M2, sd = SD2)
  if (mean(datosx1) < mean(datosx2)) { # we find and remove the outlier in each group
    datosx1[datosx1 == max(datosx1)] = mean(datosx1)
    datosx2[datosx2 == min(datosx2)] = mean(datosx2)
  } else {
    datosx1[datosx1 == min(datosx1)] = mean(datosx1)
    datosx2[datosx2 == max(datosx2)] = mean(datosx2)
  }
  tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value
}

# plot of p-values

ggplot()  +
  geom_density(aes(x=tvaluesp), fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=0.05, colour="blue", linetype="dashed") +
  xlab("p-values") + ylab("number of p-values")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  annotate("text", x = 0.06, hjust = "left", y = 15, label = "p= 0.05") +
  annotate("text", x = 1, vjust = "top", hjust = "right", y = 15, label =
             paste("nExps=",nSims,"\n","n=",n,"\n"," M1=",M1," M2=",M2,"\n","SD1=",SD1," SD2=",SD2,"\n",
                   "p<.05=",sum(tvaluesp < 0.05)*100/nSims))

# Let's see what happens with small samples

nSims = 10000
n=30 # This is the only change
M1=100
M2=100
SD1=30
SD2=30

tvaluesp <-numeric(nSims)

for(i in 1:nSims){
  datosx1<-rnorm(n = n, mean = M1, sd = SD1)
  datosx2<-rnorm(n = n, mean = M2, sd = SD2)
  if (mean(datosx1) < mean(datosx2)) {
    datosx1[datosx1 == max(datosx1)] = mean(datosx1)
    datosx2[datosx2 == min(datosx2)] = mean(datosx2)
  } else {
    datosx1[datosx1 == min(datosx1)] = mean(datosx1)
    datosx2[datosx2 == max(datosx2)] = mean(datosx2)
  }
  tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value
}

# plot of p-values

ggplot()  +
  geom_density(aes(x=tvaluesp), fill=NA, colour="black", size = 1) +
  geom_vline(xintercept=0.05, colour="blue", linetype="dashed") +
  xlab("p-value associated to the t-value") + ylab("number of p-values")  + theme_base() +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) +
  annotate("text", x = 0.06, hjust = "left", y = 15, label = "p= 0.05") +
  annotate("text", x = 1, vjust = "top", hjust = "right", y = 15, label =
             paste("nExps=",nSims,"\n","n=",n,"\n"," M1=",M1," M2=",M2,"\n","SD1=",SD1," SD2=",SD2,"\n",
                   "p<.05=",sum(tvaluesp < 0.05)*100/nSims))

# Let's see how the false alarm rate varies with sample size. We repeat the simulation for
# a range of sample sizes and count the amount of significant p-values.
# In this case we will also compute a 95% CI around the estimated false alarm rate for each sample size.

nSims = 1000
nSampleSizes = 20
nSampleSizesRep = 20
sampleSizeIncrement = 10
n=10
M1=100
M2=100
SD1=30
SD2=30

samplesize <- seq(n,(nSampleSizes*sampleSizeIncrement),by=sampleSizeIncrement)

tvaluesp <-numeric(nSims)
FAvalues <- numeric(nSampleSizesRep)
FAvaluesmean <- numeric(nSampleSizes)
FAvaluessd <- numeric(nSampleSizes)

for (s in 1:nSampleSizes) {#for each sample size
  for (r in 1:nSampleSizesRep){ #for each simulation set within each sample size
    for(i in 1:nSims){ #for each simulated experiment
      datosx1<-rnorm(n = n, mean = M1, sd = SD1) # draw data
      datosx2<-rnorm(n = n, mean = M2, sd = SD2)
      if (mean(datosx1) < mean(datosx2)) { # remove outliers
        datosx1[datosx1 == max(datosx1)] = mean(datosx1)
        datosx2[datosx2 == min(datosx2)] = mean(datosx2)
      } else if (mean(datosx1) > mean(datosx2)) {
        datosx1[datosx1 == min(datosx1)] = mean(datosx1)
        datosx2[datosx2 == max(datosx2)] = mean(datosx2)
      }
      tvaluesp[i]=t.test(datosx1, datosx2, alternative = "two.sided")$p.value
    }
    # The FA rate of this design is
    FAvalues[r]=sum(tvaluesp < 0.05)*100/nSims
  }
  FAvaluesmean[s]=mean(FAvalues)
  FAvaluessd[s]=sd(FAvalues)
  n = n + sampleSizeIncrement
}

FAdatos <- data.frame(samplesize,FAvaluesmean,FAvaluessd)

FAdatos <- FAdatos %>%
  mutate(FAvaluesminCI=FAvaluesmean-1.96*FAvaluessd) %>%
  mutate(FAvaluesmaxCI=FAvaluesmean+1.96*FAvaluessd)

# plot of FA rate
ggplot(data = FAdatos, aes(x=samplesize, y=FAvaluesmean, ymin=FAvaluesminCI, ymax = FAvaluesmaxCI))  +
  geom_point(colour="black", size = 1) +
  geom_errorbar(width = 5) +
  geom_line() +
  xlab("sample size per group") + ylab("false alarm rate")  + theme_base() +
  geom_hline(yintercept=5, colour="black", linetype="dashed") +
  annotate("text", x = 5, hjust = "left", y = 7, label = "5% false alarms")

## Correlational designs.
## Do the same simulations ...

## multivariate...

## play with it...

## Find a way to ask the Q: "Is these particular issues equally or more problematic
# in correlational designs?"

# Plan ahead and follow your plan.
# Restrict experimenters freedom.
# Pre-registration of hypotheses
# design, conditions and measures
# analysis and critical tests
# trimming and pre-prosessing of data
# objective criteria for subject and data removal
# expected effect size and required sample size to detect it with a given
# statistical power.
# data collection stopping rules.
# Check several time as long as the total alpha level is below 5%.

# Public protocol.
# check aspredicted.com

# aspredicted is more precise than putting protocols into papers.
# If not sticking to this... having to argue.

# Exploratory findings... p-values doesn't make much sense in explorative
# analysis.
# Confirmatory

# If you want to present a non-finding, make sure you had enough power.
# sort of sandwich t-test
# significantly different from...
# set your sample size ... significantly smaller than the smallest effect of
# interest.

# fidelity - do people do what we ask them to do.
# case analysis. Effect of doers or non-doers. Exclusion criteria.
# predict control doer... case analysis, predict T and C. Subjective criteria..
# false alarm rate shouldn't be affected by this.

# Share material... Sharing materials, code, data.
# OSF.io sharing etc. same as above.

# Remove publication bias.

# Chris Chambers. Registered Reports. Cortex.
# Get rewiers to review procedures.

# You will be published no matter what you get.
# peercommunity.org PCIRR.

# Then, can't I explore?
# Distinguish clearly between confirmatory vs. exploratory research.
# In exploration, p-values are a mere hint.
# Exploratory research must be followed by confirmatory research.

