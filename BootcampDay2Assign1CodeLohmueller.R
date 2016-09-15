#Assignment 1 Bootcamp Day 2

###Question 1###############
hap<-read.table('BootcampDay2hapmap.txt', header=T) ##need to establish our table with data into the Global Environment
compute_chisquare=function(x){ ## creating a function to be computed a chisquare
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x))) #minor allele frequency
  cnt0=sum(x==0,na.rm=TRUE) #counts of the types of genotypes
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2) #vector of observed counts
  #print(obscnts)
  n=sum(obscnts) #summing across all observed counts
  expcnts=c((1-freq)^2,2*freq*(1-freq),freq^2)*n #expected counts if HardyWeinberg were true, freq of the minor allele/ of the 2 homozygous genotype, 1-freq of other homozygote
  chisq=sum((obscnts-expcnts)^2/expcnts) #according to equation
  return(chisq) #gives the statistic to you
}

snps=as.matrix(hap) #define snps data matrix of total data

chisqs=apply(snps,1,compute_chisquare)  #compute the snps

pvals=pchisq(chisqs,1,lower.tail=FALSE) #assigning the lower end to pvalues 
##above is answer to part a

##part b answer is below
signifthres<-0.05
sum(pvals<signifthres)
length(pvals)
sum((pvals<signifthres))/(length(pvals)) ##proportion of pvalues that are <0.05 = 0.04509218

signifthressmaller<-0.01
sum(pvals<signifthressmaller)/(length(pvals)) ##proportion of pvalues that are <0.01 = 0.01021425

signifthressmallest<-0.001
sum(pvals<signifthressmallest)/(length(pvals)) ##proportion of pvalues that are <0.001 = 0.00124564; yes

##part c answer is below
num_pval<-length(pvals) ##there are 4014 SNPS that were tested for departure from H-W equilibrium

##part d answer is below

exp_pvals<-(seq(1,num_pval, by=1))/num_pval #sequence over a stationary number

##part e answer is below

sort_pvals<-sort(pvals, decreasing=FALSE)

## part f answer is below
log_sort_pvals<-(-log10(sort_pvals))
log_exp_pvals<-(-log10(exp_pvals))

## part g answer is below
plot(log_exp_pvals, log_sort_pvals, xlab= "-log10(expected P-value)", ylab= "-log10(observed P-value)", pch=19)

## part h answer is below
abline(0,1, h=5, col=2, lty=2)
# 0 is intercept, 1 is slope

##part i is uploaded to homework file

###Question 2 ###############

# Answer for a
zz<-read.table("BootcampDay2phenotypes.txt", header= TRUE)

#Answer for b

quantile(zz$glucose_mmolperL,0.25) # 25% of the individuals are below 4.768756

#Answer for c

quantile(zz$glucose_mmolperL, 0.75) #75% of the individuals are below 7.354975

#Answer for d
hist(zz$glucose_mmolperL, xlab= "glucose (mm/L)", main= "Histogram", las=1)
lowtail<- quantile(zz$glucose_mmolperL,0.25)
hightail<- quantile(zz$glucose_mmolperL, 0.75)
abline(v=lowtail, col=4)
abline(v=hightail, col=2)
