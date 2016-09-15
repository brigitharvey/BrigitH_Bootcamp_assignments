for (ii in 1:5) {cat("\nthe number is", ii)}
for (ii in 1:8) {cat("\n", ii)}
for (ii in 1:8) {cat("\n*", ii)}
for (* in 1:8) {cat("n", ii)}
* <- 8
x <- 1:9
for (ii in x) { if (ii<9){cat(ii, "*\n")} else{cat(ii, "*\n")}}
for (ii in x) { if (ii<9) {cat(ii, "\n")} else {cat(ii, "*\n")}}

#Exercise 1 Answer
for (ii in x) { if (ii<9) {cat("\n")} else {cat("*\n")}}
# ii is an integer to represent 1 through 9

x <- 1:10
for (ii in x) {if (ii<10) {cat("*\n")} else {cat("*\n")}}
# gives 10 asterisks
#want asterisks at odd, 10 of them, and the 9 in between are &, even
x <- 1:19
for (ii in x) {if (ii%%2){cat("*")} else {cat("&")}}

#Exercise 2 Answer
for (ii in 1:19) {if (ii%%2) {cat("*")} else {cat("&")}}
#ii%%2 results in odd outputs

#Exercise 3
#Initial value for the variable dog is 10
#Value at start of loop is 11
#Value at end of loop is 15

#Initial value for meatloaf is 0
#Value at start of loop is -4
#Value at end of loop is -30

#Initial value for bubbles is 12
#Value at start of loop is -1
#Value at end of loop is -4

#Exercise 4 Answer
years <- c( 2015, 2016, 2018, 2020, 2021)
for(ii in 1:length(years)){
  if(years[ii] %% 2 == 0){
    cat(years[ii], 'Hooray, congressional elections!', sep = '\t', fill = T)}
    if(years[ii] %% 4 ==0) {cat(years[ii], 'Hooray, presidential elections!', sep = '\t', fill = T)}
}
# ii(the numbers provided) %% 2=0 shows that only the ones with a remainder of 0 when 2 goes into them (ie even years) gives message
# ii(the numbers provided) %% 4=0 shows tht only the ones with a remainder of 0 when 4 goes into them (ie every four years) gives message

#Exercise 5 Answer
bankAccounts <- c(10, 9.2, 5.6, 3.7, 8.8, 0.5)
compounded <- rep(0,6)
interestRate <- 0.0125;
for (i in 1:length(bankAccounts)) {
  compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i]; }

#Exercise 6 Answer
bankAccounts <- c(10, 9.2, 5.6); #define bank accounts here, 3 people
interestRate <- 0.0525;   
house <- c(4.8, 3.8, 5.7); #deduct
food<- c(3.5, 4.3, 5.0);    #deduct
fun <- c(7.8, 2.1, 10.5);  #deduct
#and incomes (through TAships) of 
income <- c(21, 21, 21); #add this

#j is tracking year and i is tracking person
 
for (j in 1:5) {
  for (i in 1:length(bankAccounts)) {
    bankAccounts[i]<-(bankAccounts[i]+income[i]-house[i]-food[i]-fun[i])*(1+interestRate)
    }
  }


#Exercise 7 Answer
house <- c(4.8, 3.8, 5.7) 
food<- c(3.5, 4.3, 5.0) 
fun <- c(7.8, 2.1, 10.5)
income <- c(21, 21, 21)

years<- c(2015,2016,2017,2018,2019,2020)
for (j in 1:length(years)) {
  if(years[j] %% 2 ==1) {
    bankAccounts[1]= bankAccounts[1]+5000
    bankAccounts[3]= bankAccounts[3]+5000
  }
}

#Exercise 8 Answer
xsum<-0
x<-1
while(x<=17) {
  xsum=xsum+x
  x=x+1
}
#run all together as a clump

#Exercise 9 Answer
numbers <- function(x){
  if (x<=-1) {
    cat ("small")
  }
else if (x>-1&&x<1) {
  cat ("medium")
}
else {
  cat("big")
}
  }
