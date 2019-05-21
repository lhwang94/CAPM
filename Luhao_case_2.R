#############################################################################
# Author: Group 1
#############################################################################
rm(list=ls())                             #clearing the workspace

setwd("Users/wangluhao/Downloads")        #point to the directory
#############################################################################
# Sourcing written function
source("./univariate_ols2.R")
#############################################################################
# Install/Load packages
library(readxl)
library(openxlsx)

# clear the command window
clr <- function(){cat(rep("\n", 50))}
clr()

# Determing the significance
sig <- 0.05

# Loading excel
data <- data.frame(read_excel("25_FF_portfolios.xls"))

# Isolating sxbx's returns
port_ret <- data[,2:26]

# Isolating market excess returns
mkt_exc <- data[,27]

# Isolating rist-free rate
rf <- data[,28]

# Constructing excess returns for all the 25 portfolios
port_exc_ret <- port_ret - rf

# the numbers of observations for the market returns
n <- dim(port_exc_ret)[1]


#OLS
reg_results_matrix<-matrix(NA,dim(port_exc_ret)[2],4)
colnames(reg_results_matrix)<-c("a_est","b_est","se_alpha","se_beta")

for(i in seq(1:dim(port_exc_ret)[2])) {
  reg_results_matrix[i,] <- univariate_ols(port_exc_ret[,i], mkt_exc)
}

#Test the null hpothese
t_test <- (reg_results_matrix[,1]-0)/reg_results_matrix[,3]
# critical value of t
t_critical <- qt(1-sig/2, n-2)
# critical value of norm
norm_critical <- qnorm(1-sig/2)
#test the null hypothesis H0: a_p = 0
t_test > t_critical
t_test > norm_critical
#interval when using t_test
alpha_low_t<-reg_results_matrix[,"a_est"]-reg_results_matrix[,"se_alpha"]*qt(1-sig/2,n-2)
alpha_high_t<-reg_results_matrix[,"a_est"]+reg_results_matrix[,"se_alpha"]*qt(1-sig/2,n-2)
alpha_CI_t <- cbind (alpha_low_t, alpha_high_t)
#interval  value of norm
alpha_low_norm <- reg_results_matrix[,"a_est"]-reg_results_matrix[,"se_alpha"]*qnorm(1-sig/2)
alpha_high_norm <- reg_results_matrix[,"a_est"]+reg_results_matrix[,"se_alpha"]*qnorm(1-sig/2)
alpha_CI_norn <- cbind (alpha_low_norm, alpha_high_norm)
#p_value when using t_test
p_val_vec_alpha_t<-(1-pt(abs(t_test),n-2))*2
#p_value when using norm_test
p_val_vec_alpha_norm <- (1-pnorm(abs(t_test)))*2
#plot
plot(reg_results_matrix[,"b_est"], colMeans(port_exc_ret),type="p", 
     col="blue",xlab="Beta", ylab="Average Returns",lwd = 4)
#find the portfolio performing best
sharp_vec <- colMeans(port_exc_ret)/reg_results_matrix[,"b_est"]
names(sharp_vec)[sharp_vec==max(sharp_vec)]
#find the portfolio performing worst
names(sharp_vec)[sharp_vec==min(sharp_vec)]
plot(sharp_vec)
#convert the sharp ratio vector to matrix
sharp_ma <- matrix(sharp_vec,nrow = 5,ncol = 5)
colnames(sharp_ma) <- c("s1","s2","s3","s4","s5")
rownames(sharp_ma) <- c("b1","b2","b3","b4","b5")
#the pattern of var
var_by_size <- c(var(sharp_ma[,1]),var(sharp_ma[,2]),var(sharp_ma[,3]),var(sharp_ma[,4]),var(sharp_ma[,5]))
var_by_btm <- c(var(sharp_ma[1,]),var(sharp_ma[2,]),var(sharp_ma[3,]),var(sharp_ma[4,]),var(sharp_ma[5,]))
#the pattern of return
return_by_size <- c(mean(sharp_ma[,1]),mean(sharp_ma[,2]),mean(sharp_ma[,3]),mean(sharp_ma[,4]),mean(sharp_ma[,5]))
return_by_btm <- c(mean(sharp_ma[1,]),mean(sharp_ma[2,]),mean(sharp_ma[3,]),mean(sharp_ma[4,]),mean(sharp_ma[5,]))
# Storing the results in a single matrix
alpha_results<-round(rbind(
  t(reg_results_matrix[,"a_est"])*12*100,
  t(t_test),
  t(p_val_vec_alpha_t),
  t(p_val_vec_alpha_norm)),3)

rownames(alpha_results)<-c("Alpha Estimates",
                           "T-Statistics",
                           "P-Values using t",
                           "P-Values using norm")

# Exporting the results as a .csv file
write.xlsx(alpha_results,'alpha_results.xlsx',
           col.names=TRUE, row.names=TRUE)
