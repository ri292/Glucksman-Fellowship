
# Load libraries

library(leaps)
library(car)
library(broom)
library(zoo)
library(RcppRoll)

lmOut <- function(res, file="test.csv", ndigit=3, writecsv=T) {
  # If summary has not been run on the model then run summary
  if (length(grep("summary", class(res)))==0) res <- summary(res)
  
  co <- res$coefficients
  nvar <- nrow(co)
  ncol <- ncol(co)
  f <- res$fstatistic
  formatter <- function(x) format(round(x,ndigit),nsmall=ndigit)
  
  # This sets the number of rows before we start recording the coefficients
  nstats <- 4
  
  # G matrix stores data for output
  G <- matrix("", nrow=nvar+nstats, ncol=ncol+1)
  
  G[1,1] <- toString(res$call)
  
  # Save rownames and colnames
  G[(nstats+1):(nvar+nstats),1] <- rownames(co)
  G[nstats, 2:(ncol+1)] <- colnames(co)
  
  # Save Coefficients
  G[(nstats+1):(nvar+nstats), 2:(ncol+1)] <- formatter(co)
  
  # Save F-stat
  G[1,2] <- paste0("F(",f[2],",",f[3],")")
  G[2,2] <- formatter(f[1])
  
  # Save F-p value
  G[1,3] <- "Prob > P"
  G[2,3] <- formatter(1-pf(f[1],f[2],f[3]))
  
  # Save R2
  G[1,4] <- "R-Squared"
  G[2,4] <- formatter(res$r.squared)
  
  # Save Adj-R2
  G[1,5] <- "Adj-R2"
  G[2,5] <- formatter(res$adj.r.squared)
  
  print(G)
  if (writecsv) write.csv(G, file=file, row.names=F)
}

# Create single file with all regression outputs
regression_consolidated <- data.frame(ticker=character(0),alpha=character(0),beta=character(0),stringsAsFactors=FALSE)
ar_file <- data.frame(ticker=character(0),'tm10'=character(0),'tm9'=character(0),'tm8'=character(0),'tm7'=character(0),'tm6'=character(0),'tm5'=character(0),'tm4'=character(0),'tm3'=character(0),'tm2'=character(0),'tm1'=character(0),'t0'=character(0),'tp1'=character(0),'tp2'=character(0),'tp3'=character(0),'tp4'=character(0),'tp5'=character(0),'tp6'=character(0),'tp7'=character(0),'tp8'=character(0),'tp9'=character(0),'tp10'=character(0),stringsAsFactors=FALSE)
car_file <- data.frame(ticker=character(0),'tm10'=character(0),'tm9'=character(0),'tm8'=character(0),'tm7'=character(0),'tm6'=character(0),'tm5'=character(0),'tm4'=character(0),'tm3'=character(0),'tm2'=character(0),'tm1'=character(0),'t0'=character(0),'tp1'=character(0),'tp2'=character(0),'tp3'=character(0),'tp4'=character(0),'tp5'=character(0),'tp6'=character(0),'tp7'=character(0),'tp8'=character(0),'tp9'=character(0),'tp10'=character(0),stringsAsFactors=FALSE)
car_file_11 <- data.frame(ticker=character(0),'t0'=character(0),'tp1'=character(0),'tp2'=character(0),'tp3'=character(0),'tp4'=character(0),'tp5'=character(0),'tp6'=character(0),'tp7'=character(0),'tp8'=character(0),'tp9'=character(0),'tp10'=character(0),stringsAsFactors=FALSE)
r2_list <- data.frame(ticker=character(0),r2=character(0),stringsAsFactors=FALSE)

consolidated_counter <- 0

companies <- list('mgm','msft1','fb1','msft2','tmus1','m','adbe','el','znga','chh','pson','cof','s','dgx1','faf','justdial','hig','7203','fb2','jwn','wbc','sgms','googl1','mar','cno','293','googl2','grmn','fb3','chgg','iag','ac','dri','tmus2','nlok','bmo','cmcsa1','nuan','sti','ingn','dal','hbc','uaa','expe','fdx','ry','bce1','pypl','yum','h1','fpay','efx','ucg','wfc','nwsa','vz1','wwe','shldq','bce2','sabr','rad','gme','pay','ihg','yhoo','dgx2','dis','2353','somc','kr','adp','vz2','baba','wen','cnc','twc','h2','8136','303','hot','cmcsa2','talk','tmus3','hlt','moh','azo','dc','akrx','sbh','ngvc','antm','ms')
filepath_input <- "c:/users/riazu/Google Drive/Glucksman Fellowship/22 Stock Price Prediction/01 Regression Input/"
filepath_output <- "c:/users/riazu/Google Drive/Glucksman Fellowship/23 Stock Return Prediction/02 Prediction Output/"
filepath_reg_results <- "c:/users/riazu/Google Drive/Glucksman Fellowship/23 Stock Return Prediction/03 Regression Results/"
filepath_abnormal <- "c:/users/riazu/Google Drive/Glucksman Fellowship/23 Stock Return Prediction/04 ARs & CARs/"


#filename <- 'cof'

for (filename in companies) {

consolidated_counter <- consolidated_counter+1

data <- read.csv(paste0(filepath_input,filename,".csv"))
reg_data <- data[1:60,]

summary(reg_data)

# Create linear model
#reg_dataa <- lm(reg_data$stock_return ~ reg_data$market_return)
#reg_dataa <- lm(stock_return ~ market_return, data=reg_data)
reg_dataa <- lm(stock_return ~ rmrf, data=reg_data)
summary(reg_dataa)

# Calculate VIFs
#vif(reg_dataa)

# Plot diagnostics
stdres <- rstandard(reg_dataa)
plot(fitted(reg_dataa),stdres,xlab="Fitted values",ylab="Standardized residuals",type="b")
title(main = "Fitted values vs. Standardized residuals")
qqnorm(stdres)
hist(stdres)

lmOut(reg_dataa, file=paste0(filepath_reg_results,filename," reg output.csv"))

regression_consolidated[consolidated_counter,'ticker'] <- filename
regression_consolidated[consolidated_counter,'alpha'] <- coef(reg_dataa)["(Intercept)"]
regression_consolidated[consolidated_counter,'beta'] <- coef(reg_dataa)["rmrf"]

r2_list[consolidated_counter,'ticker'] <- filename
r2_list[consolidated_counter, 'r2'] <- summary(reg_dataa)$r.squared

# Predict stock price using model selected

predict_data <- data

# Fixed regression / prediction data having two different lengths: https://datascience.stackexchange.com/questions/52425/r-newdata-has-x-rows-but-variables-have-x-rows
predict_data1 <- predict(reg_dataa, newdata=predict_data)

# Abnormal return calculation: https://en.wikipedia.org/wiki/Abnormal_return#Cumulative_abnormal_return

predict_data$predict_stock_return <- predict_data1
predict_data$abnormal_return <- (predict_data$stock_return - predict_data$predict_stock_return)

# Split and stack data using rbind: http://www.talkstats.com/threads/append-two-data-frames-on-top-of-each-other.21373/

predict_data$cumulative_ar <- NA
predict_data_split <- predict_data[81:101,]
predict_data_split$cumulative_ar <- cumsum(predict_data_split$abnormal_return)
predict_data <- predict_data[1:80,]
predict_data <- rbind(predict_data, predict_data_split)

predict_data$cumulative_ar_11 <- NA
predict_data_split <- predict_data[91:101,]
predict_data_split$cumulative_ar_11 <- cumsum(predict_data_split$abnormal_return)
predict_data <- predict_data[1:90,]
predict_data <- rbind(predict_data, predict_data_split)


predict_data$newdate = as.Date(predict_data$date,origin = "1899-12-30")

# Output AR graphs to jpegs

jpeg(paste(filepath_output,filename,' ARs.jpg',sep=""))
plot(predict_data$newdate,predict_data$abnormal_return,type="b",main=paste("Errors (ARs) in ",toupper(filename)," stock return prediction (CAPM)",sep=""),xlab="Date",ylab="Actual - Predicted stock return")
abline(v=predict_data$newdate[90],lty=2,lwd=3)
abline(v=predict_data$newdate[80],lty=2,lwd=1)

abline(h=0,lty=2,lwd=3)
dev.off()

# Output CAR graphs to jpegs
# Use both plots and lines: https://www.r-graph-gallery.com/connected-scatterplot.html

predict_data_split <- predict_data[81:101,]

jpeg(paste(filepath_output,filename,' CARs.jpg',sep=""))
plot(predict_data_split$newdate,predict_data_split$cumulative_ar,type="b",main=paste("Errors (CARs) in ",toupper(filename)," stock return prediction (CAPM)",sep=""),xlab="Date",ylab="Cumulative Actual - Predicted stock return")
abline(v=predict_data_split$newdate[90],lty=2,lwd=3)
abline(v=predict_data_split$newdate[80],lty=2,lwd=1)
abline(h=0,lty=2,lwd=3)
dev.off()

# Output CAR_11 graphs to jpegs
# Use both plots and lines: https://www.r-graph-gallery.com/connected-scatterplot.html

predict_data_split <- predict_data[91:101,]

jpeg(paste(filepath_output,filename,' CARs_11.jpg',sep=""))
plot(predict_data_split$newdate,predict_data_split$cumulative_ar_11,type="b",main=paste("Errors (CARs) in ",toupper(filename)," stock return prediction (CAPM)",sep=""),xlab="Date",ylab="Cumulative Actual - Predicted stock return")
abline(v=predict_data_split$newdate[90],lty=2,lwd=3)
abline(h=0,lty=2,lwd=3)
dev.off()

write.csv(x=predict_data,file=paste0(filepath_output,filename,".csv"),row.names=TRUE)


ar_file[consolidated_counter,'ticker'] <- filename
car_file[consolidated_counter,'ticker'] <- filename
for (i in 2:22) {
ar_file[consolidated_counter,i] <- predict_data[i+79,'abnormal_return']
car_file[consolidated_counter,i] <- predict_data[i+79,'cumulative_ar']
}

car_file_11[consolidated_counter,'ticker'] <- filename
for (i in 2:12) {
car_file_11[consolidated_counter,i] <- predict_data[i+89,'cumulative_ar_11']
}

}

write.csv(x=regression_consolidated,file=paste0(filepath_output,"regression_consolidated.csv"),row.names=TRUE)
write.csv(ar_file,file=paste0(filepath_abnormal,"ar_file.csv"),row.names=TRUE)
write.csv(car_file,file=paste0(filepath_abnormal,"car_file.csv"),row.names=TRUE)
write.csv(r2_list,file=paste0(filepath_reg_results,"r2_list.csv"),row.names=TRUE)
write.csv(car_file_11,file=paste0(filepath_abnormal,"car_file_11.csv"),row.names=TRUE)
