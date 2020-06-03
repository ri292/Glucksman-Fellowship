
# Load libraries

library(leaps)
library(car)
library(broom)

# Create linear regression output function: https://gist.github.com/EconometricsBySimulation/6274532

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

# Create files with all regression outputs
regression_parameters <- data.frame(ticker=character(0),sp500=character(0),nasdaq=character(0),bond=character(0),tbill=character(0),stringsAsFactors=FALSE)
pred_closediff <- data.frame(ticker=character(0),'tm10'=character(0),'tm9'=character(0),'tm8'=character(0),'tm7'=character(0),'tm6'=character(0),'tm5'=character(0),'tm4'=character(0),'tm3'=character(0),'tm2'=character(0),'tm1'=character(0),'t0'=character(0),'tp1'=character(0),'tp2'=character(0),'tp3'=character(0),'tp4'=character(0),'tp5'=character(0),'tp6'=character(0),'tp7'=character(0),'tp8'=character(0),'tp9'=character(0),'tp10'=character(0),stringsAsFactors=FALSE)
scaled_pred_closediff <- data.frame(ticker=character(0),'tm10'=character(0),'tm9'=character(0),'tm8'=character(0),'tm7'=character(0),'tm6'=character(0),'tm5'=character(0),'tm4'=character(0),'tm3'=character(0),'tm2'=character(0),'tm1'=character(0),'t0'=character(0),'tp1'=character(0),'tp2'=character(0),'tp3'=character(0),'tp4'=character(0),'tp5'=character(0),'tp6'=character(0),'tp7'=character(0),'tp8'=character(0),'tp9'=character(0),'tp10'=character(0),stringsAsFactors=FALSE)
actual_close <- data.frame(ticker=character(0),'tm10'=character(0),'tm9'=character(0),'tm8'=character(0),'tm7'=character(0),'tm6'=character(0),'tm5'=character(0),'tm4'=character(0),'tm3'=character(0),'tm2'=character(0),'tm1'=character(0),'t0'=character(0),'tp1'=character(0),'tp2'=character(0),'tp3'=character(0),'tp4'=character(0),'tp5'=character(0),'tp6'=character(0),'tp7'=character(0),'tp8'=character(0),'tp9'=character(0),'tp10'=character(0),stringsAsFactors=FALSE)
predicted_close <- data.frame(ticker=character(0),'tm10'=character(0),'tm9'=character(0),'tm8'=character(0),'tm7'=character(0),'tm6'=character(0),'tm5'=character(0),'tm4'=character(0),'tm3'=character(0),'tm2'=character(0),'tm1'=character(0),'t0'=character(0),'tp1'=character(0),'tp2'=character(0),'tp3'=character(0),'tp4'=character(0),'tp5'=character(0),'tp6'=character(0),'tp7'=character(0),'tp8'=character(0),'tp9'=character(0),'tp10'=character(0),stringsAsFactors=FALSE)
r2_list <- data.frame(ticker=character(0),r2=character(0),stringsAsFactors=FALSE)

consolidated_counter <- 0

# Create loop with each company
# Using DataCamp reference: https://campus.datacamp.com/courses/intermediate-r/chapter-2-loops?ex=8

companies <- list('mgm','msft1','fb1','msft2','tmus1','m','adbe','el','znga','chh','pson','cof','s','dgx1','faf','justdial','hig','7203','fb2','jwn','wbc','sgms','googl1','mar','cno','293','googl2','grmn','fb3','chgg','iag','ac','dri','tmus2','nlok','bmo','cmcsa1','nuan','sti','ingn','dal','hbc','uaa','expe','fdx','ry','bce1','pypl','yum','h1','fpay','efx','ucg','wfc','nwsa','vz1','wwe','shldq','bce2','sabr','rad','gme','pay','ihg','yhoo','dgx2','dis','2353','somc','kr','adp','vz2','baba','wen','cnc','twc','h2','8136','303','hot','cmcsa2','talk','tmus3','hlt','moh','azo','dc','akrx','sbh','ngvc','antm','ms')
filepath_input <- "c:/users/riazu/Google Drive/Glucksman Fellowship/22 Stock Price Prediction/01 Regression Input/"
filepath_output <- "c:/users/riazu/Google Drive/Glucksman Fellowship/22 Stock Price Prediction/02 Prediction Output/"
filepath_reg_results <- "c:/users/riazu/Google Drive/Glucksman Fellowship/22 Stock Price Prediction/04 Regression Results/"
filepath_diff <- "c:/users/riazu/Google Drive/Glucksman Fellowship/22 Stock Price Prediction/05 Differences/"
#filename <- 'cof'

for (filename in companies) {

consolidated_counter <- consolidated_counter+1

data <- read.csv(paste0(filepath_input,filename,".csv"))
reg_data <- data[1:60,]

#attach(reg_data)

summary(reg_data)



# Best subsets

best_subsets <- leaps(cbind(reg_data$sp500,reg_data$nasdaq,reg_data$bond,reg_data$tbill),reg_data$close,nbest=2)


# Use best subset (lowest Mallow's Cp) to create model
# https://www.r-bloggers.com/which-function-in-r/
# http://www.r-tutor.com/r-introduction/vector/vector-index
# https://campus.datacamp.com/courses/abc-intro-2-r/data-structures?ex=3

predictors_logical <- best_subsets$which[best_subsets$Cp == min(best_subsets$Cp),]


# Create the regression equation: sp500+, nasdaq+, bond+, tbill; then remove trailing + if it exists

equation <- ""

if (best_subsets$which[best_subsets$Cp == min(best_subsets$Cp),][1] == TRUE) {
equation <- paste(equation,"sp500+",sep="")
}
if (best_subsets$which[best_subsets$Cp == min(best_subsets$Cp),][2] == TRUE) {
equation <- paste(equation,"nasdaq+",sep="")
}
if (best_subsets$which[best_subsets$Cp == min(best_subsets$Cp),][3] == TRUE) {
equation <- paste(equation,"bond+",sep="")
}
if (best_subsets$which[best_subsets$Cp == min(best_subsets$Cp),][4] == TRUE) {
equation <- paste(equation,"tbill+",sep="")
}

if (substr(equation,nchar(equation),nchar(equation)) == "+") {
equation <- substr(equation,1,nchar(equation)-1)
}


# Create inputs for regression formula
close <- reg_data$close
sp500 <- reg_data$sp500
nasdaq <- reg_data$nasdaq
bond <- reg_data$bond
tbill <- reg_data$tbill


# Create linear model
# Fixed code using: https://stackoverflow.com/questions/17024685/how-to-use-reference-variables-by-character-string-in-a-formula
regression <- paste0("close", "~", equation)
reg_dataa <- lm(as.formula(regression))
summary(reg_dataa)




# Calculate VIFs
#vif(reg_dataa)

# Plot diagnostics

stdres <- rstandard(reg_dataa)
plot(fitted(reg_dataa),stdres,xlab="Fitted values",ylab="Standardized residuals")
title(main = "Fitted values vs. Standardized residuals")
qqnorm(stdres)
hist(stdres)

#detach(reg_data)

lmOut(reg_dataa, file=paste0(filepath_reg_results,filename," reg output.csv"))

regression_parameters[consolidated_counter,'ticker'] <- filename
if (best_subsets$which[best_subsets$Cp == min(best_subsets$Cp),][1] == TRUE) {
regression_parameters[consolidated_counter,'sp500'] <- summary(reg_dataa)$coef['sp500','Estimate']
}
if (best_subsets$which[best_subsets$Cp == min(best_subsets$Cp),][2] == TRUE) {
regression_parameters[consolidated_counter,'nasdaq'] <- summary(reg_dataa)$coef['nasdaq','Estimate']
}
if (best_subsets$which[best_subsets$Cp == min(best_subsets$Cp),][3] == TRUE) {
regression_parameters[consolidated_counter,'bond'] <- summary(reg_dataa)$coef['bond','Estimate']
}
if (best_subsets$which[best_subsets$Cp == min(best_subsets$Cp),][4] == TRUE) {
regression_parameters[consolidated_counter,'tbill'] <- summary(reg_dataa)$coef['tbill','Estimate']
}

r2_list[consolidated_counter,'ticker'] <- filename
r2_list[consolidated_counter, 'r2'] <- summary(reg_dataa)$r.squared

# Predict stock price using model selected

predict_data <- data
#attach(predict_data)

predict_data1 <- predict(reg_dataa, predict_data)

predict_data$pred_close <- predict_data1
predict_data$pred_closediff <- (predict_data$close - predict_data$pred_close)
predict_data$newdate = as.Date(predict_data$date,origin = "1899-12-30")

# Export of graph
# Reference: https://www.stat.berkeley.edu/~s133/saving.html
# Use both plots and lines: https://www.r-graph-gallery.com/connected-scatterplot.html
jpeg(paste0(filepath_output,filename,'.jpg'))
plot(predict_data$newdate,predict_data$pred_closediff,type="b",main=paste("Errors in ",toupper(filename)," stock price prediction",sep=""),xlab="Date",ylab="Actual - Predicted stock price")
abline(v=predict_data$newdate[90],lty=2,lwd=3)
abline(v=predict_data$newdate[80],lty=2,lwd=1)
abline(h=0,lty=2, lwd=3)
dev.off()

write.csv(x=predict_data,file=paste0(filepath_output,filename,".csv"),row.names=TRUE)

#detach(predict_data)

pred_closediff[consolidated_counter,'ticker'] <- filename
actual_close[consolidated_counter,'ticker'] <- filename
predicted_close[consolidated_counter,'ticker'] <- filename
scaled_pred_closediff[consolidated_counter,'ticker'] <- filename
for (i in 2:22) {
pred_closediff[consolidated_counter,i] <- predict_data[i+79,'pred_closediff']
actual_close[consolidated_counter,i] <- predict_data[i+79,'close']
predicted_close[consolidated_counter,i] <- predict_data[i+79,'pred_close']
scaled_pred_closediff[consolidated_counter,i] <- (predict_data[i+79,'pred_closediff'] / predict_data[i+79,'pred_close'])
}



}

write.csv(pred_closediff,file=paste0(filepath_diff,"pred_closediff.csv"),row.names=TRUE)
write.csv(actual_close,file=paste0(filepath_diff,"actual_close.csv"),row.names=TRUE)
write.csv(predicted_close,file=paste0(filepath_diff,"predicted_close.csv"),row.names=TRUE)
write.csv(regression_parameters,file=paste0(filepath_reg_results,"regression_parameters.csv"),row.names=TRUE)
write.csv(r2_list,file=paste0(filepath_reg_results,"r2_list.csv"),row.names=TRUE)
write.csv(scaled_pred_closediff,file=paste0(filepath_diff,"scaled_pred_closediff.csv"),row.names=TRUE)
