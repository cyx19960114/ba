

library("XLConnect")
library(glmnet)
library(gplots)



value2colour <- function(x,min.value=0,max.value=1,blend.green=1){
  greva <- (max(c(min(c(x,max.value)),min.value)) - min.value)/(max.value - min.value)
  
  green.intensity <- blend.green*max(c(0,(greva-0.25)*(0.75-greva))*16)
  middle.value <- (max.value + min.value)/2
  if (x<middle.value){
    return(rgb(0,green.intensity,1-(max(c(x,min.value))-min.value)/(middle.value - min.value)))
  }else{
    return(rgb((min(c(x,max.value))-middle.value)/(max.value - middle.value),green.intensity,0))
  }
}


value2colour.v <- Vectorize(value2colour)

add.colour.legend <- function(xleft,ybottom,xright,ytop,no.steps=100,min.value=0,max.value=1,offset.value=1,cex=1){
  for (i in 1:no.steps){
    h <- (ytop - ybottom)/no.steps
	coli <- value2colour((i-1)/(no.steps-1))
    rect(xleft,ybottom+(i-1)*h,xright,ybottom+i*h,col=coli,border=coli)
  }
  text(c(xright+offset.value,xright+offset.value),c(ybottom,ytop),labels=c(min.value,max.value),cex=cex)
} 




### Applies Lasso linear regression to a data set dat where target.colum is the dependent variable. 
### Returns the best lambda value, the regression coefficients and the predicted values. 
lasso_training_results <- function(dat,target.column,lambda_seq=10^seq(2,-2,by = -.1),alpha=1,normalise=T){
  taco <- target.column
  if (!is.numeric(taco)){
    taco <- grep(target.column,colnames(dat))
  }
  x_vars <- as.matrix(dat[,-taco])
  y_var <- dat[,taco]
  y.mean <- 0
  y.sd <- 1
  if (normalise){
    x_vars <- scale(x_vars)
	y.mean <- mean(y_var,na.rm=T)
	y.sd <- sd(y_var,na.rm=T)
	y_var <- (y_var - y.mean)/y.sd
  }
 
  cv_output <- cv.glmnet(x_vars,y_var,alpha=alpha,lambda=lambda_seq)
  best_lam <- cv_output$lambda.min
  lasso_best <- glmnet(x_vars,y_var,alpha=alpha,lambda=best_lam)
  predictions <- predict(lasso_best,s=best_lam,newx=x_vars)
  if (normalise){
    predictions <- y.mean + y.sd*predictions
  }
  return(list(lambda=best_lam,coefficients=coef(lasso_best)[,1],predictions=predictions))
}



### Applies Lasso linear regression n.bootstrap times based on bootstrap samples.
### dat ist the data set with target.colum as the dependent variable. 
### Returns a matrix with the lambda values and the regression coefficients. 
lasso_bootstrap <- function(dat,target.column,lambda_seq=10^seq(2,-2,by = -.1),alpha=1,normalise=T,n.bootstrap=1000){
  
  lambda <- rep(0,n.bootstrap)
  coefficient.matrix <- matrix(0,nrow=n.bootstrap,ncol=ncol(dat)+1)
  colnames(coefficient.matrix) <- c("lambda","intercept",colnames(dat)[!names(dat) %in% target.column,drop=F])
  for (i in 1:n.bootstrap){
    inds <- sample(nrow(dat),nrow(dat),replace=T)
    lassi <- lasso_training_results(dat[inds,],target.column,lambda_seq=lambda_seq,alpha=alpha,normalise=normalise)
    coefficient.matrix[i,] <- c(lassi$lambda,lassi$coefficients)
  }
  return(coefficient.matrix)
}



### Computes the proportion of non-zero entries in a vector.
prop.nonzero <- function(x){
  return(sum(!is.na(x) & x!=0)/sum(!is.na(x)))
}

### Computes the proportion of non-zero entries per column in a matrix. 
get.proportion.of.nonzero.coeffcients <- function(coefficient.matrix){
  return(apply(coefficient.matrix,2,prop.nonzero))
}

### Checks for a vector whether all entries are non-negative or non-positive. 
sign.consistency <- function(x){
  return(sum(!is.na(x) & x>=0)==sum(!is.na(x)) | sum(!is.na(x) & x<=0)==sum(!is.na(x)))
}

### Checks for each column of a matrix whether all entries are non-negative or non-positive. 
get.sign.consistency <- function(coefficient.matrix){
  return(apply(coefficient.matrix,2,sign.consistency))
}


### Applies leave-one-out cross-validation for Lasso linear regression to a data set dat where target.colum is the dependent variable. 
### Returns the predicted values.
lasso_loo <- function(dat,target.column,lambda_seq=10^seq(2,-2,by = -.1),alpha=1,normalise=T){
  predictions <- rep(0,nrow(dat))
  
  for (i in 1:nrow(dat)){
    dati <- dat[-i,]
	means <- rep(0,ncol(dati))
	sds <- rep(1,ncol(dati))
    if (normalise){
	  means <- apply(dati,2,mean,na.rm=T)
	  sds <- apply(dati,2,sd,na.rm=T)
	  dati <- scale(dati)
	}
    lassi <- lasso_training_results(dati,target.column,lambda_seq=lambda_seq,alpha=alpha,normalise=F)
	
	taco <- target.column
    if (!is.numeric(taco)){
      taco <- grep(target.column,colnames(dat))
    }
	if (normalise){
	  x <- rep(0,ncol(dati))
	  for (j in 1:length(x)){
	    x[j] <- (dat[i,j] - means[j])/sds[j]
	  }
	  x <- c(1,x[-taco])
	  predictions[i] <- sum(lassi$coefficients*x)
	  predictions[i] <- means[taco] + sds[taco]*predictions[i]
	}else{
	  predictions[i] <- sum(lassi$coefficients*c(1,t(dat[i,-taco])))
	}
  }
  return(predictions)
}



### Computes the coefficient of determination for given y-values and their predictions.
r.squared <- function(true.values,predictions){
  mean.y <- mean(true.values)
  ss.tot <- sum((true.values - mean.y)^2)
  ss.res <- sum((true.values - predictions)^2)
  return(1 - ss.res/ss.tot)
}




###########################################################################################
###########################################################################################
###########################################################################################



datex <- loadWorkbook("D:/Klawonn/HZI/PeterSoeroes/BrainProject/OASIS.xlsx")
dat <- readWorksheet(datex,1)


dat.female <- subset(dat,dat$sex=="F")
dat.male <- subset(dat,dat$sex=="M")


###########################################################################################
################## Mean thickness #########################################################
###########################################################################################


datm <- dat

age <- datm$age
mean.thickness <- apply(datm[,-(1:3)],1,mean)
lm.simple <- lm(mean.thickness~age)
summary(lm.simple)

pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/UnivariateLinearRegression/mean_thickness_age_all.pdf")
plot(age,mean.thickness,xlab="Age",ylab="Mean cortical thickness")
abline(lm.simple,col=2,lwd=2)
dev.off()

sex <- datm$sex
lm.sex <- lm(mean.thickness~sex*age)

summary(lm.sex)


datm <- dat.female

age <- datm$age
mean.thickness <- apply(datm[,-(1:3)],1,mean)
lm.simple <- lm(mean.thickness~age)
summary(lm.simple)

pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/UnivariateLinearRegression/mean_thickness_age_female.pdf")
plot(age,mean.thickness,xlab="Age",ylab="Mean cortical thickness")
abline(lm.simple,col=2,lwd=2)
dev.off()



datm <- dat.male

age <- datm$age
mean.thickness <- apply(datm[,-(1:3)],1,mean)
lm.simple <- lm(mean.thickness~age)
summary(lm.simple)

pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/UnivariateLinearRegression/mean_thickness_age_male.pdf")
plot(age,mean.thickness,xlab="Age",ylab="Mean cortical thickness")
abline(lm.simple,col=2,lwd=2)
dev.off()





###########################################################################################
################## Regression coefficients ################################################
###########################################################################################


pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/box_plots_age.pdf")
boxplot(dat.female$age,dat.male$age,names=c("Female","Male"),ylab="Age [years]")
dev.off()



###########################################################################################
################## Regression coefficients ################################################
###########################################################################################



intercepts <- rep(0,ncol(dat)-3)
intercepts.female <- rep(0,ncol(dat)-3)
intercepts.male <- rep(0,ncol(dat)-3)

slopes <- rep(0,ncol(dat)-3)
slopes.female <- rep(0,ncol(dat)-3)
slopes.male <- rep(0,ncol(dat)-3)

for (i in 1:length(intercepts)){
  lm.all <- lm(dat[,3+i]~dat[,3])
  lm.female <- lm(dat.female[,3+i]~dat.female[,3])
  lm.male <- lm(dat.male[,3+i]~dat.male[,3])
  
  intercepts[i] <- lm.all$coefficients[1]
  intercepts.female[i] <- lm.female$coefficients[1]
  intercepts.male[i] <- lm.male$coefficients[1]
  
  slopes[i] <- lm.all$coefficients[2]
  slopes.female[i] <- lm.female$coefficients[2]
  slopes.male[i] <- lm.male$coefficients[2]
}


pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/UnivariateLinearRegression/intercepts_male_vs_female.pdf")
plot(intercepts.male,intercepts.female,pch=16,xlab="Intercept (male)",ylab="Intercept (female)")
grid()
points(c(-3,5),c(-3,5),type="l",lwd=2,col=2)
dev.off()


pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/UnivariateLinearRegression/slopes_male_vs_female.pdf")
plot(slopes.male,slopes.female,pch=16,xlab="Slope (male)",ylab="Slope (female)")
grid()
points(c(-0.1,0.1),c(-0.1,0.1),type="l",lwd=2,col=2)
dev.off()

sum(slopes.male>slopes.female)
### 47 (out of 148)

sum(slopes.male>slopes.female)/length(slopes.male)
### 0.3175676

2*pbinom(sum(slopes.male>slopes.female),length(slopes.male),0.5)
### 1.068306e-05




###########################################################################################
################## Regression coefficients male female ####################################
###########################################################################################



intercepts <- rep(0,ncol(dat)-3)
intercepts.male <- rep(0,ncol(dat)-3)

slopes <- rep(0,ncol(dat)-3)
slopes.male <- rep(0,ncol(dat)-3)

p.values.intercepts <- rep(1,ncol(dat)-3)
p.values.slopes <- rep(1,ncol(dat)-3)
p.values.intercepts.male <- rep(1,ncol(dat)-3)
p.values.slopes.male <- rep(1,ncol(dat)-3)

for (i in 1:length(intercepts)){
  lm.all <- lm(dat[,3+i]~dat[,3]*dat$sex)
  
  
  intercepts[i] <- summary(lm.all)$coefficients[1,1]
  intercepts.male[i] <- summary(lm.all)$coefficients[3,1]
  
  slopes[i] <- summary(lm.all)$coefficients[2,1]
  slopes.male[i] <- summary(lm.all)$coefficients[4,1]
  
  p.values.intercepts[i] <- summary(lm.all)$coefficients[3,4]
  p.values.slopes[i] <- summary(lm.all)$coefficients[4,4]
  p.values.intercepts.male[i] <- summary(lm.all)$coefficients[3,4]
  p.values.slopes.male[i] <- summary(lm.all)$coefficients[4,4]
}


is.different.intercepts.male <- p.values.intercepts.male < 0.05
is.different.slopes.male <- p.values.slopes.male < 0.05


interslopes <- data.frame(intercepts,intercepts.male,slopes,slopes.male,p.values.intercepts,p.values.slopes,p.values.intercepts.male,p.values.slopes.male)
rownames(interslopes) <- colnames(dat)[4:ncol(dat)]

write.csv2(interslopes,file="D:Klawonn/HZI/PeterSoeroes/BrainProject/UnivariateLinearRegression/univariate_linear_regression_thickness_age.csv")










###########################################################################################
################## Correlation ############################################################
###########################################################################################






cors <- cor(dat[,-(1:2)],method="pearson",use="pairwise.complete.obs")
pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Correlations/correlation_pearson.pdf")
heatmap.2(cors, Rowv=F,Colv=F,dendrogram="none",trace="none",cexRow=0.1,cexCol=0.1,col="bluered",notecol="black")
dev.off()


cors.female <- cor(dat.female[,-(1:2)],method="pearson",use="pairwise.complete.obs")
pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Correlations/correlation_pearson_female.pdf")
heatmap.2(cors.female,Rowv=F,Colv=F,dendrogram="none",trace="none",cexRow=0.1,cexCol=0.1,col="bluered",notecol="black")
dev.off()


cors.male <- cor(dat.male[,-(1:2)],method="pearson",use="pairwise.complete.obs")
pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Correlations/correlation_pearson_male.pdf")
heatmap.2(cors.male, Rowv=F,Colv=F,dendrogram="none",trace="none",cexRow=0.1,cexCol=0.1,col="bluered",notecol="black")
dev.off()






cors <- cor(dat[,-(1:2)],method="kendall",use="pairwise.complete.obs")
pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Correlations/correlation_kendall.pdf")
heatmap.2(cors, Rowv=F,Colv=F,dendrogram="none",trace="none",cexRow=0.1,cexCol=0.1,col="bluered",notecol="black")
dev.off()


cors.female <- cor(dat.female[,-(1:2)],method="kendall",use="pairwise.complete.obs")
pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Correlations/correlation_kendall_female.pdf")
heatmap.2(cors.female,Rowv=F,Colv=F,dendrogram="none",trace="none",cexRow=0.1,cexCol=0.1,col="bluered",notecol="black")
dev.off()


cors.male <- cor(dat.male[,-(1:2)],method="kendall",use="pairwise.complete.obs")
pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Correlations/correlation_kendall_male.pdf")
heatmap.2(cors.male, Rowv=F,Colv=F,dendrogram="none",trace="none",cexRow=0.1,cexCol=0.1,col="bluered",notecol="black")
dev.off()





###########################################################################################
################## Lasso ##################################################################
###########################################################################################



lasso.all <- lasso_training_results(dat[,-c(1:2)],"age")
r.squared(dat$age,lasso.all$predictions)
# [1] 0.8507733

lasso.female <- lasso_training_results(dat.female[,-c(1:2)],"age")
r.squared(dat.female$age,lasso.female$predictions)
# [1] 0.8923628

lasso.male <- lasso_training_results(dat.male[,-c(1:2)],"age")
r.squared(dat.male$age,lasso.male$predictions)
# [1] 0.8683125


minv <- min(c(lasso.female$coefficients[-1],lasso.male$coefficients[-1]))
maxv <- max(c(lasso.female$coefficients[-1],lasso.male$coefficients[-1]))


pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_female_vs_male.pdf")
plot(lasso.female$coefficients[-1],lasso.male$coefficients[-1],xlim=c(minv,maxv),ylim=c(minv,maxv),xlab="Coefficients female",ylab="Coefficients male",pch=16)
grid()
points(c(minv-10,maxv+10),c(minv-10,maxv+10),type="l",lwd=1,col=2)
dev.off()


inds <- 1:length(lasso.female$coefficients)

inds.cf0m0 <- subset(inds,lasso.female$coefficients==0 & lasso.male$coefficients==0) 

inds.cf0m1 <- subset(inds,lasso.female$coefficients==0 & lasso.male$coefficients!=0)

inds.cf1m0 <- subset(inds,lasso.female$coefficients!=0 & lasso.male$coefficients==0) 

inds.cf1m1 <- subset(inds,lasso.female$coefficients!=0 & lasso.male$coefficients!=0)


cf0m0 <- data.frame(lasso.female$coefficients[inds.cf0m0],lasso.male$coefficients[inds.cf0m0])
colnames(cf0m0) <- c("female","male")
rownames(cf0m0) <- names(lasso.female$coefficients)[inds.cf0m0]
write.csv2(cf0m0,file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_female0_male0.csv")

cf0m1 <- data.frame(lasso.female$coefficients[inds.cf0m1],lasso.male$coefficients[inds.cf0m1])
colnames(cf0m1) <- c("female","male")
rownames(cf0m1) <- names(lasso.female$coefficients)[inds.cf0m1]
write.csv2(cf0m1,file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_female0_male1.csv")

cf1m0 <- data.frame(lasso.female$coefficients[inds.cf1m0],lasso.male$coefficients[inds.cf1m0])
colnames(cf1m0) <- c("female","male")
rownames(cf1m0) <- names(lasso.female$coefficients)[inds.cf1m0]
write.csv2(cf1m0,file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_female1_male0.csv")

cf1m1 <- data.frame(lasso.female$coefficients[inds.cf1m1],lasso.male$coefficients[inds.cf1m1])
colnames(cf1m1) <- c("female","male")
rownames(cf1m1) <- names(lasso.female$coefficients)[inds.cf1m1]
write.csv2(cf1m1[-1,],file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_female1_male1.csv")




######


lasso.b.all <- lasso_bootstrap(dat[,-c(1:2)],"age")

lasso.b.female <- lasso_bootstrap(dat.female[,-c(1:2)],"age") 

lasso.b.male <- lasso_bootstrap(dat.male[,-c(1:2)],"age") 


prop.nonzero.all <- get.proportion.of.nonzero.coeffcients(lasso.b.all[,-1])
prop.nonzero.female <- get.proportion.of.nonzero.coeffcients(lasso.b.female[,-1])
prop.nonzero.male <- get.proportion.of.nonzero.coeffcients(lasso.b.male[,-1])

consistent.sign.all <- get.sign.consistency(lasso.b.all[,-1])
consistent.sign.female <- get.sign.consistency(lasso.b.female[,-1])
consistent.sign.male <- get.sign.consistency(lasso.b.male[,-1])


bs.data <- data.frame(prop.nonzero.all,consistent.sign.all,prop.nonzero.female,consistent.sign.female,prop.nonzero.male,consistent.sign.male)
rownames(bs.data) <- colnames(lasso.b.all)[-1]

write.csv2(bs.data,file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_bootstrap_stats.csv")

write.csv2(lasso.b.all,file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_bootstrap_all.csv",row.names=F)
write.csv2(lasso.b.female,file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_bootstrap_female.csv",row.names=F)
write.csv2(lasso.b.male,file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_bootstrap_male.csv",row.names=F)



predictions.all <- lasso_loo(dat[,-c(1:2)],"age")
r.squared(dat$age,predictions.all)
# [1] 0.7850437


minv <- min(c(dat$age,predictions.all))
maxv <- max(c(dat$age,predictions.all))

pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/loo_all.pdf")
plot(dat$age,predictions.all,xlab="Real age",ylab="Predicted age",xlim=c(minv,maxv),ylim=c(minv,maxv))
grid()
points(c(-10,200),c(-10,200),type="l",lwd=2,col=2)
r2 <- round(r.squared(dat$age,predictions.all),3)
text(minv+10,maxv-20,bquote(paste(R^2==.(r2))))
dev.off()


predictions.female <- lasso_loo(dat.female[,-c(1:2)],"age")
r.squared(dat.female$age,predictions.female)
# [1] 0.7570491

minv <- min(c(dat.female$age,predictions.female))
maxv <- max(c(dat.female$age,predictions.female))

pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/loo_female.pdf")
plot(dat.female$age,predictions.female,xlab="Real age",ylab="Predicted age",xlim=c(minv,maxv),ylim=c(minv,maxv))
grid()
points(c(-10,200),c(-10,200),type="l",lwd=2,col=2)
r2 <- round(r.squared(dat.female$age,predictions.female),3)
text(minv+10,maxv-20,bquote(paste(R^2==.(r2))))
dev.off()


predictions.male <- lasso_loo(dat.male[,-c(1:2)],"age")
r.squared(dat.male$age,predictions.male)
# [1] 0.7428852


minv <- min(c(dat.male$age,predictions.male))
maxv <- max(c(dat.male$age,predictions.male))

pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/loo_male.pdf")
plot(dat.male$age,predictions.male,xlab="Real age",ylab="Predicted age",xlim=c(minv,maxv),ylim=c(minv,maxv))
grid()
points(c(-10,200),c(-10,200),type="l",lwd=2,col=2)
r2 <- round(r.squared(dat.male$age,predictions.male),3)
text(minv+10,maxv-20,bquote(paste(R^2==.(r2))))
dev.off()


###

lasso.b.all <- read.csv2(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_bootstrap_all.csv")
lasso.b.female <- read.csv2(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_bootstrap_female.csv")
lasso.b.male <- read.csv2(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_bootstrap_male.csv")

means <- apply(lasso.b.all,2,mean)
sds <- apply(lasso.b.all,2,sd)
pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_bootstrap_mean_sd_all.pdf")
plot(means[-(1:2)],sds[-(1:2)],xlab="Mean",ylab="Standard deviation")
grid()
dev.off()


means <- apply(lasso.b.female,2,mean)
sds <- apply(lasso.b.female,2,sd)
pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_bootstrap_mean_sd_female.pdf")
plot(means[-(1:2)],sds[-(1:2)],xlab="Mean",ylab="Standard deviation")
grid()
dev.off()



means <- apply(lasso.b.male,2,mean)
sds <- apply(lasso.b.male,2,sd)
pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/Lasso/coefficients_bootstrap_mean_sd_male.pdf")
plot(means[-(1:2)],sds[-(1:2)],xlab="Mean",ylab="Standard deviation")
grid()
dev.off()




#############################################################################################################################
############################################ PCA ############################################################################
#############################################################################################################################

pca.all <- prcomp(dat[,-(1:3)],center=T,scale=T)



cols <- value2colour.v(dat$age,min.value=min(dat$age),max.value=max(dat$age),blend.green=1)
pchs <- rep(16,nrow(dat))
pchs[dat$sex=="M"] <- 15

pdf(file="D:/Klawonn/HZI/PeterSoeroes/BrainProject/pca.pdf")
plot(pca.all$x,col=cols,pch=pchs)
add.colour.legend(-28,-7,-26,-0.5,min.value=min(dat$age),max.value=max(dat$age),offset.value=1)
legend("bottomright",legend=c("female","male"),pch=c(16,15))
dev.off()




plot(pca.all$x[,1:2],col=cols,pch=pchs)

#############################################################################################################################
##### Lasso example
datr <- dat[,-c(1,2)]

x_vars <- model.matrix(age~. ,datr)[,-1]
y_var <- datr$age
lambda_seq <- 10^seq(2, -2, by = -.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), round(nrow(x_vars)/2))
x_test = x_vars[-train,]
y_test = y_var[-train]

cv_output <- cv.glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = lambda_seq)

# identifying best lamda
best_lam <- cv_output$lambda.min

lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_vars[-train,])




coef(lasso_best)



coef(lasso_best)[,1]













