library(tidyverse)

OurData <- read.csv("ClassData.csv")

OurData5<-OurData%>%
  mutate(Slp = as.numeric(Sleep_Hrs))


#Bootstrapping

mean_calc <- function(x){
  return(mean(x, na.rm = TRUE))
}

bootstrapped_means <- replicate(10000, {
  bootstrapped_data <- sample(OurData5$Slp, replace = TRUE)
  mean_calc(bootstrapped_data)  
})

bootstrapped_df <- data.frame(bootstrapped_means)

ggplot(bootstrapped_df, aes(x=bootstrapped_means))+geom_density()

quantile(bootstrapped_means, c(.025, .975))

nba = read_csv("nba.csv")
table(nba$W.L,nba$home_away)
prop.test(c(712,518),c(1230,1230))
#We are 95% that winning proportion for all nba games at home is between  0.1178885 and 0.1975587 higher than when played away
ggplot(nba, aes(x=home_away, fill=W.L))+geom_bar(position = "fill")

t.test(PTS~home_away,data=nba)
#We are 95% that average points for all nba games at home is between  1.16 and 3.06 higher than when played away
ggplot(nba, aes(x=home_away, y=PTS, fill=home_away))+geom_boxplot(outlier.colour = "red")+geom_jitter()

OurData2<-OurData%>%
  mutate(Age_Diff = as.numeric(gsub("years", "", Age_Diff_Parents)))

t.test(OurData2$Age_Diff)

ct <- read_csv("Clinical_trial.csv")
ggplot(ct, aes(x=Drug,y=Pain_Rating, fill = Drug)) + geom_boxplot() + geom_jitter()  
anova<-aov(Pain_Rating~Drug,data=ct)
summary(anova)

TukeyHSD(anova,conf.level = .95)
plot(TukeyHSD(anova,conf.level = .95))
# We have enough Stat evidence to conclude mean pain for A is much lower than B and C
# A is more effective in treating migraines than B and C

Startups <- read.csv("Startups.csv")
ggplot(Startups, aes(x=R.D.Spend, y=Profit))+geom_point()
ggplot(Startups, aes(x=R.D.Spend, y=Profit))+geom_point()+ geom_smooth()
ggplot(Startups, aes(x=R.D.Spend, y=Profit))+geom_point()+ geom_smooth(method = "lm")
ggplot(Startups, aes(x=R.D.Spend, y=Profit))+geom_point()+ geom_smooth(method = "lm", se=FALSE)
cor(Startups$Profit, Startups$R.D.Spend)
sd(Startups$Profit)
sd(Startups$R.D.Spend)
cor(Startups$Profit, Startups$R.D.Spend)*sd(Startups$Profit)/sd(Startups$R.D.Spend)
mean(Startups$Profit)-(cor(Startups$Profit, Startups$R.D.Spend)*sd(Startups$Profit)/sd(Startups$R.D.Spend))*mean(Startups$R.D.Spend)
model1<-lm(Profit~R.D.Spend, data = Startups)
coef(model1)

bootstrap_estimates <- replicate(10000, {
  bootstrap_samples <- Startups[sample(1:nrow(Startups),nrow(Startups),
                                       replace=TRUE),]
  bootstrap_models <- lm(Profit~R.D.Spend, data = bootstrap_samples)
  coef(bootstrap_models)
})

estimates <- data.frame(t(bootstrap_estimates))
ggplot(Startups, aes(x=R.D.Spend, y=Profit)) +
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE, color="blue") + 
  geom_abline(data = estimates, aes(intercept = X.Intercept., slope=R.D.Spend),
              color="pink")

summarise(estimates, mean_of_b0=mean(X.Intercept.),mean_b1=mean(R.D.Spend))

model2 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data = Startups)
coef(model2)

X <- cbind(1, Startups$R.D.Spend, Startups$Administration, Startups$Marketing.Spend)
XtX <- t(X)  %*% X
inverse_XtX <- solve(XtX)
XtY <- t(X)%*% Startups$Profit

beta <- inverse_XtX%*%XtY
beta

Startups2 <- gather(Startups, key="predictor", value = "value",
                    R.D.Spend,Administration,Marketing.Spend)

ggplot(Startups2, aes(x=value, y=Profit, color=predictor)) + 
  geom_point() + facet_wrap(~predictor, scales="free_x")

Startups_pred <- mutate(Startups, predictions=fitted(model2), resid=residuals(model2))

ggplot(Startups_pred, aes(x=predictions, y=resid)) + geom_point() + geom_hline(yintercept = 0, color="red")

ggplot(Startups_pred, aes(sample=resid))+stat_qq()+stat_qq_line(color="red")

planets <- read.csv("PlanetsData.csv")

ggplot(planets, aes(x=distance, y=revolution))+geom_point()+geom_smooth(method="lm", se=F)
model3 <- lm(revolution~distance, data=planets)
coef(model3)

planet_pred <- mutate(planets, pred=fitted(model3), resid=residuals(model3))
ggplot(planet_pred, aes(x=pred, y=resid))+geom_point() + geom_hline(yintercept = 0, color="red")
ggplot(planet_pred, aes(sample=resid))+stat_qq()+stat_qq_line(color="red")

planets2 <- mutate(planet, log_dist = log(distance), log_rev = log(revolution))
ggplot(planets2, aes(x=log_dist, y=log_rev))+geom_point()
model4 <- lm(log_rev~log_dist, data=planets2)
coef(model4)

planet_pred2 <- mutate(planets2, pred=fitted(model4), resid=residuals(model4))
ggplot(planet_pred2, aes(x=pred, y=resid))+geom_point() + geom_hline(yintercept = 0, color="red")
ggplot(planet_pred2, aes(sample=resid))+stat_qq()+stat_qq_line(color="red")


new_dat <- data.frame(R.D.Spend = c(165349.20,5000000))
new_dat
model1
predict(model1, newdata = new_dat)
predict(model1, newdata = new_dat, interval = "prediction", level = .95)
predict(model1, newdata = new_dat, interval = "confidence", level = .95)

-0.9031235 + 1.5013054 * log(93)
exp(-0.9031235 + 1.5013054 * log(93))

new <- data.frame(log_dist = log(93))
predict(model4, new)
predict(model4, new, interval = "prediction")
exp(5.886647)
exp(5.916739)

avg_profit = mean(Startups$Profit)
ggplot(Startups, aes(x=R.D.Spend, y=Profit))+geom_point()+ geom_smooth(method = "lm", se=FALSE) + geom_hline(yintercept = avg_profit, color="darkgreen")
summary(model1)

Startups3 <- Startups[,-4]
cor_mat <- round(cor(Startups3), 2)
cor_mat

library(ggcorrplot)
ggcorrplot(cor_mat, lab=TRUE, type="lower", method="circle")

summary(model2)
library(car)
vif(model2)

model5 <- lm(Profit~R.D.Spend+Marketing.Spend, data = Startups)
summary(model5)

library(MASS)
model6 <- lm(Profit~R.D.Spend+Marketing.Spend, data = Startups, method = "StepAIC")
aic <- stepAIC(model2, direction = "both")
vif(aic)

avPlots(model2)

insurance = read.csv("insurance.csv")
mod1 <- lm(charges~smoker, data=insurance)
summary(mod1)
#rather than a slope and y-intercept, the coefficients measure the average charges for non-smokers and smokers respectivily.
t.test(charges~smoker, data=insurance)
new <- data.frame(smoker = "yes")
predict(mod1,new)

mod2 <- lm(charges~region, data=insurance)
summary(mod2)

insurance$region2 <- factor(insurance$region, levels = c("southeast", "northeast", "northwest", "southwest"))
mod3 <- lm(charges~region2, data=insurance)
summary(mod3)
anov <- aov(charges~region, data=insurance)
summary(anov)

ggplot(insurance, aes(x=age, y = charges))+geom_jitter()+geom_smooth(method="lm", se=FALSE)
ggplot(insurance, aes(x=age, y = charges, color=smoker))+geom_jitter()
ggplot(insurance, aes(x=age, y = charges, color=smoker))+geom_jitter()+
  geom_smooth(method="lm", aes(group = smoker), se=FALSE)

mod4 <- lm(formula = charges ~ age+smoker, data=insurance)
summary(mod4)

mod5 <- lm(charges ~ age*smoker, data=insurance)
summary(mod5)
ggplot(insurance, aes(x=age, y = charges, color=smoker))+geom_jitter()+
  geom_smooth(method="lm", model.extract(mod4), se=FALSE)
ggplot(insurance, aes(x=age, y = charges, color=smoker))+geom_jitter()+
  geom_smooth(method="lm", model.extract(mod5), se=FALSE)

new_dat <- data.frame(age=26, smoker="no")
predict(mod5,new_dat, interval = "prediction")
ins <- insurance[,-8]
mod10 <- lm(formula = charges ~ ., data=ins)
summary(mod10)

#Outliers: sata points with large residuals. Detect with scatter plot, residual plot, 
#residuals with St Dev > 2

library(broom)
library(dplyr)
dat_ins <- mod5 %>% augment(ins)
filter(dat_ins, .std.resid>2)

mod6 <- lm(charges~., data=insurance)
summary(mod6)

diagnostics <- mod6 %>%
  augment(data=insurance)

outliers <-filter(diagnostics, abs(.std.resid)>3)
leverage<-filter(diagnostics, .hat>2*(6+1)/nrow(insurance))
influence <- filter(diagnostics, .cooksd>4/nrow(insurance))

split <- sample(1:nrow(insurance), size = floor(.8*nrow(insurance)))
train_data <- insurance[split,]
test_data <- insurance[-split,]
mod7 <- lm(charges~age+bmi+children+smoker+region, data=train_data)
summary(mod7)

predictions <- predict(mod7, test_data)
rmse <- sqrt(mean((predictions - test_data$charges)^2))
rsme
RMSE(predictions, test_data$charges)

library(caret)

CV <- function(data,k){
  folds <- createFolds(insurance$charges, k=k)
  map_dbl(folds,function(indices){
    train_data <- data[-indices,]
    test_data <- data[indices,]
    
    model <- lm(charges~.-sex,data=train_data)
    pred <- predict(model, test_data)
    sqrt(mean((pred - test_data$charges)^2))
  }) %>% mean()
}

CV(insurance,5)

repeated <- replicate(10,CV(insurance,5))
mean(repeated)

mod11 <- train(charges~.-sex, method="lm", data=insurance)
summary(mod11)
control <- trainControl(method = "cv", number=5)
mod12 <- train(charges~.-sex,method="lm", trControl= control, data=insurance)
summary(mod12)

mod12$results$RMSE

laliga <- read_csv("laliga.csv")
laliga2 <-laliga[-c(1,3,9)]

modellaliga <- lm(Points~.,data = laliga2)
summary(modellaliga)

aic <- MASS::stepAIC(modellaliga, direction="both",Trace=F)
summary(aic)

library(glmnet)

X<- model.matrix(Points~0+.,data=laliga2)
y <- laliga2$Points
rmodel <- glmnet(x=X, y=y, alpha = 0)
plot(rmodel, label=T, xvar = "lambda")
plot(rmodel, label=T, xvar = "dev")
length(rmodel$lambda)
coef(rmodel)[,50]
kcvglmnet <- cv.glmnet(x=X,y=y,alpha=0,nfolds=3)
kcvglmnet$lambda.min
kcvglmnet$lambda.1se
predict(rmodel, type="response", s=kcvglmnet$lambda.1se, newx=X[1:2,])

predict(rmodel, type="coefficient", s=kcvglmnet$lambda.1se, newx=X[1:2,])
plot(rmodel, label=T,xvar='lambda')+abline(v=log(kcvglmnet$lambda.1se))

pca <- princomp(laliga2, fix_sign = T)
summary(pca)
plot(pca, type="l")


pca2 <- princomp(laliga2, cor=T, fix_sign = T)
summary(pca2)

pca2$loadings
score <- data.frame(pca2$scores)
score

biplot(pca2)

laliga3 <- laliga2[,-1]
pca3 <-  princomp(laliga3, cor=T, fix_sign = T)
summary(pca3)
pca_data <- data.frame(Points=laliga2$Points, Scores=pca3$scores)
pca_reg <- lm(Points~., data = pca_data)
summary(pca_reg)

pca_reg2 <- lm(Points~Scores.Comp.1+Scores.Comp.2+Scores.Comp.3, data = pca_data)
summary(pca_reg2)

library(pls)

lal4 <- subset(laliga2, select = -c(Wins,Draws,Loses))
pcareg3 <- pcr(Points~., data=lal4,scale=T)
summary(pcareg3)

new_dat=lal4[c(1,2),]
predict(pcareg3, new_dat, ncomp=10)

aic <-MASS::stepAIC(log_)
nba <- read.csv("NBA_raw.csv")
nba2 <- nba %>%
  mutate(home_away=ifelse(grepl('vs',MATCHUP),'home', 'away'),
         win=ifelse(W.L == "W",1,0)) %>%
  rename(FGP=FG.,TPP=X3P.,FTP=FT.) %>%
  dplyr::select(W.L, win, home_away, MIN, PTS, FGP, TPP, FTP, REB, AST, STL, BLK, TOV, PF)

ggplot(nba2, aes(x=home_away, fill=factor(win), color= factor(win))) + geom_bar(position = 'fill')

dat <- nba2[,-c(1,2,3)]
cor_mat<-round(cor(dat), 2)
ggcorrplot::ggcorrplot(cor_mat,lab=T,method='circle',type='lower')
logit_model1 <- glm(win~.-W.L,nba2,family='binomial')
summary(logit_model1)

logit_model2 <- train(W.L~.-win,nba2,method='glm',family='binomial')
summary(logit_model2)

logit_model3 <- glm(win~.-W.L-PTS,nba2,family='binomial')
summary(logit_model3)

logit_aic <- train(W.L~.-win-PTS,nba2,method="glmStepAIC",family='binomial', trace=F)
summary(logit_aic)

logit_model5 <- train(W.L~.-win+PTS*home_away,nba2,method="glm",family='binomial', trace=F)
summary(logit_model5)

pcareg3 <- pcr(Points~., data=lal4,scale=T)
new_dat=lal4[c(1,2),]
predict(pcareg3, new_dat, ncomp=12)
cv <- pcr(Points~., data=lal4,scale=TRUE,validation="LOO")
validationplot(cv, val.type = 'MSEP')

glm_model <- glm(win~.-W.L-PTS, data=nba2, family="binomial")
summary(glm_model)
logit <- train(W.L~.-win-PTS,nba2,method="glm",family='binomial')

exp(coef(glm_model))
exp(coef(logit$finalModel))

new <- nba2[1:2,-2]
predict(glm_model, new, type="response")

### For every 1% increase in FGP, win odds increase by 44.3% (1.443-1) holding all the other variables fixed.

new2 <- nba2[1:2,-1]
predict(logit,new2,type="raw")
predict(logit,new2,type="prob")
nba3 <- mutate(nba2, prob=predict(glm_model, type='response'), odds = log(prob/(1-prob)), classify=ifelse(prob>.6, 1, 0))
ggplot(nba3, aes(x=FGP,y=odds))+geom_point()
table(nba3$classify,nba3$win)
(1082+957)/2460

ctrl <- trainControl(method = "cv", number=10, summaryFunction = twoClassSummary, classProbs = TRUE, savePredictions = TRUE)
nbamodel3 <- train(W.L~.-win-PTS-home_away,nba2,method="glm",family='binomial', trControl = ctrl, metric="ROC")
predictions <- nbamodel3$pred
summary(nbamodel3)
table(predictions$pred,predictions$obs)
library(pROC)
roc <- roc(predictions$obs,predictions$W)
roc_dat <- data.frame(TPR = roc$sensitivities, FPR=1-roc$specificities)
ggplot(roc_dat,aes(x=FPR,y=TPR))+geom_line(color="darkgreen")
new2
