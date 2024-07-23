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
coef(mod)

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
