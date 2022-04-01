# Bayesian


library(tidyverse)

diabetes<- read.csv("/System/Volumes/Data/学习课件/STA545 Baysian/diabetes.csv",header = TRUE)
summary(diabetes)
str(diabetes)  #768 obs. of  9 variables
diabetes$Outcome <- factor(diabetes$Outcome)

# removing those observation rows with 0 in any of the variables
for (i in 2:6) {
  diabetes <- diabetes[-which(diabetes[, i] == 0), ]
}

outcome<- diabetes %>% count(Outcome,sort = TRUE)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




a<-ggplot(diabetes,aes(Pregnancies,fill=Outcome))+  geom_density(alpha = 0.3,adjust = 5) +plot_title
b<-ggplot(diabetes,aes(Glucose,fill=Outcome))+  geom_density(alpha = 0.3,adjust = 5) 
c<-ggplot(diabetes,aes(BloodPressure,fill=Outcome))+  geom_density(alpha = 0.3,adjust = 5) 
d<-ggplot(diabetes,aes(SkinThickness,fill=Outcome))+  geom_density(alpha = 0.3,adjust = 5) 
e<-ggplot(diabetes,aes(Insulin,fill=Outcome))+  geom_density(alpha = 0.3,adjust = 5) 
f<-ggplot(diabetes,aes(BMI,fill=Outcome))+  geom_density(alpha = 0.3,adjust = 5) 
g<-ggplot(diabetes,aes(DiabetesPedigreeFunction,fill=Outcome))+  geom_density(alpha = 0.3,adjust = 5) 
h<-ggplot(diabetes,aes(Age,fill=Outcome))+  geom_density(alpha = 0.3,adjust = 5) 

plot_title<- ggtitle("Figure 1", 
                     "Density plots for all attributes")

MULTI<-multiplot(a,b,c,d,e,f,g,h,cols=2)

library(arsenal)

table_one <- tableby(Outcome ~ ., data = diabetes) 
summary(table_one, title = "Summary Statistic")

#create a summary table

library(gtsummary)
diabetes %>%
  tbl_summary(
    by = outcome,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,
    missing_text = "(Missing)"
  )%>% add_p()

   
             
             
             
                       

# scale the covariates for easier comparison of coefficient posteriors
for (i in 1:8) {
  diabetes[i] <- scale(diabetes[i])
}

# modify the data column names slightly for easier typing

names(diabetes) <- tolower(names(diabetes))

n=dim(diabetes)[1]
p=dim(diabetes)[2]
str(diabetes)
print(paste0("number of observations = ", n))
print(paste0("number of predictors = ", p))

# preparing the inputs
x <- model.matrix(outcome ~ . - 1, data = diabetes)
y <- diabetes$outcome

library(rstanarm)
options(mc.cores = parallel::detectCores())
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
post1 <- stan_glm(outcome ~ ., data = diabetes,
                  family = binomial(link = "logit"), 
                  prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                  seed = 14124869)

library(ggplot2)
pplot<-plot(post1, "areas", prob = 0.95, prob_outer = 1)
round(posterior_interval(post1, prob = 0.95), 2)


plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
pplot+ geom_vline(xintercept = 0) + plot_title 



round(coef(post1), 1)



round(posterior_interval(post1, prob = 0.95), 2)




library(loo)
loo1 <- loo(post1, save_psis = TRUE)
post0 <- update(post1, formula = outcome ~ 1, QR = FALSE)#Compute baseline result without covariates.
loo0 <- loo(post0)
compare<- rstanarm::compare_models(loo0,loo1)


# Predicted probabilities
linpred <- posterior_linpred(post1)
preds <- posterior_linpred(post1, transform=TRUE)
pred <- colMeans(preds)

pr <- as.integer(pred >= 0.5)
table(pr,y)
(233+74)/(233+56+29+74)
mean(pr==y)
mean(pr!=y)
# posterior classification accuracy is 0.7831633,which means 0.2168367 is the error. 



# confusion matrix
caret::confusionMatrix(as.factor(as.numeric(pr>0.5)), y)[2]
# posterior classification accuracy
round(mean(xor(pr,as.integer(y==0))),2)
# posterior balanced classification accuracy
round((mean(xor(pr[y==0]>0.5,as.integer(y[y==0])))+mean(xor(pr[y==1]<0.5,as.integer(y[y==1]))))/2,2)
# PSIS-LOO weights
log_lik=log_lik(post1, parameter_name = "log_lik")
psis=psislw(-log_lik)
#plot(psis$pareto_k)
#plot(psis$lw_smooth[,1],linpred[,1])
# LOO predictive probabilities
ploo=colSums(preds*exp(psis$lw_smooth))
# LOO classification accuracy
round(mean(xor(ploo>0.5,as.integer(y==0))),2)
# LOO balanced classification accuracy
round((mean(xor(ploo[y==0]>0.5,as.integer(y[y==0])))+mean(xor(ploo[y==1]<0.5,as.integer(y[y==1]))))/2,2)
plot(pred,ploo)

calPlotData<-caret::calibration(y ~ pred + loopred, 
                                data = data.frame(pred=pred,loopred=ploo,y=y), 
                                cuts=10, class="1")
plot_title1 <- ggtitle("Posterior Predictive Intervals \nvs Observed Event Percentage"
)
ggplot(calPlotData, auto.key = list(columns = 2))+plot_title1

library(splines)
library(MASS)
ggplot(data = data.frame(pred=pred,loopred=ploo,y=as.numeric(y)-1), aes(x=loopred, y=y)) + 
  stat_smooth(method='glm', formula = y ~ ns(x, 5), fullrange=TRUE) + 
  geom_abline(linetype = 'dashed') + ylab(label = "Observed") + xlab(label = "Predicted (LOO)") + 
  geom_jitter(height=0.03, width=0) + scale_y_continuous(breaks=seq(0,1,by=0.1)) + xlim(c(0,1))



p0 <- 2 # prior guess for the number of relevant variables
tau0 <- p0/(p-p0) * 1/sqrt(n)
hs_prior <- hs(df=1, global_df=1, global_scale=tau0)
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
post2 <- stan_glm(outcome ~ ., data = diabetes,
                  family = binomial(link = "logit"), 
                  prior = hs_prior, prior_intercept = t_prior,
                  seed = 14124869, adapt_delta = 0.999)

loo2 <- loo(post2, save_psis = TRUE)


pplot<-plot(post2, "areas", prob = 0.9, prob_outer = 1)
pplot + geom_vline(xintercept = 0)

# Predicted probabilities
linpred2 <- posterior_linpred(post2)
preds2 <- posterior_linpred(post2, transform=TRUE)
pred2 <- colMeans(preds2)

pr2 <- as.integer(pred2 >= 0.5)
table(pr2,y)
(233+74)/(233+56+29+74)
mean(pr2==y)
mean(pr2!=y)

library(bayesplot)
mcmc_pairs(as.array(post2),pars = c("pregnancies","age","bmi"))



# Conclusion 

Bayesian logistic regression is not an algorithm, but a different method of statistical inference. The main advantage is that, with Bayesian processing, you can recover the entire range of extrapolation understanding, rather than the point estimates and confidence intervals found in traditional regression. Using Bayesian approach to predict the probability of developing diabetes, the model was accurate 78% of the time. Meanwhile, all the variables in the model were significant (P <0.05%), the point estimates reported coefficient for this predictor suggests that there was a negative coefficient with insulin and blood pressure, it indicated that the higher the insulin and high pressure, the less likely they were to develop diabetes. In addition, we have strong evidence that the Bayesian logistic regression model has reasonable performance.

