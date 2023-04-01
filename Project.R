library(tidyverse)
library(pmsampsize)
library(rms)
library(rmda)
library(DescTools)
# get the directory path where the source file is located
dir_path <- dirname(rstudioapi::getSourceEditorContext()$path)

# set the working directory to the directory path where the source file is located
setwd(dir_path)
rm(dir_path)

df <- read_csv("pancreatitis.csv")

# Clean the dataset ####
remove_suffix <- function(x) {
  gsub("_.*$", "", x)
}

# apply function to each variable that contains an "_"
df1 <- data.frame(lapply(df, function(x) ifelse(grepl("_", x), remove_suffix(x), x)))

cal <- calibrate(model_rms, B = 200)
plot(cal)


fit <- glm(data=df1, outcome ~ age + gender + rx, family = binomial)
PseudoR2(fit, which = "CoxSnell")
df1 %>% group_by(outcome) %>% count()

#Sample size
pmsampsize(type = "b", rsquared = 0.02, parameters = 3, shrinkage = 0.9, prevalence = 0.1)




# clinical relevant
model_rms <- lrm(data = df1,
                 outcome ~ sod + pep + psphinc + precut + difcan + pneudil + amp + 
                   age + gender + rx, x = TRUE, y = TRUE)

#backwards selection model
model_rms_bw <- lrm(data = df1,
                    outcome ~ sod + pep + psphinc + precut + difcan + pneudil + amp + 
                      age + gender + rx + recpanc + paninj + brush + train + acinar +
                      prophystent + therastent + pdstent + sodsom + bstent, x = TRUE, y = TRUE)

fastbw(model_rms_bw, rule="p")


model_rms_bw_final <- lrm(data = df1, 
                          outcome ~ pep + psphinc + difcan + amp + age + rx +
                            train + acinar, x = TRUE, y = TRUE)


# model 1
val <- validate(model_rms, method="boot", B = 200) # how many patients are in each repition
0.5 * (val[1 , ] + 1)

# model 2
val_bw <- validate(model_rms_bw_final, method="boot", B = 200)
0.5 * (val_bw[1 , ] + 1)

cal <- calibrate(model_rms, B = 200)
plot(cal)


# model 1
dca <- decision_curve(mortality~p_pred, data=val, fitted.risk = TRUE)
plot_decision_curve(dca)

# non linear term for age?
# backwards selection with other set of 20 variables



forest.default(x = c(1,2,3,4), ci.lb = c(0.5, 0.2, 0.4, 0.2), ci.ub = c(1.3, 2.3, 3.4, 5.6),
               ylim = c(-2, 16)) 
addpoly(x = 4, ci.lb = 2.3, ci.ub = 4.6, row = -1)
