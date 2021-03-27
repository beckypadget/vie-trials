#### Preamble ####
setwd("~/Documents/1.Uni/Current/Termites/Tattooing/Data and R scripts")
if(!require(survival)){install.packages("survival")}
library(survival)

if(!require(survminer)){install.packages("survminer")}
library(survminer)

if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)

if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

if(!require(coxme)){install.packages("coxme")}
library(coxme)

if(!require(powerSurvEpi)){install.packages("powerSurvEpi")}
library("powerSurvEpi")

if(!require(coxed)){install.packages("coxed")}
library("coxed")

#### Load data and make variables ####
data <- read.csv("sorted_census.csv")
last_day <- 63 # last day of census
total_treat <- data$Num_alive[which(data$Treatment == 1 & data$Day == last_day)] # total number treatment alive on last day
total_control <- data$Num_alive[which(data$Treatment == 0 & data$Day == last_day)] # total number control alive on last day

f_data <- read.csv("reformatted_data.csv")
f_data <- f_data[-c(41),] # remove HC1 control

#### Absolute numbers ####
sum(f_data$Day_survived[which(f_data$Treatment==1)]==63)
sum(f_data$Day_survived[which(f_data$Treatment==0)]==63)

# 16,17
#### Functions ####
# List of means by day
get_daily_means <- function (day, treatment)
{
  mean_treatments <- c(mean_treatments, mean(data$Num_alive[which(data$Day == day & data$Treatment == treatment)]))
}

# Standard error by day/treatment
standard_err <- function (day, treatment)
{
  sd(data$Num_alive[which(data$Day == day & data$Treatment == treatment)])/sqrt(length(data$Num_alive[which(data$Day == day & data$Treatment == treatment)]))
}

#### Create arrays of data ####
# Means of treatments
treat_means <- c()
for (i in unique(data$Day))
{
  treat_means <- c(treat_means, get_daily_means(i, 1))
}

# Means of controls
control_means <- c()
for (i in unique(data$Day))
{
  control_means <- c(control_means, get_daily_means(i, 0))
}

# Standard errors for treatments
se_treats <- c()
for (i in unique(data$Day))
{
  se_treats <- c(se_treats, standard_err(i, 1))
}

# Standard errors for controls
se_controls <- c()
for (i in unique(data$Day))
{
  se_controls <- c(se_controls, standard_err(i, 0))
}

# Set these specifically because group 2 is a day behind so has one less day
# day_treat = data$Day[which(data$Treatment == '1')]
# day_control = data$Day[which(data$Treatment == '0')]

#### Basic plots - explore data ####
# Distributions
# Density
grey <- rgb(0.4,0.4,0.4, alpha = 0.5)
plot(density(total_treat), col = "cyan3", xlab = "Number alive", ylim = c(0, 1.75))
lines(density(total_control), col = "grey")
legend(0.2, 1.65, pch = 20, legend = c("Control", "Treatment"), col = c("grey", "cyan3"))
# Histograms
hist(total_treat) # total number alive on the final day in each group (13 values out of 5)
hist(total_control)
wilcox.test(total_treat, total_control, paired = TRUE)

# Scatter with standard error
plot_df_treatments <- c(rep("T", 33), rep("C",33))
std_errors_df <- c(se_treats,se_controls)
plot_df_2 <- data.frame(day = as.numeric(unique(data$Day)), value = c(treat_means, control_means), treatment = plot_df_treatments, se = std_errors_df)
raw_data_plot <- ggplot(plot_df_2, aes(x=day, y=value, color=treatment)) + 
  geom_point() + 
  ylim(0,5)+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2,
                position=position_dodge(0.05)) +
  #scale_color_manual(values=c("#56A5A4","#71D9D7")) + 
  theme_classic()
raw_data_plot

# total number alive for each group
total_treatment <- sum(data$Num_alive[which(data$Day == last_day & data$Treatment == "1")])
total_control <- sum(data$Num_alive[which(data$Day == last_day & data$Treatment == "0")])

## Log rank test - are survival curves different?
weight <- f_data$Mean_weight
time <- f_data$Day_survived
status <- f_data$Status
treatment <- f_data$Treatment
colony <- f_data$Colony
surv_object <- Surv(time, status)
fit1 <- survfit(Surv(time, status) ~ treatment, data = f_data)
survdiff(formula = Surv(f_data$Day_survived, f_data$Status) ~ f_data$Treatment)

ggsurvplot(fit1, data = f_data, pval = TRUE, conf.int = TRUE)

## Mixed effects Cox prop. hazards - does treatment have an effect when we account for colony/dish effects?
mixed_cox <- coxme(surv_object ~ treatment + (1|colony/Dish), data = f_data)
mixed_cox
confint(mixed_cox_2)

#### SHORT TERM EFFECTS ####
# Log-rank test
st_data <- read.csv("shortterm_data.csv")
st_time <- st_data$Day_survived
st_status <- st_data$Status
st_treatment <- st_data$Treatment
st_colony <- st_data$Colony
st_dish <- st_data$Dish
st_surv_object <- Surv(st_time, st_status)
st_fit1 <- survfit(Surv(st_time, st_status) ~ st_treatment, data = st_data)
surv_pvalue(st_fit1)
st_real_plot <- ggsurvplot(st_fit1, data = st_data, pval = TRUE, conf.int = TRUE)
st_real_plot
survdiff(formula = Surv(st_time, st_status) ~ st_data$Treatment)

# Cox mixed effects
st_cox_mixed <- coxme(st_surv_object ~ st_treatment + (1 | st_colony/st_dish), data = st_data)
summary(st_cox_mixed)

#### Post-hoc power analysis ####
# Power of Cox models depends on the number of deaths, 
# not the number of indiviudals so this plot shows our
# power compared to the probability of death we would
# have to observed for 0.8 power.
powers <- c()
for (i in seq(0.1,0.9,0.01)){
  powers <- c(powers, powerCT.default(nE=65, nC=65, pE=i, pC=0.24, RR=2.3, alpha = 0.05))
}
powers
?powerCT.default
prob_at_pwr <- seq(0.1,0.9,0.01)[which.min(abs(powers-0.8))]
plot(seq(0.1,0.9,0.01), powers, pch=20, xlab = "Probability of death in treatment group", ylab = "Power", main = "How power increases with probability of death")
points(0.35, powers[26], pch=0, col="red", cex=1.5)
segments(0, powers[26], 0.35, powers[26], col="red", lty = 2)
points(prob_at_pwr, 0.8, pch=0, col="blue", cex=1.5)
segments(prob_at_pwr, 0, prob_at_pwr,0.8, col="blue", lty = 2)
legend(0.1, 0.9, c("Power given observed probability death", "Probability required for 0.8 power"), col = c("red", "blue"), lty = 2)

#### Simulations ####
proportion_censored <- length(f_data$Day_survived[which(f_data$Day_survived==63)])/length(f_data$Day_survived)

num_deaths <- length(f_data$Day_survived[which(f_data$Treatment==0 & f_data$Day_survived < 63)])
deaths_per_day <- (num_deaths/63)
deaths_pd_pi <- deaths_per_day/65
hazard_function <- function(t){deaths_pd_pi}
cov <- data.frame(f_data$Treatment)

num_pvals <- c()
all_simdata <- list()
all_sim_time <- c()
all_sim_status <- c()
all_sim_treatment <- c()
all_sim_surv_object <- c()
all_sim_fit1 <- c()
all_pvals <- c()
all_sim_fit <- list()
for (i in 1:1000){
  simdata <- sim.survdata(N=130, T=63, num.data.frames=1, X=cov, beta=c(0.85), censor = proportion_censored, hazard.fun = hazard_function)
  sim_time <- simdata$data$y
  sim_status <- 1-simdata$data$failed
  sim_treatment <- simdata$data$Treatment
  sim_surv_object <- Surv(sim_time, sim_status)
  sim_fit1 <- survfit(Surv(sim_time, sim_status) ~ f_data.Treatment, data = simdata$data)
  all_simdata[[i]] <- simdata
  all_sim_time <- c(all_sim_time,sim_time)
  all_sim_status <- c(all_sim_status,sim_status)
  all_sim_treatment <- c(all_sim_treatment,sim_treatment)
  all_sim_surv_object <- c(all_sim_surv_object,sim_surv_object)
  all_sim_fit[[i]] <- sim_fit1
  all_pvals <- c(all_pvals, surv_pvalue(sim_fit1)[[2]])
}
length(all_pvals[all_pvals<0.05])

transparent_coral <- rgb(255, 127, 80, max = 255, alpha = 15)
transparent_cyan <- rgb(25, 196, 200, max = 255, alpha = 15)

plot(0,type='n', ylim=c(0,1),xlim=c(0,63), xlab="Day", ylab="Survival probability")

combined_km_curves_0 <- matrix(0, nrow=1000, ncol=63)
for(i in 1:length(all_simdata)){
  treatment_0_len <- length(unique(all_simdata[[i]]$data$y[all_simdata[[i]]$data$f_data.Treatment == 0]))
  treatment_1_len <- length(unique(all_simdata[[i]]$data$y[all_simdata[[i]]$data$f_data.Treatment == 1]))
  t0_ids <- 1:treatment_0_len
  
  surv_table_0 <- cbind(all_sim_fit[[i]]$surv[t0_ids], all_sim_fit[[i]]$time[t0_ids])
  
  km_curve_0 <- matrix(0, nrow=63, ncol=2)
  for (k in 1:63) {
    if (length(which(surv_table_0[,2] <= k))==0){
      km_curve_0[k, 1] <- 1
    }
    else{
      km_curve_0[k, 1] <- surv_table_0[length(which(surv_table_0[,2] <= k)), 1]
    }
    km_curve_0[k, 2] <- k
  }
  combined_km_curves_0[i,] <- km_curve_0[,1]
  lines(km_curve_0[,2],km_curve_0[,1], type="l", col="coral", lwd=0.5)
}

plot_data_0 <- data.frame(
  time = 1:63,
  mean = sapply(1:63, function(x)mean(combined_km_curves_0[,x])),
  upper_quant = sapply(1:63, function(x)quantile(combined_km_curves_0[,x], probs = 0.95)),
  lower_quant = sapply(1:63, function(x)quantile(combined_km_curves_0[,x], probs = 0.05))
)

for(j in 1:length(all_simdata)){
  treatment_0_len <- length(unique(all_simdata[[j]]$data$y[all_simdata[[j]]$data$f_data.Treatment == 0]))
  treatment_1_len <- length(unique(all_simdata[[j]]$data$y[all_simdata[[j]]$data$f_data.Treatment == 1]))
  t1_ids <- (treatment_0_len + 1):(treatment_0_len + treatment_1_len)
  
  surv_table_1 <- cbind(all_sim_fit[[j]]$surv[t1_ids], all_sim_fit[[j]]$time[t1_ids])
  
  km_curve_1 <- matrix(0, nrow=63, ncol=2)
  for (j in 1:63) {
    if (length(which(surv_table_1[,2] <= j))==0){
      km_curve_1[j, 1] <- 1
    }
    else{
      km_curve_1[j, 1] <- surv_table_1[length(which(surv_table_1[,2] <= j)), 1]
    }
    km_curve_1[j, 2] <- j
  }
  lines(km_curve_1[,2],km_curve_1[,1], type="l", col="cyan3", lwd=0.5)
  }


