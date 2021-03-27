#### PREAMBLE ####
color_scheme_set("teal")
options(buildtools.check = function(action) TRUE)

#### DATA ####
all_data <- read.csv("../Behaviour data/all_behaviour_data_copy.csv")
# Make sure everything that should be is a factor
all_data$Allogrooming <- all_data$Allogrooming/100
all_data$Antennation <- all_data$Antennation/100
all_data$Environment <- all_data$Environment/100
all_data$Trophallaxis <- all_data$Trophallaxis/100
all_data$Day <- as.factor(all_data$Day)
all_data$ID <- as.factor(all_data$ID)
all_data$Colony <- as.factor(all_data$Colony)
all_data$Day2 <- (all_data$Day==2)*1
all_data$Day4 <- (all_data$Day==4)*1
all_data$Day10 <- (all_data$Day==10)*1

#### FUNCTIONS ####
run_null_brum <- function(formula){
  gc()
  #priors <- set_prior("normal(0,2.5)", class = "b")
  model <- brm(formula,
               data = all_data,
               family = zero_inflated_beta(), 
               iter = 5000,
               control = list(adapt_delta = 0.99, max_treedepth = 15),
               save_all_pars = T,
               cores=4)
  posterior_matrix <- as.matrix(model)
  posteriors_plot <- mcmc_areas(posterior_matrix,
                                pars = colnames(posterior_matrix)[1:8],#1:11#c('b_Day4','b_Day10', 'b_treat_num:Day4', 'b_treat_num:Day10','b_TreatmentT:Day10'),
                                prob=0.95)
  return(list(model=model, posterior=posterior_matrix, plot=posteriors_plot))
}

run_brum <- function(formula){
  gc()
  priors <- set_prior("normal(0, 2.5)", class="sd")
  model <- brm(formula,
               data = all_data,
               family = zero_inflated_beta(), 
               iter = 5000,
               control = list(adapt_delta = 0.99, max_treedepth = 15),
               save_all_pars = T,
               cores=4,
               prior = priors)
  posterior_matrix <- as.matrix(model)
  posteriors_plot <- mcmc_areas(posterior_matrix,
                                pars = colnames(posterior_matrix)[1:8],#1:11#c('b_Day4','b_Day10', 'b_treat_num:Day4', 'b_treat_num:Day10','b_TreatmentT:Day10'),
                                prob=0.95)
  return(list(model=model, posterior=posterior_matrix, plot=posteriors_plot))
}

filenames_butting <- c("butting_m0.csv", "butting_m1.csv", "butting_m2.csv", "butting_m3.csv", "butting_m4.csv")
run_stan_butting <- function(formula, f_index){
  model <- stan_glmer(formula,
                      data = all_data,
                      family=neg_binomial_2,
                      diagnostic_file=filenames_butting[f_index],
                      prior = normal(0,2.5, autoscale = TRUE),
                      adapt_delta = 0.99)
  posterior_matrix <- as.matrix(model)
  posteriors_plot <- mcmc_areas(posterior_matrix,
                                pars = colnames(posterior_matrix)[1:6],
                                prob=0.95)
  return(list(model=model, posterior=posterior_matrix, plot=posteriors_plot))
}

filenames_selfgrooming <- c("selfgrooming_m0.csv", "selfgrooming_m1.csv", "selfgrooming_m2.csv", "selfgrooming_m3.csv", "selfgrooming_m4.csv")
run_stan_selfgrooming <- function(formula, f_index){
  model <- stan_glmer(formula,
                      data = all_data,
                      family=neg_binomial_2,
                      diagnostic_file=filenames_selfgrooming[f_index],
                      prior = normal(0,2.5, autoscale = TRUE),
                      adapt_delta = 0.99)
  posterior_matrix <- as.matrix(model)
  posteriors_plot <- mcmc_areas(posterior_matrix,
                                pars = colnames(posterior_matrix)[1:6],
                                prob=0.95)
  return(list(model=model, posterior=posterior_matrix, plot=posteriors_plot))
}

#### MODELS ####
allo_m0 <- run_null_brum(Allogrooming ~ 1 + (1|Colony/Dish)) # intercept only/null
allo_m1 <- run_brum(Allogrooming ~ Treatment + (1|Colony/Dish)) # Treatment only
allo_m2 <- run_brum(Allogrooming ~ Day + (1|Colony/Dish)) # day only
allo_m3 <- run_brum(Allogrooming ~ Treatment + Day + (1|Colony/Dish)) # Treatment and day/no interaction
allo_m4 <- run_brum(Allogrooming ~ Treatment * Day + (1|Colony/Dish)) # full

ant_m0 <- run_null_brum(Antennation ~ 1 + (1|Colony/Dish)) # intercept only/null
ant_m1 <- run_brum(Antennation ~ Treatment + (1|Colony/Dish)) # Treatment only
ant_m2 <- run_brum(Antennation ~ Day + (1|Colony/Dish)) # day only
ant_m3 <- run_brum(Antennation ~ Treatment + Day + (1|Colony/Dish)) # Treatment and day/no interaction
ant_m4 <- run_brum(Antennation ~ Treatment * Day + (1|Colony/Dish)) # full

env_m0 <- run_null_brum(Environment ~ 1 + (1|Colony/Dish)) # intercept only/null
env_m1 <- run_brum(Environment ~ Treatment + (1|Colony/Dish)) # Treatment only
env_m2 <- run_brum(Environment ~ Day + (1|Colony/Dish)) # day only
env_m3 <- run_brum(Environment ~ Treatment + Day + (1|Colony/Dish)) # Treatment and day/no interaction
env_m4 <- run_brum(Environment ~ Treatment * Day + (1|Colony/Dish)) # full

tro_m0 <- run_null_brum(Trophallaxis ~ 1 + (1|Colony/Dish)) # intercept only/null
tro_m1 <- run_brum(Trophallaxis ~ Treatment + (1|Colony/Dish)) # Treatment only
tro_m2 <- run_brum(Trophallaxis ~ Day + (1|Colony/Dish)) # day only
tro_m3 <- run_brum(Trophallaxis ~ Treatment + Day + (1|Colony/Dish)) # Treatment and day/no interaction
tro_m4 <- run_brum(Trophallaxis ~ Treatment * Day + (1|Colony/Dish)) # full

#### BAYES FACTORS ####
allo_comp <- bayesfactor_models(allo_m1$model, allo_m2$model, allo_m3$model, allo_m4$model, denominator = allo_m0$model)
allo_comp
allo_inc <- bayesfactor_inclusion(allo_comp)
allo_inc

ant_comp <- bayesfactor_models(ant_m1$model, ant_m2$model, ant_m3$model, ant_m4$model, denominator = ant_m0$model)
ant_comp
ant_inc <- bayesfactor_inclusion(ant_comp)
ant_inc

env_comp <- bayesfactor_models(env_m1$model, env_m2$model, env_m3$model, env_m4$model, denominator = env_m0$model)
env_comp
env_inc <- bayesfactor_inclusion(env_comp)
env_inc

tro_comp <- bayesfactor_models(tro_m1$model, tro_m2$model, tro_m3$model, tro_m4$model, denominator = tro_m0$model)
tro_inc <- bayesfactor_inclusion(tro_comp)
tro_comp
tro_inc

but_comp <- bayesfactor_models(but_m1$model, denominator = but_m2$model)
but_inc <- bayesfactor_inclusion(but_comp)
but_comp
but_inc

## Save models
allo_m0_model <- allo_m0$model
allo_m1_model <- allo_m1$model
allo_m2_model <- allo_m2$model
allo_m3_model <- allo_m3$model
allo_m4_model <- allo_m4$model

save(allo_m0_model, file="allo_m0.Rdata")
save(allo_m1_model, file="allo_m1.Rdata")
save(allo_m2_model, file="allo_m2.Rdata")
save(allo_m3_model, file="allo_m3.Rdata")
save(allo_m4_model, file="allo_m4.Rdata")
