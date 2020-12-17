#### SIMULATIONS ####
library("coxed")
library("survminer")
proportion_censored <- 0.6899225 #length(f_data$Day_survived[which(f_data$Day_survived==63)])/length(f_data$Day_survived)

num_deaths <- 17 #length(f_data$Day_survived[which(f_data$Treatment==0 & f_data$Day_survived < 63)])
deaths_per_day <- 0.2698413 #(num_deaths/63)
deaths_pd_pi <- 0.004151404 #deaths_per_day/65
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
for (i in 1:2){
  simdata <- sim.survdata(N=130, T=63, num.data.frames=1, X=cov, beta=c(0.714), censor = proportion_censored, hazard.fun = hazard_function)
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

