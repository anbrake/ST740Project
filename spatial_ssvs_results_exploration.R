
# Gamma results

# Convergence Diagnostics

load("alvin_model_output/convergence_diagnostics_spatial_ssvs8.RData")

summary(ess_spatial_ssvs8)

ess_spatial_ssvs8[ess_spatial_ssvs8 < 150]





psrf_upper_limit8 <- gr_spatial_ssvs8$psrf[, 2]

summary(psrf_upper_limit8)







# Model Results

load("summary_spatial_ssvs8.RData")

param_names8 <- rownames(summary_spatial_ssvs8$statistics)



post_means8 <- summary_spatial_ssvs8$statistics[, 1]

gamma_post_means <- post_means8[startsWith(param_names8, "gamma")]



gamma_post_means_mat <- matrix(gamma_post_means, nrow = 77, ncol = 46)



load("alvin_model_output/samples_spatial_ssvs8.RData")

samples_total <- rbind(samples_spatial_ssvs8[[1]], samples_spatial_ssvs8[[2]])

gamma_idx <- param_names8[startsWith(param_names8, "gamma")]

mips <- apply(samples_total[, gamma_idx], MARGIN = 2, mean)

mips_mat <- matrix(mips, nrow = 77, ncol = 46)



# Results for the other parameters

# Convergence Diagnostics

load("alvin_model_output/convergence_diagnostics_spatial_ssvs9.RData")

summary(ess_spatial_ssvs9)

ess_spatial_ssvs9[ess_spatial_ssvs9 < 200]

psrf_upper_limit9 <- gr_spatial_ssvs9$psrf[, 2]

summary(psrf_upper_limit9)

ess_spatial_ssvs9[ess_spatial_ssvs9 < 500]



# Model Results

load("summary_spatial_ssvs9.RData")

param_names9 <- rownames(summary_spatial_ssvs9$statistics)



post_means9 <- summary_spatial_ssvs9$statistics[, 1]

beta0_post_means <- post_means9[param_names9 == "beta0"]

delta_post_means <- post_means9[startsWith(param_names9, "delta")]

prob_post_means <- post_means9[startsWith(param_names9, "prob")]

rho_post_mean <- post_means9[param_names9 == "rho"]



# rho posterior mean and sd

summary_spatial_ssvs9$statistics[param_names9 == "rho", ]



summary_spatial_ssvs9$statistics[startsWith(param_names9, "prob"),]

max(summary_spatial_ssvs9$statistics[startsWith(param_names9, "prob"), 4])



mips_mat[41, ]

which(mips_mat[41, ] > .7)


