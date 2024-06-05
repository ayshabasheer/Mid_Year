load('res_jags_original_simplified.rds')
#apply(res_jags_original_simplified$BUGSoutput$sims.list$pi.prop[, 1, 1, 3, , 1], 2, median)

library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

estimates <- res_jags_original_simplified$BUGSoutput$sims.list$m

years <- c(2009:2015)
origins <- c("AT", "FI", "NL", "SE")
destinations <- c("AT", "FI", "NL", "SE")
ages <- c("Y00-04", "Y05-09", "Y10-14", "Y15-19", "Y20-24", "Y25-29", "Y30-34", "Y35-39", 
          "Y40-44", "Y45-49", "Y50-54", "Y55-59", "Y60-64", "Y65-69", "Y70-74", "Y75-79", 
          "Y80-84", "Y85-Inf")
sexes <- c("F", "M")

mig_flow <- data.frame()

for (a in 1:7) {
  for (b in 1:4) {
    for (c in 1:4) {
      for (d in 1:18) {
        for (e in 1:2) {
          median_value <- median(estimates[, a, b, c, d, e])
          
          mig_flow <- rbind(mig_flow, data.frame(
            origin = origins[b],
            destination = destinations[c],
            age = ages[d],
            sex = sexes[e],
            year = years[a],
            value_est = median_value
          ))
        }
      }
    }
  }
}
#write.csv(mig_flow, "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/Migration/MidYear/Roger-Castro/mig_flow.csv")
mig_flow <- mig_flow[!(mig_flow$origin == mig_flow$destination),]
#data Recieving country immigration
Reported_Immigration <- data.frame()

for (a in 1:7) {
  for (b in 1:4) {
    for (c in 1:4) {
      for (d in 1:18) {
        for (e in 1:2) {
          #median_value <- median(zR[a, b, c, d, e])
          
          Reported_Immigration <- rbind(Reported_Immigration, data.frame(
            origin = origins[b],
            destination = destinations[c],
            age = ages[d],
            sex = sexes[e],
            year = years[a],
            value_im = zR[a, b, c, d, e]
          ))
        }
      }
    }
  }
}

#data sending country emigration
Reported_Emigration <- data.frame()

for (a in 1:7) {
  for (b in 1:4) {
    for (c in 1:4) {
      for (d in 1:18) {
        for (e in 1:2) {
          #median_value <- median(zR[a, b, c, d, e])
          
          Reported_Emigration <- rbind(Reported_Emigration, data.frame(
            origin = origins[b],
            destination = destinations[c],
            age = ages[d],
            sex = sexes[e],
            year = years[a],
            value_em = zS[a, b, c, d, e]
          ))
        }
      }
    }
  }
}

Reported_Immigration <- Reported_Immigration[!(Reported_Immigration$origin == Reported_Immigration$destination),]
Reported_Emigration <- Reported_Emigration[!(Reported_Emigration$origin == Reported_Emigration$destination),]

final_df <- mig_flow %>%
  merge(Reported_Immigration, by = c("origin", "destination", "age", "sex", "year")) %>%
  merge(Reported_Emigration, by = c("origin", "destination", "age", "sex", "year"))
final_df <- final_df[!(is.na(final_df$value_im) |is.na(final_df$value_em) ),]
final_df <- final_df[order(final_df$year), ]
final_df_long <- melt(final_df, id.vars = c("origin", "destination", "age", "sex", "year"))
dev.set(2)
# Create the plot
ggplot(final_df_long, aes(x = age, y = value, color = sex, linetype = variable)) +
  geom_line(lwd = 1) +
  scale_linetype_manual(values = c(rep("solid", 10), rep("dashed", 6))) +
  scale_color_manual(values = c(brewer.pal(10, "Set3"), brewer.pal(6, "Set3"))) +
  #geom_point(aes(x = age, y = value, linetype = variable)) +
  #geom_line(aes(x = age, y = value, linetype = variable)) +
  #geom_point() +
  facet_grid(origin ~ destination + year) +
  labs(title = "Comparison of Value Estimation, Value Im, and Value Em by Age, Sex, Origin, Destination, and Year",
       x = "Age",
       y = "Value",
       color = "Sex",
       linetype = "Variable") +
  theme_minimal()
# Create the plot
ggplot(final_df_long, aes(x = age, y = value, color = sex, linetype = variable)) +
  geom_point()+
  geom_line(lwd = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +  # Adjust line type values as needed
  scale_color_manual(values = c("blue", "red")) +  # Adjust color values as needed
  facet_grid(origin ~ destination + year) +
  labs(title = "Comparison of Value Estimation, Value Im, and Value Em by Age, Sex, Origin, Destination, and Year",
       x = "Age",
       y = "Value",
       color = "Sex",
       linetype = "Variable") +
  theme_minimal()
plot <- ggplot(final_df, aes(x = age)) +
  geom_line(aes(y = value_est, color = sex), size = 1) +
  geom_line(aes(y = value_im, color = sex), size = 1) +
  #geom_smooth(aes(y = data), color = "red", linetype = "dashed", se = FALSE) +  # Smoothed line for data
  geom_line(aes(y = value_em, color = sex), size = 1) +
  #geom_smooth(aes(y = median), color = "blue", linetype = "dotted", se = FALSE) +  # Smoothed line for fit
  facet_grid(origin ~ destination + year) +
  labs(title = "Comparison of Value Estimation, Value Im, and Value Em by Age, Sex, Origin, Destination, and Year",
       x = "Age",
       y = "Value",
       color = "Sex",
       linetype = "Variable") +
  theme_minimal()
# Print the plot
print(plot)
#final_df1 <- final_df %>%
  #pivot_wider(names_from = year, values_from = OBS_VALUE)
# The plot
long_df <- melt(final_df, id.vars = c("origin", "destination", "age", "sex", "year"),
                variable.name = "source", value.name = "value")


ggplot(long_df, aes(x = interaction(origin, destination, age, sex, year), y = value, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Combination of Origin, Destination, Age, Sex, Year", y = "Value", fill = "Source",
       title = "Comparison of Migration Estimates") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5))

p <-ggplot(long_df, aes(x = interaction(origin, destination, age, sex), y = value, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ year, scales = "free_x") +
  labs(x = "Combination of Origin, Destination, Age, Sex", y = "Value", fill = "Source",
       title = "Comparison of Migration Estimates by Year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5))
library(plotly)
ggplotly(p)

library(coda)
library(rstan)
mcmc_samples <- as.mcmc.list(res_jags_original_simplified)
class(res_jags_original_simplified)
str(res_jags_original_simplified)
m <- res_jags_original_simplified_cov$BUGSoutput$sims.list
m <- m[350:2366]
dimnames(m)[[2]] <- make.names(dimnames(m)[[2]])
m1<- as.mcmc(m)
traceplot(m)
gelman.diag(as.mcmc.list(m1))
m <- as.mcmc.list(as.mcmc(m))
m <- lapply(m, mcmcUpgrade)
m1 <- mcmc.list(lapply(m,mcmc))
traceplot(m)
traceplot(res_jags_original_simplified_cov)
gelman.diag(res_jags_original_simplified_cov)
ESS <- effectiveSize(res_jags_original_simplified_cov)
pat <- paste0("m\\[([1-7]),([1-4]),([1-4]),([1-9]|[0-8]),([1-2])\\]$")
ESS1<- ESS[grep(pat, names(ESS))]
ESSClass <- cut(ESS1, breaks = c(-Inf, 50, 200, 1000, Inf))
ESS1 <- data.frame(ESS = ESS1, class = ESSClass)
ESS1 <- ESS1[!(ESS1$ESS ==0),]
esscom <- merge(ESS1, ESS2, by = "row.names", all = TRUE)
higher_ess <- ifelse(esscom$ESS > esscom$ESSs, "ESS", "ESSs")
heidel.diag(res_jags_original_simplified)
con1 <- geweke.diag(res_jags_original_simplified)
concov <-geweke.diag(res_jags_original_simplified_cov)
con <- geweke.diag(res_jags_original_simplified$BUGSoutput$sims.list$m)
check_convergence <- function(geweke_z) {
  return(all(geweke_z > -1.96 & geweke_z < 1.96))
}

con2 <- check_convergence(con1$z)
dic.samples(res_jags_original_simplified_cov$BUGSoutput$DIC)
autocorr.ploautocorr.ploautocorr.plot(res_jags_original_simplified)
# Posterior Predictive Checks
library(rstantools)
ppc <- posterior_predict(res_jags_original_simplified$BUGSoutput$sims.list$m)
plot(ppc)
loo(res_jags_original_simplified)
# Cross-Validation (if using rstanarm or similar package)
loo_result <- loo(as.mcmc(res_jags_original_simplified))

ESS1[which(ESS1$ESS == max(ESS1$ESS)),]

ESS1[which(ESS1$class == "(200,1e+03]"),]
ESSs <- effectiveSize(res_jags_original_simplified)
pat <- paste0("m\\[([1-7]),([1-4]),([1-4]),([1-9]|[0-8]),([1-2])\\]$")
ESS2<- ESSs[grep(pat, names(ESSs))]
ESSClass <- cut(ESS2, breaks = c(-Inf, 50, 200, 1000, Inf))
ESS2 <- data.frame(ESSs = ESS2, class = ESSClass)
ESS2 <- ESS2[!(ESS2$ESSs ==0),]
ESS2[which(ESS2$class == "(50,200]"),]

write.csv(final_df, "final_df.csv")
gr <- gelman.diag(as.mcmc(res_jags_original_simplified$BUGSoutput$sims.list))
mcmc_list <- res_jags_original_simplified$BUGSoutput$sims.list

# Compute Gelman-Rubin statistic for each parameter
gelman_rubin_list <- lapply(mcmc_list, function(mcmc_chain) {
  if (length(mcmc_chain) > 1) {
    gelman.diag(mcmc_chain)
  } else {
    # If only one chain is present, return NA or handle accordingly
    NA
  }
})

# Filter out NA values
gelman_rubin_list <- gelman_rubin_list[!sapply(gelman_rubin_list, is.na)]
gelman.diag(mcmc_list)

# Assuming geweke_results is a list containing Geweke diagnostics for each variable
# First, aggregate the Geweke diagnostics
aggregated_geweke <- sapply(con1[350:4382], function(x) mean(con1$z,na.rm = TRUE))

# Visualize the aggregated Geweke diagnostics
hist(aggregated_geweke, breaks = 20, main = "Histogram of Aggregated Geweke Diagnostics", xlab = "Geweke z-score")

# Optionally, you can also combine Geweke diagnostics from different chains
combined_geweke <- c()
for (i in 350:4382) {
  combined_geweke <- c(combined_geweke, con1$z[[i]])
}

# Calculate summary statistics for the combined Geweke diagnostics
summary_stats <- summary(combined_geweke)
print(summary_stats)
subset_values <- con1$z[350:4382]

# Extract values within the specified range
values_within_range <- subset_values[subset_values > -1.96 & subset_values < 1.96]
subset_valuescov <- concov$z[490:4521]

# Extract values within the specified range
values_within_rangecov <- subset_valuescov[subset_valuescov > -1.96 & subset_valuescov < 1.96]

covex<- concov$z[grep(pat, names(concov$z))]
covnex<- con1$z[grep(pat, names(con1$z))]
covgei <- merge(covex, covnex, by = "row.names", all = TRUE)
higher_ess <- ifelse(esscom$ESS > esscom$ESSs, "ESS", "ESSs")
auto <- autocorr.diag(as.mcmc(res_jags_original_simplified$BUGSoutput$sims.list$m))
auto <- autocorr.diag(res_jags_original_simplified)
auto <-acf(as.ts(as.mcmc(res_jags_original_simplified$BUGSoutput$sims.list$m)), lag.max = 50, plot = FALSE)

#############################################

estimates <- res_jags_original_simplified$BUGSoutput$sims.list$pi.prop
#sd1 <- res_jags_original_simplified$BUGSoutput$sd$pi.prop

years <- c(2009:2015)
origins <- c("AT", "FI", "NL", "SE")
destinations <- c("AT", "FI", "NL", "SE")
ages <- c("Y00-04", "Y05-09", "Y10-14", "Y15-19", "Y20-24", "Y25-29", "Y30-34", "Y35-39", 
          "Y40-44", "Y45-49", "Y50-54", "Y55-59", "Y60-64", "Y65-69", "Y70-74", "Y75-79", 
          "Y80-84", "Y85-Inf")
sexes <- c("F", "M")

mig_flow <- data.frame()

for (a in 1:7) {
  for (b in 1:4) {
    for (c in 1:4) {
      for (d in 1:18) {
        for (e in 1:2) {
          median_value <-median(estimates[, a, b, c, d, e])
          
          mig_flow <- rbind(mig_flow, data.frame(
            origin = origins[b],
            destination = destinations[c],
            age = ages[d],
            sex = sexes[e],
            year = years[a],
            proportion = median_value
          ))
        }
      }
    }
  }
}
#write.csv(mig_flow, "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/Migration/MidYear/Roger-Castro/mig_flow.csv")
mig_flow <- mig_flow[!(mig_flow$origin == mig_flow$destination),]

estimates_cov <- res_jags_original_simplified_cov$BUGSoutput$sims.list$pi.prop

mig_flow_cov <- data.frame()

for (a in 1:7) {
  for (b in 1:4) {
    for (c in 1:4) {
      for (d in 1:18) {
        for (e in 1:2) {
          median_value <- median(estimates_cov[, a, b, c, d, e])
          
          
          mig_flow_cov <- rbind(mig_flow_cov, data.frame(
            origin = origins[b],
            destination = destinations[c],
            age = ages[d],
            sex = sexes[e],
            year = years[a],
            proportion_cov = median_value
            
          ))
        }
      }
    }
  }
}
#write.csv(mig_flow, "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/Migration/MidYear/Roger-Castro/mig_flow.csv")
mig_flow_cov <- mig_flow_cov[!(mig_flow_cov$origin == mig_flow_cov$destination),]

model_com_prop <- mig_flow %>%
  merge(mig_flow_cov, by = c("origin", "destination", "age", "sex", "year"))
write.csv(model_com_prop, "model_com_prop.csv") 

#"/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/Migration/MidYear/Log-Linear/model_com.csv")
