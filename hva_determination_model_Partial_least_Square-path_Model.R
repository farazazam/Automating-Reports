



# input files to this script:
## dma_map_all_markets.csv - dma mapping table with population
## hva_determination_model_lag_df_noNA_v9.csv - preprocessed model data frame
## HVA Variable List v9 mod.csv - model configuration file

# output files
## National Model:
### hva_determination_summary.txt - model output
### hva_determination_measurement_model.csv - model coefficients
### hva_scaling_parameters.csv - variable scaling parameters
### observed_vs_predicted_scores.csv - table of observed vs predicted score
### HVA Funnel Observed vs. Predicted.pdf - plots of observed vs predicted scores
### HVA Outcome Observed vs. Predicted (Scaled and Original).pdf - scaled vs original outcome

## FM and nonFM model
### observed_vs_predicted_scores_fm_nonfm_split.csv - table of observed vs predicted score
### HVA Funnel Observed vs. Predicted (FM nonFM Split).pdf - plots of observed vs predicted scores
### HVA Outcome Observed vs. Predicted (Scaled and Original, FM nonFM Split).pdf - scaled vs original outcome



### Set Up ----------------------------------------------------

# set wd
setwd("C:/Users/Dropbox/Work/Mazda/projects/Marketing/Marketing Analytics/HVA HVL Determination/HVA Model Results/hva_model_v9_for_diane")

# load packages
pacman::p_load(dplyr, tidyr, stringr, data.table, stringi, lubridate, zoo, readxl, glmnet, selectiveInference, gsubfn, plspm, caret, gdata, car, ggplot2, plotly, rio, openxlsx, scales, ggthemes, ggplot2, cowplot)
#--------------------------------------------------------------




# Model--------------------------------------------------------

# read dma map
dma_map = read.csv("dma_map_all_markets.csv", stringsAsFactors = F)

# read processed data frame
xl = read.csv("hva_determination_model_lag_df_noNA_v9.csv", stringsAsFactors = F)

# model configuration file
model_configs = read.csv("HVA Variable List v9 mod.csv", stringsAsFactors = F) %>%
  filter(Use.In.HVA == 1)


# inner model
inner_model = cbind(
  "Calendar"        = c(0,0,0,0,0,0,1),
  "Calendar.Upper"  = c(0,0,0,1,0,0,0),
  "Upper"           = c(0,0,0,1,0,0,0),
  "Upper.Mid"       = c(0,0,0,0,1,0,0),
  "Lower.Mid"       = c(0,0,0,0,0,1,0),
  "Lower"           = c(0,0,0,0,0,0,1),
  "Outcome"         = c(0,0,0,0,0,0,0)
)
rownames(inner_model) = colnames(inner_model)

# outer model
lv = colnames(inner_model)
outer_blocks_lag = lapply(lv, function(v) model_configs$variable_lag[model_configs$Funnel == v])
names(outer_blocks_lag) = colnames(inner_model)

# latent variable modes
mod_modes = rep("B", ncol(inner_model))

# fit model
hva_pls_lag = plspm(xl, inner_model, outer_blocks_lag, modes = mod_modes, boot.val = T, scheme = "factorial", br = 100, scaled = T)


# model output
sink("hva_determination_summary.txt")
print(summary(hva_pls_lag))
sink()

# scaling parameters
scale_params = data.frame(
  variable = colnames(hva_pls_lag$manifests),
  center = attr(hva_pls_lag$manifests, "scaled:center"),
  scale = attr(hva_pls_lag$manifests, "scaled:scale"),
  stringsAsFactors = F
)

write.csv(scale_params, "hva_scaling_parameters.csv", row.names = F)



### model coefficients
action_weights = hva_pls_lag$boot$weights %>%
  data.frame(key = rownames(.), .) %>%
  separate(key, into = c("LV", "variable"), sep = "-") %>%
  select(LV, MV = variable, MV_coef = Mean.Boot, MV_se = Std.Error, mv_lower = perc.025, mv_upper = perc.975)

lv_paths = hva_pls_lag$boot$paths %>%
  data.frame(key = rownames(.), .) %>%
  separate(key, into = c("LV_from", "LV_to"), sep = " -> ") %>%
  select(LV_from, LV_to, path_coef = Mean.Boot, path_se = Std.Error, path_lower = perc.025, path_upper = perc.975)

lv_efs = hva_pls_lag$boot$total.efs %>%
  data.frame(key = rownames(.), .) %>%
  separate(key, into = c("LV_from", "LV_to"), sep = " -> ") %>%
  select(LV_from, LV_to, total_coef = Mean.Boot, total_se = Std.Error, total_lower = perc.025, total_upper = perc.975)


lv_summary = left_join(
  lv_paths, lv_efs, by = c("LV_from", "LV_to")
) %>%
  dplyr::rename(path_cf = path_coef, total_cf = total_coef, path_lwr = path_lower, path_upr = path_upper, total_lwr = total_lower, total_upr = total_upper)


action_summary = left_join(lv_summary, action_weights, by = c("LV_from" = "LV")) %>%
  mutate(path_coef = round(path_cf*MV_coef, 4),
         total_coef = round(total_cf*MV_coef, 4),
         path_serr = sqrt(((path_se^2) * (MV_se^2))/((path_se^2) + (MV_se^2))),
         path_tval = path_coef/path_serr,
         path_lower = path_coef - 2*path_serr,
         path_upper = path_coef + 2*path_serr,
         path_significant = as.numeric(sign(path_lower) == sign(path_upper)),
         total_serr = sqrt(((total_se^2) * (MV_se^2))/((total_se^2) + (MV_se^2))),
         total_tval = total_coef/total_serr,
         total_lower = total_coef - 2*total_serr,
         total_upper = total_coef + 2*total_serr,
         total_significant = as.numeric(sign(total_lower) == sign(total_upper))
  ) %>%
  select(LV_from, LV_to, MV, path_coef, LV_coef = total_cf, MV_coef, total_coef, path_serr, path_tval, path_lower, path_upper, path_significant, total_serr, total_tval, total_lower, total_upper, total_significant)


action_by_segment = action_summary %>%
  separate(MV, into = c("MV", "lag"), sep = "_lag") %>%
  left_join(model_configs, by = c("MV" = "variable")) %>%
  select(LV_from:lag, LV = Funnel, path_coef:total_significant)


action_df = action_by_segment %>%
  left_join(model_configs, by = c("MV" = "variable")) %>%
  select(LV_from:var_name) %>%
  mutate(Funnel = LV_to,
         var_name_lag = case_when(
           is.na(lag) ~ var_name,
           !is.na(lag) ~ paste0(var_name, " (lag = ", lag, ")")
         )
  ) 

action_out = action_df %>%
  mutate(path = paste(LV_from, LV_to, sep = " -> "),
         variable = paste0(MV, if_else(is.na(lag), "", paste0("_lag", lag)))) %>%
  select(variable, variable_name = var_name, Category, funnel_path = path, funnel_from = LV_from, funnel_to = LV_to,
         MV_coef, LV_coef, total_coef, total_serr:total_tval) 


# write
write.csv(action_out, "hva_determination_measurement_model.csv", row.names = F)


#------------------------------------------------------------------





### Score calculation and Plots ----------------------------------

# latent variable coefficient df
lv_efs_df = lv_efs %>%
  mutate(
    LV_from = factor(LV_from, levels = colnames(inner_model)),
    Funnel = factor(LV_to, levels = rev(colnames(inner_model))))

# X matrix
X = hva_pls_lag$manifests %>%
  as.matrix() 

# coefficient matrix
beta_mat = hva_pls_lag$boot$weights %>%
  data.frame(path = row.names(.), ., stringsAsFactors = F) %>%
  separate(path, into = c("lv", "mv"), sep = "-") %>%
  mutate(mv = factor(mv, levels = mv)) %>%
  select(mv, lv, coef = Mean.Boot) %>%
  spread(mv, coef, fill = 0) %>%
  data.frame(row.names = "lv") %>%
  replace(is.na(.), 0) %>%
  t() %>%
  as.matrix()

# calculate scores
scores = X %*% beta_mat

# create score data frame
scores_sc_df = data.frame(
  yearmon = xl$yearmon,
  dma_cd = xl$dma_cd,
  msa_population = xl$msa_population,
  scores,
  stringsAsFactors = F
)

# scaling parameter for sales
sales_sc = scale_params["sales",]

# estimate scores via inner model coefficients
est_scores = scores_sc_df %>%
  mutate(
    Upper.Mid.Est = Upper *
      filter(lv_efs_df, LV_from == "Upper", LV_to == "Upper.Mid")$total_coef,
    Lower.Mid.Est = Upper.Mid *
      filter(lv_efs_df, LV_from == "Upper.Mid", LV_to == "Lower.Mid")$total_coef,
    Lower.Est = Lower.Mid *
      filter(lv_efs_df, LV_from == "Lower.Mid", LV_to == "Lower")$total_coef,
    Outcome.Scaled.Est =
      Lower *
      filter(lv_efs_df, LV_from == "Lower", LV_to == "Outcome")$total_coef +
      Calendar *
      filter(lv_efs_df, LV_from == "Calendar", LV_to == "Outcome")$total_coef
  ) %>%
  left_join(select(dma_map, dma_cd, MARKET_NAME), by = "dma_cd") %>%
  select(yearmon, dma_cd, dma = MARKET_NAME, msa_population, Calendar:Outcome.Scaled.Est) %>%
  mutate(
    Outcome.Scaled = Outcome,
    Outcome = (Outcome.Scaled * sales_sc$scale + sales_sc$center) * msa_population,
    Outcome.Est = (Outcome.Scaled.Est * sales_sc$scale + sales_sc$center) * msa_population
  )

# compare estimated vs observed for outcome
with(est_scores, plot(Outcome, Outcome.Est)); abline(0, 1)

# create data frame for plot
plotdf = est_scores %>%
  mutate(DMA = paste(dma_cd, dma)) %>%
  select(yearmon, dma_cd, dma_name = dma, dma = DMA, msa_population,
         Calendar, Upper, Upper.Mid, Upper.Mid.Est, Lower.Mid, Lower.Mid.Est,
         Lower, Lower.Est, Outcome.Scaled, Outcome.Scaled.Est, Outcome, Outcome.Est)

# write plot data
write.csv(plotdf, "observed_vs_predicted_scores.csv", row.names = F)

# obtain unique dma
dma = unique(plotdf$dma)

# create pdf
pdf("HVA Funnel Observed vs. Predicted.pdf", height = 8, width = 15)
p1 <- ggplot(data = plotdf, aes(x = Upper.Mid, y = Upper.Mid.Est, col = 1, alpha = .5)) +
  guides(colour = F, alpha = F) +
  #stat_smooth(method = "lm", col = "black", size = .5) +
  geom_abline() +
  geom_point()
p2 <- ggplot(data = plotdf, aes(x = Lower.Mid, y = Lower.Mid.Est, col = 1, alpha = .5)) +
  guides(colour = F, alpha = F) +
  #stat_smooth(method = "lm", col = "black", size = .5) +
  geom_abline() +
  geom_point()
p3 <- ggplot(data = plotdf, aes(x = Lower, y = Lower.Est, col = 1, alpha = .5)) +
  guides(colour = F, alpha = F) +
  #stat_smooth(method = "lm", col = "black", size = .5) +
  geom_abline() +
  geom_point()
p4 <- ggplot(data = plotdf, aes(x = Outcome, y = Outcome.Est, col = 1, alpha = .5)) +
  guides(colour = F, alpha = F) +
  #stat_smooth(method = "lm", col = "black", size = .5) +
  geom_abline() +
  geom_point()
print(
  plot_grid(p1, p2, p3, p4, labels = 
              c("Upper Mid", "Lower Mid", "Lower", "Sales"), label_x = .05)
)

for(d in dma){
  tempdf = filter(plotdf, dma == d)
  p1 <- ggplot(data = tempdf, aes(x = Upper.Mid, y = Upper.Mid.Est, col = 1, alpha = .5)) +
    guides(colour = F, alpha = F) +
    #stat_smooth(method = "lm", col = "black", size = .5) +
    geom_abline() +
    geom_point()
  p2 <- ggplot(data = tempdf, aes(x = Lower.Mid, y = Lower.Mid.Est, col = 1, alpha = .5)) +
    guides(colour = F, alpha = F) +
    #stat_smooth(method = "lm", col = "black", size = .5) +
    geom_abline() +
    geom_point()
  p3 <- ggplot(data = tempdf, aes(x = Lower, y = Lower.Est, col = 1, alpha = .5)) +
    guides(colour = F, alpha = F) +
    #stat_smooth(method = "lm", col = "black", size = .5) +
    geom_abline() +
    geom_point()
  p4 <- ggplot(data = tempdf, aes(x = Outcome, y = Outcome.Est, col = 1, alpha = .5)) +
    guides(colour = F, alpha = F) +
    #stat_smooth(method = "lm", col = "black", size = .5) +
    geom_abline() +
    geom_point()
  print(
    plot_grid(p1, p2, p3, p4, labels = 
                c(
                  paste("Upper Mid,", d), 
                  paste("Lower Mid,", d), 
                  paste("Lower,", d), 
                  paste("Sales,", d)
                ), 
              label_size = 10
    )
  )
}
dev.off()

# create pdf for outcome scaled vs original
pdf("HVA Outcome Observed vs. Predicted (Scaled and Original).pdf", height = 8, width = 15)

p1 <- ggplot(data = plotdf, aes(x = Outcome.Scaled, y = Outcome.Scaled.Est, col = 1, alpha = .5)) +
  guides(colour = F, alpha = F) +
  #stat_smooth(method = "lm", col = "black", size = .5) +
  geom_abline() +
  geom_point()
p2 <- ggplot(data = plotdf, aes(x = Outcome, y = Outcome.Est, col = 1, alpha = .5)) +
  guides(colour = F, alpha = F) +
  #stat_smooth(method = "lm", col = "black", size = .5) +
  geom_abline() +
  geom_point()
print(
  plot_grid(p1, p2, labels = 
              c("Sales (Scaled)", "Sales"), label_x = .05)
)

for(d in dma){
  tempdf = filter(plotdf, dma == d)
  p1 <- ggplot(data = tempdf, aes(x = Outcome.Scaled, y = Outcome.Scaled.Est, col = 1, alpha = .5)) +
    guides(colour = F, alpha = F) +
    #stat_smooth(method = "lm", col = "black", size = .5) +
    geom_abline() +
    geom_point()
  p2 <- ggplot(data = tempdf, aes(x = Outcome, y = Outcome.Est, col = 1, alpha = .5)) +
    guides(colour = F, alpha = F) +
    #stat_smooth(method = "lm", col = "black", size = .5) +
    geom_abline() +
    geom_point()
  print(
    plot_grid(p1, p2, labels = 
                c(
                  paste("Sales (Scaled),", d), 
                  paste("Sales,", d)
                ), 
              label_size = 10
    )
  )
}
dev.off()

#--------------------------------------------------------







#### 3.7.18: Run models for FM vs non-FM models ----------
fm_dma = filter(dma_map, is_fm == 1)$dma_cd

# model data frame for fm
xl_fm = filter(xl, dma_cd %in% fm_dma)
# model data frame for non fm
xl_nonfm = filter(xl, !dma_cd %in% fm_dma)


# fit fm model
hva_pls_lag_fm = plspm(xl_fm, inner_model, outer_blocks_lag, modes = mod_modes, boot.val = T, scheme = "factorial", br = 100, scaled = T)

# fit nonfm model
hva_pls_lag_nonfm = plspm(xl_nonfm, inner_model, outer_blocks_lag, modes = mod_modes, boot.val = T, scheme = "factorial", br = 100, scaled = T)


### Note: can output model summary and scaling parameters using this code.
# # model output
# sink("hva_determination_summary.txt")
# print(summary(hva_pls_lag))
# sink()
# 
# # scaling parameters
# scale_params = data.frame(
#   variable = colnames(hva_pls_lag$manifests),
#   center = attr(hva_pls_lag$manifests, "scaled:center"),
#   scale = attr(hva_pls_lag$manifests, "scaled:scale"),
#   stringsAsFactors = F
# )
# 
# write.csv(scale_params, "hva_scaling_parameters.csv", row.names = F)



# compare coefficients
cbind(
  select(hva_pls_lag_fm$boot$total.efs, Coef_FM = Mean.Boot),
  select(hva_pls_lag_nonfm$boot$total.efs, Coef_nonFM = Mean.Boot)
)

cbind(
  select(hva_pls_lag_fm$boot$weights, Coef_FM = Mean.Boot),
  select(hva_pls_lag_nonfm$boot$weights, Coef_nonFM = Mean.Boot)
)

cbind(
  select(hva_pls_lag_fm$boot$rsq, FM = Mean.Boot),
  select(hva_pls_lag_nonfm$boot$rsq, nonFM = Mean.Boot)
)
# -------------------------------------------------------



# make score comparison plots for FM vs nonFM -----------

### FM
lv_efs_fm = hva_pls_lag_fm$boot$total.efs %>%
  data.frame(key = rownames(.), .) %>%
  separate(key, into = c("LV_from", "LV_to"), sep = " -> ") %>%
  select(LV_from, LV_to, total_coef = Mean.Boot, total_se = Std.Error, total_lower = perc.025, total_upper = perc.975)

lv_efs_fm_df = lv_efs_fm %>%
  mutate(
    LV_from = factor(LV_from, levels = colnames(inner_model)),
    Funnel = factor(LV_to, levels = rev(colnames(inner_model))))

scale_params_fm = data.frame(
  variable = colnames(hva_pls_lag_fm$manifests),
  center = attr(hva_pls_lag_fm$manifests, "scaled:center"),
  scale = attr(hva_pls_lag_fm$manifests, "scaled:scale"),
  stringsAsFactors = F
)

Xfm = hva_pls_lag_fm$manifests %>%
  as.matrix() 

beta_mat_fm = hva_pls_lag_fm$boot$weights %>%
  data.frame(path = row.names(.), ., stringsAsFactors = F) %>%
  separate(path, into = c("lv", "mv"), sep = "-") %>%
  mutate(mv = factor(mv, levels = mv)) %>%
  select(mv, lv, coef = Mean.Boot) %>%
  spread(mv, coef, fill = 0) %>%
  data.frame(row.names = "lv") %>%
  replace(is.na(.), 0) %>%
  t() %>%
  as.matrix()

scores_fm = Xfm %*% beta_mat_fm

# create data frame
scores_fm_sc_df = data.frame(
  yearmon = xl_fm$yearmon,
  dma_cd = xl_fm$dma_cd,
  msa_population = xl_fm$msa_population,
  scores_fm,
  stringsAsFactors = F
)

# estimated scores
sales_fm_sc = scale_params_fm["sales",]

est_scores_fm = scores_fm_sc_df %>%
  mutate(
    Upper.Mid.Est = Upper *
      filter(lv_efs_fm_df, LV_from == "Upper", LV_to == "Upper.Mid")$total_coef,
    Lower.Mid.Est = Upper.Mid *
      filter(lv_efs_fm_df, LV_from == "Upper.Mid", LV_to == "Lower.Mid")$total_coef,
    Lower.Est = Lower.Mid *
      filter(lv_efs_fm_df, LV_from == "Lower.Mid", LV_to == "Lower")$total_coef,
    Outcome.Scaled.Est =
      Lower *
      filter(lv_efs_fm_df, LV_from == "Lower", LV_to == "Outcome")$total_coef +
      Calendar *
      filter(lv_efs_fm_df, LV_from == "Calendar", LV_to == "Outcome")$total_coef
  ) %>%
  left_join(select(dma_map, dma_cd, MARKET_NAME), by = "dma_cd") %>%
  select(yearmon, dma_cd, dma = MARKET_NAME, msa_population, Calendar:Outcome.Scaled.Est) %>%
  mutate(
    Outcome.Scaled = Outcome,
    Outcome = (Outcome.Scaled * sales_fm_sc$scale + sales_fm_sc$center) * msa_population,
    Outcome.Est = (Outcome.Scaled.Est * sales_fm_sc$scale + sales_fm_sc$center) * msa_population
  )

# make plot df
plotdf_fm = est_scores_fm %>%
  mutate(DMA = paste(dma_cd, dma)) %>%
  select(yearmon, dma_cd, dma_name = dma, dma = DMA, msa_population,
         Calendar, Upper, Upper.Mid, Upper.Mid.Est, Lower.Mid, Lower.Mid.Est,
         Lower, Lower.Est, Outcome.Scaled, Outcome.Scaled.Est, Outcome, Outcome.Est)
### ------------------------------------

### nonFM

lv_efs_nonfm = hva_pls_lag_nonfm$boot$total.efs %>%
  data.frame(key = rownames(.), .) %>%
  separate(key, into = c("LV_from", "LV_to"), sep = " -> ") %>%
  select(LV_from, LV_to, total_coef = Mean.Boot, total_se = Std.Error, total_lower = perc.025, total_upper = perc.975)

lv_efs_nonfm_df = lv_efs_nonfm %>%
  mutate(
    LV_from = factor(LV_from, levels = colnames(inner_model)),
    Funnel = factor(LV_to, levels = rev(colnames(inner_model))))

scale_params_nonfm = data.frame(
  variable = colnames(hva_pls_lag_nonfm$manifests),
  center = attr(hva_pls_lag_nonfm$manifests, "scaled:center"),
  scale = attr(hva_pls_lag_nonfm$manifests, "scaled:scale"),
  stringsAsFactors = F
)

Xnonfm = hva_pls_lag_nonfm$manifests %>%
  as.matrix() 

beta_mat_nonfm = hva_pls_lag_nonfm$boot$weights %>%
  data.frame(path = row.names(.), ., stringsAsFactors = F) %>%
  separate(path, into = c("lv", "mv"), sep = "-") %>%
  mutate(mv = factor(mv, levels = mv)) %>%
  select(mv, lv, coef = Mean.Boot) %>%
  spread(mv, coef, fill = 0) %>%
  data.frame(row.names = "lv") %>%
  replace(is.na(.), 0) %>%
  t() %>%
  as.matrix()

scores_nonfm = Xnonfm %*% beta_mat_nonfm

# create data frame
scores_nonfm_sc_df = data.frame(
  yearmon = xl_nonfm$yearmon,
  dma_cd = xl_nonfm$dma_cd,
  msa_population = xl_nonfm$msa_population,
  scores_nonfm,
  stringsAsFactors = F
)

# estimated scores
sales_nonfm_sc = scale_params_nonfm["sales",]

est_scores_nonfm = scores_nonfm_sc_df %>%
  mutate(
    Upper.Mid.Est = Upper *
      filter(lv_efs_nonfm_df, LV_from == "Upper", LV_to == "Upper.Mid")$total_coef,
    Lower.Mid.Est = Upper.Mid *
      filter(lv_efs_nonfm_df, LV_from == "Upper.Mid", LV_to == "Lower.Mid")$total_coef,
    Lower.Est = Lower.Mid *
      filter(lv_efs_nonfm_df, LV_from == "Lower.Mid", LV_to == "Lower")$total_coef,
    Outcome.Scaled.Est =
      Lower *
      filter(lv_efs_nonfm_df, LV_from == "Lower", LV_to == "Outcome")$total_coef +
      Calendar *
      filter(lv_efs_nonfm_df, LV_from == "Calendar", LV_to == "Outcome")$total_coef
  ) %>%
  left_join(select(dma_map, dma_cd, MARKET_NAME), by = "dma_cd") %>%
  select(yearmon, dma_cd, dma = MARKET_NAME, msa_population, Calendar:Outcome.Scaled.Est) %>%
  mutate(
    Outcome.Scaled = Outcome,
    Outcome = (Outcome.Scaled * sales_nonfm_sc$scale + sales_nonfm_sc$center) * msa_population,
    Outcome.Est = (Outcome.Scaled.Est * sales_nonfm_sc$scale + sales_nonfm_sc$center) * msa_population
  )

# make plot df
plotdf_nonfm = est_scores_nonfm %>%
  mutate(DMA = paste(dma_cd, dma)) %>%
  select(yearmon, dma_cd, dma_name = dma, dma = DMA, msa_population,
         Calendar, Upper, Upper.Mid, Upper.Mid.Est, Lower.Mid, Lower.Mid.Est,
         Lower, Lower.Est, Outcome.Scaled, Outcome.Scaled.Est, Outcome, Outcome.Est)
###--------------------




## combine and plot
plotdf_all = rbind(
  mutate(plotdf_fm, Type = "FM"), 
  mutate(plotdf_nonfm, Type = "nonFM")
)

# write score data frame
write.csv(plotdf_all, "observed_vs_predicted_scores_fm_nonfm_split.csv", row.names = F)


dma = unique(plotdf_all$dma)


pdf("HVA Funnel Observed vs. Predicted (FM nonFM Split).pdf", height = 8, width = 15)

p1 <- ggplot(data = plotdf_all, aes(x = Upper.Mid, y = Upper.Mid.Est, col = Type, alpha = .3)) +
  guides(alpha = F) +
  #stat_smooth(method = "lm", col = "black", size = .5) +
  geom_abline() +
  geom_point()
p2 <- ggplot(data = plotdf_all, aes(x = Lower.Mid, y = Lower.Mid.Est, col = Type, alpha = .3)) +
  guides(alpha = F) +
  #stat_smooth(method = "lm", col = "black", size = .5) +
  geom_abline() +
  geom_point()
p3 <- ggplot(data = plotdf_all, aes(x = Lower, y = Lower.Est, col = Type, alpha = .3)) +
  guides(alpha = F) +
  #stat_smooth(method = "lm", col = "black", size = .5) +
  geom_abline() +
  geom_point()
p4 <- ggplot(data = plotdf_all, aes(x = Outcome, y = Outcome.Est, col = Type, alpha = .3)) +
  guides(alpha = F) +
  #stat_smooth(method = "lm", col = "black", size = .5) +
  geom_abline() +
  geom_point()
print(
  plot_grid(p1, p2, p3, p4, labels = 
              c("Upper Mid", "Lower Mid", "Lower", "Sales"), label_x = .05)
)

for(d in dma){
  tempdf = filter(plotdf_all, dma == d)
  p1 <- ggplot(data = tempdf, aes(x = Upper.Mid, y = Upper.Mid.Est, col = Type, alpha = .3)) +
    guides(alpha = F) +
    #stat_smooth(method = "lm", col = "black", size = .5) +
    geom_abline() +
    geom_point()
  p2 <- ggplot(data = tempdf, aes(x = Lower.Mid, y = Lower.Mid.Est, col = Type, alpha = .3)) +
    guides(alpha = F) +
    #stat_smooth(method = "lm", col = "black", size = .5) +
    geom_abline() +
    geom_point()
  p3 <- ggplot(data = tempdf, aes(x = Lower, y = Lower.Est, col = Type, alpha = .3)) +
    guides(alpha = F) +
    #stat_smooth(method = "lm", col = "black", size = .5) +
    geom_abline() +
    geom_point()
  p4 <- ggplot(data = tempdf, aes(x = Outcome, y = Outcome.Est, col = Type, alpha = .3)) +
    guides(alpha = F) +
    #stat_smooth(method = "lm", col = "black", size = .5) +
    geom_abline() +
    geom_point()
  print(
    plot_grid(p1, p2, p3, p4, labels = 
                c(
                  paste("Upper Mid,", d), 
                  paste("Lower Mid,", d), 
                  paste("Lower,", d), 
                  paste("Sales,", d)
                ), 
              label_size = 10
    )
  )
}
dev.off()


pdf("HVA Outcome Observed vs. Predicted (Scaled and Original, FM nonFM Split).pdf", height = 8, width = 15)

p1 <- ggplot(data = plotdf_all, aes(x = Outcome.Scaled, y = Outcome.Scaled.Est, col = Type, alpha = .3)) +
  guides(alpha = F) +
  #stat_smooth(method = "lm", col = "black", size = .5) +
  geom_abline() +
  geom_point()
p2 <- ggplot(data = plotdf_all, aes(x = Outcome, y = Outcome.Est, col = Type, alpha = .3)) +
  guides(alpha = F) +
  #stat_smooth(method = "lm", col = "black", size = .5) +
  geom_abline() +
  geom_point()
print(
  plot_grid(p1, p2, labels = 
              c("Sales (Scaled)", "Sales"), label_x = .05)
)

for(d in dma){
  tempdf = filter(plotdf_all, dma == d)
  p1 <- ggplot(data = tempdf, aes(x = Outcome.Scaled, y = Outcome.Scaled.Est, col = Type, alpha = .3)) +
    guides(alpha = F) +
    #stat_smooth(method = "lm", col = "black", size = .5) +
    geom_abline() +
    geom_point()
  p2 <- ggplot(data = tempdf, aes(x = Outcome, y = Outcome.Est, col = Type, alpha = .3)) +
    guides(alpha = F) +
    #stat_smooth(method = "lm", col = "black", size = .5) +
    geom_abline() +
    geom_point()
  print(
    plot_grid(p1, p2, labels = 
                c(
                  paste("Sales (Scaled),", d), 
                  paste("Sales,", d)
                ), 
              label_size = 10
    )
  )
}
dev.off()




