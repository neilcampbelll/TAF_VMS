vms_df <- subTacsat %>% select(SI_SP, LE_GEAR, LE_L5MET)

library(brms)

# Ensure both columns exist and are factors
if (!"LE_GEAR" %in% colnames(vms_df)) {
  stop("LE_GEAR column is missing from the data.")
}
if (!"LE_L5MET" %in% colnames(vms_df)) {
  stop("LE_L5MET column is missing from the data.")
}

vms_df$LE_GEAR <- substr(vms_df$LE_L5MET, 1, 3)

vms_df <- vms_df %>%
  mutate(
    LE_GEAR = as.factor(LE_GEAR),
    LE_L5MET = as.factor(LE_L5MET)
  )

# Add a small constant to all values
vms_df$SI_SP <- vms_df$SI_SP + 0.001

# Verify nesting (optional but good practice)
# Check if each L5MET belongs to only one GEAR
nesting_check <- vms_df %>%
  distinct(LE_GEAR, LE_L5MET) %>%
  group_by(LE_L5MET) %>%
  filter(n() > 1)

if (nrow(nesting_check) > 0) {
  warning("Some LE_L5MET values appear in multiple LE_GEAR groups. Check data integrity.")
  print(nesting_check)
}

write.csv(vms_df, "vms_df.csv")



vms_df <- as.data.frame(read.csv("vms_df.csv"))

vms_df <- vms_df %>%
  mutate(
    LE_GEAR = as.factor(LE_GEAR),
    LE_L5MET = as.factor(LE_L5MET)
  )

# K remains the number of components
K <- 4

# Define the mixture family
mix_gamma_k <- brms::mixture("gamma", nmix = K, order = TRUE)

# Update the formula with non-linear parameters specified for each component
bform_hierarchical <- bf(
  SI_SP ~ 1 + (1 | LE_GEAR/LE_L5MET), # Nested effect
  family = mix_gamma_k,
  nl = TRUE,
  # The following specifies non-linear parameters for each component (1 to K)
  mu1 ~ 1 + (1 | LE_GEAR/LE_L5MET),
  mu2 ~ 1 + (1 | LE_GEAR/LE_L5MET),
  mu3 ~ 1 + (1 | LE_GEAR/LE_L5MET),
  mu4 ~ 1 + (1 | LE_GEAR/LE_L5MET),
  shape1 ~ 1 + (1 | LE_GEAR/LE_L5MET),
  shape2 ~ 1 + (1 | LE_GEAR/LE_L5MET),
  shape3 ~ 1 + (1 | LE_GEAR/LE_L5MET),
  shape4 ~ 1 + (1 | LE_GEAR/LE_L5MET)
)

# Define Priors - Now need priors for sd at both levels
# Define Priors - Now need priors for sd at both levels
priors_hierarchical <- c(
  # Priors for global intercepts for each component
  prior(normal(log(1), 1.5), class = Intercept, dpar = mu1),
  prior(normal(log(3), 1.5), class = Intercept, dpar = mu2),
  prior(normal(log(6), 1.5), class = Intercept, dpar = mu3),
  prior(normal(log(10), 1.5), class = Intercept, dpar = mu4),

  # Priors for shape parameters
  prior(normal(log(2), 1), class = Intercept, dpar = shape1),
  prior(normal(log(2), 1), class = Intercept, dpar = shape2),
  prior(normal(log(2), 1), class = Intercept, dpar = shape3),
  prior(normal(log(2), 1), class = Intercept, dpar = shape4),

  # Priors for standard deviations of random effects for mu parameters at LE_GEAR level
  prior(exponential(1), class = sd, group = LE_GEAR, dpar = mu1),
  prior(exponential(1), class = sd, group = LE_GEAR, dpar = mu2),
  prior(exponential(1), class = sd, group = LE_GEAR, dpar = mu3),
  prior(exponential(1), class = sd, group = LE_GEAR, dpar = mu4),

  # Priors for standard deviations of random effects for shape parameters at LE_GEAR level
  prior(exponential(1), class = sd, group = LE_GEAR, dpar = shape1),
  prior(exponential(1), class = sd, group = LE_GEAR, dpar = shape2),
  prior(exponential(1), class = sd, group = LE_GEAR, dpar = shape3),
  prior(exponential(1), class = sd, group = LE_GEAR, dpar = shape4),

  # Priors for standard deviations of random effects for mu parameters at nested level
  prior(exponential(1), class = sd, group = "LE_GEAR:LE_L5MET", dpar = mu1),
  prior(exponential(1), class = sd, group = "LE_GEAR:LE_L5MET", dpar = mu2),
  prior(exponential(1), class = sd, group = "LE_GEAR:LE_L5MET", dpar = mu3),
  prior(exponential(1), class = sd, group = "LE_GEAR:LE_L5MET", dpar = mu4),

  # Priors for standard deviations of random effects for shape parameters at nested level
  prior(exponential(1), class = sd, group = "LE_GEAR:LE_L5MET", dpar = shape1),
  prior(exponential(1), class = sd, group = "LE_GEAR:LE_L5MET", dpar = shape2),
  prior(exponential(1), class = sd, group = "LE_GEAR:LE_L5MET", dpar = shape3),
  prior(exponential(1), class = sd, group = "LE_GEAR:LE_L5MET", dpar = shape4)
)

# (Setup parallel processing if desired: future::plan(multisession, workers = n_cores))

fit_vms_mix_hierarchical <- brm(
  formula = bform_hierarchical,
  data = vms_df,
  prior = priors_hierarchical,
  iter = 2000, # Increase for final run
  warmup = 500, # Increase for final run
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.95), # Adjust if needed
  seed = 123,
  # backend = "cmdstanr", # Optional
  refresh = 200,  # Print update every 20 iterations
  file = "brms_vms_mix_k4_gamma_hierarchical-biggest" # Cache results
)

# Check summary and diagnostics
summary(fit_vms_mix_hierarchical)
 plot(fit_vms_mix_hierarchical)

 pp_check(fit_vms_mix_hierarchical, ndraws=100)

# Pay attention to the variance components (sd) for LE_GEAR and LE_GEAR:LE_L5MET
# e.g., sd(Intercept ~ 1 | LE_GEAR)__mu1
#       sd(Intercept ~ 1 | LE_GEAR:LE_L5MET)__mu1
# etc.

# --- Option 1: Using posterior_epred (Recommended) ---

# Create newdata with all unique combinations of LE_GEAR and LE_L5MET
new_data_pred_hier <- distinct(vms_df, LE_GEAR, LE_L5MET)

# Get posterior expected predictions for parameters (mu, shape, lambda)
# for each component k, for each LE_L5MET
post_pred_params_hier <- posterior_epred(
  fit_vms_mix_hierarchical,
  newdata = new_data_pred_hier,
  re_formula = NULL # Include all random effects specified in the model
)

# Dimensions: [draws, components (K), observations in newdata]
# Need to reshape and link back to LE_GEAR, LE_L5MET

# Create a data frame mapping newdata row index to LE_GEAR/LE_L5MET
newdata_map <- new_data_pred_hier %>% mutate(obs_index = row_number())

# Process the posterior_epred output
n_draws <- dim(post_pred_params_hier)[1]
n_comps <- dim(post_pred_params_hier)[2]
n_obs <- dim(post_pred_params_hier)[3]

# This requires careful reshaping. Let's try converting to a tidy format.
# Extract mu, shape, lambda for each draw, component, observation(metier)
# We need the model's internal variable names for mu, shape, lambda for each component
# Check fit$formula$resp might help, or names(fit$data) for response
# Let's assume they follow the mu1, shape1, theta1 pattern...
# We need to extract the parameters from the epred array based on how brms structures it for mixtures.
# Often it's [draw, observation, component] or [draw, component, observation] - check documentation or explore the object.
# Let's *assume* [draw, component, observation]

param_draws_hier <- array_to_matrix(post_pred_params_hier, 1) %>% # Combine draws and components first? Might be easier draw, obs, comp
  as.data.frame() %>% # Convert draw x (comp*obs) matrix to data frame
  mutate(.draw = 1:n()) %>%
  pivot_longer(cols = starts_with("V"), # Columns V1, V2,... represent comp*obs combinations
               names_to = "col_index",
               values_to = "value",
               names_prefix = "V") %>%
  mutate(col_index = as.integer(col_index)) %>%
  # Now map col_index back to component k and observation index obs_idx
  # Order is likely obs1_k1, obs1_k2, ..., obs1_K, obs2_k1, ....
  # OR k1_obs1, k1_obs2, ..., k1_N, k2_obs1, ...
  # Let's assume the latter: (col_index - 1) = (k-1)*n_obs + (obs_idx - 1)
  mutate(
    k = floor((col_index - 1) / n_obs) + 1,
    obs_index = ((col_index - 1) %% n_obs) + 1
  ) %>%
  left_join(newdata_map, by = "obs_index") #%>%
  # Now 'value' holds the parameter (mu, shape, or lambda?) - posterior_epred gives the expected value
  # For Gamma, it gives mu. For mixture, it might give the overall expected value OR component means/weights?
  # ***** Need to VERIFY what posterior_epred returns for mixtures *****
  # It *should* return the component-specific parameters (mu_k, shape_k, lambda_k) if specified correctly.
  # Let's REASSESS: posterior_epred on the linear predictor scale might be better.
  # linpred <- posterior_linpred(fit_vms_mix_hierarchical, newdata = new_data_pred_hier, re_formula = NULL)
  # This would give log(mu_k), log(shape_k), and possibly transformed thetas. Still needs careful processing.

  # --- Option 2: Manual Extraction (More complex, prone to errors) ---
  # Requires extracting b_Intercept, r_LE_GEAR, r_LE_GEAR:LE_L5MET for each mu_k, shape_k
  # And the theta parameters, then combining them manually. Similar structure to before but with an extra random effect term.

  # Let's assume we managed to get `param_draws_hier` with columns:
  # .draw, LE_GEAR, LE_L5MET, k, gear_metier_mu, gear_metier_shape, lambda

  # Proceed with intersection calculation (code is identical to before, just uses the new parameter draws df)
  intersection_draws_hier <- param_draws_hier %>%
  # Ensure parameters are valid
  filter(!is.na(gear_metier_mu), !is.na(gear_metier_shape), !is.na(lambda),
         gear_metier_mu > 0, gear_metier_shape > 0, lambda >= 0, lambda <= 1) %>%
  arrange(.draw, LE_GEAR, LE_L5MET, k) %>%
  group_by(.draw, LE_GEAR, LE_L5MET) %>%
  mutate(
    next_k = lead(k),
    next_mu = lead(gear_metier_mu),
    next_shape = lead(gear_metier_shape),
    next_lambda = lead(lambda)
  ) %>%
  filter(!is.na(next_k)) %>%
  rowwise() %>%
  mutate(
    intersection_speed = find_intersection(
      gear_metier_mu, gear_metier_shape, lambda,
      next_mu, next_shape, next_lambda
    ),
    threshold_name = paste0("T", k, "_", next_k)
  ) %>%
  ungroup() %>%
  select(.draw, LE_GEAR, LE_L5MET, threshold_name, intersection_speed) %>%
  filter(!is.na(intersection_speed))

# Summarize thresholds (same as before)
threshold_summary_hier <- intersection_draws_hier %>%
  group_by(LE_GEAR, LE_L5MET, threshold_name) %>%
  median_qi(intersection_speed, .width = c(0.8, 0.95))

print(threshold_summary_hier)

# Identify key thresholds (e.g., T1_2 and T3_4 if K=4)
key_thresholds_hier <- threshold_summary_hier %>%
  filter(threshold_name %in% c("T1_2", "T3_4")) # Adjust as needed

print(key_thresholds_hier)

# Re-create parameter draws df if needed for plotting (param_draws_hier from Option 1/2)
# ... (code to get param_draws_hier again if not in environment)

# Select draws for visualization
# ... (sample viz_draw_indices)
viz_draws_hier <- param_draws_hier %>% filter(.draw %in% viz_draw_indices)

# Create density lines (modify grouping for the new structure)
speed_grid <- seq(speed_min_threshold, speed_max_threshold, length.out = 200)

# --- This part needs the actual param_draws_hier structure ---
# Assuming columns: .draw, LE_GEAR, LE_L5MET, k, gear_metier_mu, gear_metier_shape, lambda
density_lines_hier <- viz_draws_hier %>%
  group_by(.draw, LE_GEAR, LE_L5MET) %>%
  # Summarise densities per metier per draw
  summarise(
    speeds = list(speed_grid),
    comp_density = list(
      map_df(1:n(), ~data.frame(
        k = cur_data()$k[.x],
        density = weighted_gamma_pdf(speed_grid,
                                     cur_data()$gear_metier_mu[.x],
                                     cur_data()$gear_metier_shape[.x],
                                     cur_data()$lambda[.x])
      ))
    ), .groups = 'drop' # Avoid regrouping issues later
  ) %>%
  unnest(cols = c(speeds, comp_density), names_repair = "universal") %>%
  rename(comp_k = k...6, comp_dens = density...7, speed = speeds) # Adjust indices

# Plotting (e.g., faceted by LE_GEAR, then LE_L5MET)
plot_list_hier <- map(levels(vms_df$LE_GEAR), ~{
  gear_group = .x
  metiers_in_gear <- levels(droplevels(filter(vms_df, LE_GEAR == gear_group)$LE_L5MET))

  sub_plot_list <- map(metiers_in_gear, ~{
    metier_code = .x
    metier_data <- filter(vms_df, LE_L5MET == metier_code) # Already filtered by gear implicitly
    density_data_metier <- filter(density_lines_hier, LE_L5MET == metier_code)
    threshold_data_metier <- filter(key_thresholds_hier, LE_L5MET == metier_code)

    ggplot(metier_data, aes(x = SI_SP)) +
      geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.4, fill = "grey") +
      # Use stat_summary for mean lines, optionally add faint lines for draws
      stat_summary(data = density_data_metier, aes(x=speed, y=comp_dens, color=factor(comp_k)), fun=mean, geom="line", lwd=1) +
      geom_vline(data = threshold_data_metier, aes(xintercept = intersection_speed, linetype = threshold_name), color = "black", size = 1) +
      # Add CIs for thresholds if desired
      # geom_vline(data = threshold_data_metier, aes(xintercept = .lower, linetype = threshold_name), color = "black", size = 0.5, alpha = 0.6) +
      # geom_vline(data = threshold_data_metier, aes(xintercept = .upper, linetype = threshold_name), color = "black", size = 0.5, alpha = 0.6) +
      scale_color_viridis_d(name = "Component (k)", drop=FALSE) +
      scale_linetype_manual(name = "Threshold", values=c("T1_2"="dashed", "T3_4"="dotted"), drop=FALSE) + # Adjust names/values
      ggtitle(paste("Métier:", metier_code)) +
      coord_cartesian(xlim = c(0, speed_max_threshold)) +
      theme_bw() +
      theme(legend.position = "bottom")
  })
  # Combine plots for metiers within the gear group
  patchwork::wrap_plots(sub_plot_list) + plot_annotation(title = paste("Gear Group:", gear_group))
})

# Display the combined plots (one main plot per gear group)
# plot_list_hier[[1]] # Show first gear group
# plot_list_hier[[2]] # Show second gear group, etc.
# Or save them
# map2(plot_list_hier, levels(vms_df$LE_GEAR), ~ggsave(paste0("Threshold_Plot_", .y, ".png"), .x, width=12, height=8))
