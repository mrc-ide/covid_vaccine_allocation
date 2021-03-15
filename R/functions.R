# Set initial R0 and Rt at future time points
setR0 <- function(R0, reduction1, reduction2){
  c(R0, R0 * (1 - reduction1), R0 * (1 - reduction2))
}

# Specify the country chosen to represent each income group
get_representative_country <- function(income_group){
  case_when(income_group == "HIC" ~ "Malta",
            income_group == "UMIC" ~ "Grenada",
            income_group == "LMIC" ~ "Nicaragua",
            income_group == "LIC" ~ "Madagascar")
}

# Set vaccine efficacy against infection or disease
set_efficacy <- function(mode, efficacy, immunosenescence){
  ef <- rep(efficacy, 17)
  
  # apply immunosenescence scaling to 65 year olds and over
  if(mode == "Infection"){
    out <- list(
      vaccine_efficacy_infection = ef,
      vaccine_efficacy_disease = rep(0, 17))
    out$vaccine_efficacy_infection[14:17] <- out$vaccine_efficacy_infection[14:17] * immunosenescence
  }
  
  if(mode == "Disease"){
    out <- list(
      vaccine_efficacy_infection = rep(0, 17),
      vaccine_efficacy_disease = ef)
    out$vaccine_efficacy_disease[14:17] <- out$vaccine_efficacy_disease[14:17] * immunosenescence
  }
  
  if(mode == "Combined"){
    out <- list(
      vaccine_efficacy_infection = ef,
      vaccine_efficacy_disease = rep(0.6, 17))
    out$vaccine_efficacy_infection[14:17] <- out$vaccine_efficacy_infection[14:17] * immunosenescence
  }
  
  return(out)
}

# reduce infectiousness children under 10 years
reduce_inf_vector <- function(reduce_inf){
  rdi <- rep(1, 17)
  rdi[1:2] <- reduce_inf
  return(rdi)
}

# Set hospital and ICU capacity
get_capacity <- function(country, income_group, pop, hs_constraints){
  hc <- squire::get_healthcare_capacity(country = country)
  
  # Unconstrained healthcare
  if(hs_constraints == "Absent"){
    hc$hosp_beds <- 1000000
    hc$ICU_beds <- 1000000
  }
  
  if(hs_constraints == "Present"){
    if(income_group %in% c("HIC", "UMIC")){
      hc$hosp_beds <- 1000000
      hc$ICU_beds <- 1000000
    }
    if(income_group %in% c("LMIC", "LIC")){
      hc$ICU_beds <- 0
    }
  }
  
  hc$hosp_beds <- round(hc$hosp_beds * sum(pop) / 1000)
  hc$ICU_beds <- round(hc$ICU_beds * sum(pop) / 1000)
  
  return(hc)
}

# Parameterise poorer health outcomes in LMIC and LIC
get_prob_non_severe_death_treatment <- function(income_group, hs_constraints){
  psdt <- squire:::probs$prob_non_severe_death_treatment
  
  if(income_group  == "LIC" & hs_constraints == "Present"){
    psdt <- c(rep(0.25, 16), 0.5804312)
  }
  return(psdt)
}

# Run scenarios
run_scenario <- function(R0 = 3,
                         target_pop = 50e6,
                         income_group = "HIC",
                         hs_constraints = "Present",
                         reduction1 = 0.7,
                         reduction2 = 0.2,
                         timing1 = 120 - 60,
                         timing2 = 365 - 60,
                         coverage = 0,
                         varying_coverage = 0,
                         age_target = "1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1",
                         vaccine_coverage_mat = NA,
                         mode = "Infection",
                         efficacy = 0.7,
                         vaccine_period = 30,
                         vaccine_start = 365 - 60,
                         #dt = 0.1,
                         immunosenescence = 1,
                         duration_R = Inf,
                         duration_V = 5000,
                         dur_vacc_delay = 7,
                         seeding_cases = 60,
                         t_start = 60,
                         reduce_inf = 1,
                         coverage_children = 0,
                         coverage_middle = 0,
                         coverage_old = 0
){
  # R0
  R0 <- setR0(R0 = R0, reduction1 = reduction1, reduction2 = reduction2)
  tt_R0 <- c(0, timing1, timing2)
  # Population and mixing
  rep_country <- get_representative_country(income_group = income_group)
  pop <- squire::get_population(country = rep_country)$n
  pop_standardise <- target_pop / sum(pop)
  pop <- pop * pop_standardise
  mm <- squire::get_mixing_matrix(country = rep_country)
  # Hospital capacity
  hc <- get_capacity(country = rep_country, income_group = income_group, pop = pop, hs_constraints = hs_constraints)
  # Poorer health outcomes for LMICs and LICs
  pnsdt = get_prob_non_severe_death_treatment(income_group, hs_constraints)
  
  # Vaccine parameters
  # Efficacy
  efficacy <- set_efficacy(efficacy = efficacy, mode = mode, immunosenescence = immunosenescence)

  # reduce infectiousness in children under 10 years old
  rel_inf <- reduce_inf_vector(reduce_inf)
  
  # if varying coverage by age group, get values
  if (varying_coverage == 1){
    coverage <- c(rep(coverage_children, 3), rep(coverage_middle, 10), rep(coverage_old, 4))
  }
  
  # get vaccine age-targeting strategy
  if (is.na(age_target) == 0){
    m1 <- (as.matrix(t(as.numeric(unlist(strsplit(age_target, "_")))))) * coverage
  } else if (is.na(age_target) == 1 & varying_coverage == 0){
    m1 <- strategy_matrix(vaccine_coverage_mat, max_coverage = coverage)
  } else {
    stop("If fixed strategy matrix specified, coverage cannot be a vector of values and should represent the maximum coverage.")
  }
  
  max_vaccine = c(0, max(coverage) * target_pop / vaccine_period)
  tt_vaccine = c(0, vaccine_start)

  # Run
  r1 <- nimue::run(
    time_period = (365 * 3) - t_start,
    R0 = R0, 
    tt_R0 = tt_R0,
    population = pop,
    contact_matrix_set = mm,
    hosp_bed_capacity = hc$hosp_beds,
    ICU_bed_capacity = hc$ICU_beds,
    prob_non_severe_death_treatment = pnsdt,
    seeding_cases = seeding_cases,
    seed = 1,
    #dt = dt,
    max_vaccine = max_vaccine,
    tt_vaccine = tt_vaccine,
    dur_V = duration_V,
    dur_vaccine_delay = dur_vacc_delay,
    vaccine_efficacy_infection = efficacy$vaccine_efficacy_infection,
    vaccine_efficacy_disease = efficacy$vaccine_efficacy_disease,
    dur_R = duration_R,
    vaccine_coverage_mat = m1,
    rel_infectiousness = rel_inf
  )
  
  # Create output: 1) wrt time, 2) summaries by age
  o1 <- nimue::format(r1) %>%
    mutate(t = t + t_start)
  
  x <- nimue::format(r1,
                     compartments = NULL,
                     summaries = c("deaths", "infections", "vaccines", "hospitalisations"),
                     reduce_age = FALSE) %>%
    mutate(t = t + t_start)
  
  value_all_t <- summarise_by_age(x, 1, max(x$t), "all_t")
  value_2021 <- summarise_by_age(x, 366, 731, 2021)
  value_2022 <- summarise_by_age(x, 731, 1096, 2022)

  o2 <- rbind(value_all_t,
              value_2021,
              value_2022)
  
  tibble(output = list(o1), output_age = list(o2))
}

# Format output
format_out <- function(out, scenarios, target_pop = 50e6){
  # Combine_inputs and outputs
  out1 <- bind_cols(scenarios, bind_rows(out))
  # Isolate counterfactual (Coverage == 0)
  if ("coverage" %in% colnames(out1)) {
    outcf <- filter(out1, coverage == 0) %>%
      select(-coverage) %>%
      rename(output_cf = output,
             output_age_cf = output_age) %>%
      unique()
  } else {
    outcf <- filter(out1, (coverage_children == 0 & coverage_middle == 0 & coverage_old == 0)) %>%
    select(-coverage_children, -coverage_middle, -coverage_old) %>%
    rename(output_cf = output,
           output_age_cf = output_age) %>%
    unique()
  }

  # Combine runs and counterfactual and estimate summaries
  summaries <- left_join(out1, outcf)
  
  m <- ncol(summaries)+1
  n <- ncol(summaries)+14
  
  summarise_2021 <- summarise_outputs_age(summaries, p = 2021)
  colnames(summarise_2021)[m:n] <- paste0(colnames(summarise_2021)[m:n], "_2021")
  
  summarise_2022 <- summarise_outputs_age(summaries, p = 2022)
  colnames(summarise_2022)[m:n] <- paste0(colnames(summarise_2022)[m:n], "_2022")
  
  summarise_all_t <- summarise_outputs_age(summaries, p = "all_t")
  colnames(summarise_all_t)[m:n] <- paste0(colnames(summarise_all_t)[m:n], "_all_t")
  
  summaries <- left_join(summaries, select(summarise_2021, -contains("output"))) %>%
    left_join(select(summarise_2022, -contains("output"))) %>%
    left_join(select(summarise_all_t, -contains("output")))
}

# summarise by age and time period
summarise_by_age <- function(x, t_start, t_end, period){
  filter(x, t >= t_start, t < t_end) %>%
    group_by(age_group, compartment) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(period = factor(period))
} 

# Summarise outputs by age over different time period
summarise_outputs_age <- function(x, p) {
  mutate(x, 
         infections = round(map_dbl(output_age, pull_total, outcome = "infections", time_period = p), 2),
         hospitalisations = round(map_dbl(output_age, pull_total, outcome = "hospitalisations", time_period = p), 2),
         deaths = round(map_dbl(output_age, pull_total, outcome = "deaths", time_period = p), 2),
         yll = round(map_dbl(output_age, summarise_yll, time_period = p), 2),
         infections_cf = round(map_dbl(output_age_cf, pull_total, outcome = "infections", time_period = p), 2),
         hospitalisations_cf = round(map_dbl(output_age_cf, pull_total, outcome = "hospitalisations", time_period = p), 2),
         deaths_cf = round(map_dbl(output_age_cf, pull_total, outcome = "deaths", time_period = p), 2),
         yll_cf = round(map_dbl(output_age_cf, summarise_yll, time_period = p), 2),
         infections_averted = infections_cf - infections,
         hospitalisations_averted = hospitalisations_cf - hospitalisations,
         deaths_averted = deaths_cf - deaths,
         deaths_averted_prop = deaths_averted / deaths_cf,
         years_life_saved = yll_cf - yll,
         vaccine_n = round(map_dbl(output_age, pull_total, outcome = "vaccines", time_period = p)))
}

# Pull sum totals
pull_total <- function(x, outcome, time_period){
  filter(x, compartment == outcome, period == time_period) %>%
    pull(value) %>%
    sum()
}

# Estimate total years of life lost
summarise_yll <- function(x, lifespan = 86.6, time_period){
  filter(x, compartment == "deaths", period == time_period) %>%
    mutate(mid_age = (((as.integer(age_group) - 1) * 5) + 2.5),
           yll = pmax(0, (lifespan - mid_age) * value)) %>%
    pull(yll) %>%
    sum()
}

# Estimate proportion of epidemic over by given time
prop_over <- function(x, timing){
  d <- filter(x, compartment == "deaths")
  d_all <- sum(d$value, na.rm = TRUE)
  d2 <- filter(d, t <= timing)
  d_by <- sum(d2$value, na.rm = TRUE)
  d_by / d_all
}

# Estimate proportion in R at last time point
summarise_R <- function(x, timing, pop){
  d <- filter(x, compartment == "R", t <=timing) %>%
    filter(t == max(t)) %>%
    pull(value)
  d / pop
}

run_scenario_cluster <- function(run_number = 1,
                                 directory_out,
                                 R0 = 2.5,
                                 target_pop = 50e6,
                                 income_group = "HIC",
                                 hs_constraints = "Present",
                                 reduction1 = 0.7,
                                 reduction2 = 0.2,
                                 timing1 = 60,
                                 timing2 = 305,
                                 coverage = 0,
                                 varying_coverage = 0,
                                 age_target = "1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1",
                                 mode = "Infection",
                                 efficacy = 0.9,
                                 vaccine_period = 30,
                                 vaccine_start = 305,
                                 #dt = 0.1,
                                 immunosenescence = 1,
                                 duration_R = 365,
                                 duration_V = 5000,
                                 dur_vacc_delay = 7,
                                 seeding_cases = 60,
                                 t_start = 60,
                                 reduce_inf = 1){

  out <- run_scenario(R0 = R0,
                      target_pop = target_pop,
                      income_group = income_group,
                      hs_constraints = hs_constraints,
                      reduction1 = reduction1,
                      reduction2 = reduction2,
                      timing1 = timing1,
                      timing2 = timing2,
                      coverage = coverage,
                      varying_coverage = varying_coverage,
                      age_target = age_target,
                      mode = mode, 
                      efficacy = efficacy,
                      vaccine_period = vaccine_period,
                      vaccine_start = vaccine_start,
                      dt = dt,
                      immunosenescence = immunosenescence,
                      duration_R = duration_R,
                      duration_V = duration_V,
                      dur_vacc_delay = dur_vacc_delay,
                      seeding_cases = seeding_cases,
                      t_start = t_start,
                      reduce_inf = reduce_inf)
  
  x <- mutate(out,
              deaths = round(map_dbl(output_age, pull_total, outcome = "deaths", time_period = 2021), 2),
              yll = round(map_dbl(output_age, summarise_yll, time_period = 2021), 2))
  
  d <- select(x, deaths, yll)
  out <- data.frame(run_number = run_number, deaths = d$deaths, life_years_lost = d$yll)
  
  write.table(out, paste0("cluster_outputs_", directory_out, "/out_", run_number, ".txt"), sep = "\t", row.names = FALSE, col.names = TRUE)
  return(out)
}

run_scenario_basic <- function(run_number = 1,
                                 R0 = 2.5,
                                 target_pop = 50e6,
                                 income_group = "HIC",
                                 hs_constraints = "Present",
                                 reduction1 = 0.7,
                                 reduction2 = 0.2,
                                 timing1 = 60,
                                 timing2 = 305,
                                 coverage = 0,
                                 varying_coverage = 0,
                                 age_target = "1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1",
                                 mode = "Infection",
                                 efficacy = 0.9,
                                 vaccine_period = 30,
                                 vaccine_start = 305,
                                 #dt = 0.1,
                                 immunosenescence = 1,
                                 duration_R = 365,
                                 duration_V = 5000,
                                 dur_vacc_delay = 7,
                                 seeding_cases = 60,
                                 t_start = 60,
                                 reduce_inf = 1){
  
  out <- run_scenario(R0 = R0,
                      target_pop = target_pop,
                      income_group = income_group,
                      hs_constraints = hs_constraints,
                      reduction1 = reduction1,
                      reduction2 = reduction2,
                      timing1 = timing1,
                      timing2 = timing2,
                      coverage = coverage,
                      varying_coverage = varying_coverage,
                      age_target = age_target,
                      mode = mode, 
                      efficacy = efficacy,
                      vaccine_period = vaccine_period,
                      vaccine_start = vaccine_start,
                      #dt = dt,
                      immunosenescence = immunosenescence,
                      duration_R = duration_R,
                      duration_V = duration_V,
                      dur_vacc_delay = dur_vacc_delay,
                      seeding_cases = seeding_cases,
                      t_start = t_start,
                      reduce_inf = reduce_inf)
  
  x <- mutate(out,
              deaths = round(map_dbl(output_age, pull_total, outcome = "deaths", time_period = 2021), 2),
              yll = round(map_dbl(output_age, summarise_yll, time_period = 2021), 2),
              vaccine_n = round(map_dbl(output_age, pull_total, outcome = "vaccines", time_period = 2021)))
  
  d <- select(x, deaths, yll, vaccine_n)
  out <- data.frame(run_number = run_number, deaths = d$deaths, life_years_lost = d$yll, vaccine_n = d$vaccine_n)
  return(out)
}
