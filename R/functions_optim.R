
# process cov1 strategies
strategy_cov1_fn <- function(st_df, d){
  strat <- st_df %>%
    left_join(d) %>%
    mutate(deaths_averted_2021 = deaths_averted_2021 * doses_available / vaccine_n_2021,
           vaccine_n_2021 = doses_available) %>%
    mutate(total_vaccine_n = sum(vaccine_n_2021),
           total_deaths_averted = sum(deaths_averted_2021)) %>%
    mutate(dapd_2021 = if_else(vaccine_n_2021 == 0, 0, deaths_averted_2021 / vaccine_n_2021))
  return(strat)
}

# create parameter list
create_params_list <-
  function(t_start = 60,
           R0 = 2.5,
           Rt1 = 1,
           Rt2 = 2,
           coverage = c(0, 0.8),
           income_group = c("HIC", "UMIC", "LMIC", "LIC"),
           immunosenescence = 1,
           mode = "Infection",
           hs_constraints = "Present",
           efficacy = 0.7,
           duration_R = Inf,
           duration_V = 5000,
           seeding_cases = 60) {
    vaccine_start <- 366 - t_start
    reduction1 <- 1 - Rt1 / R0
    reduction2 <- 1 - Rt2 / R0
    timing1 <- 120 - t_start
    timing2 <- 366 - t_start + 30
    age_target <- create_fine_age_targets()
    out <- expand_grid(
      t_start = t_start,
      R0 = R0,
      reduction1 = reduction1,
      reduction2 = reduction2,
      coverage = coverage,
      age_target = age_target,
      income_group = income_group,
      immunosenescence = immunosenescence,
      mode = mode,
      hs_constraints = hs_constraints,
      efficacy = efficacy,
      duration_R = duration_R,
      duration_V = duration_V,
      vaccine_start = vaccine_start,
      seeding_cases = seeding_cases,
      timing1 = timing1,
      timing2 = timing2
    )
    return(out)
  }

# create dataframe of age targets in 5-year blocks, up to 2 contiguous groups
create_fine_age_targets <- function() {
  age_target_all <-
    expand.grid(
      a = 0:1,
      b = 0:1,
      c = 0:1,
      d = 0:1,
      e = 0:1,
      f = 0:1,
      g = 0:1,
      h = 0:1,
      j = 0:1,
      k = 0:1,
      l = 0:1,
      m = 0:1,
      n = 0:1 ,
      p = 0:1,
      q = 0:1,
      r = 0:1,
      s = 0:1
    )
  
  contig <- apply(age_target_all, 1, function(x) {
    dx1 <- (diff(which(x == 1)))
    dx2 <- (diff(which(x == 0)))
    all(dx2 == 1) | (sum(dx2 > 1) == 1 & sum(dx1 > 1) <= 1)
  })
  
  age_target <- age_target_all[contig, ]
  p = rep(0, nrow(age_target))
  
  for (i in 1:nrow(age_target)) {
    p[i] <- unite((age_target[i, ]), m, a:s, sep = "_")
  }
  age_target <- unlist(p)
  return(age_target)
}

# Find frontier within a block
front <- function(x, target = "max"){
  stopifnot(target %in% c("min", "max"))
  if(target == "min"){
    x <- x[order(x[, "cost"]), ]
    x <- x[which(x[, "y"] == cummin(x[, "y"])), ]
    x <- x[order(x[, "cost"], decreasing = FALSE), ]
    x<- x[!duplicated(x[, "y"]), ]
  } 
  if(target == "max"){
    x$y <- -x$y
    x <- x[order(x[, "cost"]), ]
    x <- x[which(x[, "y"] == cummin(x[, "y"])), ]
    x <- x[order(x[, "cost"], decreasing = FALSE), ]
    x <- x[!duplicated(x[, "y"]), ]
    x$y <- -x$y
  }
  return(x)
}

# Run the OMPR optimisation
run_optim <- function(input, budget){
  if(!all(c("doses", "deaths_averted", "country") %in% colnames(input))){
    stop("Input must have doses, deaths_averted and country columns")
  }
  stopifnot(all(input$doses >= 0))
  stopifnot(all(input$deaths_averted >= 0))
  
  # Generic names:
  input <- input %>%
    rename(cost = doses,
           y = deaths_averted)
  
  # Split by country and remove dominated solutions
  input_split <- input  %>%
    split(input$country) %>%
    lapply(front)
  
  # Determine matrix dimension
  ncountry <- length(input_split)
  nopt <- max(sapply(input_split, nrow))
  
  # Create solution and cost matrix
  outcome <- matrix(min(input$y), nrow = nopt, ncol = ncountry)
  cost <- matrix(max(input$cost), nrow = nopt, ncol = ncountry)
  for(j in seq_along(input_split)){
    outcome[1:nrow(input_split[[j]]),j] <- input_split[[j]]$y
    cost[1:nrow(input_split[[j]]),j] <- input_split[[j]]$cost
  }
  
  # Run the optimisation
  solution <- MIPModel() %>%
    # A binary matrix defining a given solution
    add_variable(x[i, j], i = 1:nopt, j = 1:ncountry, type = "binary") %>%
    # Objective - to maximise y
    set_objective(sum_expr(outcome[i, j] * x[i, j], i = 1:nopt, j = 1:ncountry), "max") %>%
    # Each column must have only 1 selection
    add_constraint(sum_expr(x[i,j], i = 1:nopt) == 1, j = 1:ncountry) %>%
    # Total cost must be <= budget
    add_constraint(sum_expr(cost[i, j] * x[i, j], i = 1:nopt, j = 1:ncountry) <= budget) %>%
    # Solve
    solve_model(with_ROI(solver = "glpk")) %>% 
    # Extract solution
    get_solution(x[i,j]) %>% 
    filter(value > 0)
  
  # Extract solution dataframe
  output <- list()
  for(j in seq_along(input_split)){
    output[[j]] <- input_split[[j]][solution[j, "i"],]
  }
  output <- bind_rows(output)
  
  # Print quick summary
  y <- sum(output$y)
  cost <- sum(output$cost)
  ypd <- 100 * (y / cost)
  message("Outcome = ", y)
  message("Cost = ", cost)
  message("100 * (Outcome / cost) = ", round(100 * (y / cost), 3))
  
  return(output)
}
