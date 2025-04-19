## ----message=FALSE, echo=FALSE------------------------------------------------
library(unittest)
# Redirect ok() output to stderr
options(unittest.output = stderr())
if (nzchar(Sys.getenv('G3_TEST_TMB'))) options(gadget3.tmb.work_dir = gadget3:::vignette_base_dir('work_dir'))

library(gadget3)
set.seed(123)

## ----warning = FALSE, message = FALSE-----------------------------------------
### Modelling maturity & sex with multiple stocks


## ----warning = FALSE, message = FALSE-----------------------------------------
library(gadget3)
library(dplyr)

actions <- list()
area_names <- g3_areas(c('IXa', 'IXb'))

# Create time definitions ####################

actions_time <- list(
  g3a_time(
    1979L, 2023L,
    step_lengths = c(3L, 3L, 3L, 3L)),
  NULL)

actions <- c(actions, actions_time)

## -----------------------------------------------------------------------------
# Create stock definition for fish ####################
st_imm <- g3_stock(c(species = "fish", 'imm'), seq(5L, 25L, 5)) |>
  g3s_livesonareas(area_names["IXa"]) |>
  g3s_age(1L, 5L)

st_mat <- g3_stock(c(species = "fish", 'mat'), seq(5L, 25L, 5)) |>
  g3s_livesonareas(area_names["IXa"]) |>
  g3s_age(3L, 10L)
stocks = list(imm = st_imm, mat = st_mat)

## -----------------------------------------------------------------------------
actions_st_imm <- list(
  g3a_growmature(st_imm,
    g3a_grow_impl_bbinom(
      maxlengthgroupgrowth = 4L ),
    # Add maturation
    maturity_f = g3a_mature_continuous(),
    output_stocks = list(st_mat),
    transition_f = ~TRUE ),
  g3a_naturalmortality(st_imm),
  g3a_initialconditions_normalcv(st_imm),
  g3a_renewal_normalparam(st_imm),
  g3a_age(st_imm, output_stocks = list(st_mat)),
  NULL)

actions_st_mat <- list(
  g3a_growmature(st_mat,
    g3a_grow_impl_bbinom(
      maxlengthgroupgrowth = 4L )),
  g3a_naturalmortality(st_mat),
  g3a_initialconditions_normalcv(st_mat),
  g3a_age(st_mat),
  NULL)

actions_likelihood_st <- list(
  g3l_understocking(stocks, nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_st_imm, actions_st_mat, actions_likelihood_st)

## -----------------------------------------------------------------------------
# Fleet data for f_surv #################################

# Landings data: For each year/step/area
expand.grid(year = 1990:1994, step = 2, area = 'IXa') |>
    # Generate a random total landings by weight
    mutate(weight = rnorm(n(), mean = 1000, sd = 100)) |>
    # Assign result to landings_f_surv
    identity() -> landings_f_surv

# Length distribution data: Generate 100 random samples in each year/step/area
expand.grid(year = 1990:1994, step = 2, area = 'IXa', length = rep(NA, 100)) |>
  # Generate random lengths for these samples
  mutate(length = rnorm(n(), mean = 50, sd = 20)) |>
  # Save unagggregated data into ldist_f_surv.raw
  identity() -> ldist_f_surv.raw

# Aggregate .raw data
ldist_f_surv.raw |>
  # Group into length bins
  group_by(
      year = year,
      step = step,
      length = cut(length, breaks = c(seq(0, 80, 20), Inf), right = FALSE) ) |>
  # Report count in each length bin
  summarise(number = n(), .groups = 'keep') |>
  # Save into ldist_f_surv
  identity() -> ldist_f_surv

# Assume 5 * 5 samples in each year/step/area
expand.grid(year = 1990:1994, step = 2, area = 'IXa', age = rep(NA, 5), length = rep(NA, 5)) |>
  # Generate random lengths/ages for these samples
  mutate(length = rnorm(n(), mean = 50, sd = 20)) |>
  # Generate random whole numbers for age
  mutate(age = floor(runif(n(), min = 1, max = 5))) |>
  # Group into length/age bins
  group_by(
      year = year,
      step = step,
      age = age,
      length = cut(length, breaks = c(seq(0, 80, 20), Inf), right = FALSE) ) |>
  # Report count in each length bin
  summarise(number = n(), .groups = 'keep') ->
  aldist_f_surv

## -----------------------------------------------------------------------------
# Create fleet definition for f_surv ####################
f_surv <- g3_fleet("f_surv") |> g3s_livesonareas(area_names["IXa"])

actions_f_surv <- list(
  g3a_predate_fleet(
    f_surv,
    stocks,
    suitabilities = g3_suitability_exponentiall50(by_stock = 'species'),
    catchability_f = g3a_predate_catchability_totalfleet(
      g3_timeareadata("landings_f_surv", landings_f_surv, "weight", areas = area_names))),
  NULL)
actions_likelihood_f_surv <- list(
  g3l_catchdistribution(
    "ldist_f_surv",
    obs_data = ldist_f_surv,
    fleets = list(f_surv),
    stocks = stocks,
    function_f = g3l_distribution_sumofsquares(),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  g3l_catchdistribution(
    "aldist_f_surv",
    obs_data = aldist_f_surv,
    fleets = list(f_surv),
    stocks = stocks,
    function_f = g3l_distribution_sumofsquares(),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_f_surv, actions_likelihood_f_surv)

## -----------------------------------------------------------------------------
suppressWarnings({  # NB: The model is incomplete, fish_imm__num isn't defined
simple_model <- g3_to_r(list(g3a_time(1990, 1994), g3a_predate_fleet(
    f_surv,
    stocks,
    suitabilities = g3_suitability_exponentiall50(by_stock = TRUE),
    catchability_f = g3a_predate_catchability_totalfleet(1) )))
})
names(attr(simple_model, "parameter_template"))

## ----message=FALSE, echo=FALSE------------------------------------------------
ok(ut_cmp_identical(sort(names(attr(simple_model, "parameter_template")), method = "radix"), c(
    "fish_imm.f_surv.alpha",
    "fish_imm.f_surv.l50",
    "fish_mat.f_surv.alpha",
    "fish_mat.f_surv.l50",
    "project_years", "retro_years",
    NULL)), "Params for simple_model, by_stock = TRUE")

## -----------------------------------------------------------------------------
suppressWarnings({  # NB: The model is incomplete, fish_imm__num isn't defined
simple_model <- g3_to_r(list(g3a_time(1990, 1994), g3a_predate_fleet(
    f_surv,
    stocks,
    suitabilities = g3_suitability_exponentiall50(by_stock = 'species'),
    catchability_f = g3a_predate_catchability_totalfleet(1) )))
names(attr(simple_model, "parameter_template"))
})

## ----message=FALSE, echo=FALSE------------------------------------------------
ok(ut_cmp_identical(sort(names(attr(simple_model, "parameter_template")), method = "radix"), c(
    "fish.f_surv.alpha",
    "fish.f_surv.l50",
    "project_years", "retro_years",
    NULL)), "Params for simple_model, by_stock = 'species'")

## -----------------------------------------------------------------------------
suppressWarnings({  # NB: The model is incomplete, fish_imm__num isn't defined
simple_model <- g3_to_r(list(g3a_time(1990, 1994), g3a_predate_fleet(
    f_surv,
    stocks,
    suitabilities = g3_suitability_exponentiall50(
        l50 = g3_parameterized("l50", by_stock = 'species', by_predator = TRUE, by_year = TRUE)),
    catchability_f = g3a_predate_catchability_totalfleet(1) )))
names(attr(simple_model, "parameter_template"))
})

## ----message=FALSE, echo=FALSE------------------------------------------------
ok(ut_cmp_identical(sort(names(attr(simple_model, "parameter_template")), method = "radix"), c(
    "fish.f_surv.l50.1990",
    "fish.f_surv.l50.1991",
    "fish.f_surv.l50.1992",
    "fish.f_surv.l50.1993",
    "fish.f_surv.l50.1994",
    "fish_imm.f_surv.alpha",
    "fish_mat.f_surv.alpha",
    "project_years", "retro_years",
    NULL)), "Params for simple_model, by_year = TRUE")

## -----------------------------------------------------------------------------
# Create abundance index for si_cpue ########################

# Generate random data
expand.grid(year = 1990:1994, step = 3, area = 'IXa') |>
    # Fill in a weight column with total biomass for the year/step/area combination
    mutate(weight = runif(n(), min = 10000, max = 100000)) ->
    dist_si_cpue

actions_likelihood_si_cpue <- list(

  g3l_abundancedistribution(
    "dist_si_cpue",
    dist_si_cpue,

    stocks = stocks,
    function_f = g3l_distribution_surveyindices_log(alpha = NULL, beta = 1),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_likelihood_si_cpue)

## -----------------------------------------------------------------------------
# Create model objective function ####################

model_code <- g3_to_tmb(c(actions, list(
    g3a_report_detail(actions),
    g3l_bounds_penalty(actions) )))

## -----------------------------------------------------------------------------
# Guess l50 / linf based on stock sizes
estimate_l50 <- g3_stock_def(st_imm, "midlen")[[length(g3_stock_def(st_imm, "midlen")) / 2]]
estimate_linf <- max(g3_stock_def(st_imm, "midlen"))
estimate_t0 <- g3_stock_def(st_imm, "minage") - 0.8

attr(model_code, "parameter_template") |>
  g3_init_val("*.rec|init.scalar", 10, lower = 0.001, upper = 200) |>
  g3_init_val("*.init.#", 10, lower = 0.001, upper = 200) |>
  g3_init_val("*.rec.#", 100, lower = 1e-6, upper = 1000) |>
  g3_init_val("*.rec.sd", 5, lower = 4, upper = 20) |>
  g3_init_val("*.M.#", 0.15, lower = 0.001, upper = 1) |>
  g3_init_val("init.F", 0.5, lower = 0.1, upper = 1) |>
  g3_init_val("*.Linf", estimate_linf, spread = 0.2) |>
  g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
  g3_init_val("*.t0", estimate_t0, spread = 2) |>
  g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
  g3_init_val("*.wbeta", 3, optimise = FALSE) |>
  g3_init_val("*.*.alpha", 0.07, lower = 0.01, upper = 0.2) |>
  g3_init_val("*.*.l50", estimate_l50, spread = 0.25) |>
  g3_init_val("*.bbin", 100, lower = 1e-05, upper = 1000) |>
  identity() -> params.in

## ----eval=nzchar(Sys.getenv('G3_TEST_TMB'))-----------------------------------
#  # Optimise model ################################
#  obj.fn <- g3_tmb_adfun(model_code, params.in)
#  
#  params.out <- gadgetutils::g3_iterative(getwd(),
#      wgts = "WGTS",
#      model = model_code,
#      params.in = params.in,
#      grouping = list(
#          fleet = c("ldist_f_surv", "aldist_f_surv"),
#          abund = c("dist_si_cpue")),
#      method = "BFGS",
#      control = list(maxit = 1000, reltol = 1e-10),
#      cv_floor = 0.05)

## ----eval=nzchar(Sys.getenv('G3_TEST_TMB'))-----------------------------------
#  # Generate detailed report ######################
#  fit <- gadgetutils::g3_fit(model_code, params.out)
#  gadgetplots::gadget_plots(fit, "figs", file_type = "html")

## ----eval=FALSE---------------------------------------------------------------
#  utils::browseURL("figs/model_output_figures.html")

## ----echo=FALSE, eval=nzchar(Sys.getenv('G3_TEST_TMB'))-----------------------
#  gadget3:::vignette_test_output("multiple-substocks", model_code, params.out)

