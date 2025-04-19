## ----message=FALSE, echo=FALSE------------------------------------------------
library(unittest)
# Redirect ok() output to stderr
options(unittest.output = stderr())
if (nzchar(Sys.getenv('G3_TEST_TMB'))) options(gadget3.tmb.work_dir = gadget3:::vignette_base_dir('work_dir'))

library(gadget3)
set.seed(123)

## ----warning = FALSE, message = FALSE-----------------------------------------
### Introduction to gadget3: A single stock model


## ----eval=FALSE---------------------------------------------------------------
#  install.packages('gadget3')

## ----eval=FALSE---------------------------------------------------------------
#  install.packages('MFDB')
#  remotes::install_github('gadget-framework/gadgetutils')
#  remotes::install_github('gadget-framework/gadgetplots')
#  remotes::install_github('gadget-framework/g3experiments')

## ----warning = FALSE, message = FALSE-----------------------------------------
library(gadget3)
library(dplyr)

## -----------------------------------------------------------------------------
actions <- list()

# Create time definitions ####################

actions_time <- list(
  g3a_time(
    1990L, 2023L,
    step_lengths = c(3L, 3L, 3L, 3L)),
  NULL)

actions <- c(actions, actions_time)

## ----comment = ''-------------------------------------------------------------
g3_to_r(actions_time)

## ----comment = ''-------------------------------------------------------------
g3_to_tmb(actions_time)

## -----------------------------------------------------------------------------
area_names <- g3_areas(c('IXa', 'IXb'))

# Create stock definition for fish ####################
fish <- g3_stock("fish", seq(10, 100, 10)) |>
  g3s_livesonareas(area_names["IXa"]) |>
  g3s_age(1L, 5L)

## -----------------------------------------------------------------------------
# aperm() re-orders dimensions for more compact printing
aperm(g3_stock_instance(fish, 0), c(1,3,2))

## -----------------------------------------------------------------------------
actions_fish <- list(
  g3a_growmature(fish, g3a_grow_impl_bbinom(
    maxlengthgroupgrowth = 4L)),
  g3a_naturalmortality(fish),
  g3a_initialconditions_normalcv(fish),
  g3a_renewal_normalcv(fish,
    run_step = 2),
  g3a_age(fish),
  NULL)

actions_likelihood_fish <- list(
  g3l_understocking(list(fish), nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_fish, actions_likelihood_fish)

## -----------------------------------------------------------------------------
head(g3a_naturalmortality)
head(g3a_naturalmortality_exp)

## -----------------------------------------------------------------------------
simple_actions <- list(
    g3a_time(1990, 1991),
    g3a_initialconditions_normalcv(fish))
simple_fn <- g3_to_r(c(simple_actions, list(
    g3a_report_detail(simple_actions) )))

params <- attr(simple_fn, 'parameter_template')
unlist(params)

## ----message=FALSE, echo=FALSE------------------------------------------------
ok(ut_cmp_identical(sort(names(params), method = "radix"), c(
    "fish.K",
    "fish.Linf",
    "fish.M.1", "fish.M.2", "fish.M.3", "fish.M.4", "fish.M.5",
    "fish.init.1", "fish.init.2", "fish.init.3", "fish.init.4", "fish.init.5",
    "fish.init.scalar",
    "fish.lencv",
    "fish.t0",
    "fish.walpha",
    "fish.wbeta",
    "init.F",
    "project_years",
    "recage",
    "report_detail",
    "retro_years")), "parameter_template names match expected")

## -----------------------------------------------------------------------------
params$fish.init.scalar <- 10
params$fish.init.1 <- 10
params$fish.init.2 <- 10
params$fish.init.3 <- 10
params$fish.init.4 <- 10
params$fish.init.5 <- 10
params$fish.M.1 <- 0.15
params$fish.M.2 <- 0.15
params$fish.M.3 <- 0.15
params$fish.M.4 <- 0.15
params$fish.M.5 <- 0.15
params$init.F <- 0.5
params$recage <- 0
params$fish.Linf <- max(g3_stock_def(fish, "midlen"))
params$fish.K <- 0.3
params$fish.t0 <- g3_stock_def(fish, "minage") - 0.8
params$fish.lencv <- 0.1
params$report_detail <- 1

# Run model and pull out final abundance from the result
g3_array_plot(g3_array_agg(
    attr(simple_fn(params), "dstart_fish__num"),
    c('age', 'length'),
    time = "1990-01"))

## -----------------------------------------------------------------------------
params$fish.K <- 0.9
g3_array_plot(g3_array_agg(
    attr(simple_fn(params), "dstart_fish__num"),
    c('age', 'length'),
    time = "1990-01"))

## ----message=FALSE, echo=FALSE------------------------------------------------
# Make reporting is working
abund <- g3_array_agg(attr(simple_fn(params), "dstart_fish__num"), c('age', 'length'), time = "1990-01")
ok(ut_cmp_identical(dimnames(abund), list(
    length = c("10:20", "20:30", "30:40", "40:50", "50:60", "60:70", "70:80", "80:90", "90:100", "100:Inf"),
    age = c("age1", "age2", "age3", "age4", "age5"))), "abund dimnames() structure")

## -----------------------------------------------------------------------------
# Fleet data for f_surv #################################

# Landings data: For each year/step/area
expand.grid(year = 1990:2023, step = 2, area = 'IXa') |>
    # Generate a random total landings by weight
    mutate(weight = rnorm(n(), mean = 1000, sd = 100)) |>
    # Assign result to landings_f_surv
    identity() -> landings_f_surv

## -----------------------------------------------------------------------------
landings_f_surv
plot(landings_f_surv[c('year', 'weight')], ylim = c(0, 2000), col = "red")

## -----------------------------------------------------------------------------
# Length distribution data: Randomly generate 100 samples in each year/step/area
expand.grid(year = 1990:2023, step = 2, area = 'IXa', length = rep(NA, 100)) |>
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

## -----------------------------------------------------------------------------
head(ldist_f_surv.raw)

## -----------------------------------------------------------------------------
cut(c(50), breaks = c(seq(0, 80, 20), Inf), right = FALSE)

## -----------------------------------------------------------------------------
summary(ldist_f_surv)
years <- 1990:1994
par(mfrow=c(2, ceiling(length(years) / 2)))
for (y in years) plot(as.data.frame(ldist_f_surv) |>
    filter(year == y & step == 2) |>
    select(length, number), main = y, ylim = c(0, 60))

## -----------------------------------------------------------------------------
# Assume 100 * 100 samples in each year/step/area
expand.grid(year = 1990:2023, step = 2, area = 'IXa', age = rep(NA, 100), length = rep(NA, 100)) |>
  # Generate random whole numbers for age
  mutate(age = floor(runif(n(), min = 1, max = 5))) |>
  # Generate random lengths for these samples
  mutate(length = rnorm(n(), mean = 30 + age * 10, sd = 20)) |>
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
summary(aldist_f_surv)
years <- 1990:1994
ages <- unique(aldist_f_surv$age)

par(mfrow=c(length(years), length(ages)), mar = c(2,2,1,0))
for (y in years) for (a in ages) plot(as.data.frame(ldist_f_surv) |>
    filter(year == y & step == 2) |>
    select(length, number), main = sprintf("year = %d, age = %s", y, a), ylim = c(0, 60))

## -----------------------------------------------------------------------------
# Create fleet definition for f_surv ####################
f_surv <- g3_fleet("f_surv") |> g3s_livesonareas(area_names["IXa"])

## -----------------------------------------------------------------------------
actions_f_surv <- list(
  g3a_predate_fleet(
    f_surv,
    list(fish),
    suitabilities = g3_suitability_exponentiall50(),
    catchability_f = g3a_predate_catchability_totalfleet(
      g3_timeareadata("landings_f_surv", landings_f_surv, "weight", areas = area_names))),
  NULL)
actions <- c(actions, actions_f_surv)

## -----------------------------------------------------------------------------
actions_likelihood_f_surv <- list(
  g3l_catchdistribution(
    "ldist_f_surv",
    obs_data = ldist_f_surv,
    fleets = list(f_surv),
    stocks = list(fish),
    function_f = g3l_distribution_sumofsquares(),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  g3l_catchdistribution(
    "aldist_f_surv",
    obs_data = aldist_f_surv,
    fleets = list(f_surv),
    stocks = list(fish),
    function_f = g3l_distribution_sumofsquares(),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  NULL)
actions <- c(actions, actions_likelihood_f_surv)

## -----------------------------------------------------------------------------
# Create abundance index for si_cpue ########################

# Generate random data
expand.grid(year = 1990:2023, step = 3, area = 'IXa') |>
    # Fill in a weight column with total biomass for the year/step/area combination
    mutate(weight = runif(n(), min = 10000, max = 100000)) ->
    dist_si_cpue

actions_likelihood_si_cpue <- list(

  g3l_abundancedistribution(
    "dist_si_cpue",
    dist_si_cpue,
    
    stocks = list(fish),
    function_f = g3l_distribution_surveyindices_log(alpha = NULL, beta = 1),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_likelihood_si_cpue)

## -----------------------------------------------------------------------------
# Create model objective function ####################

# Apply bounds in code - the other option would be using control = list(lower = g3_tmb_lower(params.in), ...)
model_code <- g3_to_tmb(c(actions, list(
    g3a_report_detail(actions),
    g3l_bounds_penalty(actions) )))

## -----------------------------------------------------------------------------
simple_code <- g3_to_tmb(list(
    g3a_time(1990, 1991),
    g3a_naturalmortality(fish) ))
attr(simple_code, 'parameter_template')

## -----------------------------------------------------------------------------
attr(simple_code, "parameter_template") |>
    g3_init_val("*.M.#", 0.1) |>
    g3_init_val("*.M.3", 0.5) |>
    g3_init_val("*.M.2|4", 0.2)

## -----------------------------------------------------------------------------
attr(simple_code, "parameter_template") |>
    g3_init_val("*.M.#", 0.15, lower = 0.001, upper = 1)

## -----------------------------------------------------------------------------
attr(simple_code, "parameter_template") |>
    g3_init_val("*.M.#", 0.15, spread = 0.5)

## -----------------------------------------------------------------------------
# Guess l50 / linf based on stock sizes
estimate_l50 <- g3_stock_def(fish, "midlen")[[length(g3_stock_def(fish, "midlen")) / 2]]
estimate_linf <- max(g3_stock_def(fish, "midlen"))
estimate_t0 <- g3_stock_def(fish, "minage") - 0.8

attr(model_code, "parameter_template") |>
  # fish.init.scalar & fish.rec.scalar: Overall scalar for recruitment/initial conditions, see g3a_renewal_normalcv()
  g3_init_val("*.rec|init.scalar", 1, optimise = FALSE) |>
  # fish.rec.(age): Per-age recriutment scalar, see g3a_renewal_normalcv()
  g3_init_val("*.init.#", 10, lower = 0.001, upper = 30) |>
  # fish.rec.(year): Recruitment level year-on-year, see g3a_renewal_normalcv()
  g3_init_val("*.rec.#", 1e-4, lower = 1e-6, upper = 1e-2) |>
  g3_init_val("*.rec.proj", 0.002) |>
  # init.F: Offset for initial M, see g3a_renewal_initabund()
  g3_init_val("init.F", 0.5, lower = 0.1, upper = 1) |>

  # fish.M.(age): per-age M for our species, see g3a_naturalmortality()
  g3_init_val("*.M.#", 0.15, lower = 0.001, upper = 10) |>

  # fish.Linf, fish.K, fish.t0: VonB parameters for our species, see g3a_renewal_vonb_t0(), g3a_grow_lengthvbsimple()
  g3_init_val("*.Linf", estimate_linf, spread = 0.2) |>
  g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
  g3_init_val("*.t0", estimate_t0, spread = 2) |>

  # fish.walpha, fish.wbeta: Age/weight relationship for initialconditions, renewal, see g3a_renewal_normalcv()
  g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
  g3_init_val("*.wbeta", 3, optimise = FALSE) |>

  # fish.f_surv.alpha, fish.f_surv.l50: Curve/l50 for fishing suitability, see g3_suitability_exponentiall50()
  g3_init_val("*.*.alpha", 0.07, lower = 0.01, upper = 0.2) |>
  g3_init_val("*.*.l50", estimate_l50, spread = 0.25) |>

  # fish.bbin: Beta for beta-binomial distribution for fish growth, see g3a_grow_impl_bbinom()
  g3_init_val("*.bbin", 1, lower = 1e-05, upper = 10) |>

  # identity() is a do-nothing function, but it lets us finish on a new line
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
#  gadget3:::vignette_test_output("introduction-single-stock", model_code, params.out)

