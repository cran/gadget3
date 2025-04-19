## ----message=FALSE, echo=FALSE------------------------------------------------
library(gadget3)
library(magrittr)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) options(gadget3.tmb.work_dir = gadget3:::vignette_base_dir('work_dir'))

## -----------------------------------------------------------------------------
get_formula <- function (size) {
    # NB: The reason we make a function here is so we have an isolated environment
    # to make examples cleaner.
    cows <- size * 2
    pigs <- size * 4
    return(~cows + pigs)
}
f <- get_formula(8)
g <- get_formula(5)

## -----------------------------------------------------------------------------
str(f)

## -----------------------------------------------------------------------------
str(as.list(environment(f)))

## -----------------------------------------------------------------------------
str(as.list(environment(g)))

## -----------------------------------------------------------------------------
g3_to_r(list(f))

## -----------------------------------------------------------------------------
g3_to_tmb(list(f))

## -----------------------------------------------------------------------------
g3a_time(1990, 1999)

## -----------------------------------------------------------------------------
g3_to_r(g3a_time(1990, 1999))

## -----------------------------------------------------------------------------
g3_to_r(g3a_time(1990, ~start_year + 4 ))

## -----------------------------------------------------------------------------
ling_imm <- g3_stock('ling_imm', seq(0, 50, 10))
g3_stock_instance(ling_imm)

## -----------------------------------------------------------------------------
ling_imm <- g3_stock('ling_imm', seq(0, 50, 10)) %>%
    g3s_age(3, 10)
g3_stock_instance(ling_imm)

## -----------------------------------------------------------------------------
ling_imm <- g3_stock('ling_imm', seq(0, 50, 10)) %>%
    g3s_livesonareas(c(1,2)) %>%
    g3s_age(3, 10)
g3_stock_instance(ling_imm)[,,'age3']

## -----------------------------------------------------------------------------
fn <- g3_to_r(g3a_growmature(
    ling_imm,
    impl_f = g3a_grow_impl_bbinom(
        delta_len_f = ~age * 10,
        delta_wgt_f = ~area * 20,
        beta_f = ~g3_param("ling.bbin"),
        maxlengthgroupgrowth = 4),
    transition_f = ~TRUE))
fn

## -----------------------------------------------------------------------------
custom_delta_l <- ~area * 99
custom_delta_w <- ~ling_imm__plusdl * 44
fn <- g3_to_r(g3a_growmature(
    ling_imm,
    impl_f = g3a_grow_impl_bbinom(
        delta_len_f = ~age * custom_delta_l * 10,
        delta_wgt_f = ~area * custom_delta_w * 20,
        beta_f = ~g3_param("ling.bbin"),
        maxlengthgroupgrowth = 4),
    transition_f = ~TRUE))
fn

## -----------------------------------------------------------------------------
ling_model <- g3_to_r(list(
    g3a_age(ling_imm),
    g3a_growmature(
        ling_imm,
        impl_f = g3a_grow_impl_bbinom(
            delta_len_f = ~age * 10,
            delta_wgt_f = ~area * 20,
            beta_f = ~g3_param("ling.bbin"),
            maxlengthgroupgrowth = 4)),
    g3a_time(1990, 1999)))

## -----------------------------------------------------------------------------
ling_imm_actions <- list(
    g3a_age(ling_imm),
    g3a_growmature(
        ling_imm,
        impl_f = g3a_grow_impl_bbinom(
            delta_len_f = ~age * 10,
            delta_wgt_f = ~area * 20,
            beta_f = ~g3_param("ling.bbin"),
            maxlengthgroupgrowth = 4)))
time_actions <- list(
    g3a_time(1990, 1999))

ling_model <- g3_to_r(c(ling_imm_actions, time_actions))

## -----------------------------------------------------------------------------
lln <- g3_fleet('lln') %>% g3s_livesonareas(1)
action <- g3a_predate_fleet(
        lln,
        list(ling_imm),
        suitabilities = list(
            ling_imm = g3_suitability_exponentiall50(
                ~g3_param('ling.lln.alpha'),
                ~g3_param('ling.lln.l50')),
            ling_mat = g3_suitability_exponentiall50(
                ~g3_param('ling.lln.alpha'),
                ~g3_param('ling.lln.l50'))),
        catchability_f = g3a_predate_catchability_totalfleet(1))
names(action)

