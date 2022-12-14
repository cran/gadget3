library(magrittr)
library(unittest)

library(gadget3)

prey_a <- g3_stock('prey_a', seq(1, 10))
prey_b <- g3_stock('prey_b', seq(1, 10))
report_a <- g3s_clone(prey_a, 'report_a') %>% gadget3:::g3s_modeltime()
report_b <- g3s_clone(prey_b, 'report_b') %>% gadget3:::g3s_modeltime()
fleet_numberfleet <- g3_fleet('fleet_numberfleet')
fleet_linearfleet <- g3_fleet('fleet_linearfleet')
fleet_effortfleet <- g3_fleet('fleet_effortfleet')
fleet_quotafleet <- g3_fleet('fleet_quotafleet')
report_numberfleet <- g3s_clone(fleet_numberfleet, 'report_numberfleet') %>% gadget3:::g3s_modeltime()
report_linearfleet <- g3s_clone(fleet_linearfleet, 'report_linearfleet') %>% gadget3:::g3s_modeltime()
report_effortfleet <- g3s_clone(fleet_effortfleet, 'report_effortfleet') %>% gadget3:::g3s_modeltime()
report_quotafleet <- g3s_clone(fleet_quotafleet, 'report_quotafleet') %>% gadget3:::g3s_modeltime()

actions <- list(
    g3a_time(2000, 2005, step_lengths = c(6,6), project_years = 0),
    g3a_initialconditions(prey_a, ~1000000 * prey_a__midlen, ~100 * prey_a__midlen),
    g3a_initialconditions(prey_b, ~1000000 * prey_b__midlen, ~100 * prey_b__midlen),
    g3a_predate_fleet(
        fleet_numberfleet,
        list(prey_a, prey_b),
        suitabilities = list(prey_a = 0.1, prey_b = 0.1),
        catchability_f = g3a_predate_catchability_numberfleet(10)),
    g3a_predate_fleet(
        fleet_linearfleet,
        list(prey_a, prey_b),
        suitabilities = list(prey_a = 0.1, prey_b = 0.1),
        catchability_f = g3a_predate_catchability_linearfleet(10)),
    g3a_predate_fleet(
        fleet_effortfleet,
        list(prey_a, prey_b),
        suitabilities = list(prey_a = 0.1, prey_b = 0.1),
        catchability_f = g3a_predate_catchability_effortfleet(list(prey_a = 5, prey_b= 6), 10)),
    g3a_predate_fleet(
        fleet_quotafleet,
        list(prey_a, prey_b),
        suitabilities = list(prey_a = 0.1, prey_b = 0.1),
        catchability_f = g3a_predate_catchability_quotafleet(
            data.frame(
                biomass = c(100, Inf),
                quota = I(list(~g3_param('quota.low', value = 1000), ~g3_param('quota.high', value = 9000)))),
            E = 10,
            recalc_f = ~cur_step == 1)),
    g3a_report_stock(report_a, prey_a, ~stock_ss(prey_a__num)),
    g3a_report_stock(report_a, prey_a, ~stock_ss(prey_a__wgt)),
    g3a_report_stock(report_a, prey_a, ~stock_ss(prey_a__totalpredate)),
    g3a_report_stock(report_a, prey_a, ~stock_ss(prey_a__consratio)),
    g3a_report_stock(report_a, prey_a, ~stock_ss(prey_a__predby_fleet_numberfleet)),
    g3a_report_stock(report_a, prey_a, ~stock_ss(prey_a__predby_fleet_linearfleet)),
    g3a_report_stock(report_a, prey_a, ~stock_ss(prey_a__predby_fleet_effortfleet)),
    g3a_report_stock(report_a, prey_a, ~stock_ss(prey_a__predby_fleet_quotafleet)),

    g3a_report_stock(report_b, prey_b, ~stock_ss(prey_b__num)),
    g3a_report_stock(report_b, prey_b, ~stock_ss(prey_b__wgt)),
    g3a_report_stock(report_b, prey_b, ~stock_ss(prey_b__totalpredate)),
    g3a_report_stock(report_b, prey_b, ~stock_ss(prey_b__consratio)),
    g3a_report_stock(report_b, prey_b, ~stock_ss(prey_b__predby_fleet_numberfleet)),
    g3a_report_stock(report_b, prey_b, ~stock_ss(prey_b__predby_fleet_linearfleet)),
    g3a_report_stock(report_b, prey_b, ~stock_ss(prey_b__predby_fleet_effortfleet)),
    g3a_report_stock(report_b, prey_b, ~stock_ss(prey_b__predby_fleet_quotafleet)),

    g3a_report_stock(report_numberfleet, fleet_numberfleet, ~stock_ss(fleet_numberfleet__catch)),
    g3a_report_stock(report_numberfleet, fleet_numberfleet, ~stock_ss(fleet_numberfleet__catchnum)),
    g3a_report_stock(report_linearfleet, fleet_linearfleet, ~stock_ss(fleet_linearfleet__catch)),
    g3a_report_stock(report_effortfleet, fleet_effortfleet, ~stock_ss(fleet_effortfleet__catch)),

    g3a_report_stock(report_quotafleet, fleet_quotafleet, ~stock_ss(fleet_quotafleet__catch)),
    list())

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
}

ok_group("Catchability", {
    params <- attr(model_fn, 'parameter_template')

    result <- model_fn(params)
    r <- attributes(result)
    # str(as.list(r), vec.len = 10000)

    # Make sure everything is internally consistent
    ok(all(r$report_a__consratio > 0.9499), "report_a__consratio: No overconsumption")
    ok(all(r$report_b__consratio > 0.9499), "report_b__consratio: No overconsumption")
    ok(ut_cmp_equal(
        r$report_a__predby_fleet_numberfleet + r$report_a__predby_fleet_linearfleet +  r$report_a__predby_fleet_effortfleet +  r$report_a__predby_fleet_quotafleet,
        r$report_a__totalpredate), "report_a__totalpredate: Equal to sum of fleet consumption")
    ok(ut_cmp_equal(
        r$report_b__predby_fleet_numberfleet + r$report_b__predby_fleet_linearfleet +  r$report_b__predby_fleet_effortfleet +  r$report_b__predby_fleet_quotafleet,
        r$report_b__totalpredate), "report_b__totalpredate: Equal to sum of fleets")
    ok(ut_cmp_equal(
        as.numeric(colSums(r$report_a__predby_fleet_numberfleet + r$report_b__predby_fleet_numberfleet)),
        as.numeric(r$report_numberfleet__catch)), "report_numberfleet__catch: Equal to sum of preys")
    ok(ut_cmp_equal(
        as.numeric(colSums((r$report_a__predby_fleet_numberfleet/r$report_a__wgt) + (r$report_b__predby_fleet_numberfleet/r$report_b__wgt))),
        as.numeric(r$report_numberfleet__catchnum)), "report_numberfleet__catchnum: Consistent with __predby_fleet_numberfleet")
    ok(ut_cmp_equal(
        as.numeric(colSums(r$report_a__predby_fleet_linearfleet + r$report_b__predby_fleet_linearfleet)),
        as.numeric(r$report_linearfleet__catch)), "report_linearfleet__catch: Equal to sum of preys")
    ok(ut_cmp_equal(
        as.numeric(colSums(r$report_a__predby_fleet_effortfleet + r$report_b__predby_fleet_effortfleet)),
        as.numeric(r$report_effortfleet__catch)), "report_effortfleet__catch: Equal to sum of preys")
    ok(ut_cmp_equal(
        as.numeric(colSums(r$report_a__predby_fleet_quotafleet + r$report_b__predby_fleet_quotafleet)),
        as.numeric(r$report_quotafleet__catch)), "report_quotafleet__catch: Equal to sum of preys")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})
