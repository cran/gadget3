library(unittest)

library(gadget3)

ok_group('edit.g3_r - Round-trip through an editor results in working code', {
    nll <- 0.0
    model_fn <- g3_to_r(list(~{
        nll <- g3_param('nll')
        return(nll)
    }))
    model_fn_n <- edit(model_fn, editor = '/bin/true')
    attr(model_fn_n, 'srcref') <- NULL
    attr(model_fn_n, 'srcfile') <- NULL
    ok(ut_cmp_identical(
        capture.output(str(model_fn)),
        capture.output(str(model_fn_n))), "str() output of 2 functions is the same")

    n <- runif(1)
    ok(ut_cmp_identical(n, model_fn(list(nll = n))), "Original function works")
    ok(ut_cmp_identical(n, model_fn_n(list(nll = n))), "Edited function works too")
})

ok_group("g3_to_r: attr.actions", {
    actions <- list(
        list("001" = ~{ 1 + 1 }, "002" = ~{2 + 2}),
        "003" = ~{3 + 3})
    model_fn <- g3_to_r(actions)
    ok(ut_cmp_identical(attr(model_fn, 'actions'), actions), "actions returned as attribute uncollated")
})

ok_group('g3_param', {
    param <- attr(g3_to_r(list(g3a_time(2000, 2004, project_years = 0), ~{
        g3_param('a')
        # NB: We don't actually use optimise, but shouldn't error if it's there
        g3_param('b', value = 4, optimise = FALSE)
    })), 'parameter_template')
    ok(ut_cmp_identical(
        param[c('a', 'b')],
        list(a = 0, b = 4)), "Param list included value values")

    # Definitions-as-code should still parse g3_param()s (it's a different path to formulas)
    model_fn <- g3_to_r(list(g3_formula(
        return(y),
        y = quote( 2 + g3_param("peep") ))))
    ok(ut_cmp_identical(model_fn(list(peep = 4)), 6), "Unevaluated code had g3_param() resolved")
})

ok_group('g3_param_table', {
    param <- attr(g3_to_r(list(g3a_time(2000, 2004, step_lengths = rep(3, times = 4), project_years = 0), ~{
        g3_param_table('pt', expand.grid(  # NB: We can use base R
            cur_year = seq(start_year, end_year),  # NB: We can use g3a_time's vars
            cur_step = 2:3))
        g3_param_table('pg', expand.grid(
            cur_year = start_year,
            # NB: We don't actually use optimise, but shouldn't error if it's there
            cur_step = 1:2), value = 4, optimise = FALSE)
    })), 'parameter_template')
    ok(ut_cmp_identical(
        param[c(paste('pt', 2000:2004, 2, sep = '.'), paste('pt', 2000:2004, 3, sep = '.'), 'pg.2000.1', 'pg.2000.2')],
        structure(
            as.list(c(rep(0, 5), rep(0, 5), rep(4, 2))),
            names = c(
                paste('pt', 2000:2004, 2, sep = '.'),
                paste('pt', 2000:2004, 3, sep = '.'),
                'pg.2000.1',
                'pg.2000.2'))), "Param table turned into multiple parameters, value set")
})
