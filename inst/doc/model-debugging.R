## ----message=FALSE, echo=FALSE------------------------------------------------
library(gadget3)
library(magrittr)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) options(gadget3.tmb.work_dir = gadget3:::vignette_base_dir('work_dir'))

## ----eval=FALSE---------------------------------------------------------------
# # Create model based on your actions
# model_fn <- g3_to_r(actions)
# 
# # Edit source code & re-run model
# model_fn <- edit(model_fn)
# model_fn(params.in)

## ----eval=FALSE---------------------------------------------------------------
# tmb_model <- g3_to_tmb(actions)
# 
# # Edit source code & re-run model
# tmb_model <- edit(tmb_model)
# obj.fn <- g3_tmb_adfun(tmb_model, params.in, type = "Fun")
# r <- obj.fn$report()

## ----eval=FALSE---------------------------------------------------------------
# tmb_model <- g3_to_tmb(c(actions, list(
#   g3a_report_detail(actions),
#   g3a_trace_var(actions),
#   gadget3::g3l_bounds_penalty(actions) )))
# obj.fn <- g3_tmb_adfun(tmb_model, params.in, type = "Fun")
# r <- obj.fn$report()

## ----eval=FALSE---------------------------------------------------------------
# obj.fn <- g3_tmb_adfun(
#     model_cpp,
#     params.in,
#     # control allows you to tweak trace level for outer optimiser, see "?stats::optim"
#     control = list(trace = 2),
#     # inner.control allows you to increase trace level for the random effect optimiser, see "?TMB::newton"
#     inner.control = list(trace = 3, maxit = 1000, tol = 1e-7) )
# obj.fn$env$tracepar <- TRUE  # Print result of every likelihood evaluation
# obj.fn$env$tracemgc <- TRUE  # Print maximum gradient at every gradient evaluation
# obj.fn$env$silent <- FALSE  # Show/hide tracing messages

## ----eval=FALSE---------------------------------------------------------------
# # Model setup will look something like this
# tmb_ling <- g3_to_tmb(...)
# tmb_param <- attr(tmb_ling, 'parameter_template')
# 
# writeLines(TMB::gdbsource(g3_tmb_adfun(
#     tmb_ling,
#     tmb_param,
#     compile_flags = "-g",
#     output_script = TRUE)))

## ----eval=FALSE---------------------------------------------------------------
# obj.fn <- g3_tmb_adfun(bounded_code, params.in, inner.control = list(trace = 3, maxit = 100))

## ----eval=FALSE---------------------------------------------------------------
# local({ min(diag(spHess(random = TRUE, set_tail = random[1]))) }, envir = obj.fn$env)

## ----eval=FALSE---------------------------------------------------------------
# obj.fn$env$ff <- edit(obj.fn$env$ff)

## ----eval=FALSE---------------------------------------------------------------
#         assign("newt.args", c(list(par = eval(random.start),
#             fn = f0, gr = function(x) f0(x, order = 1), he = H0,
#             env = env), inner.control), env = globalenv())

## ----eval=FALSE---------------------------------------------------------------
# obj.fn$fn()
# do.call(TMB:::newton, newt.args)

## ----eval=FALSE---------------------------------------------------------------
# newt <- TMB:::newton
# newt <- edit(newt)
# do.call(newt, newt.args)

