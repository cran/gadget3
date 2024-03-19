## ---- message=FALSE, echo=FALSE-----------------------------------------------
library(gadget3)
library(magrittr)

## ---- eval=FALSE--------------------------------------------------------------
#  # Model setup will look something like this
#  ling_model <- g3_to_r(...)
#  
#  ling_model <- edit(ling_model) ; ling_model(ling_param)

## ---- eval=FALSE--------------------------------------------------------------
#  # Model setup will look something like this
#  tmb_ling <- g3_to_tmb(...)
#  tmb_param <- attr(tmb_ling, 'parameter_template')
#  
#  writeLines(TMB::gdbsource(g3_tmb_adfun(
#      tmb_ling,
#      tmb_param,
#      compile_flags = "-g",
#      output_script = TRUE)))

## ---- eval=FALSE--------------------------------------------------------------
#  tmb_ling <- edit(tmb_ling)
#  writeLines(TMB::gdbsource(g3_tmb_adfun(
#      tmb_ling,
#      tmb_param,
#      compile_flags = "-g",
#      output_script = TRUE)))

## ---- eval=FALSE--------------------------------------------------------------
#  obj.fn <- g3_tmb_adfun(bounded_code, params.in, inner.control = list(trace = 3, maxit = 100))

## ---- eval=FALSE--------------------------------------------------------------
#  local({ min(diag(spHess(random = TRUE, set_tail = random[1]))) }, envir = obj.fn$env)

## ---- eval=FALSE--------------------------------------------------------------
#  obj.fn$env$ff <- edit(obj.fn$env$ff)

## ---- eval=FALSE--------------------------------------------------------------
#          assign("newt.args", c(list(par = eval(random.start),
#              fn = f0, gr = function(x) f0(x, order = 1), he = H0,
#              env = env), inner.control), env = globalenv())

## ---- eval=FALSE--------------------------------------------------------------
#  obj.fn$fn()
#  do.call(TMB:::newton, newt.args)

## ---- eval=FALSE--------------------------------------------------------------
#  newt <- TMB:::newton
#  newt <- edit(newt)
#  do.call(newt, newt.args)

