## ----message=FALSE, echo=FALSE------------------------------------------------
library(gadget3)
library(magrittr)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) options(gadget3.tmb.work_dir = gadget3:::vignette_base_dir('work_dir'))

## ----eval=FALSE---------------------------------------------------------------
#      out <- new.env(parent = emptyenv())
#      out[[step_id(run_at, 1, stock)]] <- g3_step(f_substitute(~{
#          debug_label("g3a_mature for ", stock)
#          # Matured stock will weigh the same
#          stock_with(stock, stock_with(matured, matured__wgt <- stock__wgt))
#  
#          stock_iterate(stock, stock_intersect(matured, if (run_f) {
#              debug_label("Move matured ", stock, " into temporary storage")
#              stock_ss(matured__num) <- stock_ss(stock__num) * maturity_f
#              stock_ss(stock__num) <- stock_ss(stock__num) - stock_ss(matured__num)
#          }))
#      }, list(run_f = run_f, maturity_f = maturity_f)))

## -----------------------------------------------------------------------------
st_imm <- g3_stock(c("st", "imm"), 1:10)
g3_to_r(list(
    g3a_naturalmortality(
        st_imm,
        g3_formula(
            parrot**2,
            parrot = 0,
            "-01:ut:parrot" = g3_formula({
                parrot <- runif(1)
            }))),
    NULL ))

