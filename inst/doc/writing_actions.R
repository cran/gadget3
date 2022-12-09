## ---- message=FALSE, echo=FALSE-----------------------------------------------
library(gadget3)
library(magrittr)

## ---- eval=FALSE--------------------------------------------------------------
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

