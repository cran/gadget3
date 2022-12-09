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

