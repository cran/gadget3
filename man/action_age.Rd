\name{action_age}
\alias{g3a_age}
\concept{G3 action}

\title{Gadget3 age action}
\description{
  Add ageing actions to a g3 model
}

\usage{
g3a_age(
        stock,
        output_stocks = list(),
        output_ratios = rep(1/length(output_stocks),
        times = length(output_stocks)),
        run_f = ~cur_step_final,
        run_at = g3_action_order$age,
        transition_at = g3_action_order$age)

}

\arguments{
  \item{stock}{
    \code{\link{g3_stock}} to age.
  }
  \item{output_stocks}{
    List of \code{\link{g3_stock}}s that oldest specimens in \var{stock} should move into.
  }
  \item{output_ratios}{
    Vector of proportions for how to distribute into \var{output_stocks}, default evenly spread.
  }
  \item{run_f}{
    \link{formula} specifying a condition for running this action, default is end of model year.
  }
  \item{run_at}{
    Integer order that actions will be run within model, see \code{\link{g3_action_order}}.
  }
  \item{transition_at}{
    Integer order that transition actions will be run within model, see \code{\link{g3_action_order}}.
  }
}

\value{An action (i.e. list of formula objects) that will, for the given \var{stock}...\enumerate{
  \item{Move the final age group into temporary storage, \code{stock__transitioning_num} / \code{stock__transitioning_wgt}}
  \item{Move the contents of all other age groups into the age group above}
  \item{Move the contents of the temporary storage into \var{output_stocks}}
}
If \var{stock} has only one age, and \var{output_stocks} has been specified, then the contentes will be moved,
if \var{output_stocks} is empty, then the action will do nothing.
}

\seealso{
  \url{https://gadget-framework.github.io/gadget2/userguide/chap-stock.html#sec:stockmature},
  \code{\link{g3_stock}}
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)
ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) \%>\% g3s_age(5, 15)

# Ageing for immature ling
age_action <- g3a_age(ling_imm,
    output_stocks = list(ling_mat))
}
