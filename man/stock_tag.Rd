\name{stock_tag}
\alias{g3s_tag}
\concept{G3 stock}

\title{Gadget3 tag dimension}
\description{
  Add tag dimensions to g3_stock classes
}

\usage{
g3s_tag(inner_stock, tag_ids, force_untagged = TRUE)

}

\arguments{
  \item{inner_stock}{
    A \code{\link{g3_stock}} that we extend with an area dimension
  }
  \item{tag_ids}{
    A vector of numeric tags the stock can have, generated by \code{seq_along}, e.g.
    Tag ID 0 is considered to be "untagged".
  }
  \item{force_untagged}{
    If TRUE, if "untagged" tag 0 isn't present it will be added.
  }
}

\value{
  \subsection{g3s_tag}{
      A \code{\link{g3_stock}} with an additional 'tag' dimension.

      When iterating over the stock, iterate over each tag in turn,
      \var{tag} will be set to the current integer area.

      When interacting with another stock, iterate over each tag in turn,
      the variable name will depend on the scenario, e.g. \var{prey_tag}.
  }
}

\examples{
\dontshow{library(magrittr)}

# Make a lookup of text names to integers
tags <- c('H1-00', 'H1-01')
tags <- structure(seq_along(tags), names = tags)

# prey_a can have any of these tags
prey_a <- g3_stock('prey_a', seq(1, 10)) \%>\% g3s_tag(tags)

# Use stock_instance to see what the array would look like
g3_stock_instance(prey_a)
}
