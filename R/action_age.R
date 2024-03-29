# On final step of year, move stock into the next age bracket
g3a_age <- function(
        stock,
        output_stocks = list(),
        output_ratios = rep(1 / length(output_stocks), times = length(output_stocks)),
        run_f = ~cur_step_final,
        run_at = g3_action_order$age,
        transition_at = g3_action_order$age ) {
    out <- new.env(parent = emptyenv())

    # Replace anything of form xxx[.[1,2,3]] with xxx[1,2,3]
    fix_subsets <- function (in_f) {
        call_replace(in_f, "[" = function (subset_call) {
            if (!is.call(subset_call)) {
                # Raw [ symbol, just return it
                subset_call
            } else if (length(subset_call) == 3 &&
                    is.call(subset_call[[3]]) &&
                    subset_call[[3]][[1]] == as.symbol("[") &&
                    subset_call[[3]][[2]] == quote(.)) {
                # Call of form summat[.[1,2, .. ]], remove nesting
                as.call(c(
                    head(as.list(subset_call), -1),
                    tail(as.list(subset_call[[3]]), -2)))
            } else {
                # Recurse through subsetting call
                as.call(lapply(as.list(subset_call), fix_subsets))
            }
        })
    }

    # Convert list of subset args into an actual call
    to_subset_call <- function (l) {
        as.call(c(list(as.symbol("["), as.symbol(".")), unname(l)))
    }

    # Mangle stock_num / stock_wgt to remove non-age parameters
    age_iter_ss <- to_subset_call(lapply(stock$iter_ss, function (x) {
        if (as.character(x) %in% c("stock__age_idx")) x
        else quote(x[,1])[[3]]  # i.e. anything else should be missing
    }))
    age_younger_iter_ss <- to_subset_call(lapply(stock$iter_ss, function (x) {
        if (as.character(x) %in% c("stock__age_idx")) call("-", x, 1L)  # Subtract 1 to age paramter
        else quote(x[,1])[[3]]  # i.e. anything else should be missing
    }))

    stock__num <- g3_stock_instance(stock, 0)
    stock__wgt <- g3_stock_instance(stock, 1)
    stock_movement <- g3s_age(
        g3s_clone(stock, paste0(stock$name, '_movement')),
        g3_stock_def(stock, 'maxage') + 1,
        g3_stock_def(stock, 'maxage') + 1)
    stock_movement__transitioning_num <- g3_stock_instance(stock_movement)
    stock_movement__transitioning_wgt <- g3_stock_instance(stock_movement)

    movement_age_iter_ss <- to_subset_call(lapply(stock_movement$iter_ss, function (x) {
        if (as.character(x) %in% c("stock__age_idx")) quote(g3_idx(1))  # stock_movement only has one age bracket
        else quote(x[,1])[[3]]  # i.e. anything else should be missing
    }))

    # Handle single-age special case separately
    if (g3_stock_def(stock, 'maxage') == g3_stock_def(stock, 'minage')) {
        if (length(output_stocks) == 0) {
            # Single age, no output stocks -> nothing to do
            return(list())
        }

        # Instead of using the below, just move-and-zero stocks
        out[[step_id(run_at, 1, stock)]] <- g3_step(fix_subsets(f_substitute(~if (run_f) {
            debug_label("g3a_age for ", stock)
            stock_with(stock, stock_with(stock_movement, for (age in seq(stock__maxage, stock__minage, by = -1)) g3_with(
                   stock__age_idx := g3_idx(age - stock__minage + 1L), {
                debug_trace("Move oldest ", stock, " into ", stock_movement)
                # NB: We should be doing this once in a normal iterate case, but here there's only one loop so doesn't matter
                # NB: This relies on the dimension ordering between stock_movement & stock matching
                stock_movement__transitioning_num[movement_age_iter_ss] <- stock_reshape(stock_movement, stock__num[age_iter_ss])
                stock_movement__transitioning_wgt[movement_age_iter_ss] <- stock_reshape(stock_movement, stock__wgt[age_iter_ss])
                stock__num[age_iter_ss] <- 0
            })))
        }, list(
            run_f = run_f,
            movement_age_iter_ss = movement_age_iter_ss,
            age_iter_ss = age_iter_ss))))

        # NB: move_remainder = FALSE because it's pointless here (and we can't move back into stock_movement)
        out[[step_id(transition_at, 90, stock)]] <- g3a_step_transition(stock_movement, output_stocks, output_ratios, move_remainder = FALSE, run_f = run_f)
        return(as.list(out))
    }

    # Add transition steps if output_stocks provided
    if (length(output_stocks) == 0) {
        final_year_f = fix_subsets(f_substitute(~{
            debug_trace("Oldest ", stock, " is a plus-group, combine with younger individuals")
            stock__wgt[age_iter_ss] <- ratio_add_vec(
                stock__wgt[age_iter_ss], stock__num[age_iter_ss],
                stock__wgt[age_younger_iter_ss], stock__num[age_younger_iter_ss])
            stock__num[age_iter_ss] <- stock__num[age_iter_ss] + stock__num[age_younger_iter_ss]
        }, list(
            age_iter_ss = age_iter_ss,
            age_younger_iter_ss = age_younger_iter_ss)))
    } else {
        final_year_f = fix_subsets(f_substitute(~stock_with(stock_movement, {
            debug_trace("Move oldest ", stock, " into ", stock_movement)
            # NB: We should be doing this once in a normal iterate case, but here there's only one loop so doesn't matter
            # NB: This relies on the dimension ordering between stock_movement & stock matching
            stock_movement__transitioning_num[movement_age_iter_ss] <- stock_reshape(stock_movement, stock__num[age_iter_ss])
            stock_movement__transitioning_wgt[movement_age_iter_ss] <- stock_reshape(stock_movement, stock__wgt[age_iter_ss])
            stock__num[age_iter_ss] <- stock__num[age_younger_iter_ss]
            stock__wgt[age_iter_ss] <- stock__wgt[age_younger_iter_ss]
        }), list(
            movement_age_iter_ss = movement_age_iter_ss,
            age_iter_ss = age_iter_ss,
            age_younger_iter_ss = age_younger_iter_ss)))
        # NB: move_remainder = FALSE because it's pointless here (and we can't move back into stock_movement)
        out[[step_id(transition_at, 90, stock)]] <- g3a_step_transition(stock_movement, output_stocks, output_ratios, move_remainder = FALSE, run_f = run_f)
    }

    out[[step_id(run_at, 1, stock)]] <- g3_step(fix_subsets(f_substitute(~if (run_f) {
        debug_label("g3a_age for ", stock)

        stock_with(stock, for (age in seq(stock__maxage, stock__minage, by = -1)) g3_with(
                stock__age_idx := g3_idx(age - stock__minage + 1L), {
            debug_trace("Check stock has remained finite for this step")

            if (age == stock__maxage) {
                final_year_f
            } else if (age == stock__minage) {
                debug_trace("Empty youngest ", stock, " age-group")
                stock__num[age_iter_ss] <- 0
                # NB: Leave stock__wgt[age_iter_ss] as-is, it's value is irrelevant with zero stock, and will result in NaN if we zero it.
            } else {
                debug_trace("Move ", stock, " age-group to next one up")
                stock__num[age_iter_ss] <- stock__num[age_younger_iter_ss]
                stock__wgt[age_iter_ss] <- stock__wgt[age_younger_iter_ss]
            }
        }))
    }, list(
        final_year_f = final_year_f,
        run_f = run_f,
        age_iter_ss = age_iter_ss,
        age_younger_iter_ss = age_younger_iter_ss))))

    return(as.list(out))
}
