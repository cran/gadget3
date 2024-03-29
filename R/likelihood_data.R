parse_levels <- function (lvls, var_name) {
    m <- suppressWarnings(as.numeric(lvls))
    if (!anyNA(m)) return(data.frame(
        names = lvls,
        lower_incl = TRUE,
        lower_bound = m,
        upper_bound = c(tail(m, -1), Inf),  # NB: No data about final bound, assume open-ended
        upper_incl = FALSE,
        open_ended_upper = TRUE,
        stringsAsFactors = FALSE))

    m <- regmatches(lvls, regexec('^(\\[|\\()(.*),(.*)(\\]|\\))', lvls))
    if (all(vapply(m, length, numeric(1)) == 5)) return(data.frame(
        names = lvls,
        lower_incl = vapply(m, function (mm) identical(mm[[2]], '['), logical(1)),
        lower_bound = vapply(m, function (mm) as.numeric(mm[[3]]), numeric(1)),
        upper_bound = vapply(m, function (mm) as.numeric(mm[[4]]), numeric(1)),
        upper_incl = vapply(m, function (mm) identical(mm[[5]], ']'), logical(1)),
        open_ended_upper = is.infinite(as.numeric(tail(m, 1)[[1]][[4]])),
        stringsAsFactors = FALSE))

    m <- regmatches(lvls, regexec('^(.+):(.+)$', lvls))
    if (all(vapply(m, length, numeric(1)) == 3)) return(data.frame(
        names = lvls,
        lower_incl = TRUE,
        lower_bound = vapply(m, function (mm) as.numeric(mm[[2]]), numeric(1)),
        upper_bound = vapply(m, function (mm) as.numeric(mm[[3]]), numeric(1)),
        upper_incl = FALSE,
        open_ended_upper = is.infinite(as.numeric(tail(m, 1)[[1]][[3]])),
        stringsAsFactors = FALSE))

    stop("Unknown form of ", var_name, " levels, see ?cut for formatting: ", paste(lvls, collapse = ", "))
}

g3l_likelihood_data <- function (nll_name, data, missing_val = 0, area_group = NULL, model_history = "", all_stocks = list(), all_fleets = list()) {
    mfdb_min_bound <- function (x) { if (is.null(attr(x, 'min'))) x[[1]] else attr(x, 'min') }
    mfdb_max_bound <- function (x) { if (is.null(attr(x, 'max'))) tail(x, 1) else attr(x, 'max') }
    mfdb_eval <- function (x) { if (is.call(x)) eval(x) else x }

    # vector of col names, will cross them off as we go
    handled_columns <- structure(as.list(seq_along(names(data))), names = names(data))

    # Work out time dimension, but don't add it just yet
    if ('year' %in% names(data)) {
        # NB: Let g3s_time_convert() worry about if the step column is there or not
        #     Suppress warnings from tibbles that it might be missing
        data$time <- g3s_time_convert(data$year, suppressWarnings(data$step))
        handled_columns$year <- NULL
        handled_columns$step <- NULL
    } else if ('time' %in% names(data)) {  # Convert our time=1999-01 strings back
        data$time <- g3s_time_convert(data$time)
        handled_columns$time <- NULL
    } else {
        stop("Data must contain a year column")
    }

    # Turn incoming data into stocks with correct dimensions
    if ('length' %in% names(data)) {
        if (!is.null(attr(data, 'length', exact = TRUE))) {
            length_groups <- attr(data, 'length', exact = TRUE)

            # Make sure length groups are contiguous
            if (!isTRUE(all.equal(
                    unname(head(vapply(length_groups, mfdb_max_bound, numeric(1)), -1)),
                    unname(tail(vapply(length_groups, mfdb_min_bound, numeric(1)), -1))))) {
                stop("Gaps in length groups are not supported")
            }

            # Form length groups using lower bound from all groups
            length_vec <- vapply(length_groups, mfdb_min_bound, numeric(1))

            open_ended_upper <- isTRUE(attr(length_groups[[length(length_groups)]], 'max_open_ended'))
            if (!open_ended_upper) {
                # Not open ended, so final bound should be max of last item
                length_vec <- c(
                    length_vec,
                    mfdb_max_bound(length_groups[[length(length_groups)]]))
            }

            if (isTRUE(attr(length_groups[[1]], 'min_open_ended'))) {
                # Lower bound open-ended, so set first lengthgroup to start at 0
                length_vec[[1]] <- 0
            }

            # We want to use our own names, so remove MFDB's
            names(length_vec) <- NULL

            modelstock <- g3_stock(paste(nll_name, "model", sep = "_"), length_vec, open_ended = open_ended_upper)
            obsstock <- g3_stock(paste(nll_name, "obs", sep = "_"), length_vec, open_ended = open_ended_upper)

            # Convert data$length to use our naming
            data$length <- factor(data$length, levels = names(length_groups))
            levels(data$length) <- modelstock$dimnames$length
        } else {
            # Force length to be a factor if not already
            if (!is.factor(data$length)) {
                # Make sure levels are ordered according to cut strings
                lvls <- parse_levels(unique(data$length))
                lvls <- lvls[with(lvls, order(lower_bound, upper_bound)), 'names']
                data$length <- factor(data$length, levels = lvls)
            }

            lvls <- parse_levels(levels(data$length), "length")
            open_ended_upper <- lvls$open_ended_upper[[1]]

            length_vec <- if (open_ended_upper) lvls$lower_bound else c(lvls$lower_bound, tail(lvls$upper_bound, 1))

            if (any(!lvls$lower_incl) || any(lvls$upper_incl)) {
                stop("length intervals should be inclusive-lower, i.e. cut(..., right=FALSE): ", paste(lvls$names, collapse = ", "))
            }
            if (!isTRUE(all.equal(tail(lvls$lower_bound, -1), head(lvls$upper_bound, -1)))) {
                stop("Gaps in length groups are not supported: ", paste(lvls$names, collapse = ", "))
            }

            modelstock <- g3_stock(paste(nll_name, "model", sep = "_"), length_vec, open_ended = open_ended_upper)
            obsstock <- g3_stock(paste(nll_name, "obs", sep = "_"), length_vec, open_ended = open_ended_upper)

            # Convert data$length to use our naming
            levels(data$length) <- modelstock$dimnames$length
        }
        handled_columns$length <- NULL
    } else {
        # Stocks currently have to have a length vector, even if it only has one element
        modelstock <- g3_stock(paste(nll_name, "model", sep = "_"), c(0))
        obsstock <- g3_stock(paste(nll_name, "obs", sep = "_"), c(0))
        data$length <- modelstock$dimnames$length
    }

    # Add early time dimension for surveyindices
    if (identical(model_history, 'early')) {
        modelstock <- g3s_time(
            modelstock,
            sort(unique(data$time)))
        obsstock <- g3s_time(
            obsstock,
            sort(unique(data$time)))
        data$time <- g3s_time_labels(data$time)
    }

    if ('age' %in% names(data)) {
        if (!is.null(attr(data, 'age', exact = TRUE))) {
            age_groups <- attr(data, 'age', exact = TRUE)
            age_groups <- lapply(age_groups, mfdb_eval)  # Convert seq(2, 4) back to 2,3,4

            # We want to use our own names, so remove MFDB's
            modelstock <- g3s_agegroup(modelstock, unname(age_groups))
            obsstock <- g3s_agegroup(obsstock, unname(age_groups))

            # Convert data$age to use our naming
            data$age <- factor(data$age, levels = names(age_groups))
            levels(data$age) <- modelstock$dimnames$age
        } else if (is.numeric(data$age)) {
            # Numeric age columns don't need grouping
            age_groups <- seq(min(data$age), max(data$age))

            modelstock <- g3s_age(modelstock, min(data$age), max(data$age))
            obsstock <- g3s_age(obsstock, min(data$age), max(data$age))
            # Convert age data to use our naming
            data$age <- factor(
                data$age,
                levels = age_groups,
                labels = modelstock$dimnames$age)
        } else {
            if (!is.factor(data$age)) {
                # Make sure levels are ordered according to cut strings
                lvls <- parse_levels(unique(data$age))
                lvls <- lvls[with(lvls, order(lower_bound, upper_bound)), 'names']
                data$age <- factor(data$age, levels = lvls)
            }
            lvls <- parse_levels(levels(data$age), "age")

            if (is.infinite(tail(lvls$upper_bound, 1))) {
                # No support for infinite upper bound, bodge
                lvls$upper_bound[[length(lvls$upper_bound)]] <-
                    lvls$lower_bound[[length(lvls$lower_bound)]] +
                    1  # NB: It's not going to be upper-inclusive, so will subtract one at next step
            }

            # Account for lower_incl / upper_incl
            lvls$lower_bound <- ifelse(!lvls$lower_incl, lvls$lower_bound + 1, lvls$lower_bound)
            lvls$upper_bound <- ifelse(!lvls$upper_incl, lvls$upper_bound - 1, lvls$upper_bound)
            age_groups <- lapply(seq_len(nrow(lvls)), function (i) seq(lvls[i, "lower_bound"], lvls[i, "upper_bound"]))
            # NB: We never set the original names on age_groups

            modelstock <- g3s_agegroup(modelstock, age_groups)
            obsstock <- g3s_agegroup(obsstock, age_groups)
            levels(data$age) <- modelstock$dimnames$age
        }
        handled_columns$age <- NULL
    }

    if ('tag' %in% names(data)) {
        if (is.factor(data$tag)) {
            tag_ids <- structure(
                seq_along(levels(data$tag)),
                names = levels(data$tag))
        } else {
            tag_ids <- as.integer(unique(data$tag))
        }
        modelstock <- g3s_tag(modelstock, tag_ids, force_untagged = FALSE)
        obsstock <- g3s_tag(obsstock, tag_ids, force_untagged = FALSE)
        handled_columns$tag <- NULL
    }

    if ('stock' %in% names(data)) {
        if ('stock_re' %in% names(data)) stop("Don't support both stock and stock_re")
        stock_groups <- levels(as.factor(data$stock))
        stock_map <- structure(as.list(seq_along(stock_groups)), names = stock_groups)

        unknown_stocks <- setdiff(
            stock_groups,
            vapply(all_stocks, function (s) s$name, character(1)) )
        if (length(unknown_stocks) > 0) {
            stop("Unknown stock names in likelihood data: ", paste(unknown_stocks, collapse = ", "))
        }

        # NB: We have to replace stockidx_f later whenever we intersect over these
        modelstock <- g3s_manual(modelstock, 'stock', stock_groups, ~stockidx_f)
        obsstock <- g3s_manual(obsstock, 'stock', stock_groups, ~stockidx_f)
        handled_columns$stock <- NULL
    } else if ('stock_re' %in% names(data)) {
        # Start off with everything mapping to NULL
        stock_map <- structure(
            rep(list(NULL), length(all_stocks)),
            names = vapply(all_stocks, function (s) s$name, character(1)))

        # For each regex, find all matches and map to that index
        stock_regexes <- as.character(data$stock_re[!duplicated(data$stock_re)])
        for (i in rev(seq_along(stock_regexes))) {  # NB: Reverse so first ones have precedence
            stock_map[grep(stock_regexes[[i]], names(stock_map))] <- i
        }

        unused_regexes <- setdiff(
            seq_along(stock_regexes),
            unique(unlist(stock_map)) )
        if (length(unused_regexes) > 0) {
            stop("stock_re regexes matched no stocks in likelihood data: ", paste(stock_regexes[unused_regexes], collapse = ", "))
        }

        # NB: We have to replace stockidx_f later whenever we intersect over these
        modelstock <- g3s_manual(modelstock, 'stock_re', stock_regexes, ~stockidx_f)
        obsstock <- g3s_manual(obsstock, 'stock_re', stock_regexes, ~stockidx_f)
        handled_columns$stock_re <- NULL
    } else {
        stock_map <- NULL
    }

    if ('fleet' %in% names(data)) {
        if ('fleet_re' %in% names(data)) stop("Don't support both fleet and fleet_re")
        fleet_groups <- levels(as.factor(data$fleet))
        fleet_map <- structure(as.list(seq_along(fleet_groups)), names = fleet_groups)

        # NB: We have to replace fleetidx_f later whenever we intersect over these
        modelstock <- g3s_manual(modelstock, 'fleet', fleet_groups, ~fleetidx_f)
        obsstock <- g3s_manual(obsstock, 'fleet', fleet_groups, ~fleetidx_f)
        handled_columns$fleet <- NULL
    } else if ('fleet_re' %in% names(data)) {
        # Start off with everything mapping to NULL
        fleet_map <- structure(
            rep(list(NULL), length(all_fleets)),
            names = vapply(all_fleets, function (s) s$name, character(1)))

        # For each regex, find all matches and map to that index
        fleet_regexes <- as.character(data$fleet_re[!duplicated(data$fleet_re)])
        for (i in rev(seq_along(fleet_regexes))) {  # NB: Reverse so first ones have precedence
            fleet_map[grep(fleet_regexes[[i]], names(fleet_map))] <- i
        }

        # NB: We have to replace fleetidx_f later whenever we intersect over these
        modelstock <- g3s_manual(modelstock, 'fleet_re', fleet_regexes, ~fleetidx_f)
        obsstock <- g3s_manual(obsstock, 'fleet_re', fleet_regexes, ~fleetidx_f)
        handled_columns$fleet_re <- NULL
    } else {
        fleet_map <- NULL
    }

    # Add time dimension if it's supposed to be last
    if (!identical(model_history, 'early')) {
        # NB: We always add time to observations, whereas only when explicitly requested for model
        if (identical(model_history, 'late')) {
            modelstock <- g3s_time(
                modelstock,
                sort(unique(data$time)))
        }
        obsstock <- g3s_time(
            obsstock,
            sort(unique(data$time)))
        data$time <- g3s_time_labels(data$time)
    }

    # NB: area has to be last, so we can sum for the entire area/time
    if ('area' %in% names(data)) {
        # NB: Ignore MFDB attributes on purpose, we're interested in the aggregated areas here
        used_areas <- as.character(unique(data$area))
        if (is.null(area_group)) {
            # If no area grouping provided, assume area_group are integers already
            if (suppressWarnings(anyNA(as.integer(used_areas)))) {
                stop("Areas in data don't have integer names, but areas not provided")
            }
            area_group <- structure(
                as.integer(used_areas),
                names = used_areas)
        } else {
            # Filter area_group by what we actually need
            area_group <- area_group[names(area_group) %in% used_areas]
        }
        area_group <- area_group[order(names(area_group))]  # Dimension order should match data
        modelstock <- g3s_areagroup(modelstock, area_group)
        obsstock <- g3s_areagroup(obsstock, area_group)
        handled_columns$area <- NULL
    }

    # Generate full table based on stock
    full_table <- as.data.frame.table(
        g3_stock_instance(obsstock, 0),
        stringsAsFactors = TRUE)
    # Use freq column to preserve ordering of output
    full_table$Freq <- seq_len(nrow(full_table))
    full_table <- merge(full_table, data, all.x = TRUE)

    if ('number' %in% names(full_table)) {
        # TODO: More fancy NA-handling (i.e. random effects) goes here
        if (identical(missing_val, 'stop')) {
            if (any(is.na(full_table$number))) stop("Missing values in data")
        } else {
            # Fill in missing values with given value
            full_table$number[is.na(full_table$number)] <- missing_val
        }
        # TODO: Stock_instance instead?
        number_array <- array(full_table$number[order(full_table$Freq)],
            dim = obsstock$dim,
            dimnames = obsstock$dimnames)
        handled_columns$number <- NULL
    } else {
        number_array <- NULL
    }

    if ('weight' %in% names(full_table)) {
        # TODO: More fancy NA-handling (i.e. random effects) goes here
        if (identical(missing_val, 'stop')) {
            if (any(is.na(full_table$weight))) stop("Missing values in data")
        } else {
            # Fill in missing values with given value
            full_table$weight[is.na(full_table$weight)] <- missing_val
        }
        # TODO: Stock_instance instead?
        weight_array <- array(full_table$weight[order(full_table$Freq)],
            dim = obsstock$dim,
            dimnames = obsstock$dimnames)
        handled_columns$weight <- NULL
    } else {
        weight_array <- NULL
    }

    if (length(handled_columns) > 0) {
        stop("Unrecognised columns in likelihood data: ", paste(names(handled_columns), collapse = ", "))
    }
    
    return(list(
        modelstock = modelstock,
        obsstock = obsstock,
        done_aggregating_f = if ('step' %in% names(data)) ~TRUE else ~cur_step_final,
        stock_map = stock_map,
        fleet_map = fleet_map,
        number = number_array,
        weight = weight_array,
        nll_name = nll_name))
}

