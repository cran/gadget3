## ----message=FALSE, echo=FALSE------------------------------------------------
library(gadget3)
library(dplyr)
set.seed(123)

## ----message=FALSE, echo=FALSE------------------------------------------------
library(dplyr)
area_names <- g3_areas(c('IXa', 'IXb'))
fish <- g3_stock("fish", seq(5L, 25L, 5)) |>
  g3s_livesonareas(area_names["IXa"]) |>
  g3s_age(1L, 5L)

## -----------------------------------------------------------------------------
# TODO: This isn't a brilliantly-named example, something else?
# Generate random data
expand.grid(year = 1990:1994, step = 3, area = 'IXa') |>
    # Fill in a number column with total numbers in that year/step/area combination
    mutate(number = runif(n(), min = 10000, max = 100000)) ->
    dist_si_acoustic

actions_likelihood_si_acoustic <- list(

  g3l_abundancedistribution(
    "dist_si_acoustic",
    obs_data = dist_si_acoustic,
    
    stocks = list(fish),
    function_f = g3l_distribution_surveyindices_log(alpha = NULL, beta = 1),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  NULL)

## -----------------------------------------------------------------------------
g3_distribution_preview(read.table(header = TRUE, text="
year  number
1999    1000
2000    1002
2002    1004
2003    1008
"))

## -----------------------------------------------------------------------------
g3_distribution_preview(read.table(header = TRUE, text="
year step number
1999    2   1020
2000    2   2040
2001    3   1902
"))

## -----------------------------------------------------------------------------
g3_distribution_preview(read.table(header = TRUE, text="
year  length number
1999  [0,10)  1023
1999 [10,20)  2938
1999 [20,30)  3948
1999 [30,40)  3855
2000  [0,10)  1023
2000 [10,20)  2938
# NB: No [10,30)
2000 [30,40)  3855
"))

## -----------------------------------------------------------------------------
# Generate an unaggregated length distribution
ldist.lln.raw <- data.frame(
    year = c(1999, 2000),
    length = sample(10:75, 100, replace = TRUE),
    number = 1,
    stringsAsFactors = FALSE)

# Group length into 10-long bins
ldist.lln.raw |> dplyr::group_by(
  year = year,
  length = cut(length, breaks = seq(10, 100, by = 10), right = FALSE)
) |> dplyr::summarise(number = sum(number), .groups = 'keep') -> ldist.lln.agg
ldist.lln.agg

# NB: The last 2 bins are empty, but because cut() creates a factor column,
#     gadget3 knows about them even though they don't appear in the data.
g3_distribution_preview(ldist.lln.agg)

## ----eval=nzchar(Sys.getenv('G3_TEST_TMB'))-----------------------------------
#  # Import data into a temporary database
#  library(mfdb)
#  mdb <- mfdb(tempfile(fileext=".duckdb"))
#  ldist.lln.raw$month <- 1
#  ldist.lln.raw$areacell <- 'all'  # NB: We have to have an areacell mapping for MFDB
#  mfdb_import_area(mdb, data.frame(name = c('all'), size = c(5)))
#  mfdb_import_survey(mdb, ldist.lln.raw)
#  
#  # Use mfdb_sample_count to extract & group in the same manner as above
#  ldist.lln.agg <- mfdb_sample_count(mdb, c('length'), list(
#      year=1999:2000,
#      length = mfdb_interval("len", seq(10, 100, by = 10)) ))[[1]]
#  g3_distribution_preview(ldist.lln.agg, area_group = c(all=1))
#  
#  mfdb_disconnect(mdb)

## -----------------------------------------------------------------------------
g3_distribution_preview(read.table(header = TRUE, text="
year  age  length number
1999    1  [0,10)  1026
1999    1 [10,20)  2936
1999    1 [20,30)  3962
1999    1 [30,40)  3863
1999    2  [0,10)  1026
1999    2 [10,20)  2936
1999    2 [20,30)  3962
1999    2 [30,40)  3863
"))

## -----------------------------------------------------------------------------
g3_distribution_preview(read.table(header = TRUE, text="
year  age    length number
1999  [1,1]    [0,10)  1026
1999  [1,1]   [10,20)  2936
1999  [1,1]   [20,30)  3962
1999  [1,1]   [30,40)  3863
1999  [2,4]  [0,10)  1011
# Missing [2,4] [10,20)
1999  [2,4] [20,30)  3946
1999  [2,4] [30,40)  3872
"))

## -----------------------------------------------------------------------------
area_names <- g3_areas(c('IXa', 'IXb', 'IXc'))

g3_distribution_preview(read.table(header = TRUE, text="
year    area    number
1999   IXa   1000
1999   IXb   4305
2000   IXa   7034
2000   IXb   2381
2001   IXb   3913
"), area_group = area_names)[length = '0:Inf',,]

## -----------------------------------------------------------------------------
st_imm <- g3_stock(c(species = 'fish', 'imm'), 1:10)
st_mat <- g3_stock(c(species = 'fish', 'mat'), 1:10)

g3_distribution_preview(read.table(header = TRUE, text="
year    stock    number
1999   fish_imm   1000
1999   fish_mat   4305
2000   fish_imm   7034
2000   fish_mat   2381
2001   fish_mat   3913
"), stocks = list(st_imm, st_mat))[length = '0:Inf',,]

## -----------------------------------------------------------------------------
f_comm <- g3_fleet('comm')
f_surv <- g3_fleet('surv')

g3_distribution_preview(read.table(header = TRUE, text="
year    fleet    number
1999   f_comm   1000
1999   f_surv   4305
2000   f_comm   7034
2000   f_surv   2381
2001   f_surv   3913
"), fleets = list(f_comm, f_surv))[length = '0:Inf',,]

