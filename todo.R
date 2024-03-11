# 1. Read choice data from Train dataset

data(Train, package = "mlogit")
tibble::as_tibble(Train)

# 2. Read choice data from RiskyTransport dataset

data("RiskyTransport", package = "mlogit")
tibble::as_tibble(RiskyTransport)



# 3. Read choice data from logitr::cars_us

library("logitr")

logitr::cars_us

choice_data <- choice_data(
  data = logitr::cars_us,
  format = "long",
  column_choice = "choice",
  column_alternative = NA,
  column_decider = "id",
  column_occasion = "obsnum",
  column_covariates_alternative_constant = character(),
  column_covariates_alternative_varying = c(
    "hev", "phev10", "phev20", "phev40", "bev75", "bev100", "bev150",
    "phevFastcharge", "bevFastcharge", "price", "opCost", "accelTime",
    "american", "japanese", "chinese", "skorean"
  ),
  alternatives = c("A", "B", "C"),
  ordered = FALSE,
  ranked = FALSE,
  delimiter = "_"
)

as.data.frame(choice_data, format = "wide")



