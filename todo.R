# 1. Read choice data from Train dataset

data(Train, package = "mlogit")
tibble::as_tibble(Train)

# 2. Read choice data from RiskyTransport dataset

data("RiskyTransport", package = "mlogit")
tibble::as_tibble(RiskyTransport)
