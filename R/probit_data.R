#' Define probit data
#'
#' @description
#' These functions construct and validate an object of class
#' \code{\link{probit_data}}, which defines the probit model data.
#'
#' @param data
#' TBA

probit_data <- function(
  data, column_choice = "choice", column_decider = "id",
  column_occasion = "idc", ranked = FALSE, ordered = FALSE
  ) {

}

# #' Simulate choice data
# #'
# #' @description
# #' This function simulates choice data from a probit model. It helps to create
# #' a \code{\link{probit_data}} object.
# #'
# #' @param probit_covariates
# #' An \code{\link{probit_covariates}} object, which contains the covariate
# #' matrices used for the choice data simulation.
# #' @param true_parameter
# #' An \code{\link{probit_parameter}} object, which contains the model
# #' parameters used for the choice data simulation.
# #' By default, \code{probit_parameter = probit_parameter()}, i.e. default
# #' parameters are used.
# #' @param ranked
# #' TODO
# #'
# #' @return
# #' An \code{\link{probit_data}} object.
# #'
# #' @inheritSection probit_formula Model formula
# #' @inheritSection probit_formula Random effects
# #'
# #' @examples
# #' ### simulate data from a binary probit model with two latent classes
# #' data <- simulate_probit_data(
# #'   formula = choice ~ cost | income | time, N = 10, J = 2, T = 1:10,
# #'   alternatives = c("car", "bus"), re = c("cost", "time"),
# #'   true_parameter = probit_parameter(C = 2)
# #' )
# #'
# #' ### simulate data from an ordered probit model
# #' data <- simulate_probit_data(
# #'   formula = opinion ~ age + gender, N = 50, J = 5,
# #'   alternatives = c("very bad", "bad", "indifferent", "good", "very good"),
# #'   ordered = TRUE
# #' )
# #'
# #' ### simulate data from a ranked probit model
# #' data <- simulate_probit_data(
# #'   formula = product ~ price, N = 10, J = 3, T = 1:10, ranked = TRUE
# #' )
# #'
# #' @export
# #'
# #' @seealso
# #' \itemize{
# #'   \item TODO
# #' }
#
# simulate_probit_data <- function(
#     probit_covariates = simulate_probit_covariates(
#       formula, N, J, T = 1, alternatives = LETTERS[1:J], re = NULL
#     ), true_parameter = probit_parameter(), ranked = FALSE, seed = NULL,
#     column_choice = "choice", column_decider = "id", column_occasion = "idc"
# ) {
#
#   ### construct objects
#   T <- expand_T(N = N, T = T)
#   probit_formula <- probit_formula(
#     formula = formula, re = re, ordered = ordered
#   )
#   probit_parameter <- simulate_probit_parameter(
#     x = true_parameter, formula = formula, re = re, ordered = ordered,
#     J = J, N = N, seed = seed
#   )
#   probit_alternatives <- probit_alternatives(
#     J = J, alternatives = alternatives, ordered = ordered
#   )
#   effects <- overview_effects(
#     probit_formula = probit_formula,
#     probit_alternatives = probit_alternatives
#   )
#
#
#   # TODO check 'ranked', maybe 'probit_choice_set()'?
#
#
#   if (!is.null(seed)) {
#     set.seed(seed)
#   }
#   data_list <- lapply(1:N, function(n) {
#     z_n <- probit_parameter$z[n]
#     coef <- c(probit_parameter$alpha[,z_n], probit_parameter$beta[,z_n])
#     out <- lapply(1:T[n], function(t) {
#       X_nt <- cov_vec_to_mat(
#         cov_vec = covariates(n, t),
#         probit_effects = probit_effects
#       )
#       U_nt <- rmvnorm_cpp(
#         mean = as.vector(X_nt %*% coef),
#         Sigma = probit_parameter$Sigma
#       )
#       y_nt <- which.max(U_nt)
#       list(X = X_nt, y = y_nt)
#     })
#     list(X = lapply(out, `[[`, "X"), y = lapply(out, `[[`, "y"))
#   })
#   data <- structure(
#     cbind(
#       data.frame(
#         id = rep(1:N, times = T),
#         idc = unlist(sapply(T, seq.int, simplify = FALSE)),
#         choice = probit_alternatives$alternatives[unlist(lapply(data_list, `[[`, "y"))]
#       ),
#       data.frame(
#         matrix(unlist(lapply(data_list, `[[`, "X")), ncol = P*J, byrow = TRUE)
#       )
#     )
#   )
#
#
#   # TODO
#
#   probit_data(
#     data = data,
#     ordered = ordered,
#     ranked = ranked,
#     simulated = TRUE,
#     true_parameter = probit_parameter
#   )
#
#
#
# }
#
# read_probit_data <- function() {
#
# }
#
#
# as.list.probit_data <- function() {
#
# }
#
# as.data.frame.probit_data <- function() {
#
# }



