# Function to reproduce eq. 10 of Pesaran & Shin (1998)
#' @param k horizon of the impulse response
#' @param j structural innovation that emits the shock
#' @param MA_coef Array with all the VARMA coefficients
#' @param Estimated residual variance-covariance matrix
#' @param e_vec selection vector for the innovation j
#' @export generalized impulse response

G_irf_helper_fctn <- function(k, j, MA_coef, Resid_sigma, e_vec) {
  IRF_value <- diag(Resid_sigma)[j]^(-0.5) * MA_coef[, , k] %*% Resid_sigma %*% e_vec
  return(IRF_value)
}


# Function that calculates generalized impulse responses in the spirit of Pesaran & Shin (1998)
# Adapted from https://www.clintonwatkins.com/post/2021-generalised-impulse-response-function-r/
#' @param VAR_model output from the vars::VAR function
#' @param n.ahed max horizon for the impulse responses. Capped at 30
#' @export array with the generalized impulse responses

G_irf_fctn <- function(VAR_model, n.ahead = 20) {
  var_names <- colnames(VAR_model$y)
  n_vars <- VAR_model$K
  if (n.ahead > 30) {
    n.ahead <- 30
    warning("n.ahead is being capped at 30 leads")
  }
  # Compute first n.ahead VARMA coefficients of the infinite MA representation
  MA_coef <- Phi(VAR_model, n.ahead)
  # Compute the variance-covariance matrix of the residuals, adjusted for the correct df
  n_coefs <- ncol(VAR_model$datamat[, -c(1:n_vars)])
  Resid_sigma <- crossprod(resid(VAR_model)) / (VAR_model$obs - n_coefs)
  # Calculate orthogonalized contemporaneous residual covariances. Eq. 5 in Pesaran & Shin (1998)
  P <- t(chol(Resid_sigma))

  # Initialize the array that holds the generalized impulse responses (rows: horizon, columns: response variable, depth: impulse variable)
  IRF_g <- array(data = 0, dim = c(n.ahead, n_vars, n_vars), dimnames = list(NULL, var_names, var_names))
  # Compute the generalized impulse responses
  for (j in 1:n_vars) {
    # selection vector e. See eg. eq. 7. j refers to the jth structural innovation --> column j in the
    # impulse response coefficient matrix
    e_vec <- matrix(0, n_vars, 1)
    e_vec[j, 1] <- 1
    IRF_g[, , j] <- sapply(1:n.ahead, G_irf_helper_fctn, j, MA_coef, Resid_sigma, e_vec) %>%
      t()
    colnames(IRF_g[, , j]) <- var_names
  }
  # Transform the array to a list so that the output complies with vars bootstrap function
  IRF_g_list <- list()
  for (i in 1:n_vars) {
    IRF_g_list[[i]] <- matrix(IRF_g[1:(n.ahead), var_names, var_names[i]], nrow = n.ahead)
    colnames(IRF_g_list[[i]]) <- var_names
  }
  names(IRF_g_list) <- var_names
  return(IRF_g_list)
}


# Function that calculates bootstrapped confidence intervals for the generalized impulse responses
# Adapted vars:::.boot function
#' @param x output from the vars::VAR function
#' @param n.ahed max horizon for the impulse responses. Capped at 30
#' @param Interval confidence interval
#' @param seed value
#' @export array with the generalized impulse responses

G_irf_boot_fctn <- function(x, n.ahead = 20, runs = 100, Interval = .95, seed = 1) {
  set.seed(as.integer(seed))
  if (class(x) == "varest") {
    VAR <- eval.parent(x)
  } else if (class(x) == "svarest") {
    VAR <- eval.parent(x$var)
  } else {
    stop("Bootstrap not implemented for this class.\n")
  }
  impulse <- colnames(x$y)
  response <- colnames(x$y)
  p <- VAR$p
  K <- VAR$K
  obs <- VAR$obs
  total <- VAR$totobs
  type <- VAR$type
  B <- Bcoef(VAR)
  BOOT <- vector("list", runs)
  ysampled <- matrix(0, nrow = total, ncol = K)
  colnames(ysampled) <- colnames(VAR$y)
  Zdet <- NULL
  if (ncol(VAR$datamat) > (K * (p + 1))) {
    Zdet <- as.matrix(VAR$datamat[, (K * (p + 1) + 1):ncol(VAR$datamat)])
  }
  resorig <- scale(resid(VAR), scale = FALSE)
  B <- Bcoef(VAR)
  for (i in 1:runs) {
    booted <- sample(c(1:obs), replace = TRUE)
    resid <- resorig[booted, ]
    lasty <- c(t(VAR$y[p:1, ]))
    ysampled[c(1:p), ] <- VAR$y[c(1:p), ]
    for (j in 1:obs) {
      lasty <- lasty[1:(K * p)]
      Z <- c(lasty, Zdet[j, ])
      ysampled[j + p, ] <- B %*% Z + resid[j, ]
      lasty <- c(ysampled[j + p, ], lasty)
    }
    varboot <- update(VAR, y = ysampled)
    if (class(x) == "svarest") {
      varboot <- update(x, x = varboot)
    }
    BOOT[[i]] <- G_irf_fctn(VAR_model = varboot, n.ahead = n.ahead)
  }
  lower <- (1 - Interval) / 2
  upper <- 1 - (1 - Interval) / 2
  # mat.l <- matrix(NA, nrow = n.ahead + 1, ncol = length(response))
  # mat.u <- matrix(NA, nrow = n.ahead + 1, ncol = length(response))
  mat.l <- matrix(NA, nrow = n.ahead, ncol = length(response))
  mat.u <- matrix(NA, nrow = n.ahead, ncol = length(response))
  Lower <- list()
  Upper <- list()
  idx1 <- length(impulse)
  idx2 <- length(response)
  idx3 <- n.ahead #+ 1
  temp <- rep(NA, runs)
  for (j in 1:idx1) {
    for (m in 1:idx2) {
      for (l in 1:idx3) {
        for (i in 1:runs) {
          if (idx2 > 1) {
            temp[i] <- BOOT[[i]][[j]][l, m]
          } else {
            temp[i] <- matrix(BOOT[[i]][[j]])[l, m]
          }
        }
        mat.l[l, m] <- quantile(temp, lower, na.rm = TRUE)
        mat.u[l, m] <- quantile(temp, upper, na.rm = TRUE)
      }
    }
    colnames(mat.l) <- response
    colnames(mat.u) <- response
    Lower[[j]] <- mat.l
    Upper[[j]] <- mat.u
  }
  names(Lower) <- impulse
  names(Upper) <- impulse
  G_irf <- G_irf_fctn(x, n.ahead = n.ahead)
  result <- list(Coefficients = G_irf, Lower = Lower, Upper = Upper)
  return(result)
}
