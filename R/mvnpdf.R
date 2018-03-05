# version perso
# mvnpdf <- function(x, mean, varcovM, Log=TRUE){
#
#   if (class(try(solve(varcovM), silent = TRUE)) == "try-error"){
#     stop("varcovM doit etre definie positive")
#   }
#
#   if (class(x) == "numeric"){
#     x = matrix(x, ncol=1)
#   }
#
#   p <- dim(x)[1]
#   y <- apply(x, 2, function(x){return((2*pi)^(-p/2)*det(varcovM)^(0.5)*exp(-0.5*t(x-mean) %*% solve(varcovM) %*% (x-mean)))})
#
#   if (Log==TRUE){
#     y <- log(y)
#   }
#
#   return(list("x" = x, "y" = y))
# }

#' Multivariate Normal Probability Density Function
#'
#' Compute multivariate gaussian probability distribution function
#'
#' Details
#'
#' @param x matrice des antecedents : obs en colonnes
#' @param mean vecteur esperance
#' @param varcovM matrice covariance
#' @param Log TRUE pour renvoyer log y
#'
#' @return renvoie x et y ou log y si Log TRUE
#' @export
#'
#' @examples
mvnpdf <- function(x, mean =  rep(0, nrow(x)),
                   varcovM = diag(nrow(x)), Log = TRUE) {
  n <- ncol(x)
  p <- nrow(x)
  x0 <- x - mean
  Rinv <- solve(varcovM)
  LogDetvarcovM <- log(det(varcovM))

  y <- NULL
  for (j in 1:n) {
    yj <- - p/2 * log(2*pi) - 0.5 * LogDetvarcovM -
      0.5 * t(x0[, j]) %*% Rinv %*% x0[, j]
    y <- c(y, yj)
  }
  # test
  if (!Log) {
    y <- exp(y)
  }

  return(list(x=x,y=y))
}

