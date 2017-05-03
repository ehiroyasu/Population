#' Extract a regression coefficient's P-value
#' 
#' Extracts the P-value for a specified coefficient of a regresssion. 
#' 
#' @param object A model object that supports \code{summary} and \code{coef}
#' @param i_coef Index of the coefficient for which the P-value is desired
#' @param one.sided Logical: should the P-value be a one-sided test? If so, negative coefficient estimates will generate P < 0.5
#' @param faster Logical: if true, use \code{\link{Pval_faster_lm}} to caluclate the P-value. Use this option only with an \code{object} of class \code{lm}
#' 
#' @return The P-value of the specified coefficient
Pval_coef <- function(object, i_coef = 2, one.sided = TRUE, faster = TRUE) {
  if (faster) {
    Pval <- Pval_faster_lm(object)[i_coef]
  } else {
    Pval <- summary(object)$coef[i_coef, 4]
  }
  if (one.sided) {
    Pval <- Pval/2
    if (coef(object)[i_coef] > 0) Pval <- 1 - Pval
  }
  return(Pval)
}

#' Fast calculation of coefficient P-values
#' 
#' Calculates the coefficient P-values from an \code{lm} object. Uses identical code to \code{\link{summary.lm}}, without all the other calculations the other performs. No error checking is performed; outcomes with any other type of regression object are not guaranteed!
#' 
#' @param object A model object of class \code{lm}
#' 
#' @return Vector of P-values, with same length as \code{coef(object)}
Pval_faster_lm <- function(object) {
  z <- object
  p <- z$rank
  rdf <- z$df.residual
  r <- z$residuals
  rss <- sum(r^2)
  resvar <- rss/rdf
  Qr <- qr(object)
  p1 <- 1L:p
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(R) * resvar)
  est <- z$coefficients[Qr$pivot[p1]]
  tval <- est/se
  pval <- 2 * pt(abs(tval), rdf, lower.tail = FALSE)
  return(pval)
}
