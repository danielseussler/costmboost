CostBoost = function(alpha = NULL) {
  stopifnot(alpha > 0 && alpha < 1)
  Family(
    ngradient = function(y, f, w = 1) alpha * (y > f) - (1 - alpha) * (y < f)
    , loss = function(y, f) alpha * abs(y - f) * (y > f) + (1 - alpha) * abs(y - f) * (y <= f)
    , check_y = function(y, alpha) {
      if (!is.numeric(y) || !is.null(dim(y))){
        stop("response is not a numeric vector but ",
             sQuote("family = CostBoost()"))
      }
      y
    }
    , weights = "case"
    , name = "Cost-sensitive boosting of absolute loss"
    , response = function(f) f
  )
}
