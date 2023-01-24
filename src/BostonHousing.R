library(mboost)

source(file = file.path("src", "costboost.R"))
data("BostonHousing2", package = "mlbench")

# remove uncorrected median house value
BostonHousing2 = subset(BostonHousing2, select = -medv)

fit = glmboost(
  cmedv ~ .
  , data = BostonHousing2
  , family = CostBoost(alpha = 0.4)
  , control = boost_control(mstop = 10000, nu = 0.01, trace = TRUE)
)

cv_risk = cvrisk(
  object = fit
  , folds = cv(weights = model.weights(fit), type = "kfold")
)

stop_here = mstop(cv_risk)

plot(cv_risk)
coef(object = fit[stop_here], off2int = TRUE)
