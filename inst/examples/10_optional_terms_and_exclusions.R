library(actuarialfitdist)
set.seed(10)
claims <- data.frame(ground_up_loss = rlnorm(400, log(15000), .8))
fit <- fit_severity(claims, loss = "ground_up_loss", distribution = "lognormal")
print(summary(fit))

claims$deductible <- 0
claims$deductible[4] <- claims$ground_up_loss[4] + 100
cleaned <- suppressWarnings(fit_severity(claims, "ground_up_loss",
  deductible = "deductible", distribution = "lognormal"))
print(cleaned$excluded_rows)
print(cleaned$excluded_details)
