library(deSolve)
library(ggplot2)

sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <-  beta * I * S - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}

parameters_values <- c(
  beta  = 0.000000000947130117, # infectious contact rate (/person/day)
  gamma = 0.0476    # recovery rate (/day)
)

initial_values <- c(
  S = 105582114,  # number of susceptibles at time = 0
  I =   22000,  # number of infectious at time = 0
  R =   21000   # number of recovered (and immune) at time = 0
)

time_values <- seq(0, 730)

ls()

sir_equations
parameters_values

initial_values

time_values

sir_values_1 <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)

sir_values_1

sir_values_1 <- as.data.frame(sir_values_1)
sir_values_1

options(scipen = 999)

with(sir_values_1, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "number of people")
  # adding the time series of infectious:
  lines(time, I, col = "red")
  # adding the time series of recovered:
  lines(time, R, col = "green")
})

# adding a legend:
legend("right", c("susceptibles", "infectious", "recovered"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")


locator(1)  # select one point and display its coordinates.
# put the cursor on the infection curve and the coordinates will display on the console.

