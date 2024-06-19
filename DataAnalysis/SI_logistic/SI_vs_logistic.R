# Load packages
source(file.path("../Model/Initialize/Load_packagesNfunctions.R"))

# SI model using desolve. (USED)
SI_model = function (current_timepoint, state_values, parameters)
{
  # create state variables (local variables)
  S = state_values [1]        # susceptibles
  I = state_values [2]        # infectious
  
  with (
    as.list (parameters),     # variable names within parameters can be used
    {
      # compute derivatives
      dS = (-beta * S * I)
      dI = ( beta * S * I)
      
      # combine results
      results = c (dS, dI)
      list (results)
    }
  )
}


#Logistic method (USED)
Logistic <- function(N, I0, days, beta){
  I_log1 = numeric(days+1)
  I_log1[1] = I0
  C = I0/(N-I0)
  
  for (i in 1:days){
    I_log1[i+1] = N *C*(exp(beta*i)) / (1 + C*(exp(beta*i)))
  }
  return(I_log1)
}


# initialize values
N = 1000
I0 = 1
days = 365 
t = 0:days

# Run funcions
# Bacterial beta
I1 = lsoda(c(S = (N-I0)/N, I = I0/N), t, SI_model, c(beta = 0.05))[,3]*N
I2 = Logistic(N = N, I0 = I0, days = days, beta = 0.05)
# Viral beta
I3 = lsoda(c(S = (N-I0)/N, I = I0/N), t, SI_model, c(beta = 0.2))[,3]*N
I4 = Logistic(N = N, I0 = I0, days = days, beta = 0.2)



# Plotting the difference of the 3 methods
plot.data = tibble(SI_b = I1, SI_v = I3, Logistic_b = I2, Logistic_v = I4, Days = t)
ggplot(data = plot.data, aes(x = Days)) + 
  geom_line(aes(y = SI_b, color = paste("SI model, beta =", 0.05))) +
  geom_line(aes(y = Logistic_b, color = paste("Logistic model, beta =", 0.05)), lty = 2) + 
  geom_line(aes(y = SI_v, color = paste("SI model, beta =", 0.2))) +
  geom_line(aes(y = Logistic_v, color = paste("Logistic model, beta =", 0.2)), lty = 2)+
  labs(title = paste0("Infected over time by different methods"),
       y = "Infected individuals [#]",
       x = "time [days]",
       color = "Method") +
  scale_color_manual(values = c("steelblue4","darkolivegreen", "steelblue1","yellowgreen"),
                     labels = c(expression(paste("Logistic model, ", beta, " = 0.05")),expression(paste("Logistic model, ", beta, " = 0.2")),
                                expression(paste("SI model, ", beta, " = 0.05")),expression(paste("SI model, ", beta, " = 0.2"))))



# Plotting the difference between using Logistic growth and SI models
diff_data <- bind_rows(tibble(Days = t, diff_models = I1-I2, beta = "Bacterial beta"),
                       tibble(Days = t, diff_models = I3-I4, beta = "Viral beta"))

ggplot(data = diff_data, aes(x = Days, y = diff_models, color = beta)) +
  geom_line() +
  labs(title = "Infected over time by different methods",
       y = "Difference in infected",
       x = "time [days]",
       color = "") +
  scale_color_manual(values = c("steelblue4", "darkolivegreen"),
                     labels = c(expression(paste(beta, " = 0.05")), expression(paste(beta, " = 0.2")))) +
  facet_wrap(~ beta, nrow = 2, scales = "free_y")

# Find max diff
max(I1-I2)
max(I3-I4)

