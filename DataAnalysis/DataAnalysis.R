# Pakages -----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)

# Pseudo code -------------------------------------------------------------



# SIR function ------------------------------------------------------------
# The function takes in rate for a specific disease, runs for a given number og days and outputs the status of the last day
SIR <- function(N, I0, days) {
  beta = 0.5
  gamma = 0.2
  S <- N - I0
  I <- I0
  
  #Looping over the given days
  for (day in 1:days) {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    
    #Updating number of susceptibles and infectious
    S <- S + dS
    I <- I + dI
  }
  
  # Return a data frame with results from the last day
  data.frame(Susceptible = S, Infectious = I, Prev = I/N)
}



# Load and clean data ---------------------------------------------------------------
movement.data <- readRDS("Data/movement_farm_2022.RDS")
nrow(movement.data)

# removing all slaugther entries
clean_movement.data <- subset(movement.data, RECEIVER_FARMTYPE != "15 68 Svineslagteri")

# Check amount of Herds involved in movements in 2022
length(unique(c(movement.data$FARMID_SENDER, movement.data$FARMID_RECEIVER)))

#N_sends = movement.data %>% group_by(FARMID_SENDER) %>% summarise(sends = n())

# Count number of pigs send by each farm type
N_send = movement.data %>%  group_by(SENDER_FARMTYPE) %>% count() %>% rename(Farm_type = SENDER_FARMTYPE, N_sends = n)

# Cont number of pigs received by each farm type
N_received = movement.data %>%  group_by(RECEIVER_FARMTYPE) %>% count() %>% rename(Farm_type = RECEIVER_FARMTYPE, N_received = n)


#Combine the counts of sends and received livestock
N_moved <- merge(N_send,N_received,by="Farm_type",all.x=TRUE, all.y = TRUE) 
N_moved <- N_moved %>% mutate(Total_moves = rowSums(replace(N_moved,is.na(N_moved),0)[-1]))



# Count number of pigs send and receive by each farm type for both datawith and without slaughter
N_send = movement.data %>%  count(SENDER_FARMTYPE) %>% rename(Farm_type = SENDER_FARMTYPE, sendings = n)
N_send_ns = clean.movement.data %>%  count(SENDER_FARMTYPE) %>% rename(Farm_type = SENDER_FARMTYPE, sendings_ns = n)
N_received = movement.data %>%  count(RECEIVER_FARMTYPE) %>% rename(Farm_type = RECEIVER_FARMTYPE, receivings = n)

#Combine the counts of sends and received livestock and add a summed col
N_moved <- merge(N_send, N_received, by="Farm_type",all.x=TRUE, all.y = TRUE) %>% mutate_all( ~replace_na(.,0))
N_moved <- N_moved  %>% mutate(Slaughter_sendings = c(N_send$sendings - N_send_ns$sendings_ns, 0),
                        Total_moves = rowSums(N_moved[-1]))



# Movements between each farm type 
#r = movement.data %>% select(RECEIVER_FARMTYPE, SENDER_FARMTYPE) %>% count(SENDER_FARMTYPE, RECEIVER_FARMTYPE) %>% group_by(RECEIVER_FARMTYPE)


ggplot(N_send, aes(x=Farm_type)) + 

ggplot(N_recieved, aes(x=recieve)) + 
  geom_density()


combined_moves <- bind_rows(N_send, N_received)

