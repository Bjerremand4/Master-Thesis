# -------------------------------------------------------------------------
#          Reduction of movements, same total number of animals 
# -------------------------------------------------------------------------

# Number of rows to merge (2 ≈ 50% reduction in data): 
n_merge = 100/move_freq   # Move_freq set in the interface

# Allocate storage for the reduced movements start by adding all movements to slaughter
sub.move.data = move.data %>% filter(RECEIVER_FARMTYPE == "15 68 Svineslagteri")
# exclude movements to slaughter from data
move.data = move.data %>% filter(RECEIVER_FARMTYPE != "15 68 Svineslagteri")


start_reduce = Sys.time() # Track time

for (id in sendIDs){
  # Extract all moves comming from current herd
  sub.moves = move.data %>% filter(FARMID_SENDER == id)
  # Unique ids of receiving herds from current herd
  recieve_herds = unique(sub.moves$FARMID_RECEIVER)
  
  for (herd in recieve_herds){
    # all moves from the receiving herd, from the current sending herd 
    ss = sub.moves %>% filter(FARMID_RECEIVER == herd)
    # Now indexing the mergind rows
    ss = ss %>% mutate(index = rep(1:(ifelse(nrow(ss) %% n_merge == 0, nrow(ss), nrow(ss)+1) / n_merge), each = n_merge)[1:nrow(ss)])
    # Summarize by keeping the DATE_MOVEMENT from the first row and summing N_MOVED_PIGS
    merged_moves <- ss %>% group_by(index) %>% summarize(DATE_MOVEMENT = first(DATE_MOVEMENT),
                                                      N_MOVED_PIGS = sum(N_MOVED_PIGS),
                                                      FARMID_SENDER = first(FARMID_SENDER),
                                                      SENDER_FARMTYPE = first(SENDER_FARMTYPE),
                                                      SENDER_N_PIGS = first(SENDER_N_PIGS),
                                                      FARMID_RECEIVER = first(FARMID_RECEIVER),
                                                      RECEIVER_FARMTYPE = first(RECEIVER_FARMTYPE),
                                                      RECEIVER_N_PIGS = first(RECEIVER_N_PIGS),
                                                      Year = first(Year)) %>% ungroup()
    # Insert in the merged rows in the new dataframe (without index column)
    #sub.move.data.slaughter = rbind(sub.move.data.slaughter, merged_moves[,-1])
    sub.move.data = rbind(sub.move.data, merged_moves[,-1])
    
  }
  print(paste0("ReduceMovements, iteration: ", which(sendIDs == id), "/", length(sendIDs)))
}
end_reduce = Sys.time() # Track time

# Arrange data by date before saving
sub.move.data = sub.move.data %>% arrange(DATE_MOVEMENT)

# Save the reduced data 
saveRDS(sub.move.data, file.path("../Data/Reduced_moves", paste0("sub.move.data_", move_freq,"_2%.rds")))

# RecudeMovement Runtime
Time_reduceMovements = end_reduce - start_reduce
