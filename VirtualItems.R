############  Virtual items ##############


## Using additional age, prefeered foot, height as virtual items in the association rules mining, 

### AGE
playersA = players %>% group_by(player_api_id) %>% summarise(A = max(birthday))
playersA = playersA %>% mutate(
  age = as.numeric(
    ( Sys.time() - parse_date_time(
      str_sub(playersA$A, 1, 10), "ymd"
    ))/365),
  season = "2001/2002"
  )

playersA$team_long_name = paste0("Virt_Age_class_",as.integer(cut( playersA$age, quantile(playersA$age))))

### height
playersH = players %>% group_by(player_api_id) %>% summarise(H = max(height))
playersH$team_long_name = paste0("Virt_Height_class_", as.integer(cut( playersH$H, quantile(playersH$H))))
playersH$season = "2002/2003"

### prefered foot
playersF = players_stats %>% group_by(player_api_id) %>% summarise(team_long_name = max(preferred_foot))
playersF$team_long_name = paste0("Virt_foot_",playersF$team_long_name)
playersF$season = "2003/2004"

## stack data sets
VirtualItems = bind_rows(
  playersA,
  playersH,
  playersF,
  playerClubSequence %>% select( c(player_api_id, season, team_long_name))
) %>% arrange(player_api_id, season) %>% select(player_api_id, season, team_long_name)
  

VirtualItems$seqnr = ave( VirtualItems$player_api_id, VirtualItems$player_api_id, FUN = seq_along)
VirtualItems$size = 1

#### write data to transaction file for use in cspade ####
write_delim( 
  VirtualItems %>% select( c(player_api_id, seqnr, size, team_long_name)) ,
  delim ="\t", path = "Virtualplayer_transactions.txt", col_names = FALSE
)


Virtualplayerstrxs <- read_baskets("Virtualplayer_transactions.txt", sep = "[ \t]+",info =  c("sequenceID","eventID","size"))
summary(Virtualplayerstrxs)

### perform sequence mining, for now only length two sequences
VirtualplayersClubSeq <- cspade(
  Virtualplayerstrxs, 
  parameter = list(support = 0.00007, maxlen=5), 
  control   = list(verbose = TRUE)
)

summary(VirtualplayersClubSeq)


VirtualseqResult = as(VirtualplayersClubSeq, "data.frame")
VirtualseqResult = VirtualseqResult %>%
  mutate(
    sequence = as.character(sequence),
    N = str_count(sequence,"\\,") +1,
    Ntransfers = support*11060,
    support_perc = paste(sprintf("%.4f", 100*support), "%")
    
  )

Virtuals = filter( VirtualseqResult, N ==5, str_detect(sequence,"Virt")) %>% arrange(desc(support))



#### create a nice data table that can be published to rpubs ####

DT::datatable(
  Virtuals[1:3000,c(1,4,5)], 
  rownames = FALSE,
  filter = 'top',
  options = list(
    pageLength=25)
)

