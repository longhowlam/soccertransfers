library(RSQLite)
library(dplyr)
library(tidyr)
library(arules)
library(arulesSequences)
library(readr)
library(stringr)
library(visNetwork)
library(igraph)
library(lubridate)
library(DT)

#### Get the soccer data from the SQLLITE file ####
## soccer data file is downloaded from kaggle datasets

con = dbConnect(RSQLite::SQLite(), dbname="D:/DataSetjes/database.sqlite")

## get a list of all tables
alltables = dbListTables(con)

# extract tables
players       = dbReadTable(con, "Player")
players_stats = dbReadTable(con, "Player_Stats")
teams         = dbReadTable(con, "Team")
league        = dbReadTable(con, "League")
Matches       = dbReadTable(con, "Match")

teams$team_long_name = str_replace_all(teams$team_long_name, "\\s", "_")
teams$team_long_name = str_replace_all(teams$team_long_name, "\\.", "_")
teams$team_long_name = str_replace_all(teams$team_long_name, "-", "_")

##### helper dataset for team and country
CountryClub = Matches %>% 
  group_by(home_team_api_id,country_id) %>% 
  summarise(n=n()) %>% 
  left_join(league) %>%
  left_join(teams, by=c("home_team_api_id" = "team_api_id"))


###### prepare data fro associations rule mining / sequence mining ########

## playersids are in separate columns, I need them stacked in one column

tmp = Matches %>% 
  select(
    season, 
    home_team_api_id, 
    home_player_1:home_player_11
  )%>%
  gather(
    player, 
    player_api_id, 
    -c(season, home_team_api_id)
  ) %>%
  group_by(player_api_id, home_team_api_id ) %>% 
  summarise(season = min(season))

### join with player and club info

playerClubSequence = left_join(
  tmp,
  players
  ) %>% 
  left_join(
    teams, 
    by=c("home_team_api_id"="team_api_id")
  )

playerClubSequence = playerClubSequence %>% 
  filter(
    !is.na(player_name), !is.na(team_short_name)
  )  %>%
  arrange(
    player_api_id, 
    season
  )


### add a sequence number per player
playerClubSequence$seqnr = ave( playerClubSequence$player_api_id, playerClubSequence$player_api_id, FUN = seq_along)
playerClubSequence$size = 1


##### sequence mining with cSPade algorithm in arulesSequences ####

### write data set in a trx file so that use can easily use
### the read_basket function in arulesSequence to create a transaction object

write_delim( 
  playerClubSequence %>% select( c(player_api_id, seqnr, size, team_long_name)) ,
  delim ="\t", path = "player_transactions.txt", col_names = FALSE
  )


### import as transaction baskets

playerstrxs <- read_baskets("player_transactions.txt", sep = "[ \t]+",info =  c("sequenceID","eventID","size"))
summary(playerstrxs)

### perform sequence mining, for now only length two sequences
playersClubSeq <- cspade(
  playerstrxs, 
  parameter = list(support = 0.00010, maxlen=2), 
  control   = list(verbose = TRUE)
)

summary(playersClubSeq)

### do some wrangling to put cspade results in a nice data set
### that is suitable for visNetwork. visNetwork needs two data sets
### a data set with the from and to edges and a data set with the unique nodes

seqResult = as(playersClubSeq, "data.frame")
seqResult = seqResult %>% 
  mutate(
    sequence = as.character(sequence)
  )

seqResult = bind_cols(
  seqResult,
  as.data.frame(
    str_split_fixed(seqResult$sequence, pattern =",", 2), 
    stringsAsFactors = FALSE)
  )

seqResult$from = str_extract_all(seqResult$V1,"\\w+", simplify = TRUE)[,1] 
seqResult$to   = str_extract_all(seqResult$V2,"\\w+",simplify = TRUE)[,1]


seqResult$width = exp(3000*seqResult$support)
seqResult = seqResult %>% filter(V2 !="")
seqResult$title = paste(seqResult$sequence, "<br>", round(100*seqResult$support,2), "%")

seqResult$support_perc = paste(sprintf("%.4f", 100*seqResult$support), "%")

### create a data frame with the nodes
nodes = unique(c(seqResult$from, seqResult$to))
nodesData = data.frame(id = nodes, title = nodes, label = nodes, stringsAsFactors = FALSE) %>%
  left_join(CountryClub, by = c("id"="team_long_name")) %>% 
  rename(group = name)


#### calculate betweeness centrality measures ####
# using igraph, so that we can have different sizes of
# the nodes in the network graph 

transferGraph = graph_from_data_frame(seqResult[,c(5,6)], directed = TRUE)

tmp = betweenness(transferGraph)
Clubs_betweenness = data.frame(id = names(tmp), value = tmp, stringsAsFactors = FALSE)
nodesData = nodesData %>% 
  left_join(Clubs_betweenness) %>%
  mutate(title = paste(id, "betweeness ", round(value))) %>%
  arrange(id)


#### interactive network ####

visNetwork(nodes = nodesData, edges = seqResult, width = 900, height = 700) %>%
  visNodes(size = 10) %>%
  visLegend() %>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEdges(arrows = 'from') %>%
  visPhysics(
    solver = "barnesHut",
    maxVelocity = 35,
    forceAtlas2Based = list(gravitationalConstant = -6000)
  )
  

#### create a nice data table that can be published to rpubs ####

seqResult$Ntransctions = seqResult$support*10542
DT::datatable(
  seqResult[,c(5,6,9,10)], 
  rownames = FALSE,
  options = list(
    pageLength=25)
  )



