#### Libraries needed #############################################################################
library(rvest)
library(dplyr)
library(igraph)
library(visNetwork)
library(wordcloud)
library(stringr)
library(forcats)
library(ggplot2)
library(mlr)


#### Scrape Dutch transfers ######################################################################

AllTransfers = data.frame()

for( jaar in 1992:2018 ){
  print(jaar)
  link = paste0(
    "https://www.transfermarkt.nl/eredivisie/transfers/wettbewerb/NL1/plus/?saison_id=",
    jaar,
    "&s_w=&leihe=0&leihe=1&intern=0"
  )
  out = read_html(link)
  
  transfertables = html_table(out)[4:39]
  clubs = html_nodes(out, xpath = '//div[@class="table-header"]') %>%  html_text %>%  .[2:19]
  clubs = rep(clubs,each=2)
  
  result = data.frame()
  for(i in 1:36 )
  {
    tmp = transfertables[[i]]
    tmp$Leeftijd = as.numeric(tmp$Leeftijd)
    if(i%%2 == 1)
    {
      tmp$`Nieuwe club` = clubs[i]
      tmp$speler = tmp$Aanwinst
    }
    else{
      tmp$`Vorige club` = clubs[i]
      tmp$speler = tmp$`Vertrokken speler`
    }
    
    result = bind_rows(result, tmp)
    result$jaar = jaar
  }
  AllTransfers = bind_rows(AllTransfers, result)
}

## remove some duplicated records en clubnames
AllTransfers$NieuweClub = ifelse( AllTransfers$`Nieuwe club` == "SBV Excelsior", "Excelsior", AllTransfers$`Nieuwe club`)
AllTransfers$VorigeClub = ifelse( AllTransfers$`Vorige club` == "SBV Excelsior", "Excelsior", AllTransfers$`Vorige club`)
AllTransfers$NieuweClub = ifelse( AllTransfers$NieuweClub == "AFC Ajax", "Ajax", AllTransfers$NieuweClub)
AllTransfers$VorigeClub = ifelse( AllTransfers$VorigeClub == "AFC Ajax", "Ajax", AllTransfers$VorigeClub)
AllTransfers$NieuweClub = ifelse( AllTransfers$NieuweClub == "Vitesse Arnhem", "Vitesse", AllTransfers$NieuweClub)
AllTransfers$VorigeClub = ifelse( AllTransfers$VorigeClub == "Vitesse Arnhem", "Vitesse", AllTransfers$VorigeClub)
AllTransfers$NieuweClub = ifelse( AllTransfers$NieuweClub == "RBC Roosendaal", "RBC", AllTransfers$NieuweClub)
AllTransfers$VorigeClub = ifelse( AllTransfers$VorigeClub == "RBC Roosendaal", "RBC", AllTransfers$VorigeClub)
AllTransfers$NieuweClub = ifelse( AllTransfers$NieuweClub == "Sparta Rotterdam", "Sparta", AllTransfers$NieuweClub)
AllTransfers$VorigeClub = ifelse( AllTransfers$VorigeClub == "Sparta Rotterdam", "Sparta", AllTransfers$VorigeClub)
AllTransfers$NieuweClub = ifelse( AllTransfers$NieuweClub == "NEC Nijmegen", "NEC", AllTransfers$NieuweClub)
AllTransfers$VorigeClub = ifelse( AllTransfers$VorigeClub == "NEC Nijmegen", "NEC", AllTransfers$VorigeClub)
AllTransfers$NieuweClub = ifelse( AllTransfers$NieuweClub == "Heracles Almelo", "Heracles", AllTransfers$NieuweClub)
AllTransfers$VorigeClub = ifelse( AllTransfers$VorigeClub == "Heracles Almelo", "Heracles", AllTransfers$VorigeClub)
AllTransfers$NieuweClub = ifelse( AllTransfers$NieuweClub == "Cambuur L.", "SC Cambuur", AllTransfers$NieuweClub)
AllTransfers$VorigeClub = ifelse( AllTransfers$VorigeClub == "Cambuur L.", "SC Cambuur", AllTransfers$VorigeClub)
AllTransfers$NieuweClub = ifelse( AllTransfers$NieuweClub == "MVV Maastricht", "MVV", AllTransfers$NieuweClub)
AllTransfers$VorigeClub = ifelse( AllTransfers$VorigeClub == "MVV Maastricht", "MVV", AllTransfers$VorigeClub)
AllTransfers$NieuweClub = ifelse( AllTransfers$NieuweClub == "sc Heerenveen", "Heerenveen", AllTransfers$NieuweClub)
AllTransfers$VorigeClub = ifelse( AllTransfers$VorigeClub == "sc Heerenveen", "Heerenveen", AllTransfers$VorigeClub)


## Remove columns we do not need any more and remove duplicates
AllTransfers2 = AllTransfers %>%  
  filter(
    VorigeClub != "Onbekend" ,
    NieuweClub != "Onbekend" 
  ) %>% 
  select(-Aanwinst, -`Vertrokken speler`, -Nat., `Nieuwe club`, `Vorige club`) %>% 
  distinct()

## there is a long tail of small clubs, rename them to other
AllTransfers2 = AllTransfers2 %>% mutate(NieuweClub = fct_lump(NieuweClub, n=70) %>% as.character)
AllTransfers2 = AllTransfers2 %>% mutate(VorigeClub = fct_lump(VorigeClub, n=70) %>% as.character)

club1 = AllTransfers2 %>% group_by(NieuweClub) %>%  summarise(n=n())
club2 = AllTransfers2 %>% group_by(VorigeClub) %>%  summarise(n=n())
positie = AllTransfers2 %>% group_by(Positie) %>% summarise(n=n(), leeftijd = mean(Leeftijd))

### some plots
AllTransfers3 = AllTransfers2 %>% 
  filter(Leeftijd > 17) %>% 
  mutate(
    keeper = ifelse(Positie =="Keeper",1,0),
    Centrumspits = ifelse(Positie =="Centrumspits",1,0),
    Centraleverdediger = ifelse(Positie =="Centrale verdediger",1,0)
  )

ggplot(AllTransfers3, aes(Leeftijd, keeper)) + 
  geom_smooth(se=FALSE) +
  geom_smooth( aes(Leeftijd, Centrumspits), se=FALSE) +
  geom_smooth( aes(Leeftijd, Centraleverdediger), se=FALSE)

##### Create an igraph object #####################################################################

edges = AllTransfers2 %>% rename(from = VorigeClub, to = NieuweClub ) %>%  select(from, to) 
edges = edges %>% group_by(from,to) %>% summarise(weight=n())
ig = graph_from_data_frame(edges)
is_weighted(ig)

E(ig)
edge_attr(ig)
E(ig)$width <- E(ig)$weight/10


plot(
  ig,
  vertex.label.cex=0.7, vertex.size=2, edge.arrow.size=.01 ,
  edge.width	=0.1, edge.arrow.width	 =0.1, edge.color	= "Black",
  edge.label.cex = 0.4,	
  layout = layout_with_graphopt
)

deg = betweenness(ig,directed = FALSE)
degreeDF = data.frame(persoon = names(deg), centrality = deg)
row.names(degreeDF) = NULL
degreeDF = degreeDF %>% arrange( desc(centrality))

plot(
  ig, 
  vertex.size = log(deg+1),
  vertex.label.cex=0.80,  edge.arrow.size=.1 ,
  edge.arrow.width	 =0.1, edge.color	= "Black",
  edge.label.cex = 0.1,
  layout = layout_with_graphopt
)

#############################################################################

nodes = tibble(id = unique(c(edges$from, edges$to))) %>% mutate(label = id)

visNetwork(nodes, edges ) %>% 
  visEdges(smooth = FALSE) %>% 
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T), nodesIdSelection = TRUE) %>%
  visIgraphLayout()



###### Market basket #####################################################
library(arules)
AllTransfers2 = 
  AllTransfers2 %>% 
  filter(
    Leeftijd > 16, VorigeClub != "Other", NieuweClub != "Other",
    NieuweClub != "Einde carrière",
    NieuweClub != "Zonder club",
    VorigeClub != "Einde carrière",
    VorigeClub != "Zonder club"
    
    )


TMP0 = AllTransfers2 %>% mutate(item = cut(Leeftijd,5) %>% as.character()) %>% select(speler, item) %>%  distinct()
TMP1 = AllTransfers2 %>% select(speler, VorigeClub) %>% rename(item = VorigeClub)
TMP2 = AllTransfers2 %>% select(speler, NieuweClub) %>% rename(item = NieuweClub)
TMP3 = AllTransfers2 %>% select(speler, Positie) %>% rename(item = Positie) %>%  distinct()

MBA = bind_rows( TMP1, TMP2)  %>%  distinct()

transfers = as(
  split(
    MBA$item,
    MBA$speler
  ),
  "transactions"
)

itemFrequencyPlot(transfers, topN = 35)

rules <- apriori(transfers, parameter = list(supp = 0.0005, conf = 0.08, maxlen = 3))
rules

## laat enkele regels zien
inspect(rules)

inspect( sort(rules, by = "support")[1:200])



