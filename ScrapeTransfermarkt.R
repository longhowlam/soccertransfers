library(rvest)
library(dplyr)
library(igraph)
library(visNetwork)

library(wordcloud)

AllTransfers = data.frame()
for( jaar in 1996:2018 ){
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


AllTransfers = AllTransfers %>%  select(-Aanwinst, -`Vertrokken speler`, -Nat.)
AllTransfers$`Vertrokken speler` = NULL

tmp = AllTransfers %>% group_by(`Nieuwe club`) %>%  summarise(n=n())
tmp2 = AllTransfers %>% group_by(`Vorige club`) %>%  summarise(n=n())

##############################################################################

edges = AllTransfers %>% rename(from = `Vorige club`, to = `Nieuwe club` ) %>% filter(jaar > 1015) %>%  select(from,to) 
edges = edges %>% group_by(from,to) %>% summarise(weight=n())
ig = graph_from_data_frame(edges)
is_weighted(ig)
plot(
  ig,
  vertex.label.cex=0.7, vertex.size=2, edge.arrow.size=.01 ,
  edge.width	=0.1, edge.arrow.width	 =0.1, edge.color	= "Black",
  edge.label.cex = 0.4,	
  layout = layout_with_graphopt
)

cut.off <- 1.3*mean(edges$weight) 

net.sp <- delete_edges(ig, E(ig)[weight < 9])

plot(net.sp, vertex.label.cex=0.27, vertex.size=2, edge.arrow.size=.01 ,
     edge.width	=0.1, edge.arrow.width	 =0.1, edge.color	= "Black",
     edge.label.cex = 0.4,	
     layout = layout_with_graphopt) 


deg <- degree(ig, mode="all")
deg = betweenness(ig,directed = FALSE)
hist(deg)

degreeDF = data.frame(persoon = names(deg), centrality = deg)
row.names(degreeDF) = NULL
degreeDF = degreeDF %>% arrange( desc(centrality))


plot(
  ig, 
  vertex.size = log(deg+1),
  vertex.label.cex=0.27,  edge.arrow.size=.01 ,
  edge.width	=0.1, edge.arrow.width	 =0.1, edge.color	= "Black",
  edge.label.cex = 0.1,
  layout = layout_with_graphopt
)





ceb <- cluster_edge_betweenness(ig) 

tmp = membership(ceb) 
NL_communities =  data.frame(
  persoon = names(tmp), 
  community = as.numeric(tmp))

COMM_STATS = NL_communities %>%  group_by(community) %>%  summarise(n=n())

dendPlot(ceb, mode ="hclust", use.edge.length = TRUE, cex = 0.1, horiz=TRUE)
plot(
  ceb,
  ig,
  vertex.label.cex=0.1, vertex.size=1, edge.arrow.size=.01 ,
  edge.width	=0.1, edge.arrow.width	 =0.1, edge.color	= "Black",
  edge.label.cex = 0.1,
  layout = layout_with_graphopt
)



i = 16
groep_i = NL_communities %>% filter(community == i) %>% left_join(degreeDF)
pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
wordcloud(groep_i$persoon, groep_i$centrality, colors=pal)


