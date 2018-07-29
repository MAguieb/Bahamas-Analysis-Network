
# --------------------------------------- Analyse des Réeseaux sociaux ----------------------------------------- # 

#  problematique : 

# Using Social Network Analysis techniques was possible to
# characterize the level of connections, the number of communities and
# the actors that play a central role in the network. We were able to assess
# the Bahamas network and identify its central actors of the network,
# i.e., companies with high influence in the network, in which any eventual
# investigation of legal compliance by Bahamas authorities should be focused on


rm(list=ls())


# packages & library :

## ## ## ## ## Load `package:needs` in an interactive session to set auto-load flag ## ## ## ##

# install.packages("needs")
# install.packages("fBasics")
# install.packages("wordcloud")
# install.packages("wordcloud2")
# install.packages("tm")

library(wordcloud)
library(wordcloud2)
library(tm)
library(fBasics)
library(data.table)
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(igraph)
library(intergraph)
library(ggrepel)
library(ggnetwork)
library(pander)
library(formattable)
library(highcharter)

#general
require(purrr)
require(tidyverse)
require(data.table)
require(lubridate)
require(stringr)
require(ggvis)
require(ggplot2)
require(forcats)
require(ggmap)
require(highcharter)
require(broom)
require(plotly)
require(stringi)

#network plot
require(igraph)
require(ggmap)
require(sna)
require(intergraph)
require(ggnetwork)
require(visNetwork)
require(viridis)

# achieve/appendices
require(GGally)
require(networkD3)

#  Let’s read the data in and look at the columns :

setwd ("C:/Users/Admin/Desktop/Analyse_des_réseaux_Sociaux/PROJET")

Entity <- as.data.table(read.csv("bahamas_leaks_nodes_entity.csv",na.strings=c("","NA"),stringsAsFactors = FALSE))

Address <- as.data.table(read.csv("bahamas_leaks_nodes_address.csv",na.strings=c("","NA"),stringsAsFactors = FALSE))

Intermediary <- as.data.table(read.csv("bahamas_leaks_nodes_intermediary.csv",na.strings=c("","NA"),stringsAsFactors = FALSE))

Officer <- as.data.table(read.csv("bahamas_leaks_nodes_officer.csv",na.strings=c("","NA"),stringsAsFactors = FALSE))

Edges <- as.data.table(read.csv("bahamas_leaks_edges.csv",na.strings=c("","NA"), stringsAsFactors = FALSE))

# overview of datasets and some retraitement:

# ”Entity (offshore)”: company, trust or fund created in a low-tax, offshore jurisdiction by an agent.

# ”Officer”: person or company who plays a role in an offshore entity.

# ”Intermediary”: a go-between for someone seeking an offshore corporation and an offshore service provider 
#  usually a law-firm or a middleman that asks an offshore service provider to create an offshore firm for a client.

# ”Address”: contact postal address as it appears in the original databases obtained by ICIJ.


# ________________ Analyse et traitement de chaque data set 

# Entity :

# Traitement de la colone date 

glimpse(Entity)

Entity[,("incorporation_date"):=lapply(.SD,parse_date_time,orders="%d-%m-%Y"), .SDcols="incorporation_date"]

# Traitement des NA 

Entity <-Entity[,-c("country_codes","countries","address","service_provider","closed_date","type","status","company_type","note")]

glimpse(Entity)

# Entities incorporation date by Year and Month By year :

ggplot(Entity[,.N,keyby=.(incorporation_date,year(incorporation_date),month(incorporation_date,label=T))][year>1990&year<2017],
       aes(x=month,y=N,fill=incorporation_date))+
  geom_bar(stat="identity")+facet_wrap(~year)+theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(caption="Bahamas Leaks",title=" Entities incorporation date by Year and Month")

# Entities name :

corpus = Corpus(VectorSource(list(Entity$name)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english'))

dtm_eap = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_eap <- colSums(as.matrix(dtm_eap))

frec <- NULL
frec$word <- names(freq_eap)
frec$freq <- freq_eap
wordcloud(frec$word,frec$freq , min.freq=500,colors=brewer.pal(6,"Dark2"),random.order = F)

# table de frequence name :

DT::datatable(as.data.table(frec)[order(-freq)][1:50])

# Address :

glimpse(Address)

Address <-Address[,-c("name","jurisdiction_description","service_provider","jurisdiction","closed_date","ibcRUC","incorporation_date","type","status","company_type","note" )]                
                      
glimpse(Address)

# Intermediary

glimpse(Intermediary)

Intermediary <-Intermediary[,-c("address","jurisdiction_description","service_provider", "jurisdiction","closed_date", "incorporation_date","ibcRUC","type","status","company_type","note")]                

glimpse(Intermediary)

# Officer 

glimpse(Officer)

Officer<-Officer[,-c("country_codes","countries","address","jurisdiction_description","service_provider", "jurisdiction","closed_date", "incorporation_date","ibcRUC","type","status","company_type","note")]                

glimpse(Officer)

# Comptage des nom les plus utilisé dans les bahamas officer

corpus = Corpus(VectorSource(list(Officer$name)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english'))

dtm_eap = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_eap <- colSums(as.matrix(dtm_eap))

frec <- NULL
frec$word <- names(freq_eap)
frec$freq <- freq_eap
wordcloud(frec$word,frec$freq , min.freq=50,colors=brewer.pal(6,"Dark2"),random.order = F)

# table de frequence name :

DT::datatable(as.data.table(frec)[order(-freq)][1:50])

# Edges

glimpse(Edges)

Edges<-Edges[,-c("start_date","end_date")]                

glimpse(Edges)

# voir le type de relation les plus dominant entre les noueds :

popular_rel_type<-Edges[,.N, by=c("rel_type")] %>%
  .[order(-N)] 

# within the top 30 most common relationship

table(Edges$rel_type) 

identical_relation_list <- c("similar name and address as",
                             "same name as",
                             "same company as",
                             "same name and registration date as",
                             "same address as")

Edges[rel_type %in% popular_rel_type$rel_type, Edge_Type:=1]%>%
  .[!(rel_type %in% popular_rel_type$rel_type), Edge_Type:=2]%>%
  .[rel_type %in%identical_relation_list, Edge_Type :=3]

popular_rel_type<-Edges[,.N, by=c("rel_type", "Edge_Type")] %>%
  .[order(-N)] %>%
  head(10)

# plot

hchart(popular_rel_type, "column", hcaes(x = rel_type, y = N, group = Edge_Type))%>%
  hc_title(text = "Top 10 Nodes relationship between the nodes ID",
           style = list(color = "Black", useHTML = TRUE)) 


#  voir les network relationshep 

#  Nodes
Nodes<-rbind(
  Entity[,.(node_id,nameID=name,Identity="Entities")], 
  Intermediary[,.(node_id, nameID=name, Identity="Intermediaries")], 
  Officer[,.(node_id,nameID=name,Identity="Officers")],
  Address[,.(node_id,address, Identity="Addresses")]
  , fill=TRUE)

# Edges 

Edges_simplified<-Edges[,.(node_1, node_2,rel_type)]

colnames(Edges_simplified) <- c("from",'to','type_rel')

#  seting network as graph 

ed=data.frame(node_id=unique((unlist(Edges_simplified[,c("from",'to')]))))

Nodes <- merge(ed,Nodes,by="node_id",all=TRUE)
                
net <- graph.data.frame(Edges_simplified, vertices=Nodes, directed=T)

net

class(net)  
#  faire un plot est impossible vu la taille des données et 

# degre des noeux 

nodes_attributes<-data.table(
  node_id=names(igraph::degree(net, mode = "all")), 
  nodes_degree_all=(igraph::degree(net, mode = "all")), 
  nodes_degree_out=(igraph::degree(net, mode = "out")), 
  nodes_betweenness=(igraph::betweenness(net)),
  centrality=(eigen_centrality(net)$vector))


# Clustering

clusters(net)

# voir les clusters les plus grands 

decomposed_graph_list <- decompose.graph(net)

vc <- data.table(unlist(lapply(decomposed_graph_list,vcount)),keep.rownames = T)
vc$cluster_id <- rownames(vc)
setnames(vc,"V1","vCOUNT")
Pop_list <- vc[order(-vCOUNT)] %>% head(10)

# plot

hchart(Pop_list,"column",hcaes(x = cluster_id, y=vCOUNT)) %>% 
  hc_title(text='Nombre de nodes par cluster') %>% 
  hc_yAxis(type="logarithmic")


 # etudes des nodes 

# Exploring centrality

High_Centrality_Nodes<-nodes_attributes[centrality>=0.002681][order(-centrality)]%>%head(30)

# # Changing the class of nodes_id to interget for Merging
 
High_Centrality_Nodes$node_id<- as.integer(High_Centrality_Nodes$node_id)

# # Merging with original nodes to acquire nodes attributes
 
H_Centrality_dt <- merge(High_Centrality_Nodes,Nodes,by="node_id",all=FALSE)

H_Centrality_dt[,.(nameID,Identity, node_id, address)]

DT::datatable(as.data.table(H_Centrality_dt))


#  etudes des degrees 


High_Degree_Nodes<-nodes_attributes[order(-nodes_degree_all)]%>%head(30)

High_Degree_Nodes$node_id<- as.integer(High_Degree_Nodes$node_id)

H_Degree_dt <- merge(High_Degree_Nodes,Nodes,by="node_id",all=FALSE)

H_Degree_dt[,.(nameID, nodes_degree_all, Identity, node_id, address)]

DT::datatable(as.data.table(H_Degree_dt))


#  on fabrique des reseaux a partir de la base edges en fesant des group_by 
 #  voir les Nodes Entity 
 
table(Edges_simplified$type_rel)
 
Edges_off <- Edges_simplified %>% filter(type_rel=='registered_address')

# ed_off=data.frame(node_id=unique((unlist(Edges_off[,c("from",'to')]))))
# 
# Nodes_off <- merge(ed_off,Nodes,by="node_id",all=TRUE)

Edg_RA <- data.frame(Edges_off[Edges_off$from %in% Nodes$node_id ,])
Edg_RA <- data.frame(Edges_off[Edges_off$to %in% Nodes$node_id ,])

ed_ra=data.frame(node_id=unique((unlist(Edg_RA[,c("from",'to')]))))

Node_RA <- merge(ed_ra,Nodes,by="node_id",all=FALSE)

nrow(Node_RA)


nett <- graph.data.frame(Edg_RA, vertices=Node_RA, directed=T)

nett

class(nett)

plot(nett)

#  VISUALISATION Graphs KK

hchart(nett, layout=layout_with_kk)%>%
  hc_title(text="Network Attributes of Bahamas Leaks")

hchart(nett, layout=layout.reingold.tilford)%>%
  hc_title(text="Network Attributes of Bahamas Leaks")

# #  graphe par type de node 
# 
# Edg_Entity <- data.frame(Edges_simplified[Edges_simplified$node_2 %in% Entity$node_id ,])
# Edg_Entity <- data.frame(Edges_simplified[Edges_simplified$node_1 %in% Entity$node_id ,])
# 
# ed1=data.frame(node_id=unique((unlist(Edg_Entity[,c("node_1",'node_2')]))))
# 
# Node_Enity <- merge(ed1,Entity,by="node_id",all=TRUE)
# 
# nrow(Node_Enity)
# 
# #transformer les networks en igraphs objets
# 
# net <- graph_from_data_frame(d=Edg_Entity, vertices=EEEE, directed=T) 
# 
# Entity[, -c("labels.n.","valid_until","sourceID","jurisdiction_description","jurisdiction")]
# 
# E(net)
# 
# V(net)
# 
# net
# 
# plot(net, vertex.label=NA)
# 
# hchart(net, layout=layout_with_kk)%>%
#   hc_title(text="Network Attributes of Country Nodes in panama-paradise papers")
# 
# 
# visNetwork(edges = Edg_Entity, nodes = EEEE ,main = 'ldkg') %>% 
#   visOptions(highlightNearest = TRUE)
# 
