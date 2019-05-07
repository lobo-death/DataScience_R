# verifica o diretório padrão dos datasets
getwd()

# seta o diretorio caso não seja o correto e verifica em seguida
setwd("/home/rstudio/Documents/Grafos")
getwd()

# instala pacotes necessários
install.packages("igraph")
install.packages("dplyr")

# carrega pacotes necessários
library(igraph)
library(dplyr)

# carregando o dataset de arestas
arestas<-get(load("union_edges.RData"))
head(arestas)

# carregando o dataset de personagens
personagens<-get(load("union_characters.RData"))
head(personagens)

# agrupa por parentesco, separa por cor e soma qtde
cores_arestas<-arestas %>%
  group_by(type, color) %>%
  summarise("Ligacoes" = n()) %>%
  filter(!is.na(color))

# agrupa por casa, separa por cor e soma qtde
cores_personagens<-personagens %>%
  group_by(house, color) %>%
  summarise("Personagens" = n()) %>%
  filter(!is.na(color))

# une os dois grupos
une_got<-left_join(arestas, personagens, by = c("source" = "name"))
une_got<-left_join(une_got, cores_personagens, by = "house")
une_got<-left_join(une_got, cores_arestas, by = "type")

# converte em grafico
got_graph<-graph_from_data_frame(une_got,directed = FALSE, vertices = personagens)

# simplifica o grafo
grafo<-simplify(got_graph)

# aplica a modularidade no grafo
wc<-edge.betweenness.community(grafo)
modularity(wc)

# altera a cor de fundo e plota o grafo simplificado sem popularidade calculada
par(bg="black", fg="white")
plot(got_graph, vertex.label = NA, vertex.size = 5, edge.arrow.size = 0.2, edge.curved = 0, mark.groups = wc)

# plota o grafo completo com a popularidade calculada e os relacionamentos entre parentes
plot.igraph(got_graph,main = "",vertex.label = NA, vertex.size = personagens$popularity*10, edge.arrow.size = 0.15,
            vertex.frame.color = "gray", vertex.label.cex = .4, layout = layout.fruchterman.reingold,
            rescale = TRUE, mark.groups = wc, edge.curved = 0)

# define as legendas do título e de cada uma dos agrupamentos
legend(-1.0,1.3, c(""), title = "Complex Networks - Game of Thrones", col = "#ffffff", bty = "n" ,ncol = 1)
legend(-2.0,1.0, c(""), title = "House's", col = "#ffffff", bty = "n" ,ncol = 1)
legend(-2.2,0.8, c(cores_personagens$house), pch = 21, col = "#ffffff", pt.bg = cores_personagens$color, pt.cex = 2, 
      bty = "n", ncol=1)

legend(1.0,1.0, c(""), title = "Family Degree's", col = "#ffffff", bty = "n" ,ncol = 1)
legend(1.1,0.8, c(cores_arestas$type), pch = 21, col = "#ffffff", pt.bg = cores_arestas$color, pt.cex = 2, 
       bty = "n", ncol=1)
