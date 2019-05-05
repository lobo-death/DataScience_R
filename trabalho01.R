install.packages("igraph")
library(igraph)
# cria o vetor do grafo
f <- graph(edges = c('avop:Francisco','pai:Celio',
                     'avop:Maria','pai:Celio',
                     'avom:Antonio','mae:Maria',
                     'avom:Sebastiana','mae:Maria',
                     'pai:Celio','mae:Maria',
                     'pai:Celio','filho1:Georgio',
                     'mae:Maria','filho1:Georgio',
                     'pai:Celio','filho2:Angelica',
                     'mae:Maria','filho2:Angelica',
                     'pai:Celio','filho3:Marcelo',
                     'mae:Maria','filho3:Marcelo', 
                     'filho1:Georgio','neto1:Georgio_Junior',
                     'nora1:Elaine','neto1:Georgio_Junior',
                     'genro1:Joao_Neto','neto2:Demostenes_Neto',
                     'filho2:Angelica','neto2:Demostenes_Neto',
                     'genro1:Joao_Neto','neto3:Joao_Augusto',
                     'filho2:Angelica','neto3:Joao_Augusto',
                     'filho1:Georgio','neto4:Thalita',
                     'nora1:Elaine','neto4:Thalita',
                     'genro2:Robison','neto5:Milena',
                     'filho2:Angelica','neto5:Milena',
                     'filho3:Marcelo','neto6:Laisa',
                     'nora2:Lorena','neto6:Laisa',
                     'neto2:Demostenes_Neto','bisneto1:Enzo_Gabriel',
                     'genro2:Robison','neto7:Robison_Junior',
                     'filho2:Angelica','neto7:Robison_Junior',
                     'neto2:Demostenes_Neto','bisneto2:Alicia',
                     'genro2:Robison','neto8:Arthur',
                     'filho2:Angelica','neto8:Arthur'),
           directed = F)

# plota o grafo
plot(f)
# conta o número de vértices
igraph::vcount(f)
# conta o número de arestas
igraph::ecount(f)
# calcula o grau dos vértices
d <- igraph::degree(f)
# gera o histograma
hist(d)
# calcula a matriz adjacente
as_adj(f)

p <- degree.distribution(f)

plot(p)

shapiro.test(rnorm(p))
