#install.packages("ggplot2")
library(ggplot2)

#Voce foi contratado como um consultor de dados analiticos por um site de reviews.
#Eles estao escrevendo um artigo analisando classificacoes de filmes por criticos e
#audiencia assim como o orcamento dos filmes nos anos de 2007 ate 2011

#Esta e a primeira vez que eles estao fazendo esta analise e eles nao sabem exatamente 
#o que precisam. Eles pediram pra voce olhar os dados e prover a eles 5 graficos que contem
#a historia destes dados

#Contudo, existe um grafico especifico que o CEO solicitou - um diagrama mostrando como
#a correlacao das classificacoes da audiencia e dos criticos evoluiram ao longo do tempo
#por genero. Este grafico e adicional aos outros 5

#carregando os dados 
dataFrame <- read.csv(file.choose()) #P2-Movie-Ratings

#nomeando as colunas 
colnames(dataFrame) <- c("filme", "genero", "notaCritica", "notaAudiencia", "orcamentoMilhoes", "anoLancamento")

#setando os anos e generos dos filmes por levels para serem representados corretamente em summary(x)
dataFrame$anoLancamento <- factor(dataFrame$anoLancamento) 

dataFrame$genero <- factor(dataFrame$genero)

summary(dataFrame)

#criando alguns graficos 

#Carregando dados 
dados <- ggplot(data = dataFrame)

#GRAFICO 1: Relacao das notas da audiencia por orcamento em milhoes categorizado por genero
dados  + geom_point(aes(x = orcamentoMilhoes, y = notaAudiencia, colour = genero), size = 4, alpha = 0.6) + 
  xlab("Orcamento em milhoes") + ylab("Avaliacao da audiencia") + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15)) + 
  theme(legend.title = element_text(size = 10))
  

#GRAFICO 2: Relacao das notas da critica por orcamento em milhoes categorizado por genero 
dados  + geom_point(aes(x = orcamentoMilhoes, y = notaCritica, colour = genero), size = 4, alpha = 0.6) +
  xlab("Orcamento em milhoes") + ylab("Avaliacao da critica") + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15)) + 
  theme(legend.title = element_text(size = 10))

#GRAFICO 3: Relacao da distribuicao de frequencia por orcamento em milhoes usando histograma
#categorizando por generos dos filmes
dados + geom_histogram(aes(x = orcamentoMilhoes, fill = genero), colour = "Black", binwidth = 15) +
  xlab("Orcamento em milhoes") + ylab("Frequencia") + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15)) + 
  theme(legend.title = element_text(size = 10))

#GRAFICO 4: Relacao entre as notas dadas pela critica e as notas dadas pela audiencia 
# a linha representa estatisticamente a nota proporcional dada entre os dois grupos
dados + geom_smooth(aes(x = notaCritica, y = notaAudiencia, colour = genero), fill = NA ) + 
  xlab("Avaliacao da critica") + ylab("Avaliacao da audiencia") + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15)) + 
  theme(legend.title = element_text(size = 10))

#GRAFICO 5: Relacao entre os orcamentos de filmes em cada genero por ano
dados + geom_histogram(aes(x = orcamentoMilhoes),binwidth = 30) +
  facet_grid(genero~anoLancamento) +
  xlab("Orcamento em milhoes") + ylab("Quantidade de filmes") + 
  theme(axis.title.x = element_text(size = 15)) + 
  theme(axis.title.y = element_text(size = 15)) + 
  theme(legend.title = element_text(size = 10))

#GRAFICO ADICIONAL1 
adicional <- ggplot(data = dataFrame, aes(x = notaCritica, y = notaAudiencia, colour = genero))

adicional + geom_smooth(fill = NA) + 
  facet_grid(genero~anoLancamento) +
  coord_cartesian(ylim = c(0, 100))

rm(adicional, dados, dataFrame)


#GRAFICO ADICIONAL 2: Foi pedido que voce plote um grafico com um novo conjunto de dados que mostre as relacoes
# entre os principais generos e quanto eles arrecadaram nos estados unidos. Nessa grafico tambem precisar estar
# categorizado o orcamento em milhoes do filme e os principais estudios responsaveis pela producao
dataFrame <- read.csv(file.choose()) #Section6-Homework-Data

#criando um filtro para os principais generos
filter1 <- (dataFrame$Genre == "action") | 
  (dataFrame$Genre == "adventure") |
  (dataFrame$Genre == "animation") |
  (dataFrame$Genre == "comedy") |
  (dataFrame$Genre == "drama")

#criando um filtro para os principais estudios
filter2 <- (dataFrame$Studio == "Buena Vista Studio") |
  (dataFrame$Studio == "Fox") |
  (dataFrame$Studio == "Paramount Pictures") |
  (dataFrame$Studio == "Sony") |
  (dataFrame$Studio == "Universal") |
  (dataFrame$Studio == "WB")

#filtrando os dados
dataFrame <- dataFrame[filter1 & filter2, ]

#Construindo o grafico
dados <- ggplot(data = dataFrame, aes(x = Genre, y = Gross...US))

dados + geom_jitter(aes(colour = Studio, size = Budget...mill.)) + 
  geom_boxplot(alpha = 0.5, outlier.colour  = NA) 

rm(dataFrame, filter1, filter2, dados)
