
"""Projeto_GINI.ipynb

# Passos para execução do código:


1.  Execute a primeira e responda o que lhe for solicitado;
2.  Execute projeto GINI e aguarde;
3.  Após termino do carregamento, vá ao diretório de arquivos (simbolo de pasta à esquerda) e procure pelo arquivo *MA20##_gini_informalidade_evoluestoque.csv*. Aí é só alegria ✌
😎;
4.  Para calcular outro ano é aconselhável que desconecte e exclua o ambiente de execução, e reinicie tudo. (menu superior>Ambiente de execução>Desconectar e excluir ambiente de execução)


Tempo estimado para execução é de 10 min
"""

ano <- as.integer(readline(prompt = "Digite o ano para executar o calculo (enter para confirmação) "))

"""# Projeto GINI

## Instalando pacotes e importando bibliotecas
"""

if (!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(PNADcIBGE, survey, convey, Hmisc, dplyr)

"""## Baixando Dados

Baixando a quantidade de trimestres e anos necessários em um loop e agregando a uma variável. O loop diminui a quantidade de importações
"""

#OBS: quanto maior o número de variáveis importadas em get_pnadc é aconselhável diminuir o intervalo de anos.
data <- expand.grid(year=ano,quarter = 1:4)
all_data <- lapply(seq(nrow(data)), function(i)
                    get_pnadc(year = data$year[i],quarter = data$quarter[i],vars = c("Ano", "Trimestre", "UF","UPA","Estrato",
                    "V1008","V1028","V2005","V2007","V2009","VD2003","VD3004","VD4009","VD4020"), labels=F, deflator= T, design=T, savedir="/content/"))

"""Após agregar todos os dados na variável all_data, cada df é definido como: *all_data[[1]]*. Sendo este o primeiro df baixado.

## Índice de Gini

### Código do professor Fabrício com alterações

Criando um df vazio para por os resultados do calculo do proximo loop
"""

gini <- data.frame(Ano = c(''), Trimestre = c(''), GINIrdpcMA = c(''))

#ATENÇÂO: a quantidade de repetições precisa ser a quantidade de trimestres baixados. Ex: 2012:2016 = 20 trimestres
Nrt <- as.character(21:29)#variável para uso do loop de cada estado do Nordeste
for (y in 1:4){

 #Tomando somente as variáveis e salvando a base novamente

 PNADC<-all_data[[y]]$variables
 PNADC_sel<-PNADC[c("Ano", "Trimestre", "UF","UPA","Estrato","V1008","V1028","V2005","VD2003","VD4020")]

 X<-as.data.frame(PNADC_sel[c(3:10)])
 #Alterações para filtrar o Estado
 X[["UF"]]<-as.numeric(as.character(X[["UF"]]))#EDYR
 X[["UPA"]]<-as.numeric(as.character(X[["UPA"]]))
 X[["Estrato"]]<-as.numeric(as.character(X[["Estrato"]]))
 X[["V1008"]]<-as.numeric(as.character(X[["V1008"]]))
 Xsorted<-X[order(X[["UF"]],X[["UPA"]],X[["Estrato"]],X[["V1008"]]),]
 Xfiltered<-subset(Xsorted, Xsorted["V2005"]<17 & Xsorted["UF"]==21 )#EDYR: acrescentei o filtro para Maranhão.
 Xfiltered2<-subset(Xfiltered, !is.na(Xfiltered$VD4020))
 Xagg<-aggregate(Xfiltered2[["VD4020"]], by=list(Xfiltered2[["UF"]], Xfiltered2[["UPA"]], Xfiltered2[["Estrato"]], Xfiltered2[["V1008"]]), FUN=sum, na.rm=TRUE)

 #dando nomes as colunas
 names(Xagg)[1]<-"UF"#EDYR: UF
 names(Xagg)[2]<-"UPA"
 names(Xagg)[3]<-"Estrato"
 names(Xagg)[4]<-"V1008"
 names(Xagg)[5]<-"RD"
 Xmerged<-merge(Xfiltered,Xagg,by=c("UF","UPA", "Estrato", "V1008"))#EDYR: UF
 Xmerged$RDPC=Xmerged$RD/Xmerged$VD2003
 Xmergedsorted<-Xmerged[order(Xmerged$RDPC),]
 Xmergedsorted$rank<-wtd.rank(Xmergedsorted$RDPC, weights=Xmergedsorted$V1028, normwt=FALSE, na.rm=TRUE)

 rdpc<-Xmergedsorted$RDPC
 i<-Xmergedsorted$rank
 w<-Xmergedsorted$V1028
 rendw<-rdpc*w
 pop<-sum(w)
 rendtot<-sum(rendw)
 rendmed<-rendtot/pop
 m<-sum(i*rendw)
 n2<-pop*pop
 a<-2/(n2*rendmed)
 gini [y, c ("GINIrdpcMA")]<-((a*m)-(1/pop))-1 #colocando o calculo do prof fabrício no dataframe gini
 gini [y, c ("Ano")] <- PNADC_sel ["1", c ("Ano")]#colocando o ano referido da base de dados no datafrane gini
 gini [y, c ("Trimestre")] <- PNADC_sel ["1", c ("Trimestre")]##colocando o trimestre referido da base de dados no datafrane gini

}

all_dataMA <- all_data
for (m in 1:4){
  all_dataMA[[m]] <-subset(all_data[[m]], UF == 21)
}

"""## Calculando a proporção de trabalhadores e total de trabalhadores por categoria"""

for (i in 1:4){
  nome <- paste0("informalMA", i)#passando variáveis com nomes diferentes
  assign(nome, svyby(~VD4009, by = ~Trimestre, all_dataMA[[i]], svymean, na.rm  =  TRUE))#passando o resultado da proporção para cada variável
  nome <- paste0("evoluMA", i)#criando variaveis no loop para receber os resultados
  assign(nome, svyby(~VD4009, by = ~Trimestre, all_dataMA[[i]], svytotal, na.rm  =  TRUE))#passando o resultado do total para cada variável

}
informalMA<-rbind(informalMA1,informalMA2,informalMA3,informalMA4)#unindo as variaveis para formar um DF
evoluMA<-rbind(evoluMA1,evoluMA2,evoluMA3,evoluMA4)

"""## Passando os 3 DF (gini,inforlmal,evolu) para um só df, e posterior exportação para csv"""

#juntando dfs
gini_infor_estqtra <- gini %>% inner_join(informalMA)
MA_gini_informalidade_evoluestoque<- cbind(gini_infor_estqtra, evoluMA )

#Exportando csv
write.csv2(MA_gini_informalidade_evoluestoque, sprintf("MA%s_gini_informalidade_evoluestoque.csv", ano))
