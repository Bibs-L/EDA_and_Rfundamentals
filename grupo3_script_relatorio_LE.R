
####ANÁLISE EXPLORATÓRIA DOS DADOS DE BRFSS2015 COM ENFÂSE NA OBESIDADE#######
      ### NO ÂMBITO DA CADEIRA DE LAB. DE ESTATÍSTICA; 2024/2025#####

#####realizado por:
## Ana Cardoso 122994; Beatriz Lavado 102495; 
## Fátima Figueira 124413; Maria Alice Monteiro 124366

####carregar bibliotecas
library(dplyr) #recode
library(readr)#abrir o ficheiro de dados
library(ggplot2)#graficos
library(ggmosaic)#graficos mosaico

####base de dados
library(readr)
heart_disease_health_indicators_BRFSS2015 <- read_csv("heart_disease_health_indicators_BRFSS2015.csv")
View(heart_disease_health_indicators_BRFSS2015)

####leitura dos dados, alteração de variaveis e caracterizacao da amostra
dadosTrab=as.data.frame(heart_disease_health_indicators_BRFSS2015)

dadosTrab$AnyHealthcare=as.factor(dadosTrab$AnyHealthcare)
dadosTrab$AnyHealthcare=recode_factor(dadosTrab$AnyHealthcare,"1"="Sim","0"="Não")

dadosTrab$PhysActivity=as.factor(dadosTrab$PhysActivity)
dadosTrab$PhysActivity=recode_factor(dadosTrab$PhysActivity,"1"="Sim","0"="Não")

dadosTrab$HighChol=as.factor(dadosTrab$HighChol)
dadosTrab$HighChol=recode_factor(dadosTrab$HighChol,"1"="Sim","0"="Não")

dadosTrab$Sex=as.factor(dadosTrab$Sex)
dadosTrab$Sex=recode_factor(dadosTrab$Sex, "0"="Mulher","1"="Homem")

dadosTrab$Age=as.factor(dadosTrab$Age)
dadosTrab$Age=recode_factor(dadosTrab$Age,"1"="18-24","2"="25-29" ,"3"="30-34",
                            "4"="35-39","5"="40-44","6"="45-49",
                            "7"="50-54","8"="55-59","9" ="60-64",
                            "10"="65-69", "11"="70-74","12"="75-79","13"=">=80")

dadosTrab$Income=as.factor(dadosTrab$Income)
dadosTrab$Income=recode_factor(dadosTrab$Income,"1"="<$10k","2"="[$10k-$15k[",
                               "3"="[$15k-$20k[","4"="[$20k-$25k[",
                               "5"="[$25k-$35k[","6"="[$35k-$50k[",
                               "7"="[$50k-$75k[","8"=">=$75k")

dadosTrab$Education=as.factor(dadosTrab$Education)
dadosTrab$Education=recode_factor(dadosTrab$Education,"1"="Até Pré-escolar",
                                  "2"="1º-8ºAno","3"="9º-11ºAno",
                                  "4"="12ºAno/GED","5"="1-3ºAnos Uni.",
                                  "6"=">=4ºAno Uni.")

dadosTrab$BMI_1=factor(ifelse(dadosTrab$BMI<=18,'Baixo Peso',
                              ifelse(dadosTrab$BMI<=24,'Peso Normal',ifelse(dadosTrab$BMI<=29,'Excesso de Peso',
                                                                            ifelse(dadosTrab$BMI<=35,'Obesidade Classe 1',ifelse(dadosTrab$BMI<=40,'Obesidade Classe 2',
                                                                                                                                 'Obesidade Classe 3'))))),levels = c('Baixo Peso','Peso Normal','Excesso de Peso',
                                                                                                                                                                      'Obesidade Classe 1','Obesidade Classe 2','Obesidade Classe 3'))

dadosTrab$BMI_2=factor(ifelse(dadosTrab$BMI<=18,'Baixo Peso',
                              ifelse(dadosTrab$BMI<=24,'Peso Normal',
                                     ifelse(dadosTrab$BMI<=29,'Excesso de Peso',
                                            "Obesidade"))),levels = c('Baixo Peso','Peso Normal','Excesso de Peso','Obesidade'))




dadosTrab$Alimentacao=dadosTrab$Fruits + dadosTrab$Veggies
dadosTrab$Alimentacao=as.factor(dadosTrab$Alimentacao)
dadosTrab$Alimentacao=recode_factor(dadosTrab$Alimentacao, "0"="Pobre",
                                 "1"="Intermédia", "2"="Rica")


dadosTrab$HvyAlcoholConsump=as.factor(dadosTrab$HvyAlcoholConsump)
dadosTrab$HvyAlcoholConsump=recode_factor(dadosTrab$HvyAlcoholConsump,"1"="Sim","0"="Não")

dadosTrab$NoDocbcCost=as.factor(dadosTrab$NoDocbcCost)
dadosTrab$NoDocbcCost=recode_factor(dadosTrab$NoDocbcCost,"1"="Sim","0"="Não")

dadosTrab$Smoker=as.factor(dadosTrab$Smoker)
dadosTrab$Smoker=recode_factor(dadosTrab$Smoker,"1"="Sim","0"="Não")

View(dadosTrab)

nr.mulheres=sum(dadosTrab$Sex == 'Mulher')
percentage.mulheres=(nr.mulheres/nrow(dadosTrab))*100
nr.homens=sum(dadosTrab$Sex == 'Homem')
pecentage.homens=(nr.homens/nrow(dadosTrab))*100

tabela.idades = table(dadosTrab$Age)
cbind(Freq.abs = tabela.idades, Freq.rel = prop.table(tabela.idades)*100)
#analisando a tabela, o grupo com a maior % indivíduos é 60-64anos,
#sendo que dos 50 aos 69 se encontra o grosso de respostas

tabela.idade.imc = table (dadosTrab$Age, dadosTrab$BMI_2)
Freq.abs = tabela.idade.imc
Freq.abs

ggplot(dadosTrab,aes(Age))+geom_bar(fill="lightpink")+
  labs(title = "Distribuição das Idades", x = "Idade", y = "Frequência") +
  theme_minimal()
#grafico em anexo no relatório

(media.BMI=mean(dadosTrab$BMI))
(mediana.BMI=median(dadosTrab$BMI))
(max.BMI=max(dadosTrab$BMI))
(min.BMI=min(dadosTrab$BMI)) # parenteses no inicio e fim de forma a ser possivel visualizar na consola o resultado

# em alternativa:
summary(dadosTrab$BMI)
  
#Análise visual da amostra
plot(dadosTrab$BMI_1)
hist(dadosTrab$BMI)
plot(dadosTrab$HighChol)
plot(dadosTrab$HighChol,dadosTrab$BMI,outline=FALSE)
plot(dadosTrab$Alimentacao)

ggplot(dadosTrab, aes(x = BMI, fill = Alimentacao)) +
  geom_histogram(alpha = 0.6, position = "identity", binwidth = 1) +
  labs(title = "Histograma de IMC por Alimentação", x = "IMC", y = "Frequência") +
  theme_minimal()

ggplot(dadosTrab, aes(x = BMI, fill = PhysActivity)) +
  geom_histogram(alpha = 0.4, position = "identity", binwidth = 1) +
  labs(title = "Histograma de IMC por Exercicio", x = "IMC", y = "Frequência") +
  theme_minimal()

dadosTrab_count <- table(dadosTrab$PhysActivity, dadosTrab$BMI_2)
dadosTrab_percent <- prop.table(dadosTrab_count, margin = 1) * 100
dadosTrab_df <- as.data.frame(dadosTrab_percent)
names(dadosTrab_df) <- c("PhysActivity", "BMI_2", "Percentage")

ggplot(dadosTrab_df, aes(x = PhysActivity, y = Percentage, fill = BMI_2)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_fill(vjust = 0.5),
            size = 3) +
  labs(title = "Distribuição de IMC por Prática de Atividade Física",
       x = "Atividade Física",
       y = "Percentagem",
       fill = "Categoria de IMC") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

#influencia de beber alcool e fumar, dados não mostraram relevância e por isso não foram utilizados
table(dadosTrab$Alimentacao, dadosTrab$HvyAlcoholConsump, dadosTrab$BMI_2)
table(dadosTrab$Smoker,dadosTrab$BMI_2)

#####Gráficos - Resultados e Discussão

#GRAFICO PIZZA DISTRIBUIÇÃO DE CATEGORIAS DE IMC (%)
sum(tabela.idade.imc[,1]) 
sum(tabela.idade.imc[,2])
sum(tabela.idade.imc[,3])
sum(tabela.idade.imc[,4])
#faz-se o somatorio das colunas por categoria de IMC para usar agora:

dadosTrab_dataframe = data.frame(CategoriaPeso=c('Baixo Peso','Peso Normal','Excesso de Peso',
                                                 'Obesidade'),
                                 Contagem = c(3127,68953,93749,87851))
View(dadosTrab_dataframe)

dadosTrab_dataframe$Percentagem <- dadosTrab_dataframe$Contagem / sum(dadosTrab_dataframe$Contagem) * 100

ggplot(dadosTrab_dataframe, aes(x = "", y = Percentagem, fill = CategoriaPeso)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentagem, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Distribuição Percentual de Categorias de IMC",
       fill = "Categorias de IMC") +
  scale_fill_brewer(palette = "Set2")

#IMC por Sexo e Idade

#correlacao idade-imc
# Função para calcular o ponto médio dos intervalos
ponto_medio_idade = function(intervalo) {
  if (grepl("^[0-9]+-[0-9]+$", intervalo)) {
    # Intervalo padrão, como "20-29"
    limites = as.numeric(unlist(strsplit(intervalo, "-")))
    return(mean(limites))  # Retorna o ponto médio
    
  } else if (grepl("^>=\\d+$", intervalo)) {
    # Caso "maior que" (>=80), assume o ponto médio como sendo o limite mais baixo + 5
    limite_inferior = as.numeric(sub(">=", "", intervalo))
    return(limite_inferior + 5)
    
  } else {
    warning("Intervalo inválido: ", intervalo)
    return(NA)  # Retorna NA para valores que não correspondem a nenhum formato esperado
  }
}

# Converter a coluna `Age` em texto para poder usar `strsplit`
dadosTrab$Age = as.character(dadosTrab$Age)

# Aplicar a função à coluna de intervalos de idade na base de dados
dadosTrab$Idade_PontoMedio = sapply(dadosTrab$Age, ponto_medio_idade)

# Calcular a correlação de Spearman entre IMC e os pontos médios de idade
correlacao_spearman = cor(dadosTrab$BMI, dadosTrab$Idade_PontoMedio,
                          method = "spearman", use = "complete.obs")
correlacao_spearman

#voltar a Age como factor
dadosTrab$Age=factor(dadosTrab$Age, levels = c("18-24","25-29" ,"30-34",
                            "35-39","40-44","45-49",
                            "50-54","55-59","60-64",
                            "65-69", "70-74","75-79",">=80"))

# GRAFICO DE BOLHAS
dados_bolhas=dadosTrab %>%
  count(Age, BMI_2) %>%
  group_by(Age) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(dados_bolhas, aes(x = Age, y = BMI_2, size = percentage, fill = percentage)) +
  geom_point(shape = 21, color = "black", alpha = 0.7) +
  scale_size_continuous(range = c(1, 20)) +
  scale_fill_viridis_c() +
  labs(title = "Gráfico de Bolhas: IMC por Idade",
       x = "Idade",
       y = "Categoria de IMC",
       size = "Percentagem",
       fill = "Percentagem") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle =45,hjust=1))

#GRAFICO DE DISPERSÃO: MÉDIA DE IMC POR IDADES E SEXO
# 1. Filtrar os dados para homens e calcular a média do IMC por faixa etária
homens = dadosTrab[dadosTrab$Sex == "Homem", ]
media_imc_homens <- aggregate(BMI ~ Age, data = homens, FUN = mean, na.rm = TRUE)

# 2. Filtrar os dados para mulheres e calcular a média do IMC por faixa etária
mulheres = dadosTrab[dadosTrab$Sex == "Mulher", ]
media_imc_mulheres <- aggregate(BMI ~ Age, data = mulheres, FUN = mean, na.rm = TRUE)

# 3. Exibir os resultados
media_imc_homens #Média do IMC para homens por faixa etária:

media_imc_mulheres #Média do IMC para mulheres por faixa etária:


# 4. Reorganizar os dados, numa só df, para ggplot
df_homens = data.frame(Age = media_imc_homens$Age,
                       Sex = "Homem",
                       MediaIMC = media_imc_homens$BMI)
df_mulheres = data.frame(Age = media_imc_mulheres$Age,
                         Sex = "Mulher",
                         MediaIMC = media_imc_mulheres$BMI)

df_combined <- rbind(df_homens, df_mulheres)

# 5. CRIAÇÃO DO GRÁFICO DE DISPERSÃO
ggplot(df_combined, aes(x = Age, y = MediaIMC, color = Sex)) +
  geom_point(size = 2.5, alpha = 0.7) +
  labs(title = "Média de IMC por Idade e Sexo", 
       x = "Idade", 
       y = "Média de IMC") +
  scale_color_manual(values = c("dodgerblue3", "deeppink")) +
  theme_minimal()

# INFLUÊNCIA DO NÍVEL DE EDUCAÇÃO, RENDIMENTOS
#E SEGURO DE SAÚDE NA OBESIDADE

#educação
dados_percentagem_E_BMI_2 = dadosTrab %>%
  count(Education, BMI_2) %>%
  group_by(Education) %>%
  mutate(percentagem = n / sum(n) * 100)

ggplot(dados_percentagem_E_BMI_2, aes(x = Education, y = percentagem, fill = BMI_2)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", percentagem)), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(title = "Distribuição Percentual das Categorias de IMC por Níveis de Educação",
       x = "Níveis de Educação",
       y = "Percentagem",
       fill = "Categoria de IMC") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal()

#educação e IMC Homem vs Mulher, em anexo
ggplot(data = subset(dadosTrab, Sex == "Mulher")) +
  geom_mosaic(aes(x = product(Education), fill = BMI_2)) +
  labs(title = "Relação entre Educação e Categorias de IMC nas Mulheres",
       x = "Níveis de Educação",
       y = "Categoria de IMC",fill = "Categoria de IMC") +
  theme_minimal()

ggplot(data = subset(dadosTrab, Sex == "Homem")) +
  geom_mosaic(aes(x = product(Education), fill = BMI_2)) +
  labs(title = "Relação entre Educação e Categorias de IMC nos Homens",
       x = "Níveis deEducação",
       y = "Categoria de IMC",
       fill = "Categoria de IMC") +
  theme_minimal()

#educação e rendimentos, em anexo
dados_percentagem_E_I = dadosTrab %>%
  count(Income, Education) %>%
  group_by(Income) %>%
  mutate(percentagem = n / sum(n) * 100)

ggplot(dados_percentagem_E_I, aes(x = Income, y = percentagem, fill = Education)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribuição Percentual dos Níveis de Educação em função dos Níveis de Rendimento",
       x = "Nível de Rendimento",
       y = "Percentagem",
       fill = "Nível de Educação") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal()

#rendimentos
dados_percentagem_I_BMI_2 = dadosTrab %>%
  count(Income, BMI_2) %>%
  group_by(Income) %>%
  mutate(percentagem = n / sum(n) * 100)

ggplot(dados_percentagem_I_BMI_2, aes(x = Income, y = percentagem, fill = BMI_2)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", percentagem)), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(title = "Distribuição Percentual das Categorias de IMC por Níveis de Rendimento",
       x = "Níveis de Rendimento",
       y = "Percentagem",
       fill = "Categoria de IMC") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal()

#rendimentos e IMC Homem vs Mulher, em anexo
ggplot(data = subset(dadosTrab, Sex == "Homem")) +
  geom_mosaic(aes(x = product(Income), fill = BMI_2)) +
  labs(title = "Relação entre os Níveis de Rendimentos e Categorias de IMC nos Homens",
       x = "Income",
       y = "IMC",
       fill = "Categoria de IMC") +
  theme_minimal()

ggplot(data = subset(dadosTrab, Sex == "Mulher")) +
  geom_mosaic(aes(x = product(Income), fill = BMI_2)) +
  labs(title = "Relação entre os Níveis de Rendimentos e Categorias de IMC nas Mulheres",
       x = "Income",
       y = "IMC",
       fill = "Categoria de IMC") +
  theme_minimal()

#seguro de saude
ggplot(data=dadosTrab,aes(x=BMI_2,fill=AnyHealthcare)) +
  geom_bar(position="dodge")+labs(title="Frequência de Seguros de Saúde por IMC ",
                                  x= "Categorias de IMC",
                                  y= "Nº de Indivíduos",
                                  fill= "Seguros de Saúde")+
  scale_fill_manual(values = c("Sim"= "green3","Não"="red3"))

#INFLUÊNCIA DA ALIMENTAÇÃO, COLESTEROL ELEVADO
#E PRÁTICA DE EXERCÍCIO NA OBESIDADE

#alimentacao
dados_percentagem = dadosTrab %>%
  count(Alimentacao, BMI_2) %>%
  group_by(Alimentacao) %>%
  mutate(percentagem = n / sum(n) * 100)

ggplot(dados_percentagem, aes(x = Alimentacao, y = percentagem, fill = BMI_2)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", percentagem)), 
            position = position_stack(vjust = 0.5), 
            size = 3) +
  labs(title = "Distribuição Percentual das Categorias de IMC por tipo de Alimentação",
       x = "Alimentação",
       y = "Percentagem",
       fill = "Categoria de IMC") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal()

# Colesterol Alto
ggplot(dadosTrab, aes(x = BMI_2, fill = HighChol)) +
  geom_bar(position = "fill") +
  labs(title="Relação entre Colesterol Alto e IMC",
       x = "Categoria de IMC",y= "Proporção", fill = "Colesterol Alto") +
  theme_minimal()+ scale_fill_manual(values = c("Sim"= "red3","Não"="green3"))

# atividade fisica
ggplot(data=dadosTrab,aes(x=BMI_2,fill=PhysActivity)) +
  geom_bar(position="dodge") + labs(title="Distribuição da Prática de Atividade Física por Categoria de IMC",
                                    x= "Categorias de IMC",
                                    y= "Nº de Indivíduos",
                                    fill= "Atividade Física")+
  scale_fill_manual(values = c("Sim"= "green3","Não"="red3"))
