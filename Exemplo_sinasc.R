####################################################
#### Informação em Saúde I - ISC233 
#### Manipulação de base de dados
#### Analise descritiva
#### Profa. Natalia S Paiva IESC UFRJ
#### SINASC, UF = RJ, ANO = 2019  
#### Filtro: Munic Residencia: Rio de Janeiro
######################################################


# encoding 

####
## PASSO 1 - MUDAR DIRETÓRIO
####

# Session >> Set working Directory >> chooese directory...

####
## PASSO 2 - CHAMAR PACOTES
####

# caso não tenha esses pacotes instalados em sua maquina, por favor, instale.

install.packages("nome.do.pacote")

# CHAMAR NA BIBLIOTECA TODA VEZ QUE VOCÊ FOR USAR
library(rio) # importar base de dados de diferentes extensoes
library(tidyr) # comando replace_na
library(dplyr) # manipular base de dados usando %>%
library(lubridate) # trabalhar com data
library(gtsummary) # tabelas
library(gt) # nota de rodapé na tabela
library(ggplot2) # graficos

####
## PASSO 3 -  IMPORTANDO BASE DE DADOS
####

# usando pacote microdatasus (SALDANHA et al, 2019)

#remotes::install_github("rfsaldanha/microdatasus") # apenas 1 vez na maquina
library(microdatasus)
# mais detalhes em https://github.com/rfsaldanha/microdatasus

dados2018 <- fetch_datasus(year_start = 2018, year_end = 2018, uf = "RJ",
                       information_system = "SINASC")


dados2019 <- fetch_datasus(year_start = 2019, year_end = 2019, uf = "RJ",
                           information_system = "SINASC")
  

# colocar dados 2018 e 2019 em um unico objeto; um embaixo do outro
dados <- bind_rows(dados2018, dados2019)

rm(dados2018,dados2019)

# olhar nomes das variaveis
names(dados)

# dimensoes do objeto
dim(dados) #numero de linhas e colunas

# olhar as 10 primeiras linhas do objeto 
head(dados, n = 10)

####
## PASSO 4 - SELECIONAR APENAS AS VARIAVEIS QUE TENHO INTERESSE
####

# selecionando as variaveis que vamos trabalhar: 

# filtro: Municipio de Residencia (CODMUNRES) == Rio de Janeiro (330455)

# vamos selecionar "CODMUNRES", "IDADEMAE", "ESTCIVMAE", "ESCMAE", "RACACORMAE", "LOCNASC",    
# "GESTACAO", "CONSULTAS",  "GRAVIDEZ", "PARTO", "DTNASC", "SEXO", "RACACOR", "PESO"


sinasc <- dados %>% filter( CODMUNRES == "330455" ) %>% 
                    select("CODMUNRES", "IDADEMAE", "ESTCIVMAE", "ESCMAE", "RACACORMAE", "LOCNASC",    
                            "GESTACAO", "CONSULTAS",  "GRAVIDEZ", "PARTO", "DTNASC", "SEXO", "RACACOR", "PESO")

# remover objeto dados
rm(dados)

# estrutura do banco de dados

glimpse(sinasc)

####
## PASSO 5 - RECLASSIFICAR AS VARIAVEIS
####

# OBS : para datas
# estrutura =  ddmmaaaa ; format = "%d%m%Y"
# estrutura =  dd/mm/aaaa ; format = "%d/%m/%Y"
# estrutura =  dd - mm - aaaa ; format = "%d-%m-%Y"
# estrutura =  mm/dd/aaaa ; format = "%m-%d-%Y"
# estrutura =  dd/mm/aa ; format = "%d-%m-%y"

# ver mais em: https://www.r-bloggers.com/2013/08/date-formats-in-r/

# variavel categorica = no R: factor (respostas forem "nomes"/categorias)
# variavel numerica = no R: primeiro colocar como character e depois numeric (idade, peso, altura...)
# variavel data = no R: Date
# texto (ex: endereco) = no R: character

# obs: para data, sempre veja como esta a estrutura antes de reclass
sinasc$DTNASC[1:10] # ddmmaaaa



sinasc <- sinasc %>%
  mutate(LOCNASC = as.factor(LOCNASC), 
         ESTCIVMAE =  as.factor(ESTCIVMAE), 
         ESCMAE = as.factor(ESCMAE),
         GESTACAO = as.factor(GESTACAO),
         GRAVIDEZ = as.factor(GRAVIDEZ),
         PARTO = as.factor(PARTO),
         RACACORMAE = as.factor(RACACORMAE), 
         SEXO =  as.factor(SEXO), 
         DTNASC = as.Date(DTNASC, format = "%d%m%Y"),
         IDADEMAE = as.character(IDADEMAE),
         IDADEMAE = as.numeric(IDADEMAE),
         PESO = as.character(PESO),
         PESO = as.numeric(PESO))



# vamos conferir se as class mudaram?
class(sinasc$LOCNASC)
class(sinasc$IDADEMAE)
class(sinasc$DTNASC)


# Olhe a variavel data para ver se esta ok! tem que aparecer  aaaa-mm-dd 

sinasc$DTNASC[1:10]

####
## PASSO 6 - RENOMEAR AS CATEGORIAS (olhar dicionario) 
####

# OBS 1: SEXO - 1 = Masculino ; 2 = Feminino
levels(sinasc$LOCNASC)
levels(sinasc$ESTCIVMAE)
levels(sinasc$ESCMAE)
levels(sinasc$GESTACAO)
levels(sinasc$GRAVIDEZ)
levels(sinasc$CONSULTAS)
levels(sinasc$PARTO)
levels(sinasc$RACACORMAE)
levels(sinasc$SEXO)

sinasc <- sinasc %>%
  mutate(LOCNASC = recode(LOCNASC,
                        "1"= "Hospital",
                        "2"= "Outros estabelecimentos de saúde",
                        "3"= "Domicílio",
                        "4"= "Outros",
                        "9"= "Ignorado"), 
         ESTCIVMAE =  recode(ESTCIVMAE,
                        "1"= "Solteira",
                        "2"= "Casada",
                        "3"= "Viúva",
                        "4"= "Separada judicialmente/divorciada",
                        "5"= "União estável",
                        "9"= "Ignorado"), 
         ESCMAE = recode(ESCMAE,
                        "1"= "Nenhuma",
                        "2"= "1 a 3 anos",
                        "3"= "4 a 7 anos",
                        "4"= "8 a 11 anos",
                        "5"= "12 e mais",
                        "9"= "Ignorado"),
         GESTACAO = recode(GESTACAO,
                        "1"= "Menos de 22 semanas",
                        "2"= "22 a 27 semanas",
                        "3"= "28 a 31 semanas",
                        "4"= "32 a 36 semanas",
                        "5"= "37 a 41 semanas",
                        "6"= "42 semanas e mais",
                        "9"= "Ignorado"),
         GRAVIDEZ = recode(GRAVIDEZ,
                        "1"= "Única",
                        "2"= "Dupla",
                        "3"= "Tripla ou mais",
                        "9"= "Ignorado"),
         CONSULTAS = recode(CONSULTAS,
                        "1"= "Nenhuma",
                        "2"= "de 1 a 3",
                        "3"= "de 4 a 6",
                        "4"= "7 e mais",
                        "9"= "Ignorado"),
         PARTO = recode(PARTO,
                        "1"= "Vaginal" ,
                        "2"= "Cesário",
                        "9"= "Ignorado"),
         RACACORMAE = recode(RACACORMAE,
                        "1"= "Branca",
                        "2"= "Preta",
                        "3"= "Amarela",
                        "4"= "Parda",
                        "5"= "Indígena"),
          SEXO =  recode(SEXO, 
                        "0"= "Ignorado",
                        "1"= "Feminino",
                        "2"= "Masculino") )%>%   
  mutate(fx_etaria = cut(IDADEMAE,  # CRIANDO FAIXA ETARIA
                         breaks = c(10, 20, 35, Inf), right = FALSE,
                         labels = c("10 a 19", "20 a 34 anos", "35 ou mais"))) %>%   
  mutate(peso.cat = cut(PESO,  # CRIANDO FAIXA ETARIA
                         breaks = c(0, 2500, 3000, 4000, Inf), right = FALSE,
                         labels = c("Menos de 2500 g", "2500 g a 2999 g",
                                    "3000 g a 3999 g", "4000 g ou mais")))
  
class(sinasc$fx_etaria)
class(sinasc$peso.cat)

# faca um tabela ou summary para ver se foi tudo certo
summary(sinasc)


#####
# Passo 7 - remover variavel "CODMUNRES" 
#####

sinasc <- sinasc %>% select(- CODMUNRES)


#####
# Passo 8 - Colocar o NA (missing) em "Não preenchido"
#####

# exemplo variavel PARTO
summary(sinasc$PARTO)
levels(sinasc$PARTO)

sinasc <- sinasc %>%
  mutate(PARTO = factor(PARTO, levels  = c("Vaginal", "Cesário","Ignorado", "Não preenchido"))) %>%
  mutate(PARTO = replace_na(PARTO, "Não preenchido"))

summary(sinasc$PARTO)


#####
# PASSO 9 - Mudar ordem das categorias (se necessario)
####     

# para LOCNASC queremos a ordem "Hospital", "Domicílio", "Outros estabelecimentos de saúde",
# "Outros", "Ignorado


levels(sinasc$LOCNASC)

sinasc <- sinasc %>% mutate( LOCNASC = factor(LOCNASC, 
                                       levels = c("Hospital", "Domicílio",
                                                  "Outros estabelecimentos de saúde",
                                                   "Outros", "Ignorado")))


levels(sinasc$LOCNASC)


summary(sinasc$LOCNASC)

# Sexo  = Fem, Masc, Ign

levels(sinasc$SEXO)

sinasc <- sinasc %>% mutate( SEXO = factor(SEXO, 
                                              levels = c("Feminino",  "Masculino", "Ignorado")))


levels(sinasc$SEXO)


summary(sinasc$SEXO)

##### 
# PASSO 10 - Juntar categorias na variavel 
######

# RACACORMAE: criar categoria Outras


# Colocar Amarela ou Indígena na categoria Outras

levels(sinasc$RACACORMAE)
summary(sinasc$RACACORMAE)

sinasc <- sinasc %>% mutate(RACACORMAE = factor(RACACORMAE, 
                                             levels = c("Branca", "Preta",
                                                        "Amarela", "Parda",
                                                        "Indígena", "Outras")))

summary(sinasc$RACACORMAE)

sinasc$RACACORMAE[ sinasc$RACACORMAE == "Amarela" | sinasc$RACACORMAE == "Indígena"] <- "Outras"

summary(sinasc$RACACORMAE)

# Excluir/remover categorias com Frequencia = 0

sinasc$RACACORMAE <- factor(sinasc$RACACORMAE)

summary(sinasc$RACACORMAE)


# LOCNASC: juntar Outros estabelecimentos de saúde e Outros

levels(sinasc$LOCNASC)

summary(sinasc$LOCNASC)

sinasc$LOCNASC[ sinasc$LOCNASC == "Outros estabelecimentos de saúde" ] <- "Outros"

summary(sinasc$LOCNASC)

# Excluir/remover categorias com Frequencia = 0

sinasc$LOCNASC <- factor(sinasc$LOCNASC)

summary(sinasc$LOCNASC)

###### 
# PASSO 11 - Criar variavel nova
######

# Criar variavel "Hospital.nasc";
# Se local de nascimento for = "Hospital", variavel "Hospital.nasc" recebe "Hospital, 
# caso contrario recebe "Outros locais".

levels(sinasc$LOCNASC)

sinasc <- sinasc %>%
  mutate(Hospital.nasc = ifelse(LOCNASC == "Hospital", "Sim", "Não")) %>%
   mutate( Hospital.nasc = factor(Hospital.nasc, levels = c("Sim", "Não")) )


summary(sinasc$Hospital.nasc)

# Criar variavel "Hospital_ig.nasc";
# Se local de nascimento for = "Hospital", 
# variavel "Hospital_ig.nasc" recebe "Sim", 
# Se local de nascimento for = "Ignorado", 
# "Hospital_ig.nasc" recebe "Ignorado",
# caso contrario "Hospital_ig.nasc" recebe "Não" 
# (ie, locnasc= "Domicílio" ou"Outros").

sinasc <- sinasc %>%
  mutate(Hospital_ig.nasc = ifelse(LOCNASC == "Hospital", "Sim", 
                                   ifelse(LOCNASC == "Ignorado", "Ignorado", "Não"))) %>%
  mutate( Hospital_ig.nasc = factor(Hospital_ig.nasc, levels = c("Sim", "Não", "Ignorado")) )


summary(sinasc$Hospital_ig.nasc)



###### 
# PASSO 12 - Trabalhar com IDADEMAE
###### 

# resumo 
# usando dplyr e %>%
sinasc %>% summarise(Média = mean(IDADEMAE),
                    DP = sd(IDADEMAE), # desvio-padrão
                    Mediana = median(IDADEMAE),
                    Q1 = quantile(IDADEMAE, prob = 0.25),
                    Q3 = quantile(IDADEMAE, prob = 0.75))

# vamos olhar o resumo da idade segundo tipo de parto
sinasc %>% group_by(PARTO) %>%
  summarise(Média = mean(IDADEMAE),
            DP = sd(IDADEMAE),
            Mediana = median(IDADEMAE),
            Q1 = quantile(IDADEMAE, prob = 0.25),
            Q3 = quantile(IDADEMAE, prob = 0.75))

# vamos olhar o resumo da idade segundo tipo de parto sem ignorado e nao preench
sinasc %>% filter( ! (PARTO %in% c("Ignorado", "Não preenchido")) ) %>%
   group_by(PARTO) %>%
    summarise(Média = mean(IDADEMAE),
            DP = sd(IDADEMAE),
            Mediana = median(IDADEMAE),
            Q1 = quantile(IDADEMAE, prob = 0.25),
            Q3 = quantile(IDADEMAE, prob = 0.75))


###### 
# PASSO 13 - Trabalhar com datas
###### 


# Criar a variavel  Ano de nascimento "Ano.nasc", fazer tabela e grafico
sinasc <- sinasc %>%
  mutate(Ano.nasc = year(DTNASC))

table(sinasc$Ano.nasc)
barplot(table(sinasc$Ano.nasc), col = "lightblue")


###### 
# PASSO 14 - Tabelas
###### 

options(OutDec = ",") # decimal com virgula

sinasc %>%
  select(ESTCIVMAE, ESCMAE, fx_etaria) %>%
   tbl_summary()

sinasc %>%
  select(ESTCIVMAE, ESCMAE, RACACORMAE, fx_etaria) %>%
   tbl_summary(missing_text = "Sem informação", # colocando "Sem informação" no NA
              digits = list(all_categorical() ~ c(0,1)), # n sem casa decimal e % com 1 casa decimal
              label = list(ESTCIVMAE ~ "Estado civil",
                           ESCMAE ~ "Escolaridade",
                           RACACORMAE ~ "Raça ou Cor",
                           fx_etaria ~ "Faixa etária (em anos)"),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "**Características maternas**") %>%
  modify_caption("Tabela 1: Distribuição das características maternas no município do Rio de Janeiro/ RJ em 2018-2019") %>%
  modify_footnote(
    all_stat_cols() ~ "Frequência (%)"
  ) %>%
  bold_labels() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte dos dados: MS/SVS/DASIS - Sistema de Informações sobre Nascidos Vivos - SINASC*"))
    


sinasc %>%
  select("LOCNASC", "GESTACAO", "CONSULTAS", "GRAVIDEZ",     
         "PARTO", "SEXO", "peso.cat" ) %>%
  tbl_summary(missing_text = "Sem informação", # colocando "Sem informação" no NA
              digits = list(all_categorical() ~ c(0,1)), # n sem casa decimal e % com 1 casa decimal
              label = list(LOCNASC ~ "Local do nascimento",
                           GESTACAO ~ "Duração da gestação",
                           CONSULTAS ~ "Consultas pré-natal",
                           GRAVIDEZ ~ "Tipo de gravidez",
                           PARTO ~ "Tipo de parto",
                           SEXO ~ "Sexo",
                           peso.cat ~ "Peso ao nascer (em gramas)"),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "**Características**") %>%
  modify_caption("Tabela 2: Distribuição das características neonatais e 
                 assistenciais dos nascidos vivos no município do Rio de Janeiro/ RJ em 2018-2019.") %>%
  modify_footnote( all_stat_cols() ~ "Frequência (%)") %>%
  bold_labels() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte dos dados: MS/SVS/DASIS - Sistema de Informações sobre Nascidos Vivos - SINASC*"))



sinasc %>%
  filter( !(PARTO %in% c("Ignorado", "Não preenchido"))) %>% # sem ign e nao preenc
   mutate( PARTO = factor(PARTO)) %>% 
    select("ESTCIVMAE", "ESCMAE", "RACACORMAE", "fx_etaria", 
           "Hospital.nasc", "GESTACAO", "CONSULTAS", "GRAVIDEZ", "PARTO", "SEXO", "peso.cat" ) %>%
     tbl_summary(by= PARTO,
              missing_text = "Sem informação", # colocando "Sem informação" no NA
              digits = list(all_categorical() ~ c(0,1)), # n sem casa decimal e % com 1 casa decimal
              label = list(ESTCIVMAE ~ "Estado civil",
                           ESCMAE ~ "Escolaridade",
                           RACACORMAE ~ "Raça ou Cor da mãe",
                           fx_etaria ~ "Faixa etária (em anos)",
                           Hospital.nasc ~ "Nascimento no Hospital",
                           GESTACAO ~ "Duração da gestação",
                           CONSULTAS ~ "Consultas pré-natal",
                           GRAVIDEZ ~ "Tipo de gravidez",
                           SEXO ~ "Sexo",
                           peso.cat ~ "Peso ao nascer (em gramas)"),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "**Características**") %>%
  modify_caption("Tabela 3: Distribuição das características maternas, neonatais e 
                 assistenciais dos nascidos vivos segundo Tipo de Parto no município do Rio de Janeiro/ RJ em 2018-2019") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Tipo de Parto**") %>%
  add_overall(col_label = "**Total**") %>%
  add_p() %>%
  bold_labels() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte dos dados: MS/SVS/DASIS -
                             Sistema de Informações sobre Nascidos Vivos -
                             SINASC*"))


# colocando todos os Ignorados e Nao preenchidos como NA (missing)
# e colocando o texto = "Ignorado ou Não preenchido"
# nao entram p calculo de %


sinasc %>%  
  select("ESTCIVMAE", "ESCMAE", "RACACORMAE", "fx_etaria", 
         "Hospital.nasc", "GESTACAO", "CONSULTAS", "GRAVIDEZ",
         "PARTO", "SEXO", "peso.cat") %>%
   mutate_all(funs(replace(., .== "Ignorado" | .== "Não preenchido", NA))) %>%
     mutate(across(where(is.factor) , factor)) %>%
      tbl_summary(by= PARTO,
              missing_text = "Ignorado ou Não preenchido", # colocando "Sem informação" no NA
              digits = list(all_categorical() ~ c(0,1)), # n sem casa decimal e % com 1 casa decimal
              label = list(ESTCIVMAE ~ "Estado civil",
                           ESCMAE ~ "Escolaridade",
                           RACACORMAE ~ "Raça ou Cor da mãe",
                           fx_etaria ~ "Faixa etária (em anos)",
                           Hospital.nasc ~ "Nascimento em Hospital",
                           GESTACAO ~ "Duração da gestação",
                           CONSULTAS ~ "Consultas pré-natal",
                           GRAVIDEZ ~ "Tipo de gravidez",
                           SEXO ~ "Sexo",
                           peso.cat ~ "Peso ao nascer (em gramas)"),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
    modify_header(label ~ "**Características**") %>%
    modify_caption("Tabela 3: Distribuição das características maternas, neonatais e 
                 assistenciais dos nascidos vivos segundo Tipo de Parto no município do Rio de Janeiro/ RJ em 2018-2019") %>%
    modify_spanning_header(c("stat_1", "stat_2") ~ "**Tipo de Parto**") %>%
    add_overall(col_label = "**Total**") %>%
    add_p() %>%
    bold_labels() %>%
    as_gt() %>%
    gt::tab_source_note(gt::md("*Fonte dos dados: MS/SVS/DASIS -
                             Sistema de Informações sobre Nascidos Vivos -
                             SINASC*"))




### graficos


# Grafico para Faixa etaria da mãe

ggplot(sinasc) +
  aes(x = fx_etaria) +
  geom_bar(fill= "darkgreen") +
  labs(x = "Faixa etária da mãe", y = "Frequência", 
       title = "Faixa etária (em anos) da mães dos Nascidos Vivos", 
       subtitle = "MRJ, 2018-2019",
       caption = "Fonte dos dados: SINASC") +
  theme_minimal() # minimal com linhas de grades suaves


# Grafico de colunas para Tipo de Parto
# inserindo freq absoluta nas colunas

ggplot(sinasc, aes(x= PARTO)) +  
  geom_bar(fill = "navyblue")+ # cor azul escura
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = 1.5, colour = "white", size= 3.5)+ # inserindo freq. absoluta, posicao= 1.5 (dentro da coluna), cor branca e tamanho 3.5
  labs(x = "Tipo de parto", y= "Nº de Nascidos Vivos",
       title= "Figura 1: Número de NV segundo Tipo de Parto",
       subtitle = "MRJ, 2018-2019",
       caption= "Fonte dos dados: SINASC")+
  theme_classic() # classic sem linhas de grades 


# Grafico de colunas para Tipo de Parto sem Ignorado e Não preench
# diminuindo largura da coluna
# colocando tamanho da fonte = 12

sinasc %>%
  filter(! PARTO %in% c("Ignorado", "Não preenchido")) %>%
ggplot() +
  aes(x= PARTO) +  
  geom_bar(fill = "lightblue", width= 0.4)+ # Diminuir largura da barra: width=0.4
  geom_text(aes(label = paste("n=", ..count..)), stat = "count", 
            vjust = 1.5, colour = "navyblue", size= 4.5)+ # inserindo freq. absoluta, posicao= 1.5 (dentro da coluna), cor vermelha e tamanho 3.5
   labs(x = "Tipo de parto", y= "Nº de Nascidos Vivos",
       title= "Figura 1: Número de NV segundo Tipo de Parto",
       subtitle = "MRJ, 2018-2019",
       caption= "Fonte dos dados: SINASC")+
  theme_minimal()+ # minimal com linhas de grades suaves
  theme(text = element_text(size=12))  # tamanho das letras = 12


# ver possiveis temas
# https://ggplot2.tidyverse.org/reference/ggtheme.html


# grafico de colunas para TIPO DE PARTO segundo Fx etaria (sem ignorado e nao preenc)
#sem Ignorado e Não preench


sinasc %>%
  filter(! PARTO %in% c("Ignorado", "Não preenchido")) %>%
  ggplot() +
  aes(x = PARTO, fill = fx_etaria) +
  geom_bar(position = "dodge") + # barras lado a lado
  scale_fill_viridis_d(option = "viridis") + # paleta de cores
  labs(x = "Tipo de parto", y= "Nº de Nascidos Vivos",
       title= "Figura 1: Distribuição dos NV segundo a Faixa Etária materna e Tipo de Parto",
       subtitle = "MRJ, 2018-2019",
       fill = "Faixa etária materna (em anos)",
       caption= "Fonte dos dados: SINASC")+
  theme_bw() +
  theme(legend.position= "bottom") # legenda embaixo

# grafico de colunas para TIPO DE PARTO segundo Fx etaria (sem ignorado e nao preenc)
#sem Ignorado e Não preench
# mudar posicao da legenda

sinasc %>%
  filter(! PARTO %in% c("Ignorado", "Não preenchido")) %>%
  ggplot() +
  aes(x = PARTO, fill = fx_etaria) +
  geom_bar(position = "dodge") + # barras lado a lado
  scale_fill_viridis_d(option = "viridis") + # paleta de cores
  labs(x = "Tipo de parto", y= "Nº de Nascidos Vivos",
       title= "Figura 1: Distribuição dos NV segundo a Faixa Etária materna e Tipo de Parto",
       subtitle = "MRJ, 2018-2019",
       fill = "Faixa etária materna (em anos)",
       caption= "Fonte dos dados: SINASC")+
  theme_bw() +
  theme(legend.position= "top") # legenda em cima

# posicoes da legenda legend.position: "top", "bottom", "right", "left"

# mudar cores do scale_fill_viridis_d
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html


# Boxplot PESO segundo Tipo de Parto
ggplot(sinasc) +
  aes(x = "", y = PESO, fill = PARTO) +
  geom_boxplot() +
  scale_fill_viridis_d() + # cor
  labs(x = "Tipo de parto", y= "Nº de Nascidos Vivos",
       title= "Figura 1: Distribuição dos Pesos (em gramas) dos NV segundo Tipo de Parto",
       subtitle = "MRJ, 2018-2019",
       fill = "Tipo de Parto",
       caption= "Fonte dos dados: SINASC") +
  theme_minimal() +
  theme(legend.position = "top")


# Boxplot PESO segundo Tipo de Parto 
# sem Ignorado e Não preench

sinasc %>%
  filter(! PARTO %in% c("Ignorado", "Não preenchido")) %>%
  ggplot() +
  aes(x = "", y = PESO, fill = PARTO) +
  geom_boxplot() +
  scale_fill_hue() + # cor
  labs(x = "Tipo de parto", y= "Nº de Nascidos Vivos",
       title= "Figura 1: Distribuição dos Pesos (em gramas) dos NV segundo Tipo de Parto",
       subtitle = "MRJ, 2018-2019",
       fill = "Tipo de Parto",
       caption= "Fonte dos dados: SINASC") +
  theme_minimal() +
  theme(legend.position = "top")

# Boxplot (violino) PESO segundo SEXO
# sem ignorado 


sinasc %>%
  filter( !(SEXO %in% c("Ignorado")) ) %>%
  ggplot() +
  aes(x = "", y = PESO, fill = SEXO) +
  geom_violin(adjust = 1L, scale = "area") +
  scale_fill_brewer(palette = "Set2") + # cor
  labs(x = "", y= "Peso (em gramas)",
       title= "Figura 1: Distribuição dos Pesos (em gramas) dos NV segundo Sexo",
       subtitle = "MRJ, 2018-2019",
       fill = "Sexo", # titulo da legenda
       caption= "Fonte dos dados: SINASC") +
  theme_classic()

# ver corer p comando scale_fill_brewer
# https://ggplot2.tidyverse.org/reference/scale_brewer.html


# Grafico de barras: Distribuição da Raça ou Cor materna
ggplot(sinasc) +
  aes(x = RACACORMAE) +
  geom_bar(fill= "chocolate3") + # cor chocolate3
  coord_flip() + # cria um grafico de coluna e vira para ser um graf d barra
  labs(x= "Raça ou Cor materna",
       y = "Nº de nascidos vivos", 
       title= "Figura 1: Distribuição da Raça ou Cor materna",
       subtitle = "MRJ, 2018-2019",
         caption= "Fonte dos dados: SINASC") +
  theme_classic(base_size = 12) # muda tamanho das letras


# Grafico de barras: Distribuição da Raça ou Cor materna
# sem NA

sinasc %>%
  filter ( !is.na(RACACORMAE) ) %>%
ggplot() +
  aes(x = RACACORMAE) +
  geom_bar(fill= "chocolate3") + # cor chocolate3
  coord_flip() + # cria um grafico de coluna e vira para ser um graf d barra
  labs(x= "Raça ou Cor materna",
       y = "Nº de nascidos vivos", 
       title= "Figura 1: Distribuição da Raça ou Cor materna",
       subtitle = "MRJ, 2018-2019",
       caption= "Fonte dos dados: SINASC") +
  theme_classic(base_size = 12) # muda tamanho das letras


# Grafico de barras: Distribuição da Raça ou Cor materna
# sem NA
# colocar em ordem crescente

tab.raca <- sinasc %>%   
  filter ( !is.na(RACACORMAE) ) %>% 
  group_by(RACACORMAE) %>% tally() %>%
  print()

tab.raca %>%
ggplot() +
  aes(x = reorder(RACACORMAE, n), y = n, goup= 1) +
  geom_bar(stat = "identity") + 
  coord_flip() + # cria um grafico de coluna e vira para ser um graf d barra
  labs(x= "Raça ou Cor materna",
       y = "Nº de nascidos vivos", 
       title= "Figura 1: Distribuição da Raça ou Cor materna",
       subtitle = "MRJ, 2018-2019",
       caption= "Fonte dos dados: SINASC") +
  theme_classic(base_size = 12) # muda tamanho das letras


# Grafico de barras: Distribuição da Raça ou Cor materna
# sem NA
# colocar em ordem crescente

tab.raca <- sinasc %>%   
  filter ( !is.na(RACACORMAE) ) %>% 
  group_by(RACACORMAE) %>% tally() %>%
  print()

tab.raca %>%
  ggplot() +
  aes(x = reorder(RACACORMAE, - n), y = n, goup= 1) +
  geom_bar(stat = "identity", fill= "navyblue") +  # cor azul marinho
  coord_flip() + # cria um grafico de coluna e vira para ser um graf d barra
  labs(x= "Raça ou Cor materna",
       y = "Nº de nascidos vivos", 
       title= "Figura 1: Distribuição da Raça ou Cor materna",
       subtitle = "MRJ, 2018-2019",
       caption= "Fonte dos dados: SINASC") +
  theme_classic(base_size = 12) # muda tamanho das letras


# Grafico de barras: Distribuição numero de consultas pre natal com texto

tab.consultas <- sinasc %>%
  group_by(CONSULTAS) %>%
  tally() %>% print()


ggplot(tab.consultas) +
  aes(x = CONSULTAS, y = n) +
  geom_bar(stat="identity", fill= "chocolate1") + # cor chocolate
  coord_flip() +
  ylim(0, max(tab.consultas$n)+15000) + # ESSE VALOR MUDA DE ACORDO COM VALOR MAXIMO DOS DADOS
  labs(x= "Consultas pré-natal",
       y = "Nº de nascidos vivos", 
       title= "Figura 1: Distribuição do Nº de Consultas pré-natal",
       subtitle = "MRJ, 2018-2019",
       caption= "Fonte dos dados: SINASC") +
  geom_text(aes(label= paste("n =", n)), hjust = - 0.2, 
            size= 4.0, color= "black", fontface = "bold")+ # INSERINDO TEXTO COM CONTAGEM
  theme_classic(base_size = 12)


# Grafico de pizza / setor: sexo - sem ignorado

tab.sexo <- sinasc %>% filter (! SEXO %in% c("Ignorado")) %>%
  group_by(SEXO) %>%
  tally() %>% print()

tab.sexo <- tab.sexo %>%
  mutate(Porcentagem = scales::percent(n/sum(n))) %>% print()



ggplot(tab.sexo) +
  aes(x="", y= n, fill=SEXO) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + # faz virar pizza
  scale_fill_brewer(palette = "OrRd") + # mudar a cor
  labs(x= "",
       y = "", 
       title= "Figura 1: Distribuição do Sexo dos NV",
       subtitle = "MRJ, 2018-2019",
       fill= "Sexo",
       caption= "Fonte dos dados: SINASC") +
  geom_text(aes(label = paste(Porcentagem, "( n=",n,")")), size=4, 
            position = position_stack(vjust = 0.5)) + 
  theme_void()

# cores brewer: http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html

