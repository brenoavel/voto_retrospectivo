##################################### Segundo Round - Voto Retrospectivo #################################

#### Pacotes ####

library(electionsBR)
library(dplyr)

#### Funções ####

#função para baixar arquivos grandes direto do R
download=function(url, file){
  library('RCurl')
  f = CFILE(file, mode="wb")
  a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
  close(f)
  return(a)
}

#### Bases ####

####Eleições 
#baixando os dados das eleições municipais de 2012, 2016 e 2020
eleicao2020 <- elections_tse(2020, type = 'vote_mun_zone')
eleicao2016 <- elections_tse(2016, type = 'vote_mun_zone')
eleicao2012 <- elections_tse(2012, type = 'vote_mun_zone')

#selecionando e renomeando as variáveis de interesse

eleicao2020 <- eleicao2020 %>% 
  rename(
    municipio = NM_UE,
    UF = SG_UF,
    Ano = ANO_ELEICAO,
    turno = NR_TURNO,
    partido = SG_PARTIDO,
    candidato = NM_CANDIDATO,
    votos = QT_VOTOS_NOMINAIS_VALIDOS,
    resultado = DS_SIT_TOT_TURNO
  ) %>% 
  filter(DS_CARGO == 'Prefeito') %>% 
  select(UF, municipio, Ano, turno, partido, candidato, votos, resultado)

eleicao2016 <- eleicao2016 %>% 
  rename(
    municipio = NM_UE,
    UF = SG_UF,
    Ano = ANO_ELEICAO,
    turno = NR_TURNO,
    partido = SG_PARTIDO,
    candidato = NM_CANDIDATO,
    votos = QT_VOTOS_NOMINAIS,
    resultado = DS_SIT_TOT_TURNO
  ) %>% 
  filter(DS_CARGO == 'Prefeito') %>% 
  select(UF, municipio, Ano, turno, partido, candidato, votos, resultado)

eleicao2012 <- eleicao2012 %>% 
  rename(
    municipio = NM_UE,
    UF = SG_UF,
    Ano = ANO_ELEICAO,
    turno = NR_TURNO,
    partido = SG_PARTIDO,
    candidato = NM_CANDIDATO,
    votos = QT_VOTOS_NOMINAIS,
    resultado = DS_SIT_TOT_TURNO
  ) %>% 
  filter(DS_CARGO == "Prefeito") %>% 
  select(UF, municipio, Ano, turno, partido, candidato, votos_total, resultado)

#criando a variável de incumbente 

eleicao12 <- eleicao2012 %>% filter(resultado == "ELEITO")
eleicao2016 <- eleicao2016 %>%
  mutate(incumbente = ifelse(candidato %in% eleicao12$candidato, 1, 0))

eleicao16 <- eleicao2016 %>% filter(resultado == "ELEITO")
eleicao2020<- eleicao20 %>%
  mutate(incumbente = ifelse(candidato %in% eleicao16$candidato, 1, 0))

#juntando os dados das duas eleições 
eleicao <- rbind(eleicao2016, eleicao2020)

#removendo alguns objetos do ambiente para limpar a memória 
rm(eleicao12, eleicao16, eleicao2012, eleicao2016, eleicao2020)

#####Receita de campanha
#baixando os dados de receita de campanha das eleições municipais de 2016 e 2020

receita2020 <- download('https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_de_contas_eleitorais_candidatos_2020.zip', 'receita2020.zip')
receita2016 <- download('https://cdn.tse.jus.br/estatistica/sead/odsele/prestacao_contas/prestacao_contas_final_2016.zip', 'receita2016.zip')

#extraindo os dados de receita das pastas baixadas
unzip('receita2016.zip', files = 'receitas_candidatos_prestacao_contas_final_2016_brasil.txt')
unzip('receita2020.zip', files = 'receitas_candidatos_2020_BRASIL.csv')

#carregando os arquivos de receita 
receita2016 <- read.table('receitas_candidatos_prestacao_contas_final_2016_brasil.txt', header = T, sep = ';', encoding = 'latin1')
receita2020 <- read.csv2('receitas_candidatos_2020_BRASIL.csv', encoding = 'latin1')

#trocando as ',' por '.' dos valores da receita na base de 2016
receita2016 <- data.frame(lapply(receita2016, function(x){
  gsub(",", ".", x)
}))

#filtrando somente os candidatos a prefeito, renomeando as variáveis que vamos utilizar e somando os
#valores de receita total por candidato

#2016
receita2016 <- receita2016 %>%
  filter(Cargo == 'Prefeito') %>% 
  rename(municipio = Nome.da.UE,
         candidato = Nome.candidato,
         valor = Valor.receita,
         CPF = CPF.do.candidato
  ) %>% 
  group_by(UF, municipio, candidato, CPF) %>% 
  summarise(receita_total = sum(as.numeric(valor), na.rm = T)) %>% 
  mutate(Ano = "2016")

#2020
receita2020 <- receita2020 %>%
  rename(
    candidato = NM_CANDIDATO,
    valor = VR_RECEITA,
    municipio = NM_UE,
    CPF = NR_CPF_CANDIDATO,
    UF = SG_UF, 
    turno = ST_TURNO
  ) %>%
  filter(DS_CARGO == "Prefeito", TP_PRESTACAO_CONTAS == 'FINAL') %>%
  group_by(UF, municipio, candidato, CPF) %>%
  summarise(
    receita_total = sum(as.numeric(valor))
  ) %>% 
  mutate(Ano = "2020")

#transformando a variável cpf em character
receita2020$CPF <- as.character(receita2020$CPF)

#juntando os dados de receita das duas eleições 
receita <- rbind(receita2016, receita2020)

#removendo alguns objetos do ambiente para limpar a memória 
rm(receita2016, receita2020)

####Criando base com os dados de partido dos governadores 

gov16 <- data.frame(UF = c('AC', 'AL', 'AP', 'AM', 'BA', 'DF', 'CE', 'ES', 'GO', 'MA', 'MT', 'MS', 'MG', 'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN',
                           'RS', 'SC', 'RO', 'RR', 'SP', 'SE', 'TO'),
                    partido_gov = c('PT', 'MDB', 'PDT', 'PROS', 'PT', 'PSB', 'PT', 'MDB', 'PSDB', 'PCdoB', 'PSDB', 'PSDB', 'PT', 'PSDB', 'PSB', 
                                    'PSDB', 'PSB', 'PT', 'MDB', 'PSD', 'MDB', 'PSD', 'MDB', 'PP', 'PSDB', 'MDB', 'MDB')
)

gov16 <- gov16 %>%
  mutate(Ano = 2016)

gov20 <- data.frame(UF = c('AC', 'AL', 'AP', 'AM', 'BA', 'DF', 'CE', 'ES', 'GO', 'MA', 'MT', 'MS', 'MG', 'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN',
                           'RS', 'SC', 'RO', 'RR', 'SP', 'SE', 'TO'),
                    partido_gov = c('PP', 'MDB', 'PSDB', 'PSDB', 'PT', 'PSD', 'PT', 'PSB', 'DEM', 'PDT', 'PSDB', 'PSDB', 'NOVO', 'MDB', 'PSB', 
                                    'PL', 'PSB', 'PT', 'PSC', 'PT', 'PSDB', 'PSDB', 'PROS', 'REP', 'PSDB', 'PSD', 'PSDB'))

gov20 <- gov20 %>%
  mutate(Ano = 2020)

gov <- rbind(gov16, gov20)

#### IDHM
  














