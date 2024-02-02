##################################### Segundo Round - Voto Retrospectivo #################################

#### Pacotes ####

library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(descr)
library(geobr)
library(ggthemes)
library(stargazer)
library(gridExtra)

#### Bases ####

base <- readRDS('base.Rds')
mapa <- mapa <- read_municipality(
  year = 2020,
  showProgress = F
)

pop <- read.csv('população.csv')

#### Preparação da base ####

# Dataset principal
Segund <- base %>% 
  filter(turno==1) %>% 
  mutate(STurno = ifelse(eleitorado20 >200000, "Sim", "Não"),
         seg_turno16 = ifelse(eleitorado16 >200000, "Sim", "Não"),
         seg_turno20 = ifelse(eleitorado20 >200000, "Sim", "Não"),
         DTratado =  ifelse(STurno=="Sim", eleitorado20-200000, 0),
         DNTratado =  ifelse(STurno=="Não", eleitorado20-200000, 0),
         Apoio = ifelse(partido==partido_gov, "Sim", "Não")) %>% 
  select(-desastre)

# Dataset do mapa

mapa <- rename(mapa, 
               municipio = name_muni,
               id_municipio = code_muni,
               UF = abbrev_state
) %>% 
  select(municipio, id_municipio, UF, geom)

mapa$municipio <- toupper(mapa$municipio)

map <- Segund %>% 
  select(municipio, id_municipio, UF, Ano, seg_turno16, seg_turno20) %>% 
  unique()

# 2016
map16 <- filter(map, Ano == 2016) %>%
  mutate(`Segundo turno` = case_when(seg_turno16 == 'Sim' ~ 'Com', .default = 'Sem')) %>% 
  select(-Ano)

map16$`Segundo turno` <- as.character(map16$`Segundo turno`)

map16 <- left_join(mapa, map16, by = c('municipio', 'id_municipio', 'UF'))
map16$`Segundo turno`[is.na(map16$`Segundo turno`)] <- 'Sem'

# 2020

map20 <- filter(map, Ano == 2020) %>%
  mutate(`Segundo turno` = case_when(seg_turno20 == 'Sim' ~ 'Com', .default = 'Sem')) %>% 
  select(-Ano)
map20$`Segundo turno` <- as.character(map20$`Segundo turno`)

map20 <- left_join(mapa, map20, by = c('municipio', 'id_municipio', 'UF'))
map20$`Segundo turno`[is.na(map20$`Segundo turno`)] <- 'Sem'

#### Mapa ####

colors <- c("#FE9B2B", ggplot2::alpha('gray100', 0.3))

# 2016

ggplot(map16) +
  geom_sf(aes(fill = `Segundo turno`), color = ggplot2::alpha('gray90', 0.2)) +
  theme_map() +
  scale_fill_manual(values = colors)

# 2020

ggplot(map20) +
  geom_sf(aes(fill = `Segundo turno`), color = ggplot2::alpha('gray90', 0.2)) +
  theme_map() +
  scale_fill_manual(values = colors)






#### Descritivo ####

# dispersão VI e VD

Segund %>%
  filter(incumbente == 1) %>% 
  select(eleitorado20, per_votos, STurno, Ano) %>% 
  mutate(eleitorado20 = eleitorado20/1000) %>% 
  na.omit() %>% 
  ggplot(aes(x = eleitorado20, y = per_votos,)) +
  geom_point(aes(color = STurno)) + geom_smooth(method = "lm", se = FALSE, color = "#CC0000") +
  scale_x_continuous(breaks = seq(0,500, 100), limits = c(0,500))+
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0, 100))+
  scale_color_manual(values = c("#A2A19F", "#FE9B2B"), name = 'Segundo turno') +
  labs(x = 'Eleitorado (Mil)', y = 'Votos (%)') +
  theme_minimal() +
  facet_wrap(vars(Ano))

# receita/eleitor por voto com e sem segundo turno

Segund %>% 
  filter(incumbente == 1) %>% 
  select(receita_total, eleitorado20, per_votos, STurno, Ano) %>% 
  mutate(receita = receita_total/eleitorado20) %>% 
  na.omit() %>% 
  ggplot(aes(x = receita, y = per_votos)) +
  geom_point(aes(color = STurno)) + geom_smooth(method = "lm", se = FALSE, color = "#CC0000") +
  #scale_x_continuous(breaks = seq(0,70, 10), limits = c(0,70))+
  #scale_y_continuous(breaks = seq(0,100,10), limits = c(0, 100))+
  scale_color_manual(values = c("#A2A19F", ggplot2::alpha("#FE9B2B", 0.3)), name = 'Segundo turno') +
  labs(x = 'Receita/eleitor', y = 'Votos (%)') +
  theme_minimal() +
  facet_wrap(vars(Ano))

test <- Segund %>% 
      filter(incumbente == 1)
  
t <- t.test(test$per_votos ~ test$STurno, var.equal = T)

# pib per capita por voto

pop <- pop %>% 
  rename(UF = sigla_uf,
         Ano = ano) %>% 
  select(UF, id_municipio, populacao)

pib <- merge(Segund, pop, by = c('UF', 'id_municipio'), all.x = T)

pib %>%
  filter(incumbente == 1) %>% 
  select(municipio,pib20, populacao, per_votos, votosT, STurno, Ano) %>% 
  mutate(pib = pib20/populacao) %>% 
  na.omit() %>%
  ggplot(aes(x = pib, y = per_votos)) +
  geom_point(aes(color = STurno)) + geom_smooth(method = "lm", se = FALSE, color = "#CC0000") +
  scale_x_continuous(breaks = seq(0,408000, 75000))+
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0, 100))+
  scale_color_manual(values = c("#A2A19F", ggplot2::alpha("#FE9B2B", 0.3)), name = 'Segundo turno') +
  labs(x = 'Pib per capita', y = 'Votos (%)') +
  theme_minimal() +
  facet_wrap(vars(Ano))

# idhm por voto

pib %>% 
  filter(incumbente == 1) %>% 
  select(IDHM, per_votos, votosT, STurno, Ano) %>% 
  na.omit() %>%
  ggplot(aes(x = IDHM, y = per_votos)) +
  geom_point(aes(color = STurno)) + geom_smooth(method = "lm", se = FALSE, color = "#CC0000") +
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0, 100))+
  scale_color_manual(values = c("#A2A19F", ggplot2::alpha("#FE9B2B", 0.3)), name = 'Segundo turno') +
  labs(x = 'IDHM', y = 'Votos (%)') +
  theme_minimal() +
  facet_wrap(vars(Ano))

## Distribuição por uf

Segunds <- Segund %>% 
  filter(STurno=="Sim", incumbente == 1) %>% 
  group_by(UF, Ano) %>% 
  summarise(n = n()) 

G3 <- Segunds %>% 
  filter(Ano == 2016) %>% 
  ggplot(aes(x = reorder(UF, n), y = n, fill="#A2A19F")) +
  geom_bar(stat = "identity", color="#A2A19F", fill="#A2A19F") +  
  coord_flip()+ geom_hline(yintercept = mean(Segunds$n), linetype = "dashed", color = "#A2A19F",)+
  scale_y_continuous(breaks = seq(0,16, 2), limits = c(0,16))+
  geom_text(aes(x = "PE", y = 5.61, label = "Média \n Nacional"),
            hjust = -0.2, vjust = -0.5, color = "#A2A19F") +
  geom_bar(data = . %>% filter(UF == "SP"), stat = "identity", color = "#FE9B2B",
           fill = "#FE9B2B", width = 0.9) +
  labs(title = " ",
       x = " ",
       y = "n") + guides(fill=FALSE)+         
  theme_minimal()


G4 <- Segunds %>% 
  filter(Ano == 2020) %>% 
  ggplot(aes(x = reorder(UF, n), y = n, fill="#A2A19F")) +
  geom_bar(stat = "identity", color="#A2A19F", fill="#A2A19F") +  
  coord_flip()+ geom_hline(yintercept = mean(Segunds$n), linetype = "dashed", color = "#A2A19F",)+
  scale_y_continuous(breaks = seq(0,16, 2), limits = c(0,16))+
  geom_text(aes(x = "PE", y = 5.61, label = "Média \n Nacional"),
            hjust = -0.2, vjust = -0.5, color = "#A2A19F") +
  geom_bar(data = . %>% filter(UF == "SP"), stat = "identity", color = "#FE9B2B",
           fill = "#FE9B2B", width = 0.9) +
  labs(title = " ",
       x = " ",
       y = "n") + guides(fill=FALSE)+         
  theme_minimal()


grid.arrange(G3, G4, ncol = 2)

## Histograma do percentual de votos 

G1 <- Segund %>% 
  filter(STurno=="Não") %>% 
  ggplot( aes(x = per_votos)) +
  geom_histogram(binwidth = 0.5, color="#A2A19F", fill="#A2A19F", alpha = 0.7) +
  labs(title = "",
       x = "Votos (%)",
       y = "Frequência") +
  scale_y_continuous(breaks = seq(0,160, 20), limits = c(0,160))+
  theme_minimal()

G2 <- Segund %>% 
  filter(STurno=="Sim") %>% 
  ggplot(aes(x = per_votos)) +
  geom_histogram(binwidth = 2, color="#FE9B2B", fill="#FE9B2B", alpha = 0.7) +
  scale_y_continuous(breaks = seq(0,8, 2), limits = c(0,8))+
  labs(title = "",
       x = "Votos (%)",
       y = "Frequência") +
  theme_minimal()

grid.arrange(G1, G2, ncol = 2)


