library(readxl)
library(dplyr)
library(psych)
library(forecast)
library(ggplot2)
library(descr)
library(rdrobust)
library(Rmisc)
library(car)

### Base com os dados eleitorais

Segund <- readRDS("segundoRound.RDS")


### Preparação da Base 

# Criação da variável de tratamento - Struno
# Normalização da variável de tratamento - Tamanho do Eleitorado
# Criação de variáveis de controle

Segund <- Segund %>% 
  filter(turno==1, incumbente == 1) %>% 
  mutate(STurno = ifelse(eleitorado20 >200000, "Sim", "Não"),
         DTratado =  ifelse(STurno=="Sim", eleitorado20-200000, 0),
         DNTratado =  ifelse(STurno=="Não", eleitorado20-200000, 0),
         Apoio = ifelse(partido==partido_gov, "Sim", "Não")) %>% 
  select(-desastre)


# 

Segund %>% 
  select(eleitorado20, per_votos, STurno) %>% 
  mutate(eleitorado20 = eleitorado20/1000) %>% 
  na.omit() %>% 
  ggplot(aes(x =eleitorado20, y = per_votos, color = STurno)) +
  geom_point() +geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_x_continuous(breaks = seq(0,500, 100), limits = c(0,500))+
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0, 100))+
  scale_color_manual(values = c("#A2A19F", "#FE9B2B"))+
  labs(title = " ", x = "Eleitorado (Mil)",
       y = "votos (%)") +
  theme_minimal()+theme(legend.position = "bottom") 

# A demostração pela reta e pela curva suavizada é importante para verificar se outro modelo
# não linear se ajustaria mellhor, ou seja, para afastar o risco do salto ser derivado de 
# ajsute não adequdo do modelo.

# Regressão Prévio

# Foram testados modelos quadráticos mas os resultados não mostraram o melhor ajuste.


Segmod <- select(Segund, per_votos, DTratado, DNTratado,
                 STurno, candidato, UF, eleitorado20 )
Segmod <- na.omit(Segmod)

modprev <- lm(per_votos ~ DTratado+DNTratado+STurno, Segmod)
summary(modprev)

Segmod$Pred <- predict(modprev)

coefic <- coef(modprev)

Segmod %>% 
  select(eleitorado20, Pred, STurno) %>% 
  mutate(eleitorado20 = eleitorado20/1000) %>% 
  filter(eleitorado20>50) %>% 
  na.omit() %>% 
  ggplot(aes(x =eleitorado20, y = Pred, color = STurno)) +
  geom_point() +geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(breaks = seq(20,60,10), limits = c(20,60))+
  scale_x_continuous(breaks = seq(0,300, 100), limits = c(0,300))+
  geom_vline(xintercept = 200, linetype = "dashed", color = "#CC0000")+
  scale_color_manual(values = c("#A2A19F", "#FE9B2B"))+
  labs(title = " ", x = "Eleitorado (Mil)",
       y = "Predito") +
  theme_minimal()+theme(legend.position = "bottom")  


## Modelo Completo

# Foram testados outros valores de corte (150 e 100), ao final, os  resultados não apresentaram
# sigfificância estatística. A ideia é testa uma possível aleatoriedade do corte em 200.000.

Segmodcomp <- Segund %>% 
  select(Ano,per_votos, DTratado, DNTratado, STurno,receita_total,IDHM, pib20, Apoio, eleitorado20) %>% 
  mutate(receita_total = if_else(is.na(receita_total),mean(receita_total), receita_total))

Segmodcomp <- na.omit(Segmodcomp)

modcomp <- lm(per_votos~DTratado+DNTratado+STurno+
                IDHM+pib20+receita_total+Apoio+as.factor(Ano), Segmodcomp)
summary(modcomp)

Segmodcomp$Precomp <- predict(modcomp)

Segmodcomp$Pred <- predict(modcomp)

coefic <- coef(modcomp)

Segmodcomp %>% 
  select(eleitorado20, Pred, STurno) %>% 
  mutate(eleitorado20 = eleitorado20/1000) %>% 
  filter(eleitorado20>50) %>% 
  na.omit() %>% 
  ggplot(aes(x =eleitorado20, y = Pred, color = STurno)) +
  geom_point() +geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = seq(20,60,10), limits = c(20,60))+
  scale_x_continuous(breaks = seq(0,300, 100), limits = c(0,300))+
  geom_vline(xintercept = 200, linetype = "dashed", color = "#CC0000")+
  scale_color_manual(values = c("#A2A19F", "#FE9B2B"))+
  labs(title = " ", x = "Eleitorado (Mil)",
       y = "Predito") +
  theme_minimal()+theme(legend.position = "bottom") 


# Obs.: realizamos teste estimando o modelo apenas com eleitorado acima 500000,
# a ideia é elimiar o risco da inclinação da reta deve-se aos municípios pequnos,
# o resultado continuam significativos.

## Teste com múltiplas fronteiras =

# bindwidth - 50

predTratad <- filter(Segmodcomp, eleitorado20>200000 &eleitorado20<250000)
predNTratad <- filter(Segmodcomp, eleitorado20<=200000 &eleitorado20>150000)


mean(predTratad$Precomp)-mean(predNTratad$Precomp)

### Defesa da fronteira 

# Fronteira com 50 mil para cada lado

Segmodcomp %>% 
  select(eleitorado20, Pred, STurno) %>% 
  mutate(eleitorado20 = eleitorado20/1000) %>% 
  filter(eleitorado20>50) %>% 
  na.omit() %>% 
  ggplot(aes(x =eleitorado20, y = Pred, color = STurno)) +
  geom_point() +
  scale_y_continuous(breaks = seq(20,60,10), limits = c(20,60))+
  scale_x_continuous(breaks = seq(0,400, 50), limits = c(0,400))+
  geom_vline(xintercept = 150, linetype = "dashed", color = "#CC0000")+
  geom_vline(xintercept = 250, linetype = "dashed", color = "#CC0000")+
  scale_color_manual(values = c("#A2A19F", "#FE9B2B"))+
  labs(title = " ", x = "Eleitorado (Mil)",
       y = "Predito") +
  theme_minimal()+theme(legend.position = "bottom") 


# Grafico de barra de erro 

Bbarra <- bind_rows(predNTratad, predTratad)


GBar <- summarySE(Bbarra, measurevar = 'Precomp', groupvars = "STurno" )


ggplot(GBar, aes(x=STurno, y=Precomp)) + 
  geom_line(linetype="solid", size=0.8)+
  geom_errorbar(aes(ymin=Precomp-ci, ymax=Precomp+ci), size = 0.8, width=.2,
                color= c("#A2A19F", "#FE9B2B")) +
  theme_minimal()+
  geom_point(size = 2,  color= c("#A2A19F", "#FE9B2B"))+
  theme(legend.position="bottom")+ xlab('2º Turno')+
  ylab('Votos Preditos (%)')+
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100))


describe.by(Bbarra$Precomp, group = Bbarra$STurno)
leveneTest(Bbarra$Precomp, Bbarra$STurno)
t.test(Bbarra$Precomp ~ Bbarra$STurno, var.equal = T)



# bindwidth - 25

predTratad <- filter(Segmodcomp, eleitorado20>200000 &eleitorado20<225000)
predNTratad <- filter(Segmodcomp, eleitorado20<=200000 &eleitorado20>175000)


mean(predTratad$Precomp)-mean(predNTratad$Precomp)

### Defesa da fronteira 

# Fronteira com 25 mil para cada lado

Segmodcomp %>% 
  select(eleitorado20, Pred, STurno) %>% 
  mutate(eleitorado20 = eleitorado20/1000) %>% 
  filter(eleitorado20>50) %>% 
  na.omit() %>% 
  ggplot(aes(x =eleitorado20, y = Pred, color = STurno)) +
  geom_point() +
  scale_y_continuous(breaks = seq(20,60,10), limits = c(20,60))+
  scale_x_continuous(breaks = seq(0,400, 50), limits = c(0,400))+
  geom_vline(xintercept = 175, linetype = "dashed", color = "#CC0000")+
  geom_vline(xintercept = 225, linetype = "dashed", color = "#CC0000")+
  scale_color_manual(values = c("#A2A19F", "#FE9B2B"))+
  labs(title = " ", x = "Eleitorado (Mil)",
       y = "Predito") +
  theme_minimal()+theme(legend.position = "bottom") 


# Grafico de barra de erro 

Bbarra <- bind_rows(predNTratad, predTratad)


GBar <- summarySE(Bbarra, measurevar = 'Precomp', groupvars = "STurno" )


ggplot(GBar, aes(x=STurno, y=Precomp)) + 
  geom_line(linetype="solid", size=0.8)+
  geom_errorbar(aes(ymin=Precomp-ci, ymax=Precomp+ci), size = 0.8, width=.2,
                color= c("#A2A19F", "#FE9B2B")) +
  theme_minimal()+
  geom_point(size = 2, color= c("#A2A19F", "#FE9B2B"))+
  theme(legend.position="bottom")+ xlab('2º Turno')+
  ylab('Votos Preditos (%)')+
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100))


describe.by(Bbarra$Precomp, group = Bbarra$STurno)
leveneTest(Bbarra$Precomp, Bbarra$STurno)
t.test(Bbarra$Precomp ~ Bbarra$STurno, var.equal = T)

# Conclusão:

# Como não há razão para acreditar as estimativas mudariam signifiativamente no ponto de corte
# a indícios de que a diferença encontrada no local do corte deve-se ao elemento do segundo turno.
# Afimrações mais robustaz necessitam de testes de robustez e/ou métodos complementares,
# como, por exemplo, repetir a análise com uma metodologia de pareamento.

## Teste do Modelo Robusto 


SegRobust <- Segund %>% 
  filter(eleitorado20>=140000 &eleitorado20<=260000)

llr <- rdrobust::rdrobust(SegRobust$per_votos, 
                          SegRobust$eleitorado20+SegRobust$Ano,
                          c = 200000,
                          kernel = "tri",
                          bwselect = "mserd",
)
summary(llr)



rdrobust::rdplot(SegRobust$per_votos, 
                 SegRobust$eleitorado20, 
                 c = 200000,
                 kernel = "tri",
                 p=2,
                 title = "",
                 x.label = "Eleitorado",
                 y.label =  "Votos (%)"
)


Segtest <- Segund %>% 
  filter(eleitorado20>=150000&eleitorado20<=250000)

