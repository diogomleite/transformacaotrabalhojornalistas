#Vínculos precários

#período todo
reg1 <- lm(X~fracao, data = precários_prevalencia)
summary(reg1)
#Queda
precarios1 <- precários_prevalencia[-(1:25),]
reg2 <- lm(X~fracao, data = precarios1)
summary(reg2)
#Aumento
precarios2 <- precários_prevalencia[-(24:37),]
reg_2 <- lm(X~fracao, data=precarios2)
summary(reg_2)

#Carga horária

#período todo
reg3 <- lm(ano ~ quantidade_horas_contratadas, data = horas)
summary(reg3)
#Aumento
horas1 <- horas[-(18:28),]
reg_aumento <- lm(ano ~ quantidade_horas_contratadas, data = horas1)
summary(reg_aumento)
#Queda
horas2 <- horas[-(1:16),]
reg_queda <- lm(ano ~ quantidade_horas_contratadas, data = horas2)
summary(reg_queda)

#Tempo no emprego

#período todo
reg4 <- lm(ano ~ tempo_emprego, data = tempo)
summary(reg4)
#Aumento
tempo1 <- tempo[-(1:16),]
reg_periodo1 <- lm(ano ~ tempo_emprego, data = tempo1)
summary(reg_periodo1)
#Queda
tempo2 <- tempo[-(18:28),]
reg_periodo1 <- lm(ano ~ tempo_emprego, data = tempo2)
summary(reg_periodo1)

#idade

#período todo
reg5 <- lm(ano ~ idade, data = idade)
summary(reg5)
#Queda
idade1 <- idade[-(18:28),]
reg_queda1 <- lm(ano ~ idade, data = idade1)
summary(reg_queda1)
#Aumento
idade2 <- idade[-(1:16),]
reg_aumento1 <- lm(ano ~ idade, data = idade2)
summary(reg_aumento1)

#número de profissionais

#pós-2002
total_pos <- profissionais[-(1:18),]
regpos <- lm(X ~ total, data = total_pos)
summary(regpos)
#pré-2002
total_pre <- profissionais[-(19:37),]
regpre <- lm(X ~ total, data = total_pre)
summary(regpre)
#Queda
queda <- profissionais[-(1:25),]
reg_queda <- lm(X ~ total, data = queda)
summary(reg_queda)

#Salário

#período todo
reg6 <- lm(ano ~ remuneracao, data = salário)
summary(reg6)
#Queda
salario1 <- salário[-(1:12),]
reg_queda1 <- lm(ano ~ remuneracao, data = salario1)
summary(reg_queda1)

#Assessores

#período todo
reg7 <- lm(X ~ prevalencia, data = assessores)
summary(reg7)

#Suplementação para gerar gráficos
ano <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
ano1 <- c(1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002)

#Gráficos

plot(ano, assessores$prevalencia, type = 'l', main = 'Prevalência de assessores', ylab = 'Fração de assessores', xlab = 'Ano')
plot(ano, assessores$total, type = 'l', main = 'Número total de assessores', ylab = 'Número de assessores', xlab = 'Ano')

par(mfrow = c(2, 1))
plot(ano1, total_pre$total, type = 'l', main = 'Número total de profissionais (1985-2002)', ylab = 'Número de profissionais', xlab = 'Ano')
plot(ano, total_pos$total, type = 'l', main = 'Número total de profissionais (2002-2020)', ylab = 'Número de profissionais', xlab = 'Ano')

par(mfrow = c(1, 1))
plot(idade$ano, idade$idade, type = 'l', main = 'Idade média dos profissionais', ylab = 'Idade média (anos)', xlab = 'Ano')
plot(tempo$ano, tempo$tempo_emprego, type = 'l', main = 'Tempo médio de permanência no emprego', ylab = 'Tempo médio (meses)', xlab = 'Ano')
plot(salário$ano, salário$remuneracao, type = 'l', main = 'Salário médio', ylab = 'Salário médio (nº de salários mínimos)', xlab = 'Ano')
plot(horas$ano, horas$quantidade_horas_contratadas, type = 'l', main = 'Carga horária contratada', ylab = 'Carga horária média (horas)', xlab = 'Ano')

precarios_pos <- precários[-(1:18),]
precarios_pre <- precários[-(19:37),]

par(mfrow = c(2, 1))
plot(ano1, precarios_pre$total, type = 'l', main = 'Número de vínculos de trabalho precários (1985 - 2002)', ylab = 'Número de vínculos de trabalho precários', xlab = 'Ano')
plot(ano, precarios_pos$total, type = 'l', main = 'Número de vínculos de trabalho precários (2003 - 2020)', ylab = 'Número de vínculos de trabalho precários', xlab = 'Ano')
par(mfrow = c(1, 1))
plot(salário$ano, precários_prevalencia$fracao, type = 'l', main = 'Prevalência de vínculos precários', ylab = 'Fração de vínculos precários', xlab = 'Ano')

#Correlação Carga horária vs. Total de profissionais
horas <- horas[-(1:9),]
profissionais <- profissionais[-(1:18),]
cor(profissionais$total, horas$quantidade_horas_contratadas)
