#Vínculos precários

reg_periodo1 <- lm(ano_crescimento ~ fracao_crescimento, data = precarios_prevalencia)
summary(reg_periodo1)
reg_periodo2 <- lm(ano_atual ~ fracao_atual, data = precarios_prevalencia)
summary(reg_periodo2)

#Carga horária

reg <- lm(ano ~ quantidade_horas_contratadas, data = horas)
summary(reg)
reg_periodo1 <- lm(ano_aumento ~ horas_aumento, data = horas)
summary(reg_periodo1)
reg_periodo2 <- lm(ano_queda ~ horas_queda, data = horas)
summary(reg_periodo2)

#Remuneração

reg <- lm(ano_pos94 ~ tempo_pos94, data = tempo)
summary(reg)
reg_periodo1 <- lm(ano_aumento ~ tempo_aumento, data = tempo)
summary(reg_periodo1)

#idade

reg <- lm(ano ~ idade, data = idade)
summary(reg)
reg_queda <- lm(ano_queda ~ idade_queda, data = idade)
summary(reg_queda)
reg_aumento <- lm(ano_aumento ~ idade_aumento, data = idade)
summary(reg_aumento)

#número de profissionais

regpre <- lm(ano_pre2002 ~ total_pre2002, data = profissionais)
summary(regpre)
regpos <- lm(ano_pos2002 ~ total_pos2002, data = profissionais)
summary(regpos)
reg_queda <- lm(ano_queda ~ total_queda, data = profissionais)
summary(reg_queda)

#Assessores

reg <- lm(ano ~ prevalencia, data = assessores)
summary(reg)

#Gráficos

plot(assessores$ano, assessores$prevalencia, type = 'l', main = 'Prevalência de assessores', ylab = 'Fração de assessores', xlab = 'Ano')
plot(assessores$ano, assessores$total, type = 'l', main = 'Número total de assessores', ylab = 'Número de assessores', xlab = 'Ano')

par(mfrow = c(2, 1))
plot(profissionais$ano_pre2002, profissionais$total_pre2002, type = 'l', main = 'Número total de profissionais (1985-2002)', ylab = 'Número de profissionais', xlab = 'Ano')
plot(profissionais$ano_pos2002, profissionais$ano_pos2002, type = 'l', main = 'Número total de profissionais (2002-2020)', ylab = 'Número de profissionais', xlab = 'Ano')

par(mfrow = c(1, 1))
plot(idade$ano, idade$idade, type = 'l', main = 'Idade média dos profissionais', ylab = 'Idade média (anos)', xlab = 'Ano')
plot(tempo$ano_pos94, tempo$tempo_pos94, type = 'l', main = 'Tempo médio de permanência no emprego', ylab = 'Tempo médio (meses)', xlab = 'Ano')
plot(salario$ano, salario$remuneracao, type = 'l', main = 'Salário médio', ylab = 'Salário médio (nº de salários mínimos)', xlab = 'Ano')
plot(horas$ano, horas$quantidade_horas_contratadas, type = 'l', main = 'Carga horária contratada', ylab = 'Carga horária média (horas)', xlab = 'Ano')

par(mfrow = c(2, 1))
plot(precarios$ano_pre2002, precarios$total_pre2002, type = 'l', main = 'Número de vínculos de trabalho precários (1985 - 2002)', ylab = 'Número de vínculos de trabalho precários', xlab = 'Ano')
plot(precarios$ano_pos2002, precarios$total_pos2002, type = 'l', main = 'Número de vínculos de trabalho precários (2003 - 2020)', ylab = 'Número de vínculos de trabalho precários', xlab = 'Ano')
par(mfrow = c(1, 1))
plot(precarios_prevalencia$ano, precarios_prevalencia$fracao, type = 'l', main = 'Prevalência de vínculos precários', ylab = 'Fração de vínculos precários', xlab = 'Ano')

