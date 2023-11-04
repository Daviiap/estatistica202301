# Definindo dataset
data <- Orange

# Definindo as estimativas para a média
# Pontual
estimativa_pontual_media <- mean(data$age)
# Intervalo de confiança
estimativa_intervalo_media <- t.test(data$age, conf.level = 0.95)

# Printando as estimativas
print(estimativa_pontual_media)
print(estimativa_intervalo_media$conf.int[1])
print(estimativa_intervalo_media$conf.int[2])

# Fazendo o teste de hipótese para média = 920
valor_teste_media <- 920
teste_hip_media <- t.test(data$age,
                          alternative = "two.sided",
                          mu = valor_teste_media,
                          confident.interval = 0.95)

print(teste_hip_media$p.value)

# Definindo as estimativas para a variância
# Pontual
estimativa_pontual_var <- var(data$age)

print(estimativa_pontual_var)
