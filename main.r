# Definindo dataset
data <- Orange

# Definindo as estimativas para a média
# Pontual
estimativa_pontual_media <- mean(data$age)
# Intervalo de confiança
estimativa_intervalo_media <- t.test(data$age, conf.level = 0.95)

print(estimativa_pontual_media)
print(estimativa_intervalo_media$conf.int[1])
print(estimativa_intervalo_media$conf.int[2])

teste_hip_media <- t.test(data$age,
                          alternative = "two.sided",
                          mu = 920,
                          confident.interval = 0.95)

print(teste_hip_media$p.value)
