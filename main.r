# Definindo dataset
data <- Orange

# Calcular a estimativa para a média da idade (variável resposta)
estimativa_media <- mean(data$age)

# Calcular a estimativa para a variância da idade (variável resposta)
estimativa_variancia <- var(data$age)

# Definindo valor de teste para a variancia
valor_teste_media <- 920

# Resultado para o teste de hipotese da média
resultado_teste_media <- t.test(data$age, mu = valor_teste_media)

print(resultado_teste_media)


modelo <- lm(age ~ circumference, data = data)
coeficientes <- coef(modelo)
var_erro <- summary(modelo)$sigma^2
confint(modelo)