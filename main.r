data <- Orange

# Análise de aderência e associação

# Teste de normalidade para variável resposta (teste Shapiro-Wilk)
# Considerando que a variável resposta é age

shapiro_test <- shapiro.test(data$age)
print(shapiro_test)
# Como o valor-p é menor que 0.05, rejeitamos a hipótese nula de que a variável resposta segue uma distribuição normal


# Calculando o coeficiente de correlação de Pearson entre as variáveis age e circumference

correlation <- cor.test(data$age, data$circumference, method = "pearson")
print(correlation)


