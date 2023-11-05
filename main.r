data <- Orange

# Ajuste do modelo de regressão linear
modelo <- lm(circumference ~ age, data = Orange)

# Coeficiente de interceptação (a) e coeficiente de inclinação (b)
a <- coef(modelo)[1]
b <- coef(modelo)[2]

cat("Coeficiente angular (a):", a, "\n")
cat("Coeficiente linear (b):", b, "\n")

# Variância residual (erro)
var_erro <- summary(modelo)$sigma^2
cat("Variância do erro:", var_erro, "\n")

# Coeficiente de determinação (R²)
r2 <- summary(modelo)$r.squared
cat("Coeficiente de determinação (R²):", r2, "\n")

# Interpretação do R²
cat("O coeficiente de determinação (R²) representa a proporção da variabilidade na variável 'circumference' que é explicada pela variável 'age' no modelo de regressão.\n")
cat("Neste caso, aproximadamente", r2 * 100, "% da variabilidade é explicada pela idade das árvores.", "\n")

# Gráfico de dispersão dos dados e reta ajustada
plot(Orange$age, Orange$circumference, main = "Regressão Linear", xlab = "Idade", ylab = "Circunferência")
abline(modelo, col = "red")
