data <- Orange

## PONTO 01

get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Médias
media_age <- mean(data$age)
media_circum <- mean(data$circumference)

print("Média de Age:")
print(media_age)
print("Média de Circumference:")
print(media_circum)

# Medianas
mediana_age <- median(data$age)
mediana_circum <- median(data$circumference)

print("Mediana de Age:")
print(mediana_age)
print("Mediana de Circumference:")
print(mediana_circum)

moda_age <- get_mode((data$age))
moda_circum <- get_mode((data$circumference))

print("Moda de Age:")
print(moda_age)
print("Moda de Circumference:")
print(moda_circum)

# Variâncias
var_age <- var(data$age)
var_circum <- var(data$circumference)

print("Variância de Age:")
print(var_age)
print("Variância de Circumference:")
print(var_circum)

# Desvio-Padrão
desvio_age <- sd(data$age)
desvio_circum <- sd(data$circumference)

print("Desvio-Padrão de Age:")
print(desvio_age)
print("Desvio-Padrão de Circumference:")
print(desvio_circum)

# Histograma de "age"
png(
  file = "hist_age.png",
  width = 600, height = 350
)
hist(data$age, main = "Histograma de Age", xlab = "Idade")
dev.off()

# Histograma de "circumference"
png(
  file = "hist_circumference.png",
  width = 600, height = 350
)
hist(
  data$circumference, main = "Histograma de Circumference", 
  xlab = "Circunferência"
)
dev.off()

# Gráfico de dispersão entre "age" e "circumference"
png(
  file = "dispersao.png",
  width = 600, height = 350
)
plot(
  data$circumference,
  data$age,
  xlab = "Circunferência",
  ylab = "Idade",
  main = "Gráfico de Dispersão"
)
dev.off()

# Box plot de "age"
png(
  file = "box-plot-age.png",
  width = 600, height = 350
)
boxplot(
  data$age,
  main = "Box Plot de Age",
  ylab = "Idade"
)
dev.off()

# Box de "circumference"
png(
  file = "box-plot-circum.png",
  width = 600, height = 350
)

boxplot(
  data$circumference,
  main = "Box Plot de Circumference",
  ylab = "Circunferência"
)
dev.off()

## PONTO 02

# Estimando a média da variável 'age'
media_idade <- mean(Orange$age)
print(paste('Estimação pontual da média:', media_idade))

# Estimando a variância da variável 'age'
variancia_idade <- var(Orange$age)
print(paste('Estimação pontual da variância:', variancia_idade))

# Calculando o intervalo de confiança para a média
n <- length(Orange$age)
se_media <- sqrt(variancia_idade / n)
alpha <- 0.05
z <- qnorm(1 - alpha/2)
intervalo_media <- c(media_idade - z * se_media, media_idade + z * se_media)
print(paste('Intervalo de confiança para a média:', toString(intervalo_media)))

# Calculando o intervalo de confiança para a variância
chi_lower <- qchisq(alpha/2, n-1)
chi_upper <- qchisq(1 - alpha/2, n-1)
intervalo_variancia <- ((n - 1) * variancia_idade) / c(chi_upper, chi_lower)
print(paste('Intervalo de confiança para a variância:', toString(intervalo_variancia)))

## PONTO 03

# Definindo dataset
data <- Orange

# Fazendo o teste de hipótese para média = 920
valor_teste_media <- 920
teste_hip_media <- t.test(data$age,
                          alternative = "two.sided",
                          mu = valor_teste_media,
                          confident.interval = 0.95)

cat(paste('p valor para o teste de hipotese para média = 920: ', toString(teste_hip_media$p.value)))

## PONTO 04

# Análise de aderência e associação

# Teste de normalidade para variável resposta (teste Shapiro-Wilk)
# Considerando que a variável resposta é age

shapiro_test <- shapiro.test(data$age)
print(shapiro_test)
# Como o valor-p é menor que 0.05, rejeitamos a hipótese nula de que a variável resposta segue uma distribuição normal

# Calculando o coeficiente de correlação de Pearson entre as variáveis age e circumference

correlation <- cor.test(data$age, data$circumference, method = "pearson")
print(correlation)

## PONTO 05

# Ajuste do modelo de regressão linear
modelo <- lm(age ~ circumference, data = Orange)

# Coeficiente de interceptação (a) e coeficiente de inclinação (b)
a <- coef(modelo)[1]
b <- coef(modelo)[2]

cat("Coeficiente angular (beta-0):", a, "\n")
cat("Coeficiente linear (beta-1):", b, "\n")

# Variância residual (erro)
var_erro <- summary(modelo)$sigma^2
cat("Variância do erro:", var_erro, "\n")

# Coeficiente de determinação (R²)
r2 <- summary(modelo)$r.squared
cat("Coeficiente de determinação (R²):", r2, "\n")

# Interpretação do R²
cat("O coeficiente de determinação (R²) representa a proporção da variabilidade na variável 'age' que é explicada pela variável 'circumference' no modelo de regressão.\n")
cat("Neste caso, aproximadamente", r2 * 100, "% da variabilidade é explicada pela idade das árvores.", "\n")

# Gráfico de dispersão dos dados e reta ajustada
png(
  file = "reta-ajustada.png",
  width = 600, height = 350
)
plot(Orange$circumference, Orange$age, main = "Regressão Linear", xlab = "Circunferência", ylab = "Idade")
abline(modelo, col = "red")
dev.off()
