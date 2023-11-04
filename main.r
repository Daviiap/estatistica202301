data <- Orange

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
  file = "hist_age_ircum.png",
  width = 600, height = 350
)
plot(
  data$age,
  data$circumference,
  xlab = "Idade",
  ylab = "Circunferência",
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
