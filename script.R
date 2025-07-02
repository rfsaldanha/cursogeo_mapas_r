# Introdução ao R

# Usando o R como calculadora
2 + 2
5 - 1
2 * 3
4 / 5
3^2

# Gerando sequencia de valores
1:100

# Guardando resultados com objetos
valor <- 5
pares <- c(0, 2, 4, 6, 8)
cores <- c("azul", "vermelho", "amarelo")

valor
pares
cores

# Ver objetos guardados
ls()

# Salvar em disco
getwd()
saveRDS(object = valor, file = "valor.rds")
save.image(file = "aula.RData")

# Matriz
matrix(1:100, nrow = 20, ncol = 5)
array(1:100, dim = c(5, 5, 4))

# Listas
lista1 <- list(1:10, 10:1, valor)
lista1
lista1[1]

lista2 <- list(a = 1:10, b = 10:1, c = valor)
lista2
lista2$a


# Graficos simples
plot(1:10, 10:1)
plot(1:10, 10:1, pch = 2)
plot(1:10, 10:1, pch = 2, col = 2)
plot(1:10, 10:1, pch = 2, xlab = "eixo x")
plot(1:10, 10:1, pch = 2, xlab = "eixo x", ylab = "eixo y")


hist(rnorm(1000), col = 33)
arrows(2, 100, 2, 70, col = 2)

boxplot(iris$Sepal.Length ~ iris$Species)

# Crianção funções
media <- function(objeto) {
  res <- sum(objeto) / length(objeto)

  return(res)
}
media(c(1, 2, 3))

alturas <- c(1.4, 1.7, 2.0, 1.6, 1.8)
media(alturas)

# Medidas estatisticas
sum(alturas) # Coma dos elementos do objeto "alturas"
length(alturas) # Contagem de elementos
mean(alturas) # Média
median(alturas) # Mediana
var(alturas) # Variância
sd(alturas) # Desvio padrão
quantile(alturas) # Quartis


# Bibliotecas (pacotes) no R
install.packages("geobr")
install.packages("sf")
install.packages("tidyverse")
install.packages("remotes")

# https://rfsaldanha.github.io/microdatasus/
remotes::install_github("rfsaldanha/microdatasus")

library(geobr)
library(sf)
library(tidyverse)
library(microdatasus)

# Download base gráfica
mun <- read_municipality(code_muni = 33, year = 2020)
mun$MUNIC_RES <- str_sub(mun$code_muni, end = -2)


# Download de dados tabulares
# de Sistemas de Informação de Saúde

df <- fetch_datasus(
  year_start = 2021,
  month_start = 4,
  year_end = 2021,
  month_end = 4,
  uf = "RJ",
  information_system = "SIH-RD"
)

df_a <- process_sih(df)

df_b <- process_sih(df, municipality_data = FALSE)


# Filtro SIH Covid
df_c <- df_a %>%
  filter(DIAG_PRINC == "B342" & MARCA_UTI != "Não utilizou UTI")

# Cálculo de um indicador
df_d <- df_c %>%
  group_by(MUNIC_RES, MARCA_UTI) %>%
  summarise(freq = n()) %>%
  mutate(MARCA_UTI, percent = (freq / sum(freq) * 100))


# Juntar (join) dados graficos e dados tabulares
mapa <- mun %>%
  left_join(y = df_d, by = c("MUNIC_RES" = "MUNIC_RES"))


# Gráfico
ggplot() +
  geom_sf(data = mun, alpha = .9, color = 1) +
  geom_sf(data = mapa, aes(fill = percent), size = .15) +
  facet_wrap(~MARCA_UTI, ncol = 5) +
  labs(subtitle = "Utilização de UTI, 2020", size = 8) +
  scale_fill_viridis_c(direction = -1, name = "%", limits = c(0, 100))


### VARIAÇÃO INDICADORES

# Cálculo indicador
df_d <- df_c %>%
  group_by(MUNIC_RES, GESTAO) %>%
  summarise(n = n()) %>%
  transmute(GESTAO, percent = (n / sum(n) * 100))


# Join dados graficos e dados tabulares
mapa <- mun %>%
  left_join(y = df_d, by = c("MUNIC_RES" = "MUNIC_RES"))


# Gráfico
ggplot() +
  geom_sf(data = mun, alpha = .9, color = 1) +
  geom_sf(data = mapa, aes(fill = percent), size = .15) +
  facet_wrap(~GESTAO, ncol = 5) +
  labs(subtitle = "Utilização de UTI, 2020", size = 8) +
  scale_fill_viridis_c(direction = -1, name = "%", limits = c(0, 100))

# Exportar banco
write.table(
  df_d,
  "banco.csv",
  sep = ";",
  row.names = T
)

# Exportar dado geográfico
write_sf(mapa, "mapa.geojson")
write_sf(mapa, "mapa.shp")
