library(readxl)
library(tidyverse)
gini <- read_excel("gini.xlsx", skip = 3) |> 
  rename(valor = "...2")

# remover linhas com NA
gini <- gini |> 
  filter(!is.na(valor))

# gráfico de linhas 
gini |> 
  ggplot(aes(x = Ano, y = valor, group = 1)) +
  geom_line(color = "lightblue") +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.1) +
  labs(title = "",
       x = "Ano",
       y = "Índice de Gini") +
  geom_text(aes(x = Ano, y = valor, label = paste0(valor)),
            vjust = -0.15, size = 3)+
  theme_minimal()


pip_per_capita <- read_excel("pip_per_capita.xlsx", 
                             sheet = "PIB per capita - val...; Brasil", 
                             skip = 3) |> 
  rename(valor = "...2")


# remover linhas com NA
pip_per_capita <- pip_per_capita |> 
  filter(!is.na(valor))

# selecionar ano de 2001 a 2013
pip_per_capita <- pip_per_capita |> 
  filter(Ano >= 2001 & Ano <= 2013)

# gráfico de linhas
pip_per_capita |> 
  ggplot(aes(x = Ano, y = valor, group = 1)) +
  geom_line(color = "lightblue") +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.1) +
  labs(title = "",
       x = "",
       y = "") +
  geom_text(aes(x = Ano, y = valor, label = paste0(valor)),
            vjust = -0.15, size = 3)+
  theme_minimal()

