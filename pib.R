library(readxl)
library(dplyr)
pip_valores_correntes <- read_excel("C:/Users/welli/OneDrive/3-DISSERTACAO/pip_per_capita.xlsx", 
                             sheet = "PIB - valores correntes; Brasil", 
                             skip = 3) |> 
  rename('pib' = '...2')

# remover última linha
pip_valores_correntes <- pip_valores_correntes[-nrow(pip_valores_correntes),]

pip_valores_correntes <- pip_valores_correntes |> 
  filter(Ano >= 2001 & Ano <= 2013)

pip_per_capita_valores_correntes <- read_excel("C:/Users/welli/OneDrive/3-DISSERTACAO/pip_per_capita.xlsx", 
                             sheet = "PIB per capita - val...; Brasil", 
                             skip = 3) |> 
  rename(pib = '...2')
# remover última linha
pip_per_capita_valores_correntes <- pip_per_capita_valores_correntes[-nrow(pip_per_capita_valores_correntes),]


pib_per_capita_var_vol <- read_excel("C:/Users/welli/OneDrive/3-DISSERTACAO/pip_per_capita.xlsx", 
                             sheet = "PIB - variação em vo...; Brasil", 
                             skip = 3) |> 
  rename(pib = '...2')
pip_per_capita_valores_correntes <- pip_per_capita_valores_correntes |> 
  filter(Ano >= 2001 & Ano <= 2013)


# remover última linha
pib_per_capita_var_vol <- pib_per_capita_var_vol[-nrow(pib_per_capita_var_vol),]

# filtrar apenas os anos de 2001 a 2013
pib_per_capita_var_vol <- pib_per_capita_var_vol |> 
  filter(Ano >= 2001 & Ano <= 2013)

# gráfico de linhas com ggplot
library(ggplot2)
pip_valores_correntes |> 
  ggplot(aes(x = Ano, y = pib, group = 1)) +
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  labs(title = "",
       x = "Ano",
       y = "PIB") +
  geom_text(aes(x = Ano, y = pib, label = paste0(pib)),
            vjust = -0.15, size = 3)+
  theme(plot.title = element_text(size=12),axis.text.x= element_text(size=12),
        axis.text.y= element_text(size=8), axis.title=element_text(size=6))+
  theme_minimal()+
  theme(axis.text.y = element_blank())


pip_per_capita_valores_correntes |>
  ggplot(aes(x = Ano, y = pib, group = 1)) +
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.1) +
  labs(title = "",
       x = "Ano",
       y = "PIB per capita - valores correntes (R$)") +
  geom_text(aes(x = Ano, y = pib, label = paste0(pib)),
            vjust = -0.15, size = 3)+
  theme(plot.title = element_text(size=12),axis.text.x= element_text(size=12),
        axis.text.y= element_text(size=8), axis.title=element_text(size=6))+
  theme_minimal()+
  theme(axis.text.y = element_blank())

pib_per_capita_var_vol |>
  ggplot(aes(x = Ano, y = pib, group = 1)) +
  geom_line(color = "lightblue")+
  geom_point(color = "blue")+
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.1) +
  labs(title = "",
       x = "Ano",
       y = "Variação do PIB") +
  geom_text(aes(x = Ano, y = pib, label = paste0(pib)),
            vjust = -0.15, size = 3)+
  theme(plot.title = element_text(size=12),axis.text.x= element_text(size=12),
        axis.text.y= element_text(size=8), axis.title=element_text(size=6))+
  theme_minimal()+ 
  geom_smooth(method = "lm", se = FALSE, color = "red",  size=0.2)+
  theme(axis.text.y = element_blank())

