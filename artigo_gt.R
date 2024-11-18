# -------------------------------------------------------------------------
# Artigo GT - WVS ---------------------------------------------------------
# Welllington Santos Souza ------------------------------------------------
# -------------------------------------------------------------------------

# tirar número científico
options(scipen=999)
# limpando o ambiente
rm(list = ls())
# Instalando e carregando os pacotes utilizados na análise
if(!require(foreign)){install.packages("foreign")}: library(foreign)
if(!require(tidyverse)){install.packages("tidyverse")}: library(tidyverse)
if(!require(haven))(install.packages("haven")); library(haven)
if(!require(codebook))(install.packages("codebook"));library(codebook)
if(!require(srvyr))(install.packages("srvyr"));library(srvyr)
if(!require(descr))(install.packages("descr"));library(descr)
if(!require(knitr))(install.packages("knitr")); library(knitr)
if(!require(flextable))(install.packages("flextable")); library(flextable)
if(!require(kableExtra))(install.packages("kableExtra")); library(kableExtra)
if(!require(survey))(install.packages("survey")); library(survey)
if(!require(performance))(install.packages("performance")); library(performance)
if(!require(ggeffects))(install.packages("ggeffects")); library(ggeffects)
if(!require(sjPlot))(install.packages("sjPlot")); library(sjPlot)
if(!require(gtsummary))(install.packages("gtsummary")); library(gtsummary)
if(!require(flextable))(install.packages("flextable")); library(flextable)
if(!require(equatiomatic))(install.packages("equatiomatic")); library(equatiomatic)
if(!require(pacman)) install.packages("pacman");library(pacman)

pacman::p_load(psych, car, MASS, DescTools, QuantPsyc, ggplot2)

# Agora que temos os pacotes instalados podemos carregar a base de dados
# carregando dados gerais 
wvs <- read_rds("WVS_Cross-National_Wave_7_Rds_v5_0.rds")

# filtrando por Brasil
wvs_Brasil <- wvs %>% 
  filter(B_COUNTRY_ALPHA == "BRA")

# removendo dados gerais
rm(wvs)

# faxina nos dados

# criando variáveis que vão entrar no modelo

# variáveis dependentes

# empregabilidade 
wvs_Brasil = wvs_Brasil |>
  mutate(empregabilidade = ifelse(Q279 %in% c(1,2,3), "Sim", ifelse(Q279 %in% c(4,5,6,7), 
                                                              "Não", NA)))
# tabela de frequência
wvs_Brasil |> 
  group_by(empregabilidade) |> 
  summarise(n = n())


# variável escala de redimentos
# variável tricotômica onde de 1 a 3 será rendimentos baixos, de 4 a 7 
# rendimentos médios e 8 a 10 altos rendimentos

wvs_Brasil = wvs_Brasil |>
  mutate(renda = Q288R) |> 
  mutate(renda = case_when(
    renda == 1 ~ "Baixo",
    renda == 2 ~ "Médio",
    renda == 3 ~ "Alto",
    TRUE ~ NA
  )) 

# tabela de frequência
wvs_Brasil |> 
  group_by(renda) |> 
  summarise(n = n())

# variável de controle
# idade
wvs_Brasil = wvs_Brasil |>
  mutate(idade = X003R) |> 
  mutate(idade = case_when(
    idade == 1 ~ "18-24",
    idade == 2 ~ "25-34",
    idade == 3 ~ "35-44",
    idade == 4 ~ "45-54",
    idade == 5 ~ "55-64",
    idade == 6 ~ "65 +",
    TRUE ~ NA
  ))

# tabela de frequência
wvs_Brasil |> 
  group_by(idade) |> 
  summarise(n = n())

#  sexo
wvs_Brasil = wvs_Brasil |>
  mutate(sexo = Q260) |> 
  mutate(sexo = case_when(
    sexo == 1 ~ "Masculino",
    sexo == 2 ~ "Feminino",
    TRUE ~ NA
  ))

# tabela de frequência
wvs_Brasil |> 
  group_by(sexo) |> 
  summarise(n = n())

# escolaridade
wvs_Brasil = wvs_Brasil |>
  mutate(escolaridade = Q275R) |> 
  mutate(escolaridade = case_when(
    escolaridade == 1 ~ "Baixa",
    escolaridade == 2 ~ "Média",
    escolaridade == 3 ~ "Alta",
    TRUE ~ NA
  ))
  
# tabela de frequência
wvs_Brasil |> 
  group_by(escolaridade) |> 
  summarise(n = n())

# estado civil
wvs_Brasil = wvs_Brasil |>
  mutate(estado_civil = Q273) 

# tabela de frequência
wvs_Brasil |> 
  group_by(estado_civil) |> 
  summarise(n = n())

# Religião 
wvs_Brasil = wvs_Brasil |>
  mutate(religiao = Q289CS9) |> 
  mutate( religiao = case_when(
    religiao == 100000020 ~ "Sem rel.",
    religiao == 20213020 ~ "Evang. Lut.",
    religiao == 90500000 ~ "Espírita",
    religiao == 10100000 ~ "Católico",
    religiao == 90000000 ~ "Outra rel.",
    religiao == 90300000 ~ "Rel. mat. afr.",
    religiao == 20000000 ~ "Protest.",
    religiao == 40000000 ~ "Judaísmo",
    religiao == 70000000 ~ "Budismo",
    TRUE ~ NA
  ))

# tabela de frequência
wvs_Brasil |> 
  group_by(religiao) |> 
  summarise(n = n())

# Importância da religião na vida do individuo
wvs_Brasil = wvs_Brasil |>
  mutate(imp_religiao = Q6) |>
  mutate(imp_religiao = case_when(
    imp_religiao == 1 ~ "Muito importante",
    imp_religiao == 2 ~ "Importante",
    imp_religiao == 3 ~ "Pouco importante",
    imp_religiao == 4 ~ "Não é importante",
    TRUE ~ NA
  ))


# tabela de frequência
wvs_Brasil |> 
  group_by(imp_religiao) |> 
  summarise(n = n())

# Frequência de participação em cultos religiosos
wvs_Brasil = wvs_Brasil |>
  mutate(freq_culto = Q171) |> 
  mutate(freq_culto = case_when(
    freq_culto == 1 ~ "Mais de 1x/sem",
    freq_culto == 2 ~ "1x/sem",
    freq_culto == 3 ~ "1x/mês",
    freq_culto == 4 ~ "Dias santos",
    freq_culto == 5 ~ "1x/ano",
    freq_culto == 6 ~ "Menos de 1x/ano",
    freq_culto == 7 ~ "Nunca",
    TRUE ~ NA)) 

# tabela de frequência
wvs_Brasil |> 
  group_by(freq_culto) |> 
  summarise(n = n())

#  cor raça
wvs_Brasil = wvs_Brasil |>
  mutate(cor_raca = Q290) |> 
  mutate(cor_raca = case_when(
    cor_raca == 76001 ~ "Branca",
    cor_raca %in% c(76002,76003) ~ "Preta/Parda",
    cor_raca == 76004 ~ "Amarela",
    cor_raca == 76005 ~ "Indígena",
    TRUE ~ NA
  ))

# tabela de frequência
wvs_Brasil |> 
  group_by(cor_raca) |> 
  summarise(n = n())

# transformar variáveis em fatores
wvs_Brasil = wvs_Brasil |>
  mutate(empregabilidade = factor(empregabilidade)) |>
  mutate(idade = factor(idade, levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65 +"))) |>
  mutate(sexo = factor(sexo, levels = c("Masculino", "Feminino"))) |>
  mutate(escolaridade = factor(escolaridade, levels = c("Baixa", "Média", "Alta"))) |>
  mutate(estado_civil = factor(estado_civil)) |>
  mutate(religiao = factor(religiao, levels = c("Sem rel.", "Evang. Lut.", "Espírita", "Católico", "Outra rel.", "Rel. mat. afr.", "Protest.", "Judaísmo", "Budismo"))) |>
  mutate(imp_religiao = factor(imp_religiao, levels = c("Muito importante", "Importante", "Pouco importante", "Não é importante"))) |>
  mutate(freq_culto = factor(freq_culto, levels = c("Mais de 1x/sem", "1x/sem", "1x/mês", "Dias santos", "1x/ano", "Menos de 1x/ano", "Nunca"))) |>
  mutate(cor_raca = factor(cor_raca, levels = c("Branca", "Preta/Parda", "Amarela", "Indígena")))


# remover NA das variáveis do modelo
wvs_Brasil = wvs_Brasil |>
  filter(!is.na(empregabilidade)) |>
  filter(!is.na(idade)) |>
  filter(!is.na(sexo)) |>
  filter(!is.na(escolaridade)) |>
  filter(!is.na(estado_civil)) |>
  filter(!is.na(religiao)) |>
  filter(!is.na(imp_religiao)) |>
  filter(!is.na(freq_culto)) |>
  filter(!is.na(cor_raca))

# criando o survey design
survey_wvs_Brasil <- svydesign(ids = ~D_INTERVIEW, weights = ~ W_WEIGHT, 
                               data=wvs_Brasil)

# tabela de proporção da variável sexo
svymean(~escolaridade, design = survey_wvs_Brasil)

# modelo de regressão logística - modelo 1
modelo1 <- svyglm(empregabilidade ~ freq_culto + religiao + idade + sexo + escolaridade +  
                     cor_raca, 
                  design = survey_wvs_Brasil, family = binomial)

# sumário do modelo
summary(modelo1)


# Executa o teste ANOVA
anova(modelo1, test = "Chisq")

Anova(modelo1)

# library(kableExtra)
# 
# # Criando o dataframe com os resultados da ANOVA
# anova_table <- data.frame(
#   Variável = c("Frequência ao culto", "Religião", "Idade", "Sexo", "Escolaridade", "Cor/raça"),
#   Estatística_Chi2 = c(26.0527, 10.1443, 219.5146, 69.2531, 50.5787, 7.7048),
#   Deff = c(1.0116, 0.8861, 0.9964, 1.0257, 1.0039, 1.0516),
#   GL = c(6, 8, 5, 1, 2, 3),
#   `p-valor` = c(0.0002494, 0.1879492, 2.2e-16, 2.339e-16, 1.2056e-11, 0.0626961),
#   Significância = c("***", "", "***", "***", "***", ".")
# )

# # Criando a tabela formatada com kableExtra
# anova_table  |> 
#   kbl(
#     caption = "Teste de Análise de Deviance para o modelo de regressão logística (Rao-Scott LRT)",
#     align = "lccccc",
#     digits = 4
#   )  |> 
#   kable_styling(full_width = FALSE, position = "center") %>%
#   column_spec(1, bold = TRUE) %>%
#   footnote(
#     general = "Significância: ***p < 0,001; **p < 0,01; *p < 0,05; .p < 0,1.",
#     general_title = "Nota:",
#     footnote_as_chunk = TRUE
#   ) 



# verificando pressupostos
check_model(modelo1)

# visualizando as predições
fancy_plot <-  ggeffect(modelo1) |> 
  plot() |> 
  plot_grid()

# salvando resultado
ggsave(
  "fancy_plot.png",
  plot = fancy_plot,
  device = "png",
  dpi = 999,
  width = 15, height = 10
)

fancy_table <- tbl_regression(
  modelo1,
  exponentiate = T,
  add_parwise_contrasts = T,
  contrast_adjust = "bonferroni", 
  parwise_reversal = F,
  pvalue_fun = ~style_pvalue(.x, digits = 3)) |> 
  add_significance_stars(hide_p = F, hide_se = T, hide_ci = F) |>
  bold_p()


# salvando resultado

# docx
fancy_table |> 
  as_flex_table() |>
  save_as_docx(path="fancy_table.docx")

# image
fancy_table |> 
  as_flex_table() |>
  save_as_image("fancy_table.png")

# extraindo equação do modelo
extract_eq(modelo1)


# Gráfico das Razões de Chance
plot_model(modelo1, show.values = TRUE, value.offset = 0.3, value.size = 4,
           title = "", show.p = TRUE, p.shape = 16, p.size = 5)


# modelo 2 - probabilidade de rendimentos
# library(nnet)
# modelo2 <- multinom(renda ~ freq_culto + religiao + idade + sexo + escolaridade +  
#                      cor_raca, 
#                   design = survey_wvs_Brasil)
# citation(
#   package = "survey"
# )









