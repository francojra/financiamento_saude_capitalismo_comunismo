
# Financiamento em serviços de saúde -------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 23/10/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/financing-healthcare ------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Boa saúde é parte chave para nossa qualidade de vida. Nesse registro nós focamos
### sobre os investimentos em saúde - um dos mais importantes contributos para proteger
### e melhorar a saúde. 

### Os cuidados de saúde com financiamento público são um legado da era das luzes. Os primeiros
### exemplos sobre seguros de saúde datam do final do século 19. Dados desses primeiros
### sistemas mostram que gastos com saúde apenas começaram a aumentar vários anos após
### a expansão do seguro saúde, com a descobertas de novos tratamentos.

### Os impactos que os desenvolvimentos científicos tiveram sobre gastos em saúde é símbolo
### nos Estados Unidos: em décadas recentes, como a possibilidade de tratamentos expandiram 
### rapidamente, gastos com saúde aumentaram - tanto no privado, quanto no público, tanto
### per capita como em taxa de produto interno bruto. Isso ocorreu sem grande mudanças na
### cobertura dos seguro de saúde, e teve duas consequências importantes: (i) os Estados Unidos
### atualmente gasta mais dinheiro do governo sobre saúde por pessoa que muitos países com 
### programas universais; (ii) a despesa é tão concentrada que o 1% de topo dos gastadores 
### representa mais de 20% da despesa total com cuidados de saúde.

### As despesas globais nos cuidados com saúde como parte do rendimento mundial tem aumentado,
### continuamente mas lentamente nas duas últimas décadas. Entretanto, existe no mundo substancial
### heterogeneidade. Regionalmente, países de alta renda gastam - e vem gastando - uma taxa 
### muito maior da renda que países mais pobres (cerca de duas vezes mais). Além disso, em
### contraste aos países de alta renda, os países de renda média e baixa apresentam menor financiamento
### público em saúde - embora tenham vindo a recuperar seu atraso - e o papel das despesas é muito
### mais alto (cerca de 50% dos gastos totais).

### Fincancimento em saúde nos países em desenvolvimento no século 21 tem sido grandemente moldado
### pelo fluxo de recursos canalizados através da ajuda ao desenvolvimento. Esses fluxos que aumentaram
### após a introdução dos Objetivos para o Desenvolvimento do Milênio, contam com cerca de 0,7%
### dos gastos dos recursos pelos países de renda alta sobre serviços em saúde. Apesar de parecer uma
### pequena proporção pelos comitês nacionais dos países ricos, para os países de baixa renda essa
### porcentagem é de ajuda substancial. Na África Sub-saariana eles financiaram mais que 25% dos gastos
### totais sobre saúde. Isso implica que desenvolver assistências para saúde, se devidamente orientada e gerida, 
### tem o potencial de reduzir drasticamente a desigualdade nos resultados sanitários: a robusta relação 
### empiricamente observada entre os resultados da saúde e as despesas de saúde é indicativa de grandes 
### retornos para os investimentos na própria saúde.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

fin_saude <- read.csv("public-health-expenditure-share-GDP-OWID.csv")
view(fin_saude)
names(fin_saude)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

fin_saude <- fin_saude %>%
  select(-Code) %>%
  rename(gasto_saude = public_health_expenditure_pc_gdp) %>%
  filter(between(Year, 2000, 2019)) %>%
  view()

fin_saude1 <- fin_saude %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(gasto_saude),
            sd = sd(gasto_saude), n = n(),
            se = sd/sqrt(n)) %>%
  view()

fin_saude2 <- fin_saude %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  view() 

fin_saude3 <- fin_saude %>%
  filter(Entity %in% c("United States", "China", "Brazil")) %>%
  view() 

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(fin_saude1, aes(x = fct_reorder(Entity, media), 
                       y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2, size = 0.8) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("China", "Japão", "Alemanha", "Estados Unidos")) +
  labs(x = "Países", y = "Taxa de financiamento em\n saúde pelo governo (%)") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none", axis.text = element_text(color = "black"))

ggplot(fin_saude2, aes(x = Year, y = gasto_saude,
                       group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499"),
                     labels = c("China", "Alemanha", 
                                "Japão", "Estados Unidos")) +
  labs(x = "Tempo (anos)", 
       y = "Taxa de financiamento em\n saúde pelo governo (%)",
       color = "Países") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(axis.text = element_text(color = "black"))

ggplot(fin_saude3, aes(x = Year, y = gasto_saude,
                       group = Entity, color = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", 
       y = "Taxa de financiamento em\n saúde pelo governo (%)",
       color = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))


