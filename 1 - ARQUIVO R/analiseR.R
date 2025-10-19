# ================================= INFORMAÇÕES GERAIS =============================
# Estatística descritiva e gráficos comparando 2014 e 2024 (12 meses)
# Programa Bolsa Família - Dados extraídos por amostragem aleatória

# ================================= GARANTIA E CARREGAMENTO DE PACOTES =============================

if (!require("readxl")) install.packages("readxl")
if (!require("modeest")) install.packages("modeest")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("scales")) install.packages("scales")

library(readxl)
library(modeest)
library(ggplot2)
library(dplyr)
library(scales)

# ================================= FUNÇÃO AUXILIAR DE MÊS ===================================

# Função para normalizar a coluna do mês (tira caracteres e garante que é numérico)
normalize_month_col <- function(df) {
  possible <- c("mes_competencia", "mes", "month", "meses")
  found <- intersect(possible, names(df))
  if (length(found) == 0) stop("Coluna de mês não encontrada. Use 'mes_competencia' ou 'mes'.")
  df$mes_competencia <- as.integer(gsub("[^0-9]", "", as.character(df[[found[1]]])))
  df
}

# ================================= IMPORTAÇÃO E PREPARAÇÃO DE DADOS =============================

dados_2014 <- read_xlsx("D:/1 - FACULDADE PROGRAMAÇÃO/1 - PROGRAMAÇÃO/2 - BACK-END/R/2- PROGRAMAS/TRABALHOS/bolsaFamilia - Copia/2 - PLANILHAS/coleta2014.xlsx")
dados_2024 <- read_xlsx("D:/1 - FACULDADE PROGRAMAÇÃO/1 - PROGRAMAÇÃO/2 - BACK-END/R/2- PROGRAMAS/TRABALHOS/bolsaFamilia - Copia/2 - PLANILHAS/coleta2024.xlsx")

# Normaliza a coluna do mês em ambos os dataframes
dados_2014 <- normalize_month_col(dados_2014) 
dados_2024 <- normalize_month_col(dados_2024)

# Adiciona a coluna de identificação do ano para o Boxplot
dados_2014$ano <- "2014"
dados_2024$ano <- "2024"

# Unifica os dados para gráficos comparativos de distribuição (Boxplot)
dados_total <- rbind(dados_2014, dados_2024)

# Variáveis base para estatística descritiva
valores_2014 <- dados_2014$valor_parcela
valores_2024 <- dados_2024$valor_parcela

# ================================= ESTATÍSTICA DESCRITIVA (POR ANO) =====================

# Média
media_2014 <- mean(valores_2014, na.rm = TRUE)
media_2024 <- mean(valores_2024, na.rm = TRUE)

media_2014
media_2024

# Mediana
mediana_2014 <- median(valores_2014, na.rm = TRUE)
mediana_2024 <- median(valores_2024, na.rm = TRUE)

mediana_2014
mediana_2024

# Moda
moda_2014 <- mfv(valores_2014)
moda_2024 <- mfv(valores_2024)

moda_2014
moda_2024


# Desvio-padrão
desvio_2014 <- sd(valores_2014, na.rm = TRUE)
desvio_2024 <- sd(valores_2024, na.rm = TRUE)

desvio_2014
desvio_2024


# Coeficiente de variação
cv_2014 <- (desvio_2014 / media_2014) * 100
cv_2024 <- (desvio_2024 / media_2024) * 100

cv_2014
cv_2024


# Dataframe de médias anuais (para Gráfico de Barras Anual)
medias_df <- data.frame(
  ano = c("2014", "2024"),
  media = c(media_2014, media_2024)
)

# ================================= GRÁFICO 1: BARRAS (COMPARAÇÃO ANUAL) =====================

ggplot(medias_df, aes(x = ano, y = media, fill = ano)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(media, 2)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("2014" = "#F44336", "2024" = "#2196F3")) +
  labs(
    title = "Comparação da Média por Ano (Bolsa Família)",
    x = "Ano",
    y = "Valor médio das parcelas (R$)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ================================= GRÁFICO 2: BOXPLOT (DISTRIBUIÇÃO ANUAL) =============================

ggplot(dados_total, aes(x = ano, y = valor_parcela, fill = ano)) +
  geom_boxplot(alpha = 0.8, width = 0.5, outlier.size = 1.5) +
  geom_point(data = medias_df, aes(x = ano, y = media), color = "black", shape = 18, size = 3.5) +
  geom_hline(data = medias_df, aes(yintercept = media, color = ano), linetype = "dashed") +
  scale_fill_manual(values = c("2014" = "#E57373", "2024" = "#64B5F6")) +
  scale_color_manual(values = c("2014" = "#C62828", "2024" = "#1565C0")) +
  scale_y_continuous(labels = dollar_format(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Distribuição dos Valores por Ano (2014 vs 2024)",
    subtitle = "Linha tracejada: média | Símbolo: média pontual",
    x = "Ano",
    y = "Valor da Parcela (R$)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

# ================================= PREPARAÇÃO PARA GRÁFICOS MENSAIS (COM ORDEM CORRETA) =============================

# Calcula as médias mensais para cada ano
medias_2014 <- aggregate(valor_parcela ~ mes_competencia, data = dados_2014, FUN = mean, na.rm = TRUE)
medias_2024 <- aggregate(valor_parcela ~ mes_competencia, data = dados_2024, FUN = mean, na.rm = TRUE)

# Une as médias mensais em um único dataframe
dados_comparativo <- rbind(
  data.frame(ano = "2014", medias_2014),
  data.frame(ano = "2024", medias_2024)
)

# Vetor ORDENADO de rótulos de meses (Jan a Dez)
mes_labels_ordenado_vetor <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")

# A CORREÇÃO ESTÁ AQUI: Cria a coluna mes_label_ordenado como FATOR ORDENADO
dados_comparativo$mes_label_ordenado <- factor(
  mes_labels_ordenado_vetor[dados_comparativo$mes_competencia],
  levels = mes_labels_ordenado_vetor # Garante a ordem cronológica
)

# ================================= GRÁFICO 3: LINHA (EVOLUÇÃO MENSAL) =============================

ggplot(dados_comparativo, aes(x = mes_label_ordenado, y = valor_parcela, color = ano, group = ano)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("2014" = "#F44336", "2024" = "#2196F3")) +
  scale_y_continuous(labels = label_currency(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Comparativo das Médias Mensais (2014 vs 2024)",
    x = "Mês",
    y = "Valor Médio das Parcelas (R$)",
    color = "Ano"
  ) +
  theme_bw(base_size = 12) + # theme_bw para o fundo quadriculado
  theme(
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey90"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# ================================= GRÁFICO 4: BARRAS AGRUPADAS (MENSAL) =============================

ggplot(dados_comparativo, aes(x = mes_label_ordenado, y = valor_parcela, fill = ano)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(valor_parcela, 0)), position = position_dodge(0.7), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("2014" = "#E57373", "2024" = "#64B5F6")) +
  scale_y_continuous(labels = label_currency(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Comparativo das Médias Mensais (Barras) — 2014 x 2024",
    x = "Mês",
    y = "Valor Médio (R$)",
    fill = "Ano"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))