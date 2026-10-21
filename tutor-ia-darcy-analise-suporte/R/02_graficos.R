
# GRÁFICOS - CONVERSAS_SUCESSO x CONVERSAS_NAO_SUCESSO
# Cole este bloco no final do script mapeamento_conversas_zammad_v2.R

library(ggplot2)
library(dplyr)
library(readr)
library(forcats)

# Se estiver rodando este bloco separado do script principal,
# descomente as duas linhas abaixo para ler os CSVs do disco:
# conversas_sucesso     <- read_csv("conversas_sucesso.csv", show_col_types = FALSE)
# conversas_nao_sucesso <- read_csv("conversas_nao_sucesso.csv", show_col_types = FALSE)

# junta as duas bases numa só, com uma coluna "grupo" para diferenciar
# nos gráficos (facilita usar facet/preenchimento por cor)
conversas_sucesso$grupo     <- "Sucesso"
conversas_nao_sucesso$grupo <- "Não sucesso"
todas <- bind_rows(conversas_sucesso, conversas_nao_sucesso)
todas$grupo <- factor(todas$grupo, levels = c("Sucesso", "Não sucesso"))

# paleta de cores fixa, para manter consistência entre os 3 gráficos
cores_grupo <- c("Sucesso" = "#2E7D32", "Não sucesso" = "#C62828")


# GRÁFICO 1 - Duração das conversas (sucesso x não sucesso)

# duracao_seg vem em segundos; convertemos para minutos para ficar
# mais legível no eixo do gráfico. Usamos boxplot (mostra mediana e
# dispersão) e limitamos o eixo Y no percentil 95 para não deixar
# os poucos casos muito longos (outliers) esmagarem a visualização.

todas$duracao_min_plot <- todas$duracao_seg / 60
limite_y <- quantile(todas$duracao_min_plot, 0.95, na.rm = TRUE)

grafico_duracao <- ggplot(todas, aes(x = grupo, y = duracao_min_plot, fill = grupo)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.5) +
  coord_cartesian(ylim = c(0, limite_y)) +
  scale_fill_manual(values = cores_grupo) +
  labs(
    title = "Duração das conversas: sucesso x não sucesso",
    subtitle = "Eixo Y cortado no percentil 95 para melhor visualização",
    x = NULL, y = "Duração (minutos)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(grafico_duracao)
ggsave("grafico_duracao.png", grafico_duracao, width = 7, height = 5, dpi = 150)

# GRÁFICO 2 - Tópicos principais (topico_1) por grupo

# Barras lado a lado (dodge) comparando a proporção de cada tópico
# dentro de cada grupo (sucesso x não sucesso). Usamos proporção (%)
# em vez de contagem bruta porque os grupos têm tamanhos diferentes.

topicos_pct <- todas %>%
  count(grupo, topico_1) %>%
  group_by(grupo) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

grafico_topicos <- ggplot(topicos_pct, aes(x = fct_reorder(topico_1, pct, .fun = max), y = pct, fill = grupo)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  coord_flip() +
  scale_fill_manual(values = cores_grupo) +
  labs(
    title = "Tópico principal por grupo de conversas",
    x = NULL, y = "% das conversas do grupo", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

print(grafico_topicos)
ggsave("grafico_topicos.png", grafico_topicos, width = 8, height = 5, dpi = 150)

# GRÁFICO 3 - Número de mensagens por conversa (sucesso x não sucesso)

# Complementa o gráfico de duração: mostra se conversas bem-sucedidas
# tendem a ter mais/menos idas e vindas do que as que não deram certo.

grafico_mensagens <- ggplot(todas, aes(x = n_mensagens, fill = grupo)) +
  geom_histogram(binwidth = 2, alpha = 0.6, position = "identity", color = "white") +
  scale_fill_manual(values = cores_grupo) +
  coord_cartesian(xlim = c(0, quantile(todas$n_mensagens, 0.97, na.rm = TRUE))) +
  labs(
    title = "Número de mensagens por conversa",
    x = "Nº de mensagens na conversa", y = "Nº de conversas", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

print(grafico_mensagens)
ggsave("grafico_mensagens.png", grafico_mensagens, width = 7, height = 5, dpi = 150)
