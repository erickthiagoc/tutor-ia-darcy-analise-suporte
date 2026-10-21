Sys.setlocale("LC_ALL", "C.UTF-8")
library(ggplot2); library(dplyr); library(readr)

conversas_sucesso     <- read_csv("conversas_sucesso.csv", show_col_types = FALSE)
conversas_nao_sucesso <- read_csv("conversas_nao_sucesso.csv", show_col_types = FALSE)
conversas_sucesso$grupo     <- "Sucesso"
conversas_nao_sucesso$grupo <- "Não sucesso"
todas <- bind_rows(conversas_sucesso, conversas_nao_sucesso)
todas$grupo <- factor(todas$grupo, levels = c("Sucesso", "Não sucesso"))
cores_grupo <- c("Sucesso" = "#2E7D32", "Não sucesso" = "#C62828")

# Em vez de dois histogramas sobrepostos (que geram uma 3a cor de
# mistura sem legenda), agrupamos o nº de mensagens em faixas e
# usamos barras lado a lado (position = "dodge"): cada barra mantém
# a cor exata da legenda, sem sobreposição.

todas <- todas %>%
  mutate(faixa_mensagens = cut(
    n_mensagens,
    breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 30, Inf),
    labels = c("1-2", "3-4", "5-6", "7-8", "9-10", "11-15", "16-20", "21-30", "31+"),
    right = TRUE
  ))

contagem_faixas <- todas %>% count(grupo, faixa_mensagens)

grafico_mensagens <- ggplot(contagem_faixas, aes(x = faixa_mensagens, y = n, fill = grupo)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_fill_manual(values = cores_grupo) +
  labs(
    title = "Número de mensagens por conversa",
    x = "Nº de mensagens na conversa (faixas)",
    y = "Nº de conversas",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

print(grafico_mensagens)
ggsave("grafico_mensagens_v2.png", grafico_mensagens, width = 8, height = 5, dpi = 150)
