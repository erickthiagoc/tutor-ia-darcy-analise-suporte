
# ANÁLISES COMPLEMENTARES CITADAS NO RELATÓRIO FINAL
# Cole este bloco ao final de mapeamento_conversas_zammad_v2.R
# (precisa dos objetos: df, conversas_uteis, conversas_sucesso,
#  conversas_nao_sucesso e da função classificar_topicos)

library(dplyr)

# 1) Junta as duas bases numa só, com a coluna "grupo"
conversas_sucesso$grupo     <- "Sucesso"
conversas_nao_sucesso$grupo <- "Não sucesso"
todas <- bind_rows(conversas_sucesso, conversas_nao_sucesso)

# 2) Estatísticas descritivas (Tabela 5): duração em minutos e nº de mensagens
todas %>%
  group_by(grupo) %>%
  summarise(
    n           = n(),
    dur_media   = mean(duracao_seg) / 60,
    dur_dp      = sd(duracao_seg) / 60,
    dur_mediana = median(duracao_seg) / 60,
    dur_q1      = quantile(duracao_seg, 0.25) / 60,
    dur_q3      = quantile(duracao_seg, 0.75) / 60,
    msg_media   = mean(n_mensagens),
    msg_dp      = sd(n_mensagens),
    msg_mediana = median(n_mensagens),
    msg_q1      = quantile(n_mensagens, 0.25),
    msg_q3      = quantile(n_mensagens, 0.75)
  )

# 3) Testes estatísticos
# Mann-Whitney (no R: wilcox.test). Pode aparecer um aviso sobre empates
# ("cannot compute exact p-value with ties"); é esperado e não invalida o teste.
wilcox.test(duracao_seg ~ grupo, data = todas)
wilcox.test(n_mensagens ~ grupo, data = todas)

# Qui-quadrado de independência: tema principal x grupo (Tabela 3)
tabela_tema <- table(todas$topico_1, todas$grupo)
tabela_tema
chisq.test(tabela_tema)

# 4) Análise de sensibilidade dos temas (Tabela 6):
#    refaz a categorização usando só as mensagens dos CLIENTES
texto_cliente <- df %>%
  filter(remetente == "cliente") %>%
  group_by(conversa_id) %>%
  summarise(texto = paste(fala_norm, collapse = " . "), .groups = "drop")

sens <- conversas_uteis %>%
  select(conversa_id, topico_1) %>%
  left_join(texto_cliente, by = "conversa_id")
sens$texto[is.na(sens$texto)] <- ""   # conversa sem fala de cliente -> "Outros"

res <- t(sapply(sens$texto, classificar_topicos))
sens$topico_1_cliente <- res[, 1]

round(prop.table(table(sens$topico_1)) * 100, 1)          # texto completo
round(prop.table(table(sens$topico_1_cliente)) * 100, 1)  # somente cliente
mean(sens$topico_1 != sens$topico_1_cliente)              # fração que mudou de tema (esperado ~0,18)
