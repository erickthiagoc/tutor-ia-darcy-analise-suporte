
# MAPEAMENTO DE CONVERSAS - ZAMMAD / APRENDER 3  (versão 2)
# Projeto: base de treino para futuro tutor de IA
# Autor: Erick
# Data: julho/2026

#
# O QUE MUDOU DA V1
#
# A base agora vem com metadados reais do Zammad:
#   - conversa_id : identifica o ticket
#   - timestamp   : ordena as mensagens dentro do ticket
#   - remetente   : "cliente" ou "suporte"
#
# Isso elimina toda a parte de RECONSTRUÇÃO HEURÍSTICA de conversas e
# de ESTIMATIVA de quem falou (etapas 2 e 3 do script anterior). Elas
# foram substituídas por um simples agrupamento/ordenação (seção 2
# abaixo). O restante do pipeline (limpeza de HTML, mascaramento de
# PII, classificação de desfecho, categorização de tópicos) continua
# com a mesma lógica da v1, porque essas etapas dependem do CONTEÚDO
# do texto, não de metadados.
#
# O desfecho (sucesso/falha/indeterminado) e os tópicos continuam
# sendo estimados por palavras-chave, não por um status oficial do
# ticket -- isso não veio na exportação. Se o Zammad tiver um campo de
# status (aberto/fechado/resolvido), vale pedir ele também: aí a
# classificação de desfecho deixa de ser um "achismo por palavra-chave"
# e vira um dado real.



# ---- pacotes ----
# install.packages(c("readr","dplyr","stringr","tidyr","purrr"))
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

Sys.setlocale("LC_ALL", "C.UTF-8")

CAMINHO_ARQUIVO <- "zammad.csv"


# 1. LEITURA

df <- read_csv(CAMINHO_ARQUIVO, show_col_types = FALSE)

# padroniza nomes de remetente (por segurança, caso venha com espaços/maiúsculas)
df$remetente <- str_to_lower(str_squish(df$remetente))


# 2. LIMPEZA DE TEXTO (igual à v1)


limpar_html <- function(x){
  x <- ifelse(is.na(x), "", x)
  x <- str_replace_all(x, "(?i)<br\\s*/?>", " ")
  x <- str_replace_all(x, "(?i)</p>|</div>|</li>", " ")
  x <- str_replace_all(x, "<[^>]+>", " ")
  x <- str_replace_all(x, "\u00a0", " ")
  str_squish(x)
}

df$fala_limpa <- limpar_html(df$fala)
df$fala_norm  <- str_to_lower(df$fala_limpa)
df <- df %>% filter(fala_norm != "")  # remove linhas vazias remanescentes


# 3. MASCARAMENTO DE DADOS PESSOAIS (LGPD) - igual à v1

# LIMITAÇÃO: nomes próprios não são pegos por regex (precisaria de NER).

mascarar_pii <- function(x){
  x <- str_replace_all(x, "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", "[EMAIL_OCULTO]")
  x <- str_replace_all(x, "\\b\\d{3}\\.?\\d{3}\\.?\\d{3}-?\\d{2}\\b", "[CPF_OCULTO]")
  x <- str_replace_all(x, "\\b\\d{9,15}\\b", "[NUMERO_OCULTO]")
  x
}

df$fala_mascarada <- mascarar_pii(df$fala_limpa)


# 4. AGRUPAMENTO EM CONVERSAS (agora exato, via conversa_id + timestamp)


df <- df %>% arrange(conversa_id, timestamp)

conversas <- df %>%
  group_by(conversa_id) %>%
  summarise(
    n_mensagens    = n(),
    inicio         = min(timestamp),
    fim            = max(timestamp),
    duracao_seg    = as.numeric(difftime(max(timestamp), min(timestamp), units = "secs")),
    texto_completo = paste(fala_norm, collapse = " . "),
    dialogo        = paste0("[", remetente, "] ", fala_mascarada, collapse = "\n"),
    .groups = "drop"
  )

# duracao_seg fica em segundos (numero puro) - bom para ordenar/calcular medias.
# duracao_hms e a versao formatada "HH:MM:SS" - boa para leitura/apresentacao.
# Mantemos as duas: uma para conta, outra para exibir.
formatar_hms <- function(segundos){
  segundos <- round(segundos)
  h <- segundos %/% 3600
  m <- (segundos %% 3600) %/% 60
  s <- segundos %% 60
  sprintf("%02d:%02d:%02d", h, m, s)
}
conversas$duracao_hms <- formatar_hms(conversas$duracao_seg)

# 5. DESFECHO - mesma lógica de palavras-chave da v1, aplicada às
#    últimas mensagens da conversa (em ordem cronológica real agora)

padrao_sucesso <- regex(
  paste("obrigad", "de nada", "disponha", "resolvid", "consegui",
        "funcionou", "deu certo", "show", "perfeito", "ajudou", sep = "|"),
  ignore_case = TRUE
)
padrao_falha <- regex(
  paste("por falta de comunica..o", "atendimento ser. encerrado",
        "n.o consegui", "continua (o mesmo|dando|acontecendo)",
        "ainda n.o (funciona|consigo)", "n.o resolveu", "sem retorno",
        "n.o adiantou", sep = "|"),
  ignore_case = TRUE
)

classificar_desfecho <- function(mensagens){
  if (length(mensagens) == 0) return("indeterminado")
  janela <- rev(tail(mensagens, 3))
  for (m in janela){
    if (str_detect(m, padrao_falha))   return("falha_ou_abandono")
    if (str_detect(m, padrao_sucesso)) return("sucesso")
  }
  "indeterminado"
}

lista_msgs_por_conversa <- df %>%
  group_by(conversa_id) %>%
  summarise(msgs = list(fala_norm), .groups = "drop")

conversas$desfecho <- map_chr(lista_msgs_por_conversa$msgs, classificar_desfecho)


# 6. TÓPICOS - mesmo dicionário da v1

topicos_kw <- list(
  "Acesso e Senha" = regex(
    paste("senha", "login", "logar", "acess[ao]", "cadastr",
          "cpf como login", "esqueci a senha", sep = "|"), ignore_case = TRUE),
  "Disciplinas e Turmas" = regex(
    paste("disciplina", "turma", "mat[eé]ria", "sigaa", "m[oó]dulo",
          "curso", "inscri[cç][aã]o", "matr[ií]cula em", "oferta", "vaga",
          sep = "|"), ignore_case = TRUE),
  "E-mail institucional" = regex(
    paste("e-?mail institucional", "@aluno\\.unb",
          "caixa de entrada institucional", sep = "|"), ignore_case = TRUE),
  "Contato com Professor/Coordenação" = regex(
    paste("professor", "tutor(a)?\\b", "coordena[cç][aã]o", "docente",
          sep = "|"), ignore_case = TRUE),
  "Erros Técnicos da Plataforma" = regex(
    paste("erro", "bug", "trava", "n[aã]o (abre|carrega|funciona)",
          "fora do ar", "inst[aá]vel", "p[aá]gina em branco", sep = "|"),
    ignore_case = TRUE),
  "Contato/Comunicação" = regex(
    paste("telefone", "whatsapp", "liga[cç][aã]o", "e-?mail para",
          "entrar em contato", sep = "|"), ignore_case = TRUE)
)

classificar_topicos <- function(texto){
  contagens <- map_int(topicos_kw, ~ str_count(texto, .x))
  contagens <- contagens[contagens > 0]
  if (length(contagens) == 0) return(c("Outros / Validação Padrão", NA))
  top2 <- names(sort(contagens, decreasing = TRUE))[1:2]
  top2[is.na(top2)] <- NA
  top2
}

topicos_resultado <- t(sapply(conversas$texto_completo, classificar_topicos))
conversas$topico_1 <- topicos_resultado[, 1]
conversas$topico_2 <- topicos_resultado[, 2]


# 7. FILTRAGEM E EXPORTAÇÃO

conversas_uteis <- conversas %>%
  filter(n_mensagens >= 2) %>%
  select(conversa_id, n_mensagens, inicio, fim, duracao_hms, duracao_seg,
         desfecho, topico_1, topico_2, dialogo)

conversas_sucesso     <- conversas_uteis %>% filter(desfecho == "sucesso")
conversas_nao_sucesso <- conversas_uteis %>% filter(desfecho != "sucesso")

write.csv(conversas_sucesso, "conversas_sucesso.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(conversas_nao_sucesso, "conversas_nao_sucesso.csv", row.names = FALSE, fileEncoding = "UTF-8")

# base longa (nível mensagem), útil para auditoria/QA
base_auditoria <- df %>%
  select(conversa_id, timestamp, remetente, fala_mascarada)
write.csv(base_auditoria, "mensagens_auditoria.csv", row.names = FALSE, fileEncoding = "UTF-8")


# 8. RESUMO NO CONSOLE

cat("Total de conversas (tickets):", nrow(conversas), "\n")
cat("Conversas úteis (>=2 mensagens):", nrow(conversas_uteis), "\n")
cat("\nDistribuição de desfecho:\n")
print(table(conversas_uteis$desfecho))
cat("\nDistribuição de tópico principal (%):\n")
print(round(prop.table(table(conversas_uteis$topico_1)) * 100, 2))
cat("\nDuração média das conversas:", formatar_hms(mean(conversas_uteis$duracao_seg)), "(HH:MM:SS)\n")
