## Funções Manuais

# Este script contém a definição de funções manuais personalizadas para 
# manipulação e padronização de dados, utilizadas neste trabalho.

# 1. As funções implementadas têm o objetivo de facilitar a padronização de variáveis 
#    categóricas, conversão de valores binários e ajustes na formatação de dados.

# 2. Cada função é projetada para ser reutilizada ao longo do processo de análise,
#    garantindo consistência nos dados.

# 3. As funções são comentadas de forma detalhada para proporcionar compreensão
#    sobre seu propósito e como usá-las corretamente em diferentes cenários.

# Este conjunto de funções tem como objetivo otimizar o processo de limpeza e preparação dos dados para análise.


# Gera um resumo rapido das variaveis do banco
resumo.variaveis <- function(dados, caption, variaveis_removidas = NULL) {
  resumo_resultado <- data.frame(
    Ordem = paste0("x", seq_along(dados)),
    Variavel = names(dados),
    Tipo_Classe = sapply(dados, function(x) class(x)[1]),
    Tamanho_n = sapply(dados, length),
    NAs = sapply(dados, function(x) sum(is.na(x))),
    Niveis_Categoria = sapply(dados, function(x) if (is.factor(x)) nlevels(x) else NA)
  )
  
  flextable(resumo_resultado) %>%
    set_header_labels(
      Ordem = "Ordem",
      Variavel = "Variável",
      Tipo_Classe = "Tipo (Classe)",
      Tamanho_n = "Tamanho da Amostra (n)",
      NAs = "Valores Ausentes (NAs)",
      Niveis_Categoria = "Quantidade Níveis Categoria"
    ) %>%
    bold(part = "header") %>%                     # Negrito no cabeçalho
    bg(part = "header", bg = "#DCE6F1") %>%       # Cor de fundo do cabeçalho
    color(part = "header", color = "#003366") %>% # Cor da fonte do cabeçalho
    width(j = 1, width = 0.8) %>%
    width(j = 2, width = 3) %>%
    width(j = 3, width = 1.5) %>%
    width(j = 4, width = 2) %>%
    width(j = 5, width = 2.0) %>%
    width(j = 6, width = 1.5) %>%
    set_caption(caption, style = "color: #003366; font-weight: bold; font-size: 12pt;") %>% # Estilo do título
    color(j = 2, part = "body", 
          color = ifelse(resumo_resultado$Variavel %in% variaveis_removidas, "red", "#333333"))
}


# Função para padronizar categorias (removendo acentos e uniformizando)
padronizar.categorias <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")  # Remove acentuação
  x <- tools::toTitleCase(tolower(x))                    # Deixa apenas a primeira letra maiúscula
  return(as.factor(x))                                   # Converte para fator
}

# Função para criar tabelas de frequência para todas as variáveis do data.frame fornecido
tabela.frequencia <- function(dados) {
  lista_var <- list()
  for (k in seq_along(dados)) {
    nome <- names(dados)[k]
    lista_var[[nome]] <- fdt_cat(dados[[k]])
  }
  return(lista_var)
}

tabela.frequencia.hierarquica <- function(dados, titulo = "Tabela de Frequência") {
  flextable({
    lista <- list()
    for (i in seq_along(dados)) {
      nome <- names(dados)[i]
      if (!is.factor(dados[[i]]) && !is.character(dados[[i]])) next
      tabela <- fdt_cat(dados[[i]])
      tabela <- tabela[order(tabela$Category), ]
      colnames(tabela) <- c("Resposta", "Frequência", "Frequência Relativa",
                            "Frequência Relativa (%)", "Frequência Acumulada", "Frequência Acumulada (%)")
      
      # Arredondar as colunas
      tabela[, c("Frequência", "Frequência Relativa", "Frequência Relativa (%)")] <-
        round(tabela[, c("Frequência", "Frequência Relativa", "Frequência Relativa (%)")], 2)
      
      # Adiciona o símbolo de porcentagem apenas na visualização, sem alterar o tipo de dado
      tabela$`Frequência Relativa (%)` <- paste0(tabela$`Frequência Relativa (%)`, " %")
      tabela$`Frequência Relativa` <- paste0(tabela$`Frequência Relativa`, " %")
      
      linha_titulo <- data.frame(
        Pergunta = nome,
        Resposta = NA,
        Frequência = NA,
        `Frequência Relativa (%)` = NA,
        check.names = FALSE
      )
      tabela$Pergunta <- NA
      tabela <- tabela[, c("Pergunta", "Resposta", "Frequência", "Frequência Relativa (%)")]
      linha_titulo <- linha_titulo[, c("Pergunta", "Resposta", "Frequência", "Frequência Relativa (%)")]
      lista[[i]] <- rbind(linha_titulo, tabela)
    }
    resultado <- do.call(rbind, lista)
    rownames(resultado) <- NULL
    resultado
  }) %>%
    set_caption(titulo) %>%
    bold(part = "header") %>%  # Coloca o cabeçalho em negrito
    align(i = 1, part = "header", align = "center") %>%  # Alinha o cabeçalho no centro
    align(j = "Resposta", align = "left") %>%  # Alinha 'Resposta' à esquerda
    align(j = c("Frequência", "Frequência Relativa (%)"), align = "center") %>%  # Centraliza 'Frequência' e 'Frequência Relativa (%)'
    {
      # Adiciona a borda nas linhas de categorias
      vars <- names(dados)[sapply(dados, function(x) is.factor(x) || is.character(x))]
      indices <- cumsum(sapply(vars, function(v) length(unique(dados[[v]])))) + seq_along(vars)
      for (i in indices) {
        . <- border(., i = i, border.bottom = fp_border(width = 1.5, color = "black"))
      }
      autofit(.)
    }
}


# Função para gerar a tabela de contingência com percentuais
tabela.teste.qui.quadrado <- function(df, classificacao, fator_risco) {
  # Criar a tabela de contingência e pivotar diretamente
  tf <- table(df[[classificacao]], df[[fator_risco]]) %>%
    as.data.frame.table() %>%
    setNames(c("Classificacao", "Fator_de_Risco", "Freq")) %>%
    pivot_wider(names_from = Fator_de_Risco, values_from = Freq, values_fill = list(Freq = 0))
  
  # Adicionar a coluna Total (por linha)
  tf$Total <- tf$Nao + tf$Sim
  
  # Total geral da amostra
  total_geral <- sum(tf$Total)
  
  # Percentuais em relação à amostra total, agora com quebra de linha
  tf$Nao_fmt   <- paste0(tf$Nao, "\n(", round(100 * tf$Nao / total_geral, 2), "%)")
  tf$Sim_fmt   <- paste0(tf$Sim, "\n(", round(100 * tf$Sim / total_geral, 2), "%)")
  tf$Total_fmt <- paste0(tf$Total, "\n(", round(100 * tf$Total / total_geral, 2), "%)")
  
  # Adicionar coluna valor_p como NA
  tf$valor_p <- NA
  
  # Selecionar apenas as colunas formatadas
  tff <- tf[, c("Classificacao", "Sim_fmt", "Nao_fmt", "Total_fmt", "valor_p")]
  
  # Totais para a linha final
  sim_total <- sum(tf$Sim)
  nao_total <- sum(tf$Nao)
  linha_total <- sum(tf$Total)
  percentual_total <- round(100 * linha_total / total_geral, 2)
  
  # Adicionar linha Total final
  tff <- rbind(tff,
               data.frame(
                 Classificacao = "Total",
                 Sim_fmt   = paste0(sim_total, "\n(", round(100 * sim_total / total_geral, 2), "%)"),
                 Nao_fmt   = paste0(nao_total, "\n(", round(100 * nao_total / total_geral, 2), "%)"),
                 Total_fmt = paste0(linha_total, "\n(", percentual_total, "%)"),
                 valor_p   = NA
               )
  )
  
  # Gerar a tabela final com o flextable
  flextable(tff) %>%
    set_header_labels(
      Classificacao = "Classificação",
      Sim_fmt = "Sim",
      Nao_fmt = "Não",
      Total_fmt = "Total",
      valor_p = "Valor P"
    ) %>%
    set_caption(paste("Teste Qui Quadrado para a variavel ",fator_risco," mediante a", classificacao)) %>%
    autofit() %>%
    add_header_row(
      values = c("", fator_risco, "Teste Qui-Quadrado"),
      colwidths = c(1, 3, 1)) %>% 
    bold(part = "header") %>%  
    align(j = c("Sim_fmt", "Nao_fmt", "Total_fmt", "valor_p"), align = "center", part = "all") %>% 
    border(i = length(unique(df[[classificacao]])), border.bottom = fp_border(width = 1.5, color = "black")) %>% 
    width(j = 1, width = 3) %>%
    width(j = c(2,3,4), width = 1.2) %>%
    width(j = 5, width = 2)
}

# Verifica e instala pacotes ausentes
carregar.pacotes <- function(p) {
  inst <- p[!p %in% installed.packages()]
  if (length(inst)) install.packages(inst, dependencies = TRUE)
  invisible(lapply(p, library, character.only = TRUE))
}


