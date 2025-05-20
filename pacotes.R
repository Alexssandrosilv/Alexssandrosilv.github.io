## Bibliotecas

# Este script configura o ambiente com as bibliotecas "pacotes" utilizados neste trabalho
# 
# 1.  Bibliotecas ausentes são instalados automaticamente.
# 2.  Todos as bibliotecas são carregadas de forma eficiente.
# 3.  Cada biblioteca é comentada para facilitar a consulta sobre seu uso e aplicabilidade.

## Carregando os Pacotes
pacotes <- c(
  # Leitura de dados
  "readxl",          # Leitura de arquivos Excel (.xls e .xlsx)
  
  # Conjunto de pacotes integrados
  "tidyverse",       # Inclui dplyr, ggplot2, tidyr, readr, tibble, stringr, forcats
  
  # Estatística descritiva e psicometria
  "psych",           # Estatísticas descritivas, psicometria e análise fatorial
  "Hmisc",           # Imputação, tabelas de frequência, descrição de dados
  "skimr",           # Sumários estatísticos rápidos e informativos
  
  # Tabelas e relatórios
  "officer",       # Geração de documentos Word (.docx) e PowerPoint (.pptx)
  "flextable",       # Tabelas para Word, PowerPoint e HTML
  "fdth",            # Tabelas de frequência (quantitativos e qualitativos)
  
  # Integração com Google
  "googlesheets4",   # Leitura/escrita de planilhas no Google Sheets
  "googledrive"      # Acesso e manipulação de arquivos no Google Drive
)

# Verifica e instala pacotes ausentes
carregar.pacotes <- function(p) {
  inst <- p[!p %in% installed.packages()]
  if (length(inst)) install.packages(inst, dependencies = TRUE)
  invisible(lapply(p, library, character.only = TRUE))
}

# Carrega os pacotes
carregar.pacotes(pacotes)
