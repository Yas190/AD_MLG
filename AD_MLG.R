#ANALISE DESCRITIVA#############################################################
library(ggplot2)
library(dplyr)
library(readr)
library(hnp)
library(modEvA)
library(broom)
library(reshape2)

# Carregar os dados
df <- read_csv("advertising.csv")
df <- as_tibble(df)

# Exibir as primeiras linhas do dataframe
head(df)

# Estrutura e resumo dos dados
str(df)
summary(df)

# Converter a coluna Timestamp para o formato de data
df$Timestamp <- as.POSIXct(df$Timestamp, format = "%Y-%m-%d %H:%M:%S")

# Percentual das idades mais comuns
df_percentual_age <- df %>%
  count(Age) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(percent)) %>%
  head(10)

# Gráfico de barras das idades mais comuns
ggplot(df_percentual_age, aes(x = reorder(Age, -percent), y = percent)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_text(aes(label = sprintf("%.2f%%", percent)), vjust = -0.5, size = 3.5) +
  labs(x = "Idade", y = "Percentual", title = "Distribuição Percentual das Idades Mais Comuns") +
  theme_minimal(base_size = 14)

# Histograma das idades
ggplot(df, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "dodgerblue", color = "black") +
  labs(x = "Idade", y = "Frequência", title = "Histograma das Idades") +
  theme_minimal(base_size = 14)

# Percentual de gênero (Male)
df_percentual_male <- df %>%
  count(Male) %>%
  mutate(percent = n / sum(n) * 100)

# Gráfico de barras do percentual de gênero
ggplot(df_percentual_male, aes(x = factor(Male, labels = c("Feminino", "Masculino")), y = percent)) +
  geom_bar(stat = "identity", fill = c("pink", "lightblue"), color = "black") +
  geom_text(aes(label = sprintf("%.2f%%", percent)), vjust = -0.5, size = 3.5) +
  labs(x = "Gênero", y = "Percentual", title = "Distribuição Percentual por Gênero") +
  theme_minimal(base_size = 14)

# Percentual de cliques no anúncio
df_percentual_click <- df %>%
  count(`Clicked on Ad`) %>%
  mutate(percent = n / sum(n) * 100)

# Gráfico de barras dos cliques
ggplot(df_percentual_click, aes(x = factor(`Clicked on Ad`, labels = c("Não clicou", "Clicou")), y = percent)) +
  geom_bar(stat = "identity", fill = c("tomato", "forestgreen"), color = "black") +
  geom_text(aes(label = sprintf("%.2f%%", percent)), vjust = -0.5, size = 3.5) +
  labs(x = "Clique no Anúncio", y = "Percentual", title = "Distribuição Percentual de Cliques") +
  theme_minimal(base_size = 14)

# Boxplots para variáveis numéricas
# Boxplot: Tempo diário no site vs. clique no anúncio
ggplot(df, aes(x = factor(`Clicked on Ad`, labels = c("Não clicou", "Clicou")), y = `Daily Time Spent on Site`)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Clique no Anúncio", y = "Tempo Diário no Site", title = "Boxplot: Tempo Diário no Site por Clique") +
  theme_minimal(base_size = 14)

# Boxplot: Renda da área vs. clique no anúncio
ggplot(df, aes(x = factor(`Clicked on Ad`, labels = c("Não clicou", "Clicou")), y = `Area Income`)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(x = "Clique no Anúncio", y = "Renda da Área", title = "Boxplot: Renda da Área por Clique") +
  theme_minimal(base_size = 14)

# Mapa de calor das correlações
# Selecionar variáveis numéricas
numeric_vars <- dplyr::select(df, `Daily Time Spent on Site`, `Age`,`Area Income`, `Daily Internet Usage`, `Clicked on Ad`)

# Calcular a matriz de correlação e arredondar os valores
cor_matrix <- round(cor(numeric_vars), 2)

# Derreter a matriz de correlação para formato longo
cor_long <- melt(cor_matrix)

# Manter apenas a metade inferior da matriz
cor_long <- cor_long[as.numeric(cor_long$Var1) > as.numeric(cor_long$Var2), ]

# Heatmap de correlação triangular com anotações
ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "white", high = "red", mid = "lightpink", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name = "Correlação") +
  labs(x = "", y = "", title = "Mapa de Calor das Correlações") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_fixed()

################################################################################

str(df)
df$Clicked_on_Ad <- as.factor(df$`Clicked on Ad`)

# Converter `Male` em fator (0 = feminino, 1 = masculino)
df$Male <- as.factor(df$Male)

# Selecionar as variáveis de interesse e a variável resposta
df_model <- dplyr::select(df, `Clicked on Ad`, Age, `Daily Time Spent on Site`, `Area Income`, `Daily Internet Usage`, Male)
df_model2 <- df_model[-246, ]


modelo <- glm(`Clicked on Ad` ~ Age + `Daily Time Spent on Site` + `Area Income` + `Daily Internet Usage`,
              data = df_model2, family = binomial)
summary(modelo)
# Ajustar o modelo de regressão logística

# modelo_old <- glm(Clicked_on_Ad ~ Age + `Daily Time Spent on Site` + `Area Income` + `Daily Internet Usage` + Male,
#               data = df_model, family = binomial)
# 
# summary(modelo)
# 
# step(modelo)

modelo_old <- glm(`Clicked on Ad` ~ Age + `Daily Time Spent on Site` + `Area Income` + `Daily Internet Usage`,
              data = df_model, family = binomial)
summary(modelo_old)


# Verificação do ajuste do modelo usando o desvio
n <- nrow(df) # Tamanho amostral
D <- deviance(modelo) # Desvio do modelo
gl <- n - modelo$rank # Graus de liberdade
pvalor <- 1 - pchisq(D, gl) # Nível descritivo
cat("P-valor para o ajuste do modelo:", pvalor, "\n")

### ANÁLISE DE DIAGNÓSTICO

# Calcular resíduos studentizados e de desvio manualmente e adicionar índice
residuos <- df %>%
  mutate(
    Index = row_number(),
    resid_pearson = rstandard(modelo),        # Resíduos studentizados
    resid_deviance = residuals(modelo, type = "deviance"), # Resíduos de desvio
    fitted_values = fitted(modelo)            # Valores ajustados
  )

# # Resíduos studentizados vs. índices
# ggplot(residuos, aes(x = Index, y = resid_pearson)) +
#   geom_point(color = "dodgerblue") +
#   geom_hline(yintercept = c(-2, 0, 2), linetype = "dashed", color = "red") +
#   labs(title = "Resíduos Studentizados vs Índices", x = "Índice", y = "Resíduo Studentizado") +
#   theme_minimal()
# 
# # Resíduos studentizados vs. valores ajustados
# ggplot(residuos, aes(x = fitted_values, y = resid_pearson)) +
#   geom_point(color = "purple") +
#   geom_hline(yintercept = c(-2, 0, 2), linetype = "dashed", color = "red") +
#   labs(title = "Resíduos Studentizados vs Ajustados", x = "Valores Ajustados", y = "Resíduo Studentizado") +
#   theme_minimal()

# Resíduos de desvio vs. índices
ggplot(residuos, aes(x = Index, y = resid_deviance)) +
  geom_point(color = "forestgreen") +
  geom_hline(yintercept = c(-2, 0, 2), linetype = "dashed", color = "red") +
  labs(title = "Resíduos de Desvio vs Índices", x = "Índice", y = "Resíduo de Desvio") +
  theme_minimal()

# # Resíduos de desvio vs. valores ajustados
# ggplot(residuos, aes(x = fitted_values, y = resid_deviance)) +
#   geom_point(color = "darkorange") +
#   geom_hline(yintercept = c(-2, 0, 2), linetype = "dashed", color = "red") +
#   labs(title = "Resíduos de Desvio vs Ajustados", x = "Valores Ajustados", y = "Resíduo de Desvio") +
#   theme_minimal()

# Envelope de simulação para os resíduos (usando hnp)
hnp(modelo, half = FALSE, sim = 500, conf = 0.95, paint.out = T)

### ANÁLISE DE ALAVANCAGEM E INFLUÊNCIA ###

# Alavancagem
alavancagem <- hatvalues(modelo)
df$Alavancagem <- alavancagem

# ggplot(df, aes(x = modelo$fitted.values, y = Alavancagem)) +
#   geom_point(color = "steelblue") +
#   labs(title = "Alavancagem vs Ajustados", x = "Valores Ajustados", y = "Alavancagem") +
#   theme_minimal()

# Gráfico de alavancagem por índice
ggplot(residuos, aes(x = Index, y = alavancagem)) +
  geom_point(color = "darkred") +
  geom_hline(yintercept = 2 * mean(alavancagem), linetype = "dashed", color = "blue") +  # Linha de referência
  labs(title = "Alavancagem por Índice", x = "Índice", y = "Alavancagem") +
  theme_minimal()

limiar_alavancagem <- 2 * mean(alavancagem)
pontos_alavancagem <- which(alavancagem > limiar_alavancagem)
cat("Pontos influentes por alavancagem (>", limiar_alavancagem, "):", pontos_alavancagem, "\n")

# DFFITS
dffits_vals <- dffits(modelo)
df$DFFITS <- dffits_vals

ggplot(df, aes(x = 1:nrow(df), y = DFFITS)) +
  geom_point(color = "firebrick") +
  labs(title = "DFFITS por Índice", x = "Índice", y = "DFFITS") +
  theme_minimal()

limiar_dffits <- 2 * sqrt(ncol(df) / nrow(df))
pontos_dffits <- which(abs(dffits_vals) > limiar_dffits)
cat("Pontos influentes por DFFITS (|>|", limiar_dffits, "):", pontos_dffits, "\n")

# Distância de Cook
cooks_dist <- cooks.distance(modelo)
df$Cooks <- cooks_dist

# ggplot(df, aes(x = modelo$fitted.values, y = Cooks)) +
#   geom_point(color = "darkgreen") +
#   labs(title = "Distância de Cook vs Ajustados", x = "Valores Ajustados", y = "Distância de Cook") +
#   theme_minimal()

# Plotar a Distância de Cook por Índice
ggplot(df, aes(x = 1:nrow(df), y = Cooks)) +
  geom_point(color = "firebrick") +
  geom_hline(yintercept = c(0.5, 1), linetype = "dashed", color = "red") +
  labs(title = "Distância de Cook por Índice", x = "Índice", y = "Distância de Cook") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pontos_cooks <- which(cooks_dist > 0.5)  # Usando 0.5 como referência
cat("Pontos influentes por Distância de Cook (> 0.5):", pontos_cooks, "\n")

### PSEUDO-R² ###

# Pseudo-R² do modelo
pseudo_r2 <- RsqGLM(modelo)
cat("Pseudo-R² do Modelo:\n")
print(pseudo_r2)

### VERIFICAÇÃO DA FUNÇÃO DE LIGAÇÃO ###

eta_hat <- modelo$fitted.values
eta_hat2 <- eta_hat^2

# Verificação da função de ligação incluindo eta_hat^2
verificar_ligacao <- glm(`Clicked on Ad` ~ Age + `Daily Time Spent on Site` + `Area Income` + `Daily Internet Usage` + eta_hat2,
                         data = df, family = binomial)
summary(verificar_ligacao)

# interpretaC'C#o das covariC!veis
# Age
exp(1.843e-01)

#`Daily Time Spent on Site
exp(-1.946e-01)

#`Area Income`
exp(1.441e-04)

#`Daily Internet Usage`
exp(-6.438e-02)

################################################################################

# Transformações nas covariáveis
# df_trans <- df %>%
#   mutate(
#     log_Age = log(Age + 1),  # Logaritmo (adicionamos +1 para evitar log(0))
#     sqrt_Age = sqrt(Age),     # Raiz quadrada
#     Age_squared = Age^2,      # Quadrado
#     
#     log_DailyTime = log(`Daily Time Spent on Site` + 1),
#     sqrt_DailyTime = sqrt(`Daily Time Spent on Site`),
#     DailyTime_squared = `Daily Time Spent on Site`^2,
#     
#     log_AreaIncome = log(`Area Income` + 1),
#     sqrt_AreaIncome = sqrt(`Area Income`),
#     AreaIncome_squared = `Area Income`^2,
#     
#     log_InternetUsage = log(`Daily Internet Usage` + 1),
#     sqrt_InternetUsage = sqrt(`Daily Internet Usage`),
#     InternetUsage_squared = `Daily Internet Usage`^2
#   )
# 
# # Ajustar modelos com diferentes transformações e comparar pelo AIC
# # Modelo com logaritmo
# modelo_log <- glm(Clicked_on_Ad ~ log_Age + log_DailyTime + log_AreaIncome + log_InternetUsage,
#                   data = df_trans, family = binomial)
# cat("AIC do modelo com logaritmo:", AIC(modelo_log), "\n")
# 
# summary(modelo_log)
# 
# hnp(modelo_log, half = FALSE, sim = 500, conf = 0.95, paint.out = T)

