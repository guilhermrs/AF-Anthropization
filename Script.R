#### Ecoregions ####
# load packages
library(MuMIn)
library(nlme)
library(ggplot2)
library(dplyr)
library(viridis)
library(ggplot2)

dados <- read.csv("Estatisticas.csv", header = TRUE)

RStudio.Version()$version
R.version.string
### loop para deletar todos hexagonos repetidos "id" para trabalhar individualmente


dados_semrepeticao <- data.frame(area=NA,ID= NA, Longitude= NA, Latitude = NA, Slope_mean = NA, Elevation_= NA, ECO_NAME= NA, G200_REGIO=NA, X_antropiza= NA) #criando uma tabela vazia que guardará os resultados do loop abaixo

ids <- unique(dados$ID) #contagem dos ids unicos, que não se repetem


for(i in 1:length(ids)){#para cada valor de i que vai de 1 até 13149, faça os comandos abaixo
  linhas<- which(dados$ID==i)#quais linhas da planilha original possuem o i
  if(length(linhas)==1){#se tiver só uma linha copie os dados dela p "dados_semrepeticao"
    dados_semrepeticao[nrow(dados_semrepeticao)+1,]<-dados[linhas,]
  }
}

dados_semrepeticao <- dados_semrepeticao[-1,] #excluir a primeira linha de "dados 3 que tinha NAs"

table(dados_semrepeticao$ECO_NAME)

#Retirar as ecoregiões com n<100,excluir dados com NA e Cerrado###

linhas_excluir1 <- which(dados_semrepeticao$ECO_NAME==""| dados_semrepeticao$ECO_NAME=="Atlantic Coast restingas"|dados_semrepeticao$ECO_NAME=="Campos Rupestres montane savanna"|dados_semrepeticao$ECO_NAME=="Maranhão Babaçu forests"|dados_semrepeticao$ECO_NAME=="Pantanal"|dados_semrepeticao$ECO_NAME=="Southern Atlantic mangroves"|dados_semrepeticao$ECO_NAME=="Caatinga Enclaves moist forests"|dados_semrepeticao$ECO_NAME=="Northeastern Brazil restingas"|dados_semrepeticao$ECO_NAME=="Pantanal"|dados_semrepeticao$ECO_NAME=="Cerrado"|dados_semrepeticao$ECO_NAME=="Caatinga")

dados_ecoregioes <- dados_semrepeticao[-linhas_excluir1,]

table(dados_ecoregioes$ECO_NAME)

### Test Correlation
cor(dados_ecoregioes$Slope_mean, dados_ecoregioes$Elevation_, method = c("pearson"))

cor(dados_ecoregioes$Slope_mean, dados_ecoregioes$Elevation_, method = c("pearson"))


hist(residuals(MintElvSlpEco))

write.csv(dados_ecoregioes, "dados_ecoregioes.csv")

## 1º Model Selection

modelo_corLin_ecoregion <- gls(X_antropiza~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, correlation = corLin(form = ~Latitude + Longitude), method = "REML")

modelo_corExp_ecoregion <- gls(X_antropiza~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

modelo_Gaus_ecoregion <- gls(X_antropiza~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, correlation = corGaus(form = ~Latitude + Longitude), method = "REML")

modelo_Ratio_ecoregion <- gls(X_antropiza~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, correlation = corRatio(form = ~Latitude + Longitude), method = "REML")

modelo_Spher_ecoregion <- gls(X_antropiza~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, correlation = corSpher(form = ~Latitude + Longitude), method = "REML")

modelo_null_ecoregion <- gls(X_antropiza~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, method = "REML")

model.sel(modelo_corLin_ecoregion, modelo_corExp_ecoregion, modelo_Gaus_ecoregion, modelo_Ratio_ecoregion, modelo_Spher_ecoregion, modelo_null_ecoregion)

summary(modelo_corExp_ecoregion) ### ganhou, próxima etapa rodar para outras combinações ##

## 2º Seleção de Modelos

## without interaction 8 possibilities

MEl <- gls(X_antropiza~Elevation_, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MSlp <- gls(X_antropiza~Slope_mean, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MEco <- gls(X_antropiza~ECO_NAME, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MElvSlp <- gls(X_antropiza~Elevation_+Slope_mean, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MElevEco <- gls(X_antropiza~Elevation_+ECO_NAME, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MSlpEco <- gls(X_antropiza~Slope_mean+ECO_NAME, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MElvSlpEco <- gls(X_antropiza~Elevation_+Slope_mean+ECO_NAME, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

Mnull <- gls(X_antropiza~1, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

## w/ interaction 3 possibilities

MIntSlpEco <- gls(X_antropiza~Slope_mean*ECO_NAME, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MintElvEco <- gls(X_antropiza~Elevation_*ECO_NAME, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MintElvSlpEco<- gls(X_antropiza~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

model.sel(MEl,MSlp,MEco,MElvSlp,MElevEco,MSlpEco, MElvSlpEco, Mnull, MIntSlpEco, MintElvEco, MintElvSlpEco)


saveRDS(MEl, file = "MEl.rds")

saveRDS(MSlp, file = "MSlp.rds")

saveRDS(MEco, file = "MEco.rds")

saveRDS(MElvSlp, file = "MElvSlp.rds")

saveRDS(MElevEco, file = "MElevEco.rds")

saveRDS(MSlpEco, file = "MSlpEco.rds")

saveRDS(MElvSlpEco, file = "MElvSlpEco.rds")

saveRDS(Mnull, file = "Mnull.rds")

saveRDS(MIntSlpEco, file = "MIntSlpEco.rds")

saveRDS(MintElvEco, file = "MintElvEco.rds")

saveRDS(MintElvSlpEco, file = "MintElvSlpEco.rds")

# Restore the object
## 1 analysis
modelo_corLin_ecoregion <- readRDS(file = "modelo_corLin_ecoregion.rds")
modelo_corExp_ecoregion <- readRDS(file = "modelo_corExp_ecoregion.rds")
modelo_Gaus_ecoregion <- readRDS(file = "modelo_Gaus_ecoregion.rds")
modelo_Ratio_ecoregion <- readRDS(file = "modelo_Ratio_ecoregion.rds")
modelo_Spher_ecoregion <- readRDS(file = "modelo_Spher_ecoregion.rds")
modelo_null_ecoregion <- readRDS(file = "modelo_null_ecoregion.rds")

## 2 analysis
MEl <- readRDS(file = "MEl.rds")
MSlp <- readRDS(file = "MSlp.rds")
MEco <- readRDS(file = "MEco.rds")
MElvSlp <- readRDS(file = "MElvSlp.rds")
MElevEco <- readRDS(file = "MElevEco.rds")
MSlpEco <- readRDS(file = "MSlpEco.rds")
MElvSlpEco <- readRDS(file = "MElvSlpEco.rds")
Mnull <- readRDS(file = "Mnull.rds")
MIntSlpEco <- readRDS(file = "MIntSlpEco.rds")
MintElvEco <- readRDS(file = "MintElvEco.rds")
MintElvSlpEco <- readRDS(file = "MintElvSlpEco.rds")

write.csv(dados_ecoregioes, "dados.ecoregiões.csv")

## Summary 
summary(MintElvSlpEco)


# Criando o dataframe com os dados fornecidos
dados_coefficients <- data.frame(
  Ecoregion = c("Alto Parana Atlantic forests", "Araucaria moist forests", 
                "Atlantic dry forests", "Bahia coastal forests", "Bahia interior forests", 
                "Pernambuco coastal forests", "Pernambuco interior forests", "Serra do Mar coastal forests"),
  Elevation = c(0.0001609, -0.0001161, -0.0008173, -0.0002721, -0.0002547, -0.0001134, -0.0000301, -0.0001446),
  Elevation_Std_Error = c(0.0000322, 0.0000666, 0.0001054, 0.0000882, 0.0000733, 0.00021844, 0.000194449, 0.00007),
  Slope = c(-0.0148021, -0.0107115, -0.0093326, -0.0079689, -0.0082714, -0.0117439, -0.0101556, -0.01199),
  Slope_Std_Error = c(0.000719681, 0.001559485, 0.002237191, 0.001751799, 0.001615089, 0.003539805, 0.003687306, 0.001618884)
)

# Calcular desvio-padrão das variáveis
sd_elevation <- sd(dados$Elevation)
sd_slope <- sd(dados$Slope)

# Criando novos coeficientes padronizados
dados_coefficients$Elevation_z <- dados_coefficients$Elevation / sd_elevation
dados_coefficients$Slope_z <- dados_coefficients$Slope / sd_slope


# Carregar os dados originais
df <- read.csv("ecoregion_interactions_with_std_corrected.csv")

# Calcular os coeficientes padronizados
df$Elevation_z <- df$Elevation / sd(df$Elevation)
df$Slope_z <- df$Slope / sd(df$Slope)

# Salvar o novo CSV para garantir que os dados padronizados estão disponíveis
write.csv(df, "ecoregion_interactions_with_std_corrected.csv", row.names = FALSE)


# Função para criar o forest plot com coeficientes padronizados
plot_forest_combined <- function(data_path) {
  # Ler os dados
  df <- read.csv(data_path)
  
  # Transformar os dados em formato longo
  df_long <- df %>%
    tidyr::pivot_longer(cols = c(Elevation_z, Slope_z),
                        names_to = "Variable",
                        values_to = "Coefficient") %>%
    tidyr::pivot_longer(cols = c(Elevation_Std_Error, Slope_Std_Error),
                        names_to = "Error_Variable",
                        values_to = "Std_Error") %>%
    filter((Variable == "Elevation_z" & Error_Variable == "Elevation_Std_Error") |
             (Variable == "Slope_z" & Error_Variable == "Slope_Std_Error"))
  
  # Criar o gráfico combinado
  ggplot(df_long, aes(x = Coefficient, y = Ecoregion, color = Variable)) +
    geom_point(size = 3) +
    geom_errorbar(aes(xmin = Coefficient - Std_Error, xmax = Coefficient + Std_Error), width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    scale_color_viridis_d(option = "D") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black")
    ) +
    labs(title = "Forest Plot com Coeficientes Padronizados",
         x = "Coefficients (Z-Transformation)",
         y = "Ecoregion",
         color = "Variable")
}

# Uso da função
plot_forest_combined("ecoregion_interactions_with_std_corrected.csv")




## Forest Plot
# Função para criar o forest plot
plot_forest <- function(data_path) {
  # Ler os dados
  df <- read.csv(data_path)
  
  # Transformar os dados em formato longo para facilitar o plot
  df_long <- df %>%
    tidyr::pivot_longer(cols = c(Elevation, Slope),
                        names_to = "Variable",
                        values_to = "Coefficient") %>%
    tidyr::pivot_longer(cols = c(Elevation_Std_Error, Slope_Std_Error),
                        names_to = "Error_Variable",
                        values_to = "Std_Error") %>%
    filter((Variable == "Elevation" & Error_Variable == "Elevation_Std_Error") |
             (Variable == "Slope" & Error_Variable == "Slope_Std_Error"))
  
  # Criar o gráfico
  ggplot(df_long, aes(x = Coefficient, y = Ecoregion, color = Variable)) +
    geom_point(size = 3) +
    geom_errorbar(aes(xmin = Coefficient - Std_Error, xmax = Coefficient + Std_Error), width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    scale_color_viridis_d(option = "D") +
    facet_wrap(~Variable, scales = "free_x") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),  # Remove linhas de grade
      axis.line = element_line(color = "black"),  # Adiciona eixos x e y
      strip.text = element_blank()  # Remove facet titles
    ) +
    labs(title = "",
         x = "Coefficients",
         y = "Ecoregion",
         color = "Variable")
}

# Uso da função
plot_forest("ecoregion_interactions_with_std_corrected.csv")


# Função para criar o forest plot com as variáveis no mesmo gráfico
plot_forest_combined <- function(data_path) {
  # Ler os dados
  df <- read.csv(data_path)
  
  # Transformar os dados em formato longo
  df_long <- df %>%
    tidyr::pivot_longer(cols = c(Elevation, Slope),
                        names_to = "Variable",
                        values_to = "Coefficient") %>%
    tidyr::pivot_longer(cols = c(Elevation_Std_Error, Slope_Std_Error),
                        names_to = "Error_Variable",
                        values_to = "Std_Error") %>%
    filter((Variable == "Elevation" & Error_Variable == "Elevation_Std_Error") |
             (Variable == "Slope" & Error_Variable == "Slope_Std_Error"))
  
  # Criar o gráfico combinado
  ggplot(df_long, aes(x = Coefficient, y = Ecoregion, color = Variable)) +
    geom_point(size = 3) +
    geom_errorbar(aes(xmin = Coefficient - Std_Error, xmax = Coefficient + Std_Error), width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    scale_color_viridis_d(option = "D") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),  # Remove linhas de grade
      axis.line = element_line(color = "black")  # Adiciona eixos x e y
    ) +
    labs(title = "Forest Plot com Altitude e Declividade",
         x = "Coefficients",
         y = "Ecoregion",
         color = "Variable")
}

# Uso da função
plot_forest_combined("ecoregion_interactions_with_std_corrected.csv")



# Função para criar o forest plot com padronização da altitude
plot_forest_standardized <- function(data_path) {
  # Ler os dados
  df <- read.csv(data_path)
  
  # Padronizar os coeficientes e erros padrão da altitude
  df <- df %>%
    mutate(
      Elevation = Elevation * 30,
      Elevation_Std_Error = Elevation_Std_Error * 30
    )
  
  # Transformar os dados em formato longo
  df_long <- df %>%
    tidyr::pivot_longer(cols = c(Elevation, Slope),
                        names_to = "Variable",
                        values_to = "Coefficient") %>%
    tidyr::pivot_longer(cols = c(Elevation_Std_Error, Slope_Std_Error),
                        names_to = "Error_Variable",
                        values_to = "Std_Error") %>%
    filter((Variable == "Elevation" & Error_Variable == "Elevation_Std_Error") |
             (Variable == "Slope" & Error_Variable == "Slope_Std_Error"))
  
  # Criar o gráfico combinado
  ggplot(df_long, aes(x = Coefficient, y = Ecoregion, color = Variable)) +
    geom_point(size = 3) +
    geom_errorbar(aes(xmin = Coefficient - Std_Error, xmax = Coefficient + Std_Error), width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    scale_color_viridis_d(option = "D") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),  # Remove linhas de grade
      axis.line = element_line(color = "black")  # Adiciona eixos x e y
    ) +
    labs(title = "",
         x = "Coefficients (per-unit)",
         y = "Ecoregion",
         color = "Variable")
}

# Uso da função
plot_forest_standardized("ecoregion_interactions_with_std_corrected.csv")


### Plot Slope controlling Elevation (=median)

dados_ecoregioes_controlando_slp <- dados_ecoregioes

dados_ecoregioes_controlando_slp$Slope_mean <- median(dados_ecoregioes_controlando_slp$Slope_mean)

dados_ecoregioes_controlando_slp$predictions <- predict(MintElvSlpEco, newdata = dados_ecoregioes_controlando_slp)

dados_ecoregioes_controlando_slp$resid <- dados_ecoregioes_controlando_slp$X_antropiza - dados_ecoregioes_controlando_slp$predictions

dados_ecoregioes_controlando_slp$partial <- dados_ecoregioes_controlando_slp$predictions+dados_ecoregioes_controlando_slp$resid # 


cor(dados_ecoregioes_controlando_slp$predictions, dados_ecoregioes_controlando_slp$Elevation_, method = c("pearson"))


### Plot

ggplot(dados_ecoregioes_controlando_slp, aes(x = Elevation_, y = predictions, color = ECO_NAME)) +
  geom_point(aes(y = partial), shape = 16, size = 0.7, alpha = 0.8) +  # Points: residuals + predictions
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +  # Lines: predicted values
  labs(
    x = "Elevation (m)", 
    y = "Anthropized Area",
    title = "",
    subtitle = "",
    color = NULL  # Remove color legend label
  ) +
  facet_wrap(~ ECO_NAME, ncol = 3, scales = "free_y") +  # Independent y-axes for each ecoregion
  scale_color_viridis_d(option = "D") + 
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",  # Remove legend
    panel.grid = element_blank(),
    panel.spacing = unit(1.5, "lines"),  # Increased spacing between facets
    axis.line = element_line(color = "black")
  )


### Plot Slope controlling Elevation (= median)

dados_ecoregioes_controlando_elevation <- dados_ecoregioes

dados_ecoregioes_controlando_elevation$Elevation_ <- median(dados_ecoregioes_controlando_elevation$Elevation_)

dados_ecoregioes_controlando_elevation$predictions <- predict(MintElvSlpEco, newdata = dados_ecoregioes_controlando_elevation)

dados_ecoregioes_controlando_elevation$resid <- dados_ecoregioes_controlando_elevation$X_antropiza - dados_ecoregioes_controlando_elevation$predictions

dados_ecoregioes_controlando_elevation$partial <- dados_ecoregioes_controlando_elevation$predictions+dados_ecoregioes_controlando_elevation$resid # 


cor(dados_ecoregioes_controlando_elevation$predictions, dados_ecoregioes_controlando_elevation$Slope_mean, method = c("pearson"))


### Plot

ggplot(dados_ecoregioes_controlando_elevation, aes(x = Slope_mean, y = predictions, color = ECO_NAME)) +
  geom_point(aes(y = partial), shape = 16, size = 0.7, alpha = 0.8) +  # Points: residuals + predictions
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +  # Lines: predicted values
  labs(
    x = "Slope (%)", 
    y = "Anthropized Area",
    title = "",
    subtitle = "",
    color = NULL  # Remove color legend label
  ) +
  facet_wrap(~ ECO_NAME, ncol = 3, scales = "free_y") +  # Independent y-axes for each ecoregion
  scale_color_viridis_d(option = "D") + 
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",  # Remove legend
    panel.grid = element_blank(),
    panel.spacing = unit(1.5, "lines"),  # Increased spacing between facets
    axis.line = element_line(color = "black")
  )
