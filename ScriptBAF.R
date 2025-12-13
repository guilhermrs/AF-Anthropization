#### Ecoregions ####
# load packages
library(MuMIn)
library(nlme)
library(ggplot2)
library(dplyr)
library(viridis)
library(ggplot2)
library(tidyr)


dados_ecoregioes <- read.csv("EstatisticasAtt.csv", header = TRUE)

cor(dados_ecoregioes$Slope_mean, dados_ecoregioes$Elevation_, method = c("pearson"))

cor(dados_ecoregioes$Agro, dados_ecoregioes$Antro, method = c("pearson"))

cor(dados_ecoregioes$Urb, dados_ecoregioes$Antro, method = c("pearson"))


## 1º Model Selection

modelo_corLin_ecoregion <- gls(Antro~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, correlation = corLin(form = ~Latitude + Longitude), method = "REML")

modelo_corExp_ecoregion <- gls(Antro~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

modelo_Gaus_ecoregion <- gls(Antro~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, correlation = corGaus(form = ~Latitude + Longitude), method = "REML")

modelo_Ratio_ecoregion <- gls(Antro~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, correlation = corRatio(form = ~Latitude + Longitude), method = "REML")

modelo_Spher_ecoregion <- gls(Antro~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, correlation = corSpher(form = ~Latitude + Longitude), method = "REML")

modelo_null_ecoregion <- gls(Antro~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, method = "REML")

model.sel(modelo_corLin_ecoregion, modelo_corExp_ecoregion, modelo_Gaus_ecoregion, modelo_Ratio_ecoregion, modelo_Spher_ecoregion, modelo_null_ecoregion)

summary(modelo_corExp_ecoregion) ### ganhou, próxima etapa rodar para outras combinações ##

## 2º Seleção de Modelos

## without interaction 8 possibilities

MEl <- gls(Antro~Elevation_, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MSlp <- gls(Antro~Slope_mean, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MEco <- gls(Antro~ECO_NAME, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MElvSlp <- gls(Antro~Elevation_+Slope_mean, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MElevEco <- gls(Antro~Elevation_+ECO_NAME, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MSlpEco <- gls(Antro~Slope_mean+ECO_NAME, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MElvSlpEco <- gls(Antro~Elevation_+Slope_mean+ECO_NAME, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

Mnull <- gls(Antro~1, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

## w/ interaction 3 possibilities

MIntSlpEco <- gls(Antro~Slope_mean*ECO_NAME, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MintElvEco <- gls(Antro~Elevation_*ECO_NAME, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")

MintElvSlpEco<- gls(Antro~Elevation_+Slope_mean+ECO_NAME:Elevation_+ECO_NAME:Slope_mean, data = dados_ecoregioes, correlation = corExp(form = ~Latitude + Longitude), method = "REML")  # = "modelo_corExp_ecoregion"

model.sel(MEl,MSlp,MEco,MElvSlp,MElevEco,MSlpEco, MElvSlpEco, Mnull, MIntSlpEco, MintElvEco, modelo_corExp_ecoregion)


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

## Summary First place!

summary(MElvSlp) 

modelo_best <- MElvSlp  
dados_ecoregioes$Fitted <- predict(modelo_best)

sd_elev <- sd(dados_ecoregioes$Elevation_)
sd_slope <- sd(dados_ecoregioes$Slope_mean)
sd_antro <- sd(dados_ecoregioes$Fitted)

coef_elev <- coef(modelo_best)["Elevation_"]
coef_slope <- coef(modelo_best)["Slope_mean"]

se_elev <- summary(modelo_best)$tTable["Elevation_", "Std.Error"]
se_slope <- summary(modelo_best)$tTable["Slope_mean", "Std.Error"]

elev_std_coef <- coef_elev * (sd_elev / sd_antro)
elev_std_se   <- se_elev   * (sd_elev / sd_antro)

slope_std_coef <- coef_slope * (sd_slope / sd_antro)
slope_std_se   <- se_slope   * (sd_slope / sd_antro)

# Criar data frame final  
dados_std <- data.frame(
  Variable = c("Elevation", "Slope"),
  Std_Coefficient = c(elev_std_coef, slope_std_coef),
  Std_Error = c(elev_std_se, slope_std_se)
)

dados_std

ggplot(dados_std, aes(x = Std_Coefficient, y = Variable)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = Std_Coefficient - Std_Error,
                     xmax = Std_Coefficient + Std_Error),
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_viridis_d(option = "D") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  labs(
    x = "Standardized Coefficient (per 1 SD)",
    y = "",
    title = ""
    
  )


# Criando o dataframe com os dados fornecidos para o modelo em segundo lugar e visualizar interações
modelo_corExp_ecoregion <- readRDS(file = "modelo_corExp_ecoregion.rds")

summary(modelo_corExp_ecoregion) #Second place

dados_predict <- dados_ecoregioes
dados_predict$Fitted <- predict(modelo_corExp_ecoregion, newdata = dados_predict)

sd_elevation <- sd(dados_predict$Elevation)
sd_slope <- sd(dados_predict$Slope)
sd_anthropized <- sd(dados_predict$Fitted)

write.csv(dados_predict, "dados_predict.csv")

# Standard deviations
#sd_elevation <- 290.12
#sd_slope <- 10.1616
#sd_anthropized <- 0.1648

dados_coefficients <- data.frame(
  Ecoregion = c(
    "Alto Paraná Atlantic forests",
    "Araucaria moist forests",
    "Atlantic dry forests",
    "Bahia coastal forests",
    "Bahia interior forests",
    "Pernambuco coastal forests",
    "Pernambuco interior forests",
    "Serra do Mar coastal forests"
  ),
  
  # --- Elevation ---
  Elevation = c(
    0.0001776,
    0.0001776 + (-0.0002779),
    0.0001776 + (-0.0009147),
    0.0001776 + (-0.0004043),
    0.0001776 + (-0.0003670),
    0.0001776 + (-0.0003768),
    0.0001776 + (-0.0002291),
    0.0001776 + (-0.0003145)
  ),
  
  Elevation_Std_Error = c(
    0.000032169,
    0.000034494,
    0.000071403,
    0.000056227,
    0.000040995,
    0.000186294,
    0.000160785,
    0.000037897
  ),
  
  # --- Slope ---
  Slope = c(
    -0.0148887,
    -0.0148887 + 0.0040937,
    -0.0148887 + 0.0064158,
    -0.0148887 + 0.0069969,
    -0.0148887 + 0.0058744,
    -0.0148887 + 0.0021370,
    -0.0148887 + 0.0004987,
    -0.0148887 + 0.0027660
  ),
  
  Slope_Std_Error = c(
    0.000723608,
    0.000844858,
    0.001527079,
    0.001038419,
    0.000900474,
    0.002828902,
    0.002975108,
    0.000904278
  )
)


# Standardize coefficients and errors
dados_coefficients <- dados_coefficients %>%
  mutate(
    Elevation_std_coef = Elevation * (sd_elevation / sd_anthropized),
    Elevation_std_se = Elevation_Std_Error * (sd_elevation / sd_anthropized),
    Slope_std_coef = Slope * (sd_slope / sd_anthropized),
    Slope_std_se = Slope_Std_Error * (sd_slope / sd_anthropized)
  )



write.csv(dados_coefficients, "dados_coefficients.csv")


df_long <- dados_coefficients %>%
  select(Ecoregion, Elevation_std_coef, Elevation_std_se, Slope_std_coef, Slope_std_se) %>%
  pivot_longer(
    cols = -Ecoregion,
    names_to = c("Variable", ".value"),
    names_pattern = "(Elevation|Slope)_std_(coef|se)"
  ) %>%
  rename(Coefficient = coef, Std_Error = se)


Fig2_forestplot_ecoregion <- ggplot(df_long, 
                                    aes(x = Coefficient, 
                                        y = Ecoregion, 
                                        color = Variable)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = Coefficient - Std_Error, 
                    xmax = Coefficient + Std_Error), 
                width = 0.2) +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black") +
  scale_color_viridis_d(option = "D") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    plot.title = element_text(color = "black"),
    plot.subtitle = element_text(color = "black")
  ) +
  labs(
    title = "",
    x = "Standardized Coefficients (per 1 SD)",
    y = "Ecoregion",
    color = "Variable"
  )

Fig2_forestplot_ecoregion


tiff("Fig2_ForestPlot_ecoregion.tiff",
     width = 18, height = 20, units = "cm",
     res = 600, compression = "lzw")

print(Fig2_forestplot_ecoregion)

dev.off()



### Plot Slope controlling Elevation (= median)

dados_ecoregioes_controlando_elevation <- dados_ecoregioes

dados_ecoregioes_controlando_elevation$Elevation_ <- median(dados_ecoregioes_controlando_elevation$Elevation_)

dados_ecoregioes_controlando_elevation$predictions <- predict(modelo_corExp_ecoregion, newdata = dados_ecoregioes_controlando_elevation)

dados_ecoregioes_controlando_elevation$resid <- dados_ecoregioes_controlando_elevation$Antro - dados_ecoregioes_controlando_elevation$predictions

dados_ecoregioes_controlando_elevation$partial <- dados_ecoregioes_controlando_elevation$predictions+dados_ecoregioes_controlando_elevation$resid # 


cor(dados_ecoregioes_controlando_elevation$predictions, dados_ecoregioes_controlando_elevation$Slope_mean, method = c("pearson"))


### Plot

library(dplyr)
library(ggplot2)
library(viridis)

# 1) limites X globais e breaks desejados
global_x_min <- floor(min(dados_ecoregioes_controlando_elevation$Slope_mean, na.rm=TRUE))
global_x_max <- ceiling(max(dados_ecoregioes_controlando_elevation$Slope_mean, na.rm=TRUE))
x_breaks <- seq(0, 60, by = 20)  # ajuste conforme quiser

# 2) calcular posições das linhas por ecoregião (linha X) e linha Y (vertical)
line_df <- dados_ecoregioes_controlando_elevation %>%
  group_by(ECO_NAME) %>%
  summarise(
    x_min = global_x_min,     # usar limite global para a linha X começar
    x_max = global_x_max,     # e terminar
    y_min = min(c(predictions, partial), na.rm = TRUE),
    y_max = max(c(predictions, partial), na.rm = TRUE)
  ) %>%
  # recuar a linha horizontal um pouco acima do y_min para não sobrepor pontos
  mutate(
    line_y = y_min + 0.03 * (y_max - y_min),
    # posição da "linha-y" vertical: um pouco à direita do x_min para aparecer coerente
    line_x = x_min + 0.01 * (x_max - x_min)
  )

# 3) Plot com geom_segment para linha horizontal e vertical em cada faceta
Fig3_Slope_Ecoregion <- ggplot(
  dados_ecoregioes_controlando_elevation,
  aes(x = Slope_mean, y = predictions, color = ECO_NAME)
) +
  geom_point(aes(y = partial), shape = 16, size = 0.7, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.9) +
  
  # linha horizontal por faceta (como "eixo x")
  geom_segment(
    data = line_df,
    aes(x = x_min, xend = x_max, y = line_y, yend = line_y),
    inherit.aes = FALSE,
    color = "black",
    size = 0.35
  ) +
  
  # linha vertical por faceta (como "eixo y" discreto)
  geom_segment(
    data = line_df,
    aes(x = line_x, xend = line_x, y = y_min, yend = y_max),
    inherit.aes = FALSE,
    color = "black",
    size = 0.35
  ) +
  
  scale_x_continuous(limits = c(global_x_min, global_x_max),
                     breaks = x_breaks,
                     expand = c(0,0)) +
  scale_color_viridis_d(option = "D") +
  labs(x = "Slope (%)", y = "Anthropized area", color = NULL) +
  facet_wrap(~ ECO_NAME, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "none",
    
    # garantir textos em preto
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    
    # esconder a linha de eixo original (opcional) para não duplicar visual
    axis.line = element_blank(),
    
    # ajustar margens dos strips (nomes das facetas)
    strip.text = element_text(size = 11, face = "plain", color = "black")
  ) +
  coord_cartesian(expand = FALSE)


Fig3_Slope_Ecoregion

tiff("Fig3_Slope_Ecoregion.tiff",
     width = 18, height = 20, units = "cm",
     res = 600, compression = "lzw")

print(Fig3_Slope_Ecoregion)

dev.off()




### Plot Elevation controlling Slope (=median)

dados_ecoregioes_controlando_slp <- dados_ecoregioes

dados_ecoregioes_controlando_slp$Slope_mean <- median(dados_ecoregioes_controlando_slp$Slope_mean)

dados_ecoregioes_controlando_slp$predictions <- predict(modelo_corExp_ecoregion, newdata = dados_ecoregioes_controlando_slp)

dados_ecoregioes_controlando_slp$resid <- dados_ecoregioes_controlando_slp$Antro - dados_ecoregioes_controlando_slp$predictions

dados_ecoregioes_controlando_slp$partial <- dados_ecoregioes_controlando_slp$predictions+dados_ecoregioes_controlando_slp$resid # 


cor(dados_ecoregioes_controlando_slp$predictions, dados_ecoregioes_controlando_slp$Elevation_, method = c("pearson"))


### Plot

# 1) limites X globais (elevation)
global_x_min <- floor(min(dados_ecoregioes_controlando_slp$Elevation_, na.rm=TRUE))
global_x_max <- ceiling(max(dados_ecoregioes_controlando_slp$Elevation_, na.rm=TRUE))

# breaks opcionais – pode ajustar se quiser
x_breaks <- seq(0, global_x_max, by = 500)

# 2) dataframe para as linhas horizontais e verticais por faceta
line_df4 <- dados_ecoregioes_controlando_slp %>%
  group_by(ECO_NAME) %>%
  summarise(
    x_min = global_x_min,
    x_max = global_x_max,
    y_min = min(c(predictions, partial), na.rm = TRUE),
    y_max = max(c(predictions, partial), na.rm = TRUE)
  ) %>%
  mutate(
    line_y = y_min + 0.03 * (y_max - y_min),
    line_x = x_min + 0.01 * (x_max - x_min)
  )

# 3) Plot com as linhas artificiais por faceta
Fig4_Elevation_Ecoregion <- ggplot(
  dados_ecoregioes_controlando_slp,
  aes(x = Elevation_, y = predictions, color = ECO_NAME)
) +
  geom_point(aes(y = partial), shape = 16, size = 0.7, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  
  # linha horizontal (eixo X)
  geom_segment(
    data = line_df4,
    aes(x = x_min, xend = x_max, y = line_y, yend = line_y),
    inherit.aes = FALSE,
    color = "black",
    size = 0.35
  ) +
  
  # linha vertical (eixo Y)
  geom_segment(
    data = line_df4,
    aes(x = line_x, xend = line_x, y = y_min, yend = y_max),
    inherit.aes = FALSE,
    color = "black",
    size = 0.35
  ) +
  
  scale_x_continuous(limits = c(global_x_min, global_x_max),
                     breaks = x_breaks, expand = c(0,0)) +
  scale_color_viridis_d(option = "D") +
  labs(x = "Elevation (m)", y = "Anthropized area", color = NULL) +
  
  facet_wrap(~ ECO_NAME, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "none",
    
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.line = element_blank(),
    plot.title = element_text(color = "black"),
    strip.text = element_text(size = 11, color = "black")
  ) +
  coord_cartesian(expand = FALSE)

Fig4_Elevation_Ecoregion

tiff("Fig4_Elevation_Ecoregion.tiff",
     width = 18, height = 20, units = "cm",
     res = 600, compression = "lzw")

print(Fig4_Elevation_Ecoregion)

dev.off()
