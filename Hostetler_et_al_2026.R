#Hostetler et al., 2026 script #####
R.version
library(ggplot2)
citation("ggplot2")
packageVersion("ggplot2")
library(cowplot)
citation("cowplot")
packageVersion("cowplot")
library(dplyr)
citation("dplyr")
packageVersion("dplyr")
library(lubridate)
citation("lubridate")
packageVersion("lubridate")
library(gridExtra)
citation("gridExtra")
packageVersion("gridExtra")
library(agricolae)
citation("agricolae")
packageVersion("agricolae")
library(rcompanion)
citation("rcompanion")
packageVersion("rcompanion")
library(factoextra) #PCA
citation("factoextra")
packageVersion("factoextra")
library(ggfortify) #PCA
citation("ggfortify")
packageVersion("ggfortify")
library(dplyr)
citation("dplyr")
packageVersion("dplyr")
library(tidyr)
citation("tidyr")
packageVersion("tidyr")
library(lubridate)
citation("lubridate")
packageVersion("lubridate")
library(lme4)
citation("lme4")
packageVersion("lme4")
library(lmerTest) 
citation("lmerTest")
packageVersion("lmerTest")
library(ggeffects)
citation("ggeffects")
packageVersion("ggeffects")
library(tibble)
citation("tibble")
packageVersion("tibble")
library(car)
citation("car")
packageVersion("car")
library(emmeans)
citation("emmeans")
packageVersion("emmeans")
library(performance)
citation("performance")
packageVersion("performance")
library(MASS)
citation("MASS")
packageVersion("MASS")
library(multcompView)
citation("multcompView")
packageVersion("multcompView")
library(multcomp)
citation("multcomp")
packageVersion("multcomp")


cat("\014")
rm(list=ls()) 
ls() 
setwd(dir = "/Users/ashley/Desktop/Hostetler_Kennebeck_et_al_2026/")
getwd()

df1 = read.csv("Data/EnviormentalData.csv", header = TRUE, na.strings = "NA")
df2 = read.csv("Data/LightData.csv", header = TRUE, na.strings = "NA")
df1$Date = mdy(df1$Date)
df1 = df1 %>%
  filter(
    (Replication == 1 & Date >= mdy("02/13/24") & Date <= mdy("02/28/24")) |
      (Replication == 2 & Date >= mdy("03/05/24") & Date <= mdy("03/20/24")) |
      (Replication == 3 & Date >= mdy("04/16/24") & Date <= mdy("05/01/24")) |
      (Replication == 4 & Date >= mdy("05/07/24") & Date <= mdy("05/22/24")) |
      (Replication == 5 & Date >= mdy("05/28/24") & Date <= mdy("06/12/24"))
  )
head(df1)
df1 = df1 %>%
  mutate(
    DateTime = mdy_hm(Date.Time..EST.),
    Date = as_date(DateTime),
    Month = floor_date(DateTime, "month")
  )
str(df1)

head(df2)
df2$Date = mdy(df2$Date)
head(df2)
df2 = df2 %>%
  mutate(
    Replication = case_when(
      Date >= mdy("02/13/24") & Date <= mdy("02/28/24") ~ 1,
      Date >= mdy("03/05/24") & Date <= mdy("03/20/24") ~ 2,
      Date >= mdy("04/16/24") & Date <= mdy("05/01/24") ~ 3,
      Date >= mdy("05/07/24") & Date <= mdy("05/22/24") ~ 4,
      Date >= mdy("05/28/24") & Date <= mdy("06/12/24") ~ 5,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(Replication))
head(df2)
str(df2)
df2 = df2 %>%
  mutate(
    DateTime = mdy_hms(paste(format(Date, "%m/%d/%Y"), Hour)),
    Date = as_date(DateTime),
    Month = floor_date(DateTime, "month")
  )
unique(df1$Replication)
unique(df1$Month)
head(df1)
df1 %>%
  group_by(Replication) %>%
  summarise(n_data_points = n())
df1 %>%
  group_by(Replication) %>%
  arrange(DateTime) %>%
  summarise(
    min_interval = min(diff(DateTime)),
    max_interval = max(diff(DateTime)),
    n_data_points = n()
  )
rep2 = df1 %>% filter(Replication == 2)
rep2_resampled = rep2 %>%
  mutate(DateTime_10min = floor_date(DateTime, unit = "10 minutes")) %>%
  group_by(DateTime_10min) %>%
  summarise(
    TempC = mean(TempC, na.rm = TRUE),
    RH = mean(RH, na.rm = TRUE),
    DewPoint = mean(DewPoint, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Replication = 2,
    Date = as.Date(DateTime_10min)
  )
df1 = df1 %>%
  filter(Replication != 2) %>%
  bind_rows(rep2_resampled %>%
              dplyr::select(Replication, Date, TempC, RH, DewPoint, DateTime = DateTime_10min))
df1 %>%
  group_by(Replication) %>%
  summarise(n_data_points = n())
df1 %>%
  group_by(Replication) %>%
  arrange(DateTime) %>%
  summarise(
    min_interval = min(diff(DateTime)),
    max_interval = max(diff(DateTime)),
    n_data_points = n()
  )
df_intervals = df1 %>%
  arrange(Replication, DateTime) %>%
  group_by(Replication) %>%
  mutate(
    interval = as.numeric(difftime(DateTime, lag(DateTime), units = "mins"))
  ) %>%
  filter(interval > 10)  # keep only intervals greater than 10 minutes
df_intervals %>%
  summarise(
    n_long_intervals = n(),
    max_interval = max(interval, na.rm = TRUE)
  )
env_tempRH_summary = df1 %>%
  group_by(Replication) %>%
  summarise(
    Temp_mean = mean(TempC, na.rm = TRUE),
    Temp_sd   = sd(TempC, na.rm = TRUE),
    RH_mean   = mean(RH, na.rm = TRUE),
    RH_sd     = sd(RH, na.rm = TRUE),
    .groups = "drop"
  )
head(env_tempRH_summary)

unique(df2$Month)
df2 %>%
  group_by(Replication) %>%
  summarise(n_data_points = n())
df2 %>%
  group_by(Replication) %>%
  arrange(DateTime) %>%
  summarise(
    min_interval = min(diff(DateTime)),
    max_interval = max(diff(DateTime)),
    n_data_points = n()
  )
env_solar_summary = df2 %>%
  group_by(Replication) %>%
  summarise(
    Solar_sum = sum(Solar.Radiation, na.rm = TRUE),
    .groups = "drop"
  )
head(env_solar_summary)
head(env_tempRH_summary)
env_data = merge(env_solar_summary, env_tempRH_summary, by = c("Replication"))
head(env_data)

df1 = read.csv("Data/Clinostat Overall Data.csv", header = TRUE, na.strings = "NA")
head(df1)
df1 = subset(df1, Notes == "NotBroken")
df1$Replication = as.character(df1$Replication)
df1$clinostat_type = gsub("C ", "C", df1$clinostat_type)
colnames(df1)
data = df1
data$shootdiam_mm = (data$X1_shoot_diameter_mm + data$X2_shoot_diameter_mm)/2
colnames(data)
data = data[,c(1:5,8,10,11,18,16,17)]
colnames(data)
colnames(data)[2] = "Clinostat"
colnames(data)[4] = "Cultivar"
colnames(data)[6] = "Apical Bud Height"
colnames(data)[7] = "Shoot Fresh Mass"
colnames(data)[8] = "Shoot Dry Mass"
colnames(data)[9] = "Shoot Diameter"
head(data)

df1 = read.csv("Data/root_scale_data.csv", header = TRUE, na.strings = "NA")
df2 = read.csv("Data/model_17_FIJI_data.csv", header = TRUE, na.strings = "NA")
head(df1)
head(df2)
df = merge(df2, df1, by = c("Replication","clinostat_type","plant_no"))
head(df)
df$length_in = df$length/df$pixels_per_inch
head(df)
df = df[,c(1,2,3,9)]
head(data)
colnames(df)[2] = "Clinostat"
head(df)
head(data)
data = merge(data, df, by = c("Replication","Clinostat","plant_no"), all.x = TRUE)
head(data)
data$length_in[is.na(data$length_in)] = 0
colnames(data)[12] = "RootLength_in"
colnames(data)
data$RootLength_cm = data$RootLength_in * 2.54
head(data)
colnames(data)
data = data[,c(1:11,13)]
data$Replication = as.character(data$Replication)
rep_labels = c(
  "1" = "February",
  "2" = "March",
  "3" = "April",
  "4" = "May",
  "5" = "June"
)
cultivar_labels = c(
  "H7996" = "H7996",
  "MM" = "MM")
head(data)
names(data) = make.names(names(data))
head(data)

head(env_data)
str(data)
str(env_data)
env_data$Replication = as.factor(env_data$Replication)
data = data %>%
  left_join(env_data, by = c("Replication"))
head(data)
str(data)
head(data)
rm(list = setdiff(ls(), c("data","cultivar_labels","rep_labels")))

#Figure S1 ####
df1 = data
colnames(df1)
str(df1)
i = c(6:12)
df1[,i] = apply(df1[ ,i], 2,
                function(x) as.numeric(x))
pca = prcomp(df1[,c(6:9,12)], scale. = TRUE, center = TRUE)
scores = as.data.frame(pca$x)
scores$fusarium = df1$fusarium
loadings = as.data.frame(pca$rotation[, 1:2])
loadings$Variable = rownames(loadings)
pc1_range = range(scores$PC1)
pc2_range = range(scores$PC2)
score_range = max(diff(pc1_range), diff(pc2_range)) 
loading_range = max(abs(loadings$PC1), abs(loadings$PC2))
loading_range
scale_factor = score_range / loading_range * 0.3
loadings$PC1 = loadings$PC1 * scale_factor
loadings$PC2 = loadings$PC2 * scale_factor
hulls = scores %>%
  group_by(fusarium) %>%
  slice(chull(PC1, PC2))
head(scores)
scores$fusarium = as.character(scores$fusarium)
hulls$fusarium = as.character(hulls$fusarium)
#pdf(file="Figures/FigS1.pdf", width=7, height=5)
ggplot(scores, aes(x = PC1, y = PC2, color = fusarium, fill = fusarium)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_polygon(data = hulls, aes(group = fusarium), alpha = 0.2, color = NA)+
  labs(x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 2), "%)"),
       y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 2), "%)")) +
  scale_color_manual(values = c("gray40", "gray10"),
                     labels = c("0" = "Mock", "1" = "Inoculated")) +
  scale_fill_manual(values = c("gray40", "gray10"),
                    labels = c("0" = "Mock", "1" = "Inoculated")) +
  labs(color = "F. oxysporum", fill = "F. oxysporum")+
  theme_bw()+
  theme(legend.position = "top",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
#dev.off()

#Figure S1-2
df1 = subset(data, Clinostat == "C")
colnames(df1)
str(df1)
i = c(6:12)
df1[,i] = apply(df1[ ,i], 2,
                function(x) as.numeric(x))
pca = prcomp(df1[,c(6:9,12)], scale. = TRUE, center = TRUE)
scores = as.data.frame(pca$x)
scores$fusarium = df1$fusarium
loadings = as.data.frame(pca$rotation[, 1:2])
loadings$Variable = rownames(loadings)
pc1_range = range(scores$PC1)
pc2_range = range(scores$PC2)
score_range = max(diff(pc1_range), diff(pc2_range)) 
loading_range = max(abs(loadings$PC1), abs(loadings$PC2))
loading_range
scale_factor = score_range / loading_range * 0.3
loadings$PC1 = loadings$PC1 * scale_factor
loadings$PC2 = loadings$PC2 * scale_factor
hulls = scores %>%
  group_by(fusarium) %>%
  slice(chull(PC1, PC2))
head(scores)
scores$fusarium = as.character(scores$fusarium)
hulls$fusarium = as.character(hulls$fusarium)
#pdf(file="Figures/FigS1.pdf", width=7, height=5)
PlotA = ggplot(scores, aes(x = PC1, y = PC2, color = fusarium, fill = fusarium)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_polygon(data = hulls, aes(group = fusarium), alpha = 0.2, color = NA)+
  labs(x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 2), "%)"),
       y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 2), "%)"),
       title = "Clinostat - Control") +
  scale_color_manual(values = c("gray40", "gray10"),
                     labels = c("0" = "Mock", "1" = "Inoculated")) +
  scale_fill_manual(values = c("gray40", "gray10"),
                    labels = c("0" = "Mock", "1" = "Inoculated")) +
  labs(color = "F. oxysporum", fill = "F. oxysporum")+
  theme_bw()+
  theme(legend.position = "top",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))

df1 = subset(data, Clinostat == "SM")
colnames(df1)
str(df1)
i = c(6:12)
df1[,i] = apply(df1[ ,i], 2,
                function(x) as.numeric(x))
pca = prcomp(df1[,c(6:9,12)], scale. = TRUE, center = TRUE)
scores = as.data.frame(pca$x)
scores$fusarium = df1$fusarium
loadings = as.data.frame(pca$rotation[, 1:2])
loadings$Variable = rownames(loadings)
pc1_range = range(scores$PC1)
pc2_range = range(scores$PC2)
score_range = max(diff(pc1_range), diff(pc2_range))
loading_range = max(abs(loadings$PC1), abs(loadings$PC2))
loading_range 
scale_factor = score_range / loading_range * 0.3
loadings$PC1 = loadings$PC1 * scale_factor
loadings$PC2 = loadings$PC2 * scale_factor
hulls = scores %>%
  group_by(fusarium) %>%
  slice(chull(PC1, PC2))
head(scores)
scores$fusarium = as.character(scores$fusarium)
hulls$fusarium = as.character(hulls$fusarium)
#pdf(file="Figures/FigS1.pdf", width=7, height=5)
PlotB = ggplot(scores, aes(x = PC1, y = PC2, color = fusarium, fill = fusarium)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_polygon(data = hulls, aes(group = fusarium), alpha = 0.2, color = NA)+
  labs(x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 2), "%)"),
       y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 2), "%)"),
       title = "Clinostat - Simulated Microgravity") +
  scale_color_manual(values = c("gray40", "gray10"),
                     labels = c("0" = "Mock", "1" = "Inoculated")) +
  scale_fill_manual(values = c("gray40", "gray10"),
                    labels = c("0" = "Mock", "1" = "Inoculated")) +
  labs(color = "F. oxysporum", fill = "F. oxysporum")+
  theme_bw()+
  theme(legend.position = "top",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
gglist = list(PlotA, PlotB)
combined_plot = plot_grid(plotlist = gglist, ncol = 2, labels = LETTERS[1:2])
combined_plot
#pdf(file="Figures/FigS1-2.pdf", width=7, height=5)
combined_plot  
#dev.off()
rm(list = setdiff(ls(), c("data","cultivar_labels","rep_labels")))

#Figure 2 ####
df1 = data
head(df1)
pca = prcomp(df1[,c(6:9,12)], scale. = TRUE, center = TRUE)
scores = as.data.frame(pca$x)
scores$Replication = df1$Replication
loadings = as.data.frame(pca$rotation[, 1:2])
loadings$Variable = rownames(loadings)
loadings
pc1_range = range(scores$PC1)
pc2_range = range(scores$PC2)
score_range = max(diff(pc1_range), diff(pc2_range)) 
loading_range = max(abs(loadings$PC1), abs(loadings$PC2))
loading_range 
scale_factor = score_range / loading_range * 0.3
loadings$PC1 = loadings$PC1 * scale_factor
loadings$PC2 = loadings$PC2 * scale_factor
hulls = scores %>%
  group_by(Replication) %>%
  slice(chull(PC1, PC2))
head(scores)
scores$Replication = as.character(scores$Replication)
hulls$Replication = as.character(hulls$Replication)
Figure2A = ggplot(scores, aes(x = PC1, y = PC2, color = Replication, fill = Replication)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_polygon(data = hulls, aes(group = Replication), alpha = 0.2, color = NA)+
  labs(x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 2), "%)"),
       y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 2), "%)")) +
  scale_color_manual(values = c("red", "orange","yellow","green","blue"),
                     labels = c("1" = "February", "2" = "March", "3" = "April", "4" = "May", "5" = "June")) +
  scale_fill_manual(values = c("red", "orange","yellow","green","blue"),
                    labels = c("1" = "February", "2" = "March", "3" = "April", "4" = "May", "5" = "June")) +
  labs(color = "Trial", fill = "Trial")+
  theme_bw()+
  theme(legend.position = "top",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
Figure2A

pca = prcomp(df1[,c(6:9,12)], scale. = TRUE, center = TRUE)
scores = as.data.frame(pca$x)
scores$Cultivar = df1$Cultivar
loadings = as.data.frame(pca$rotation[, 1:2])
loadings$Variable = rownames(loadings)
loadings
pc1_range = range(scores$PC1)
pc2_range = range(scores$PC2)
score_range = max(diff(pc1_range), diff(pc2_range))
loading_range = max(abs(loadings$PC1), abs(loadings$PC2))
loading_range
scale_factor = score_range / loading_range * 0.3
loadings$PC1 = loadings$PC1 * scale_factor
loadings$PC2 = loadings$PC2 * scale_factor
hulls = scores %>%
  group_by(Cultivar) %>%
  slice(chull(PC1, PC2))
head(scores)
scores$Cultivar = as.character(scores$Cultivar)
hulls$Cultivar = as.character(hulls$Cultivar)
Figure2B = ggplot(scores, aes(x = PC1, y = PC2, color = Cultivar, fill = Cultivar)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_polygon(data = hulls, aes(group = Cultivar), alpha = 0.2, color = NA)+
  labs(x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 2), "%)"),
       y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 2), "%)")) +
  scale_color_manual(values = c("darkcyan", "mediumorchid4"),
                     labels = c("0" = "Mock", "1" = "Inoculated")) +
  scale_fill_manual(values = c("darkcyan", "mediumorchid4"),
                    labels = c("0" = "Mock", "1" = "Inoculated")) +
  labs(color = "Cultivar", fill = "Cultivar")+
  theme_bw()+
  theme(legend.position = "top",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
Figure2B

head(df1)
pca = prcomp(df1[,c(6:9,12)], scale. = TRUE, center = TRUE)
scores = as.data.frame(pca$x)
scores$Clinostat = df1$Clinostat
loadings = as.data.frame(pca$rotation[, 1:2])
loadings$Variable = rownames(loadings)
loadings
pc1_range = range(scores$PC1)
pc2_range = range(scores$PC2)
score_range = max(diff(pc1_range), diff(pc2_range)) 
loading_range = max(abs(loadings$PC1), abs(loadings$PC2))
loading_range 
scale_factor = score_range / loading_range * 0.3
loadings$PC1 = loadings$PC1 * scale_factor
loadings$PC2 = loadings$PC2 * scale_factor
hulls = scores %>%
  group_by(Clinostat) %>%
  slice(chull(PC1, PC2))
head(scores)
scores$Clinostat = as.character(scores$Clinostat)
hulls$Clinostat = as.character(hulls$Clinostat)
Figure2C = ggplot(scores, aes(x = PC1, y = PC2, color = Clinostat, fill = Clinostat)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_polygon(data = hulls, aes(group = Clinostat), alpha = 0.2, color = NA)+
  labs(x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 2), "%)"),
       y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 2), "%)")) +
  scale_color_manual(values = c("palegreen4", "burlywood1"),
                     labels = c("C" = "Control", "SM" = "Simulated Microgravity")) +
  scale_fill_manual(values = c("palegreen4", "burlywood1"),
                    labels = c("C" = "Control", "SM" = "Simulated Microgravity")) +
  labs(color = "Gravity Condition", fill = "Gravity Condition")+
  theme_bw()+
  theme(legend.position = "top",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
Figure2C

#pdf(file="Figures/Figure2-1.pdf", width=7, height=5)
ggdraw() +
  draw_plot(Figure2A, x = 0, y = 0, width = 0.33, height = 0.5) +
  draw_plot(Figure2B, x = 0.33, y = 0, width = 0.33, height = 0.5) +
  draw_plot(Figure2C, x = 0.66, y = 0, width = 0.33, height = 0.5) +
  draw_plot_label(label = c("A", "B","C"), 
                  size = 12,
                  x = c(0,0.33,0.66), 
                  y = c(0.55,0.55,0.55))
#dev.off()
rm(list = setdiff(ls(), c("data","cultivar_labels","rep_labels")))

#Figure 3####
all_plots = list()
all_scores = list()
all_loadings_combined = data.frame()
variance_explained = data.frame()
df0 = data

for (rep_num in 1:5) {
  df1 = data %>% filter(Replication == rep_num)
  
  # PCA
  pca = prcomp(df1[,c(6:9,12)], scale. = TRUE, center = TRUE)
  scores = as.data.frame(pca$x)
  scores$Clinostat = df1$Clinostat
  scores$clinostat_type = df1$clinostat_type
  scores$Cultivar = df1$Cultivar
  scores$Cultivar = df1$Cultivar
  
  loadings = as.data.frame(pca$rotation[, 1:2])
  loadings$Variable = rownames(loadings)
  loadings$Replication = rep_num 
  all_loadings_combined = rbind(all_loadings_combined, loadings)
  
  # Variance explained
  var_exp = summary(pca)$importance[2, 1:2]
  variance_explained = rbind(
    variance_explained,
    data.frame(
      Replication = rep_num,
      PC1 = round(var_exp[1] * 100, 2),
      PC2 = round(var_exp[2] * 100, 2)
    )
  )
  # Scaling arrows
  pc1_range = range(scores$PC1)
  pc2_range = range(scores$PC2)
  score_range = max(diff(pc1_range), diff(pc2_range))
  loading_range = max(abs(loadings$PC1), abs(loadings$PC2))
  scale_factor = score_range / loading_range * 0.3
  
  loadings$PC1 = loadings$PC1 * scale_factor
  loadings$PC2 = loadings$PC2 * scale_factor
  
  # Convex hulls
  hulls = scores %>%
    group_by(Clinostat) %>%
    slice(chull(PC1, PC2))
  hulls2 = scores %>%
    group_by(Cultivar) %>%
    slice(chull(PC1, PC2))
  
  # Plot
  p = ggplot(scores, aes(x = PC1, y = PC2, color = Cultivar, fill = Clinostat, shape = Cultivar)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_polygon(data = hulls, aes(group = Clinostat), alpha = 0.2, color = NA) +
    #geom_segment(data = loadings,
    #             aes(x = 0, y = 0, xend = PC1, yend = PC2),
    #             arrow = arrow(length = unit(0.2, "cm")),
    #             color = "black", inherit.aes = FALSE) +
    #geom_text(data = loadings,
    #          aes(x = PC1, y = PC2, label = Variable),
    #          color = "black", vjust = 0.5, hjust = 0.5, inherit.aes = FALSE) +
    labs(
      x = paste0("PC1 (", round(var_exp[1] * 100, 2), "%)"),
      y = paste0("PC2 (", round(var_exp[2] * 100, 2), "%)"),
      #title = paste0("Replication ", rep_num, ": ", rep_labels[as.character(rep_num)])
      title = paste0(rep_labels[as.character(rep_num)])
    ) +
    scale_color_manual(values = c("black", "gray"),
                       labels = c("H7966" = "H7966", "MM" = "MM")) +
    scale_shape(
      labels = c("H7966" = "H7966", "MM" = "MM")) +
    scale_fill_manual(values = c("palegreen4", "burlywood3"),
                      labels = c("C" = "Control", "SM" = "Simulated Microgravity")) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 10),
      axis.title = element_text(face = "bold", size = 10)
    )
  
  # Save outputs
  all_plots[[rep_num]] = p
  all_scores[[rep_num]] = scores
}

variance_explained
all_loadings_combined = as_tibble(all_loadings_combined)
PC1_loadings = all_loadings_combined %>%
  dplyr::select(Replication, Variable, PC1) %>%
  tidyr::pivot_wider(
    names_from = Replication,
    values_from = PC1,
    names_prefix = "Rep")
PC1_loadings
PC2_loadings = all_loadings_combined %>%
  dplyr::select(Replication, Variable, PC2) %>%
  tidyr::pivot_wider(
    names_from = Replication,
    values_from = PC2,
    names_prefix = "Rep"
  )
PC2_loadings
combined_plot = plot_grid(
  plotlist = all_plots,
  ncol = 3, nrow = 2,
  align = 'hv'
)
combined_plot
#ggsave(
#  filename = "Figures/Figure3.pdf",
#  plot = combined_plot,
#  width = 7,
#  height = 5,
#  units = "in"
#)
data = df0
rm(list = setdiff(ls(), c("data","cultivar_labels","rep_labels")))


#FigureS3####
all_plots = list()
all_scores = list()
all_loadings_combined = data.frame()
variance_explained = data.frame()
for (rep_num in 1:5) {
  df1 = data %>% filter(Replication == rep_num)
  
  # PCA
  pca = prcomp(df1[,c(6:9,12)], scale. = TRUE, center = TRUE)
  scores = as.data.frame(pca$x)
  scores$Cultivar = df1$Cultivar
  scores$Cultivar = df1$Cultivar
  
  loadings = as.data.frame(pca$rotation[, 1:2])
  loadings$Variable = rownames(loadings)
  loadings$Replication = rep_num  
  all_loadings_combined = rbind(all_loadings_combined, loadings)
  
  # Variance explained
  var_exp = summary(pca)$importance[2, 1:2]
  variance_explained = rbind(
    variance_explained,
    data.frame(
      Replication = rep_num,
      PC1 = round(var_exp[1] * 100, 2),
      PC2 = round(var_exp[2] * 100, 2)
    )
  )
  # Scaling arrows
  pc1_range = range(scores$PC1)
  pc2_range = range(scores$PC2)
  score_range = max(diff(pc1_range), diff(pc2_range))
  loading_range = max(abs(loadings$PC1), abs(loadings$PC2))
  scale_factor = score_range / loading_range * 0.3
  
  loadings$PC1 = loadings$PC1 * scale_factor
  loadings$PC2 = loadings$PC2 * scale_factor
  
  # Convex hulls
  hulls = scores %>%
    group_by(Cultivar) %>%
    slice(chull(PC1, PC2))
  
  # Plot
  p = ggplot(scores, aes(x = PC1, y = PC2, color = Cultivar, fill = Cultivar)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_polygon(data = hulls, aes(group = Cultivar), alpha = 0.2, color = NA) +
    labs(
      x = paste0("PC1 (", round(var_exp[1] * 100, 2), "%)"),
      y = paste0("PC2 (", round(var_exp[2] * 100, 2), "%)"),
      title = paste0(rep_labels[as.character(rep_num)])
    ) +
    scale_color_manual(values = c("pink4", "slategray3"),
                       labels = c("H7966" = "H7966", "MM" = "MM")) +
    scale_fill_manual(values = c("pink4", "slategray3"),
                      labels = c("H7966" = "H7966", "MM" = "MM")) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 10),
      axis.title = element_text(face = "bold", size = 10)
    )
  
  # Save outputs
  all_plots[[rep_num]] = p
  all_scores[[rep_num]] = scores
}
variance_explained
variance_explained$Total = variance_explained$PC1 + variance_explained$PC2
variance_explained

PC1_loadings = all_loadings_combined %>%
  dplyr::select(Replication, Variable, PC1) %>%
  tidyr::pivot_wider(
    names_from = Replication,
    values_from = PC1,
    names_prefix = "Rep"
  )
PC1_loadings
PC2_loadings = all_loadings_combined %>%
  dplyr::select(Replication, Variable, PC2) %>%
  tidyr::pivot_wider(
    names_from = Replication,
    values_from = PC2,
    names_prefix = "Rep"
  )
PC2_loadings
combined_plot = plot_grid(
  plotlist = all_plots,
  ncol = 3, nrow = 2,
  align = 'hv'
)
combined_plot
#ggsave(
#  filename = "Figures/FigS3.pdf",
#  plot = combined_plot,
#  width = 7,
#  height = 5,
#  units = "in")
rm(list = setdiff(ls(), c("data","cultivar_labels","rep_labels")))

#Figure 4 & Figure S4####
head(data)
df0 = data
colnames(data)
traits = colnames(data)[c(6:9,12)] 
traits
data_scaled = data %>%
  mutate(
    Temp_mean_s  = scale(Temp_mean),
    RH_mean_s    = scale(RH_mean),
    Solar_sum_s  = scale(Solar_sum)
  )
results_list = list()

for(trait in traits){
  
  ## Base model
  form_base = as.formula(
    paste0("`", trait, "` ~ Cultivar * Clinostat + (1 | Replication)"))
  model_base = lmer(form_base, data = data_scaled, REML = FALSE)
  
  ## Single-covariate models
  form_temp = update(form_base, . ~ . + Temp_mean_s)
  form_rh   = update(form_base, . ~ . + RH_mean_s)
  form_sol  = update(form_base, . ~ . + Solar_sum_s)
  
  model_temp = lmer(form_temp, data = data_scaled, REML = FALSE)
  model_rh   = lmer(form_rh,   data = data_scaled, REML = FALSE)
  model_sol  = lmer(form_sol,  data = data_scaled, REML = FALSE)
  
  ## Full environmental model
  form_env = update(form_base, . ~ . + Temp_mean_s + RH_mean_s + Solar_sum_s)
  model_env = lmer(form_env, data = data_scaled, REML = FALSE)
  
  ## Likelihood-ratio tests
  a_temp = anova(model_base, model_temp)
  a_rh   = anova(model_base, model_rh)
  a_sol  = anova(model_base, model_sol)
  a_env  = anova(model_base, model_env)
  
  ## Variance components helper
  get_var = function(model){
    as.data.frame(VarCorr(model)) |>
      dplyr::select(grp, sdcor)
  }
  
  var_base = get_var(model_base)
  var_temp = get_var(model_temp)
  var_rh   = get_var(model_rh)
  var_sol  = get_var(model_sol)
  var_env  = get_var(model_env)
  
  ## Store results
  results_list[[trait]] = tibble(
    Trait = trait,
    
    ## Base SDs
    Replication_SD_Base = var_base$sdcor[var_base$grp == "Replication"],
    Residual_SD_Base    = var_base$sdcor[var_base$grp == "Residual"],
    
    ## Full environmental model
    Chisq_Env = a_env$Chisq[2],
    P_Env     = a_env$`Pr(>Chisq)`[2],
    Replication_SD_Env = var_env$sdcor[var_env$grp == "Replication"],
    Residual_SD_Env    = var_env$sdcor[var_env$grp == "Residual"],
    
    ## Temp-only
    Chisq_Temp = a_temp$Chisq[2],
    P_Temp     = a_temp$`Pr(>Chisq)`[2],
    Replication_SD_Temp = var_temp$sdcor[var_temp$grp == "Replication"],
    Residual_SD_Temp    = var_temp$sdcor[var_temp$grp == "Residual"],
    
    ## RH-only
    Chisq_RH = a_rh$Chisq[2],
    P_RH     = a_rh$`Pr(>Chisq)`[2],
    Replication_SD_RH = var_rh$sdcor[var_rh$grp == "Replication"],
    Residual_SD_RH    = var_rh$sdcor[var_rh$grp == "Residual"],
    
    ## Solar-only
    Chisq_Solar = a_sol$Chisq[2],
    P_Solar     = a_sol$`Pr(>Chisq)`[2],
    Replication_SD_Solar = var_sol$sdcor[var_sol$grp == "Replication"],
    Residual_SD_Solar    = var_sol$sdcor[var_sol$grp == "Residual"]
  )
}
results_df = bind_rows(results_list)
head(results_df)
#write.csv(results_df, "Figures/TableS6.csv", row.names = FALSE)
data = df0
rm(list = setdiff(ls(), c("data","cultivar_labels","rep_labels")))
head(data)
df0 = data
data$DMP = (data$Shoot.Dry.Mass/data$Shoot.Fresh.Mass) * 100
colnames(data)
traits = colnames(data)[c(6:9,12,18)]
results_list = list()
diag_list = list()
emmeans_bt_list = list()
contrasts_bt_list = list()
tukey_main_list = list()
tukey_letters_list = list()

rep_levels = unique(data$Replication)

for (trait in traits) {
  for (rep in rep_levels) {
    
    subset_data = data %>% filter(Replication == rep)
    y = subset_data[[trait]]
    
    # Skip low-variation traits
    if (length(unique(na.omit(y))) < 3) {
      message("Skipping ", trait, " Rep ", rep, ": not enough variation")
      next
    }
    
    form = as.formula(paste0("`", trait, "` ~ Cultivar * Clinostat"))
    model = try(lm(form, data = subset_data), silent = TRUE)
    if (inherits(model, "try-error")) next
    
    shapiro_p = shapiro.test(residuals(model))$p.value
    lambda = NA
    trans_used = "None"
    
    # Attempt Box-Cox if needed
    if (shapiro_p < 0.05 && min(y, na.rm = TRUE) >= 0) {
      
      y_shift = ifelse(min(y, na.rm = TRUE) == 0, y + 1e-6, y)
      pt = try(powerTransform(y_shift), silent = TRUE)
      
      if (!inherits(pt, "try-error")) {
        lambda = pt$lambda
        
        if (abs(lambda - 1) > 0.1) {
          y_trans = bcPower(y_shift, lambda)
          new_col = paste0(trait, "_Tukey")
          subset_data[[new_col]] = y_trans
          
          form_trans = as.formula(paste0("`", new_col, "` ~ Cultivar * Clinostat"))
          model = try(lm(form_trans, data = subset_data), silent = TRUE)
          if (!inherits(model, "try-error")) {
            trans_used = "Tukey"
            message("Transformed ", trait, " Rep ", rep, " (lambda = ", round(lambda, 3), ")")
          }
        }
      } else {
        message("Box-Cox failed for ", trait, " Rep ", rep)
      }
    }
    
    coef_tab = as.data.frame(summary(model)$coefficients)
    coef_tab$Trait = trait
    coef_tab$Replication = rep
    coef_tab$Transformation = trans_used
    results_list[[paste(trait, rep, sep = "_")]] = coef_tab
    
    diag_list[[paste(trait, rep, sep = "_")]] = data.frame(
      Trait = trait,
      Replication = rep,
      AIC = AIC(model),
      Shapiro_p = shapiro_p,
      Lambda = lambda,
      Transformation = trans_used
    )
    
    em = emmeans(model, ~ Cultivar * Clinostat)
    
    if (trans_used == "Tukey") {
      em_bt = summary(regrid(em, transform = "response"))
    } else {
      em_bt = summary(em)
    }
    
    em_bt_df = as.data.frame(em_bt)
    em_bt_df$Trait = trait
    em_bt_df$Replication = rep
    emmeans_bt_list[[paste(trait, rep, sep = "_")]] = em_bt_df
    
    con = contrast(em, method = "pairwise")
    con_bt = summary(regrid(con, transform = "response"))
    
    con_bt_df = as.data.frame(con_bt)
    con_bt_df$Trait = trait
    con_bt_df$Replication = rep
    contrasts_bt_list[[paste(trait, rep, sep = "_")]] = con_bt_df
    
    em_cultivar = emmeans(model, ~ Cultivar)
    tuk_cultivar = pairs(em_cultivar, adjust = "tukey")
    
    if (trans_used == "Tukey") {
      tuk_cultivar_bt = summary(regrid(tuk_cultivar, transform = "response"))
    } else {
      tuk_cultivar_bt = summary(tuk_cultivar)
    }
    
    tuk_cultivar_df = as.data.frame(tuk_cultivar_bt)
    tuk_cultivar_df$Trait = trait
    tuk_cultivar_df$Replication = rep
    tuk_cultivar_df$Effect = "Cultivar"
    
    letters_cultivar = multcomp::cld(em_cultivar, adjust = "tukey", Letters = letters)
    letters_cultivar_df = as.data.frame(letters_cultivar)
    letters_cultivar_df$Trait = trait
    letters_cultivar_df$Replication = rep
    letters_cultivar_df$Effect = "Cultivar"
    
    em_clinostat = emmeans(model, ~ Clinostat)
    tuk_clinostat = pairs(em_clinostat, adjust = "tukey")
    
    if (trans_used == "Tukey") {
      tuk_clinostat_bt = summary(regrid(tuk_clinostat, transform = "response"))
    } else {
      tuk_clinostat_bt = summary(tuk_clinostat)
    }
    
    tuk_clinostat_df = as.data.frame(tuk_clinostat_bt)
    tuk_clinostat_df$Trait = trait
    tuk_clinostat_df$Replication = rep
    tuk_clinostat_df$Effect = "Clinostat"
    
    letters_clinostat = multcomp::cld(em_clinostat, adjust = "tukey", Letters = letters)
    letters_clinostat_df = as.data.frame(letters_clinostat)
    letters_clinostat_df$Trait = trait
    letters_clinostat_df$Replication = rep
    letters_clinostat_df$Effect = "Clinostat"
    
    tukey_main_list[[paste(trait, rep, sep = "_")]] =
      bind_rows(tuk_cultivar_df, tuk_clinostat_df)
    
    tukey_letters_list[[paste(trait, rep, sep = "_")]] =
      bind_rows(letters_cultivar_df, letters_clinostat_df)
    
  }
}

results_df      = bind_rows(results_list)
diag_df         = bind_rows(diag_list)
emmeans_df      = bind_rows(emmeans_bt_list)
contrasts_df    = bind_rows(contrasts_bt_list)
tukey_main_df   = bind_rows(tukey_main_list)
tukey_letters_df = bind_rows(tukey_letters_list)
#write.csv(results_df, "TableS7.csv", row.names = TRUE)
#write.csv(tukey_letters_df, "TableS8.csv", row.names = FALSE)

data = df0
rm(list = setdiff(ls(), c("data","cultivar_labels","rep_labels")))
head(data)
data$Replication = factor(data$Replication, levels = c(1, 2, 3, 4, 5))
df0 = data
data$DMP = (data$Shoot.Dry.Mass/data$Shoot.Fresh.Mass) * 100
A = ggplot(data, aes(x = Cultivar, y = Apical.Bud.Height))+
  geom_boxplot(aes(fill = Cultivar), position = position_dodge(width = 0.8)) +
  labs(
    fill = "Cultivar",
    x = "Cultivar",
    y = "Apical Bud Height (cm)"
  )+
  scale_x_discrete(labels = c("H7966" = "H7966", "MM" = "MM")) +
  scale_fill_manual(values = c("pink4", "slategray3"),
                    labels = c("H7966" = "H7966", "MM" = "MM")) +
  scale_y_continuous(limits=c(0,20), breaks=seq(0,20,2))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))+
  facet_wrap(~Replication, labeller = labeller(Replication = rep_labels), nrow=1)
A

B = ggplot(data, aes(x = Clinostat, y = Shoot.Fresh.Mass))+
  geom_boxplot(aes(fill = Clinostat), position = position_dodge(width = 0.8)) +
  labs(
    fill = "Clinostat Type",
    x = "Clinostat Type",
    y = "Aboveground Fresh Mass (g)"
  )+
  #scale_x_discrete(labels = c("C" = "Control", "SM" = "Simulated Microgravity")) +
  scale_fill_manual(values = c("palegreen4", "burlywood1"),
                    labels = c("C" = "Control", "SM" = "Simulated Microgravity"))+ 
  scale_y_continuous(limits=c(0,20), breaks=seq(0,20,2))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))+
  facet_wrap(~Replication, labeller = labeller(Replication = rep_labels), nrow=1)
B
C = ggplot(data, aes(x = Clinostat, y = Shoot.Dry.Mass))+
  geom_boxplot(aes(fill = Clinostat), position = position_dodge(width = 0.8)) +
  labs(
    fill = "Clinostat Type",
    x = "Clinostat Type",
    y = "Aboveground Dry Mass (g)"
  )+
  #scale_x_discrete(labels = c("C" = "Control", "SM" = "Simulated Microgravity")) +
  scale_fill_manual(values = c("palegreen4", "burlywood1"),
                    labels = c("C" = "Control", "SM" = "Simulated Microgravity"))+ 
  scale_y_continuous(limits=c(0,2), breaks=seq(0,2,0.2))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))+
  facet_wrap(~Replication, labeller = labeller(Replication = rep_labels), nrow=1)
C
D = ggplot(data, aes(x = Clinostat, y = Shoot.Diameter))+
  geom_boxplot(aes(fill = Clinostat), position = position_dodge(width = 0.8)) +
  labs(
    fill = "Clinostat Type",
    x = "Clinostat Type",
    y = "Average Shoot Diameter (mm)"
  )+
  scale_fill_manual(values = c("palegreen4", "burlywood1"),
                    labels = c("C" = "Control", "SM" = "Simulated Microgravity"))+
  scale_y_continuous(limits=c(0,7), breaks=seq(0,7,1))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))+
  facet_wrap(~Replication, labeller = labeller(Replication = rep_labels), nrow=1)
D
E = ggplot(data, aes(x = Clinostat, y = RootLength_cm))+
  geom_boxplot(aes(fill = Clinostat), position = position_dodge(width = 0.8)) +
  labs(
    fill = "Clinostat Type",
    x = "Clinostat Type",
    y = "Root Length (cm)"
  )+
  scale_fill_manual(values = c("palegreen4", "burlywood1"),
                    labels = c("C" = "Control", "SM" = "Simulated Microgravity"))+ 
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))+
  facet_wrap(~Replication, labeller = labeller(Replication = rep_labels), nrow=1)
E
F = ggplot(data, aes(x = Clinostat, y = DMP))+
  geom_boxplot(aes(fill = Clinostat), position = position_dodge(width = 0.8)) +
  labs(
    fill = "Clinostat Type",
    x = "Clinostat Type",
    y = "Dry Mass (%)"
  )+
  scale_fill_manual(values = c("palegreen4", "burlywood1"),
                    labels = c("C" = "Control", "SM" = "Simulated Microgravity"))+ 
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))+
  facet_wrap(~Replication, labeller = labeller(Replication = rep_labels), nrow=1)
F
gglist = list(B, C, D, E)
combined_plot = plot_grid(plotlist = gglist, ncol = 2, labels = LETTERS[1:5])
combined_plot
#pdf(file="Figures/Figure4.pdf", width=8, height=6)
combined_plot  
#dev.off()
#pdf(file="Figures/FigS4.pdf", width=6, height=4)
A  
#dev.off()
#pdf(file="FigS5.pdf", width=6, height=4)
F  
#dev.off()
data = df0
rm(list = setdiff(ls(), c("data","cultivar_labels","rep_labels")))

#Figure 5 & Figure S6 ####
df0 = data
data = subset(df0, Clinostat == "C")
head(data)

SW = matrix(NA,nrow=2,ncol=2)
rownames(SW) = c("W","pvalue")
SW[1,1] = "W"
SW[2,1] = "pvalue"
Z = ""
colnames(data)
df2 = data
head(df2)
colnames(df2)
for (i in c(6:9,12)){
  df3 = df2[,c(1,4,i)]
  a = colnames(df3)[3]
  colnames(df3)[3] = "trait"
  aov = lm(trait ~ Cultivar+Replication, data = df3)
  hsd = aov(aov)              
  resid = residuals(object = hsd)
  shap = shapiro.test(x=resid)
  SW[1,2] = shap$statistic 
  SW[2,2] = shap$p.value 
  colnames(SW)[2] = a 
  SW = as.data.frame(SW)
  write.table(a, file = "Figures/TableS9-ShapiroResults.txt", sep = "\t",
              row.names = FALSE, col.names = FALSE, append=TRUE)
  write.table(SW, file = "Figures/TableS9-ShapiroResults.txt", sep = "\t",
              row.names = FALSE, col.names = FALSE, append=TRUE)
  write.table(Z, file = "Figures/TableS9-ShapiroResults.txt", sep = "\t",
              row.names = FALSE, col.names = FALSE, append=TRUE)
  if (shap$p.value > 0.05){  
    print(shapiro.test(x=resid)$p.value)
    aov = lm(trait ~ Cultivar+Replication, data = df3)
    write.table(a, file = "Figures/TableS9.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
    anova = anova(aov)
    anova = as.data.frame(anova)
    Z = ""
    write.table(anova, file = "Figures/TableS9.txt", sep = "\t",
                row.names = TRUE, col.names = TRUE, append=TRUE)
    write.table(Z, file = "Figures/TableS9.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
    hsd = aov(aov)  
    hsd_test = HSD.test(hsd, trt = c("Replication"), console = TRUE)  
    means1 = as.data.frame(hsd_test$means)
    colnames(means1)[1] = a 
    write.table(means1, file = "Figures/TableS10a.txt", sep = "\t",
                row.names = TRUE, col.names = TRUE, append=TRUE)
    write.table(Z, file = "Figures/TableS10a.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
    groups1 = as.data.frame(hsd_test$groups)
    colnames(groups1)[1] = a 
    write.table(groups1, file = "Figures/TableS10a.txt", sep = "\t",
                row.names = TRUE, col.names = TRUE, append=TRUE)
    write.table(Z, file = "Figures/TableS10b.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
  }
  #for data that needs normalization
  if (shap$p.value < 0.05){
    print(shapiro.test(x=resid)$p.value)
    norm_col_name = paste0("norm_", a)
    par(mfrow=c(3,1))
    df3$norm_col_name = transformTukey(df3$trait,
                                       start = -10,
                                       end = 10,
                                       int = 0.025,
                                       plotit = TRUE, #can be false 
                                       verbose = FALSE,
                                       quiet = FALSE,
                                       statistic = 1,
                                       returnLambda = FALSE  )
    aov = lm(norm_col_name ~ Cultivar+Replication, data = df3)
    col = norm_col_name
    write.table(col, file = "Figures/TableS9.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
    anova = anova(aov)
    anova = as.data.frame(anova)
    write.table(anova, file = "Figures/TableS9.txt", sep = "\t",
                row.names = TRUE, col.names = TRUE, append=TRUE)
    write.table(Z, file = "Figures/TableS9.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
    hsd = aov(aov)              
    hsd_test = HSD.test(hsd, trt = c("Replication"), console = TRUE)  
    means1 = as.data.frame(hsd_test$means)
    colnames(means1)[1] = col 
    write.table(means1, file = "Figures/TableS10a.txt", sep = "\t",
                row.names = TRUE, col.names = TRUE, append=TRUE)
    write.table(Z, file = "Figures/TableS10a.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
    groups1 = as.data.frame(hsd_test$groups)
    colnames(groups1)[1] = col 
    write.table(groups1, file = "Figures/TableS10b.txt", sep = "\t",
                row.names = TRUE, col.names = TRUE, append=TRUE)
    write.table(Z, file = "Figures/TableS10b.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
  }
}

unique(data$Solar_sum)
data$Replication = factor(data$Replication, levels = c(1, 2, 4, 3, 5))
solar_factor = diff(range(data$Solar_sum)) / 20
data$Solar_scaled = data$Solar_sum / solar_factor
B = ggplot(data, aes(x = Replication, y = Shoot.Fresh.Mass))+
  geom_boxplot(aes(fill = "palegreen4"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Solar_scaled),
    aes(x = Replication, y = Solar_scaled),
    shape = 17, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Shoot Fresh Mass (g)")+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Shoot Fresh Mass (g)")+
  scale_fill_manual(values = c("palegreen4")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
B
solar_factor = diff(range(data$Solar_sum)) / 2
data$Solar_scaled = data$Solar_sum / solar_factor
C = ggplot(data, aes(x = Replication, y = Shoot.Dry.Mass))+
  geom_boxplot(aes(fill = "palegreen4"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Solar_scaled),
    aes(x = Replication, y = Solar_scaled),
    shape = 17, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Shoot Dry Mass (g)")+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Shoot Dry Mass (g)")+
  scale_fill_manual(values = c("palegreen4")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
C
solar_factor = diff(range(data$Solar_sum)) / 8
data$Solar_scaled = data$Solar_sum / solar_factor
D = ggplot(data, aes(x = Replication, y = Shoot.Diameter))+
  geom_boxplot(aes(fill = "palegreen4"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Solar_scaled),
    aes(x = Replication, y = Solar_scaled),
    shape = 17, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Shoot Diameter (mm)")+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Shoot Diameter (mm)")+
  scale_fill_manual(values = c("palegreen4")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
D
solar_factor = diff(range(data$Solar_sum)) / 40
data$Solar_scaled = data$Solar_sum / solar_factor
E = ggplot(data, aes(x = Replication, y = RootLength_cm))+
  geom_boxplot(aes(fill = "palegreen4"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Solar_scaled),
    aes(x = Replication, y = Solar_scaled),
    shape = 17, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Root Length (cm)")+
  #,
   # sec.axis = sec_axis(
   #   ~ . * solar_factor,
   #   name = "Solar Sum",
   #   breaks = pretty(c(0, data$Solar_sum))))+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Root Length (cm)")+
  scale_fill_manual(values = c("palegreen4")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
E

data = subset(df0, Clinostat == "SM")
head(data)
SW = matrix(NA,nrow=2,ncol=2)
rownames(SW) = c("W","pvalue")
SW[1,1] = "W"
SW[2,1] = "pvalue"
Z = ""
colnames(data)
df2 = data
head(df2)
colnames(df2)
for (i in c(6:9,12)){
  df3 = df2[,c(1,4,i)]
  a = colnames(df3)[3]
  colnames(df3)[3] = "trait"
  aov = lm(trait ~ Cultivar+Replication, data = df3)
  hsd = aov(aov)              
  resid = residuals(object = hsd)
  shap = shapiro.test(x=resid)
  SW[1,2] = shap$statistic 
  SW[2,2] = shap$p.value 
  colnames(SW)[2] = a 
  SW = as.data.frame(SW)
  write.table(a, file = "Figures/TableS11-ShapiroResults.txt", sep = "\t",
              row.names = FALSE, col.names = FALSE, append=TRUE)
  write.table(SW, file = "Figures/TableS11-ShapiroResults.txt", sep = "\t",
              row.names = FALSE, col.names = FALSE, append=TRUE)
  write.table(Z, file = "Figures/TableS11-ShapiroResults.txt", sep = "\t",
              row.names = FALSE, col.names = FALSE, append=TRUE)
  if (shap$p.value > 0.05){  
    print(shapiro.test(x=resid)$p.value)
    aov = lm(trait ~ Cultivar+Replication, data = df3)
    write.table(a, file = "Figures/TableS11.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
    anova = anova(aov)
    anova = as.data.frame(anova)
    Z = ""
    write.table(anova, file = "Figures/TableS11.txt", sep = "\t",
                row.names = TRUE, col.names = TRUE, append=TRUE)
    write.table(Z, file = "Figures/TableS11.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
    hsd = aov(aov)  
    hsd_test = HSD.test(hsd, trt = c("Replication"), console = TRUE)  
    means1 = as.data.frame(hsd_test$means)
    colnames(means1)[1] = a 
    write.table(means1, file = "Figures/TableS12a.txt", sep = "\t",
                row.names = TRUE, col.names = TRUE, append=TRUE)
    write.table(Z, file = "Figures/TableS12a.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
    groups1 = as.data.frame(hsd_test$groups)
    colnames(groups1)[1] = a 
    write.table(groups1, file = "Figures/TableS12b.txt", sep = "\t",
                row.names = TRUE, col.names = TRUE, append=TRUE)
    write.table(Z, file = "Figures/TableS12b.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
  }
  #for data that needs normalization
  if (shap$p.value < 0.05){
    print(shapiro.test(x=resid)$p.value)
    norm_col_name = paste0("norm_", a)
    par(mfrow=c(3,1))
    df3$norm_col_name = transformTukey(df3$trait,
                                       start = -10,
                                       end = 10,
                                       int = 0.025,
                                       plotit = TRUE, #can be false 
                                       verbose = FALSE,
                                       quiet = FALSE,
                                       statistic = 1,
                                       returnLambda = FALSE  )
    aov = lm(norm_col_name ~ Cultivar+Replication, data = df3)
    col = norm_col_name
    write.table(col, file = "Figures/TableS11.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
    anova = anova(aov)
    anova = as.data.frame(anova)
    write.table(anova, file = "Figures/TableS11.txt", sep = "\t",
                row.names = TRUE, col.names = TRUE, append=TRUE)
    write.table(Z, file = "Figures/TableS11.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
    hsd = aov(aov)              
    hsd_test = HSD.test(hsd, trt = c("Replication"), console = TRUE)  
    means1 = as.data.frame(hsd_test$means)
    colnames(means1)[1] = col 
    write.table(means1, file = "Figures/TableS12a.txt", sep = "\t",
                row.names = TRUE, col.names = TRUE, append=TRUE)
    write.table(Z, file = "Figures/TableS12a.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
    groups1 = as.data.frame(hsd_test$groups)
    colnames(groups1)[1] = col 
    write.table(groups1, file = "Figures/TableS12b.txt", sep = "\t",
                row.names = TRUE, col.names = TRUE, append=TRUE)
    write.table(Z, file = "Figures/TableS12b.txt", sep = "\t",
                row.names = FALSE, col.names = FALSE, append=TRUE)
  }
}

unique(data$Solar_sum)
data$Replication = factor(data$Replication, levels = c(1, 2, 4, 3, 5))
solar_factor = diff(range(data$Solar_sum)) / 10
data$Solar_scaled = data$Solar_sum / solar_factor
B2 = ggplot(data, aes(x = Replication, y = Shoot.Fresh.Mass))+
  geom_boxplot(aes(fill = "burlywood1"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Solar_scaled),
    aes(x = Replication, y = Solar_scaled),
    shape = 17, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Shoot Fresh Mass (g)")+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Shoot Fresh Mass (g)")+
  scale_fill_manual(values = c("burlywood1")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
B2
solar_factor = diff(range(data$Solar_sum)) / 2
data$Solar_scaled = data$Solar_sum / solar_factor
C2 = ggplot(data, aes(x = Replication, y = Shoot.Dry.Mass))+
  geom_boxplot(aes(fill = "burlywood1"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Solar_scaled),
    aes(x = Replication, y = Solar_scaled),
    shape = 17, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Shoot Dry Mass (g)")+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Shoot Dry Mass (g)")+
  scale_fill_manual(values = c("burlywood1")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
C2
solar_factor = diff(range(data$Solar_sum)) / 6
data$Solar_scaled = data$Solar_sum / solar_factor
D2 = ggplot(data, aes(x = Replication, y = Shoot.Diameter))+
  geom_boxplot(aes(fill = "burlywood1"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Solar_scaled),
    aes(x = Replication, y = Solar_scaled),
    shape = 17, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Shoot Diameter (mm)")+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Shoot Diameter (mm)")+
  scale_fill_manual(values = c("burlywood1")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
D2
solar_factor = diff(range(data$Solar_sum)) / 20
data$Solar_scaled = data$Solar_sum / solar_factor
E2 = ggplot(data, aes(x = Replication, y = RootLength_cm))+
  geom_boxplot(aes(fill = "burlywood1"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Solar_scaled),
    aes(x = Replication, y = Solar_scaled),
    shape = 17, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Root Length (cm)")+
    #,
  #sec.axis = sec_axis(
   # ~ . * solar_factor,
  #  name = "Solar Sum",
   # breaks = pretty(c(0, data$Solar_sum))))+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Root Length (cm)")+
  scale_fill_manual(values = c("burlywood1")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
E2

gglist = list(B, C, D, E, B2, C2, D2, E2)
combined_plot = plot_grid(plotlist = gglist, ncol = 4, labels = c("A", "", "", "", "B", "", "", ""), label_x = 0, label_y = 1)
combined_plot
#pdf(file="Figures/FigureS6.pdf", width=8, height=6)
combined_plot  
#dev.off()

data = subset(df0, Clinostat == "C")
head(data)
data$Replication = factor(data$Replication, levels = c(1, 2, 4, 3, 5))
temp_factor = diff(range(data$Temp_mean)) /5
data$Temp_scaled = data$Temp_mean / temp_factor
B = ggplot(data, aes(x = Replication, y = Shoot.Fresh.Mass))+
  geom_boxplot(aes(fill = "palegreen4"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Temp_scaled),
    aes(x = Replication, y = Temp_scaled),
    shape = 8, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Shoot Fresh Mass (g)")+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Shoot Fresh Mass (g)")+
  scale_fill_manual(values = c("palegreen4")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
B
temp_factor = diff(range(data$Temp_mean))
data$Temp_scaled = data$Temp_mean / temp_factor
C = ggplot(data, aes(x = Replication, y = Shoot.Dry.Mass))+
  geom_boxplot(aes(fill = "palegreen4"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Temp_scaled),
    aes(x = Replication, y = Temp_scaled),
    shape = 8, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Shoot Dry Mass (g)")+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Shoot Dry Mass (g)")+
  scale_fill_manual(values = c("palegreen4")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
C
temp_factor = diff(range(data$Temp_mean))/1.5
data$Temp_scaled = data$Temp_mean / temp_factor
D = ggplot(data, aes(x = Replication, y = Shoot.Diameter))+
  geom_boxplot(aes(fill = "palegreen4"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Temp_scaled),
    aes(x = Replication, y = Temp_scaled),
    shape = 8, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Shoot Diameter (mm)")+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Shoot Diameter (mm)")+
  scale_fill_manual(values = c("palegreen4")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
D
temp_factor = diff(range(data$Temp_mean))/10
data$Temp_scaled = data$Temp_mean / temp_factor
E = ggplot(data, aes(x = Replication, y = RootLength_cm))+
  geom_boxplot(aes(fill = "palegreen4"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Temp_scaled),
    aes(x = Replication, y = Temp_scaled),
    shape = 8, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Root Length (cm)")+
    #,
    #sec.axis = sec_axis(
    #  ~ . * temp_factor,
    #  name = "Temperature",
    #  breaks = pretty(c(0, data$Temp_mean))))+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Root Length (cm)")+
  scale_fill_manual(values = c("palegreen4")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
E
data = subset(df0, Clinostat == "SM")
data$Replication = factor(data$Replication, levels = c(1, 2, 4, 3, 5))
temp_factor = diff(range(data$Temp_mean)) /2.5
data$Temp_scaled = data$Temp_mean / temp_factor
B2 = ggplot(data, aes(x = Replication, y = Shoot.Fresh.Mass))+
  geom_boxplot(aes(fill = "burlywood1"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Temp_scaled),
    aes(x = Replication, y = Temp_scaled),
    shape = 8, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Shoot Fresh Mass (g)")+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Shoot Fresh Mass (g)")+
  scale_fill_manual(values = c("burlywood1")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
B2
temp_factor = diff(range(data$Temp_mean))/0.35
data$Temp_scaled = data$Temp_mean / temp_factor
C2 = ggplot(data, aes(x = Replication, y = Shoot.Dry.Mass))+
  geom_boxplot(aes(fill = "burlywood1"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Temp_scaled),
    aes(x = Replication, y = Temp_scaled),
    shape = 8, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Shoot Dry Mass (g)")+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Shoot Dry Mass (g)")+
  scale_fill_manual(values = c("burlywood1")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
C2
temp_factor = diff(range(data$Temp_mean))/1.25
data$Temp_scaled = data$Temp_mean / temp_factor
D2 = ggplot(data, aes(x = Replication, y = Shoot.Diameter))+
  geom_boxplot(aes(fill = "burlywood1"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Temp_scaled),
    aes(x = Replication, y = Temp_scaled),
    shape = 8, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Shoot Diameter (mm)")+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Shoot Diameter (mm)")+
  scale_fill_manual(values = c("burlywood1")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
D2
temp_factor = diff(range(data$Temp_mean))/4
data$Temp_scaled = data$Temp_mean / temp_factor
E2 = ggplot(data, aes(x = Replication, y = RootLength_cm))+
  geom_boxplot(aes(fill = "burlywood1"), position = position_dodge(width = 0.8)) +
  geom_point(
    data = data %>% distinct(Replication, Temp_scaled),
    aes(x = Replication, y = Temp_scaled),
    shape = 8, size = 2, color = "black") +
  scale_y_continuous(
    limits = c(0, NA),
    name = "Root Length (cm)")+
    #,
    #sec.axis = sec_axis(
      #~ . * temp_factor,
      #name = "Temperature",
      #breaks = pretty(c(0, data$Temp_mean))))+
  scale_x_discrete(labels = rep_labels) +
  labs(
    x = NULL,
    y = "Root Length (cm)")+
  scale_fill_manual(values = c("burlywood1")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10, angle = 60, hjust=1),
        axis.text.y = element_text(size=10),
        axis.text = element_text(size=10),
        axis.title = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10))
E2
data = df0
colnames(data)
traits = colnames(data)[c(7:9,12)] 
mean_df = data %>%
  group_by(Replication, Clinostat) %>%
  summarise(
    across(all_of(traits), mean, na.rm = TRUE),
    .groups = "drop"
  )
head(mean_df)
norm_df = mean_df %>%
  group_by(Replication) %>%
  mutate(across(all_of(traits),
                ~ .x / .x[Clinostat == "C"])) %>%
  ungroup()
norm_df
norm_long = norm_df %>%
  pivot_longer(
    cols = c(Shoot.Fresh.Mass:RootLength_cm),
    names_to = "Trait",
    values_to = "Value"
  )
head(norm_long)
norm_long$Replication = factor(norm_long$Replication, levels = c(1, 2, 4, 3, 5))
F = ggplot(norm_long %>% 
             filter(Clinostat == "SM"),
           aes(x = Replication, y = Value, shape = Trait)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_point(size = 3, position = position_jitter(width = 0.15, height = 0)) +
  labs(
    y = "Ratio SM:C",
    x = "Increasing Temperature ---->",
    shape = "Trait") +
  scale_shape_manual(values = c(16, 17, 15, 1, 8),
                     labels = c("Apical.Bud.Height" = "Apical Bud Height", "Shoot.Fresh.Mass" = "Shoot Fresh Mass", "Shoot.Dry.Mass" = "Shoot Dry Mass", "Shoot.Diameter" = "Shoot Diameter", "RootLength_cm" = "Root Length")) +
  scale_y_continuous(limits=c(0,2.5), breaks=seq(0,2.5,0.5))+
  scale_x_discrete(
    labels = c(
      "1" = "February",
      "2" = "March",
      "3" = "April",
      "4" = "May",
      "5" = "June")) +
  theme_bw()+
  theme(legend.position = "top",
    axis.text.x = element_text(size=10, angle = 60, hjust=1),
    axis.text.y = element_text(size=10),
    axis.text = element_text(size=10),
    legend.title = element_text(face="bold", size=12),
    axis.title = element_text(face="bold", size=10),
    axis.title.x = element_text(face="bold", size=12),
    axis.title.y = element_text(face="bold", size=12))
F
#pdf(file="Figures/Figure5.pdf", width=8, height=6)
ggdraw() +
  draw_plot(B, x = 0, y = 0.50, width = 0.175, height = 0.5) +
  draw_plot(C, x = 0.175, y = 0.50, width = 0.175, height = 0.5) +
  draw_plot(D, x = 0.35, y = 0.50, width = 0.175, height = 0.5) +
  draw_plot(E, x = 0.525, y = 0.50, width = 0.175, height = 0.5) +
  draw_plot(B2, x = 0, y = 0, width = 0.175, height = 0.5) +
  draw_plot(C2, x = 0.175, y = 0, width = 0.175, height = 0.5) +
  draw_plot(D2, x = 0.35, y = 0, width = 0.175, height = 0.5) +
  draw_plot(E2, x = 0.525, y = 0, width = 0.175, height = 0.5) +
  draw_plot(F, x = 0.7, y = 0, width = 0.3, height = 0.75) +
  draw_plot_label(label = c("A", "B", "C"), 
                  size = 10,
                  x = c(0,0,0.7), 
                  y = c(1,0.5,0.75))
#dev.off()
data = df0
rm(list = setdiff(ls(), c("data","cultivar_labels","rep_labels")))