#Clean workspace
rm(list = ls())

#1.Set Directory####

wd <- "C:/Documentos/PhD/Projeto/Abordagem 1.b/mensuração densidade"
setwd(wd)
dir()

#2.Packages required####
#install.packages("")
library(tidyverse)
library(lme4)

#3.Reading database####

#reading all the file with "_output" in the name inside of the Output file
dir()
dir_files <- file.path(wd,"Output")
list_files <- list.files(dir_files)
length(list_files)

#create an empty data frame
base_total <- data.frame()

#test the code
#name_data <- list.files(file.path(dir_files,list_files[1]), pattern = "\\.txt$")
# file_path <- file.path(dir_files,list_files[1],name_data[1])
# data_file <-  read.table(file_path, sep = ";", header = TRUE)
# data_file$species <- paste(sub("\\_output$", "", file))
# data_file$position <- substr(data_file$Cod, nchar(data_file$Cod) - 1, nchar(data_file$Cod)-1) 
# data_file$ray <- substr(data_file$Cod, nchar(data_file$Cod), nchar(data_file$Cod))
# data_file$cod_arv <- substr(data_file$Cod,1,nchar(data_file$Cod) - 2)

for (file in 1:length(list_files)) {
    
    #select a file and list all the file .txt inside of it  
    name_data <-  list.files(file.path(dir_files,list_files[file]), pattern = "\\.txt$") 
  
  #select the each database inside of the file
  for (data in 1:length(name_data)) { 
    
    # database path
    data_path <- file.path(dir_files,list_files[file],name_data[data])
    #read the database 
    data_file <-  read.table(data_path, sep = ";", header = TRUE) 
   
    #add specie name using the file name
    data_file$species <- paste(sub("\\_output$", "", list_files[file])) 
    
    #add height position (base, middle, top)
    data_file$position <- substr(data_file$Cod, nchar(data_file$Cod) - 1, nchar(data_file$Cod)-1) 
    
    #add the number of the ray 
    data_file$ray <- substr(data_file$Cod, nchar(data_file$Cod), nchar(data_file$Cod))
    
    substr(data_file$Cod,1,nchar(data_file$Cod) - 2)#o código da árvore vai vir do código da amostr obtida no script medição_raiosX
    
    base_total <- rbind(base_total, data_file) #criei a base final juntando todas as bases na base vazia

  }
}

str(base_total)
unique(base_total$species)
length(unique(base_total$Cod)) 

#check if any position has na
base_total$Cod[which(is.na(base_total$position))]

write.table(base_total,file.path(wd,"base_total.txt"), sep = ";")


#4. Verificando qualidade de dados ####

check_base <-  base_total %>% 
  filter(Profile >= 0.2 & Profile <= 1.5) %>%
  dplyr::select(species, cod_arv, position, ray, Profile, DistPith2Bark_mm) %>%
  group_by(species, cod_arv, position, ray)%>%
  summarise(WD = mean(Profile), SD = sd(Profile), P2B = max(DistPith2Bark_mm))

check_base2 <-  base_total %>% 
  filter(Profile >= 0.2 & Profile <= 1.5) %>%
  dplyr::select(species, cod_arv, position, ray, Profile, DistPith2Bark_mm) %>%
  group_by(position)%>%
  summarise(WD = mean(Profile), SD = sd(Profile), P2B = max(DistPith2Bark_mm))

#5. Verificando graficamente####

##5.1.verificando diferença entre base, meio e topo e posição do lenho####

#transformando em fator os níveis
base_total$position <- factor(base_total$position, level = c("T", "M", "B"))
base_total$cod_arv <- factor(base_total$cod_arv)
base_total$ray <- factor(base_total$ray)
base_total$group_perc <- factor(base_total$group_perc)

#criei uma base para dividir o crescimento radial em percentual e plotei o boxplot

base_total <- base_total %>%
  filter(Profile >= 0.2 & Profile <= 1.5) %>%
  dplyr::select(species, cod_arv, position, ray, Profile, DistPith2Bark_mm) %>%
  group_by(species, cod_arv, position, ray)%>% 
  mutate(perc_dist2pith = DistPith2Bark_mm/max(DistPith2Bark_mm)*100) %>%
  mutate(group_perc = cut(perc_dist2pith, breaks = seq(0, 100, by = 10), include.lowest = TRUE))
  
ggplot(base_total, aes(factor(group_perc), Profile)) +
  geom_boxplot() +
  facet_wrap(~ position, nrow = 3) + # Dividir os dados em diferentes painéis por position
  labs(x = "Percentual", y = "Profile") + # Rótulos dos eixos
  theme_minimal()

ggplot(base_total %>%
         filter(Profile >= 0.2 & Profile <= 1.5), 
       aes(DistPith2Bark_mm, Profile)) +
  geom_line (aes(group = cod_arv)) +
  geom_smooth(method = "lm") +
  facet_wrap(~position + group_perc, scales = "free_x", ncol = 5) + # Dividir os dados em diferentes painéis por position
  labs(x = "Percentual", y = "Distance Pith to Bark") + 
  theme_bw()

ggplot(base_total %>%
         filter(Profile >= 0.2 & Profile <= 1.5), 
       aes(DistPith2Bark_mm, Profile)) +
  geom_line (aes(group = cod_arv), alpha = 0.3) +
  geom_smooth(method = "gam") +
  facet_wrap(~position, nrow = 3) + # Dividir os dados em diferentes painéis por position
  labs(x = "Percentual", y = "Distance Pith to Bark") + 
  theme_bw()

ggplot(base_total %>%
         filter(Profile >= 0.2 & Profile <= 1.5), 
       aes(DistPith2Bark_mm, Profile)) +
  geom_line (aes(group = cod_arv), alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_wrap(~position, nrow = 3) + # Dividir os dados em diferentes painéis por position
  labs(x = "Percentual", y = "Distance Pith to Bark") + 
  theme_bw()

#testando a linearidade dos dados
summary(lm(Profile ~ DistPith2Bark_mm, data  = base_total %>%
     filter(Profile >= 0.2 & Profile <= 1.5)%>%
     filter(position == "T")))

summary(lm(Profile ~ DistPith2Bark_mm, data  = base_total %>%
             filter(Profile >= 0.2 & Profile <= 1.5)%>%
             filter(position == "M")))

summary(lm(Profile ~ DistPith2Bark_mm, data  = base_total %>%
             filter(Profile >= 0.2 & Profile <= 1.5)%>%
             filter(position == "B")))

#Avaliando a diferença entre médias considerando a posição no tronco, o indivíduo e a repetição do raio

modelo_anova <- aov(Profile ~ position + Error(position:ray:cod_arv), data = base_total)
summary(modelo_anova) 

#5. Modelo misto 

# 5.3. Variance component analysis####

#modelo simples, considerando só o intercepto
simp_mod <- lm(Profile ~ DistPith2Bark_mm, data = base_total)
summary(simp_mod)

#modelo considerando efeito aleatório da posição/individuo/raio
mod_arv1 <- lme4::lmer(Profile ~ DistPith2Bark_mm  + (1 | position/cod_arv/ray), REML = TRUE, data = base_total) #mantive o REML (Máxima Verossimilhança Restrita) pois lida melhor com dados desbalanceados

mod_arv2 <- lme4::lmer(Profile ~ DistPith2Bark_mm  + (1|cod_arv/position/ray), REML = TRUE, data = base_total)

summary(mod_arv2)
anova(mod_arv1, mod_arv2)
vc=VarCorr(mod_arv2)
vc

cod_arv <-  vc$cod_arv[1] 
pos_cod_arv <- vc$`position:cod_arv`[1]
poscos_rau <- vc$`ray:(position:cod_arv)`[1]
res.var =attr(vc,"sc")^2
all_var =cod_arv + pos_cod_arv + poscos_rau + res.var
attr(vc$cod_arv,"stddev")^2

poscos_rau/all_var #0.03687329
pos_cod_arv/all_var #0.310094
cod_arv/all_var #6.419091e-08
res.var/all_var #0.6530326

1.223e-03/all_var*100 #ray = 3.843253
2.028e-11/all_var*100 #cod_arv = 6.37295e-08
9.570e-03/all_var*100 #position = 30.07353
2.166e-02/all_var*100 #residuo = 68.06612

#modelo considerando efeito aleatório do indivíduo e da posição da altura

mod_pos <- lme4::lmer(Profile ~ DistPith2Bark_mm + (1 | cod_arv/position), REML = FALSE, data = base_total)
summary(mod_pos)

all_var =9.100e-03 + 3.441e-12 + 2.277e-02 
arv_var_perc = 3.441e-12/ all_var * 100 #praticamente zero
pos_var_perc = 9.100e-03 / all_var * 100 #representa 28.55% da variância total

anova(mod_arv, mod_pos) #são significativamente diferentes. A explicação só pela arv tem menor AIC 

#modelo considerando efeito aleatório do indivíduo, da posição da altura, do raio

mod_porc <- lme4::lmer(Profile ~ DistPith2Bark_mm + (1 | cod_arv/position/ray), REML = FALSE, data = base_total)
summary(mod_porc)

all_var = 1.410e-03 + 8.102e-03 + 2.462e-11 + 2.202e-02

cod_arv = 2.462e-11/all_var*100 #0 % variância
ray = 1.410e-03/all_var*100 #  4.47% variância
position = 8.102e-03/all_var*100 #25.69% variância
Random_effects = 2.202e-02/all_var*100 #69.83382% 

#5.4. Perfil de densidade ####

mod0 <- lm(Profile ~ DistPith2Bark_mm, data = base_total)
summary(mod0) 

mod1 <- lm(Profile ~ DistPith2Bark_mm + I(DistPith2Bark_mm^2) , data = base_total)
summary(mod1) 

mod2 <- lme4::lmer(Profile ~DistPith2Bark_mm + I(DistPith2Bark_mm^2) + (1 | position/ray/cod_arv), REML = FALSE, data = base_total)

summary(mod2)

mod3 <-  lme4::lmer(Profile ~DistPith2Bark_mm + (1 | cod_arv/position/ray), REML = FALSE, data = base_total)

summary(mod3)

anova(mod2, mod3)

mod4 <-  lme4::lmer(Profile ~DistPith2Bark_mm + (1 |position), REML = FALSE, data = base_total)
pred_values <- predict(mod4, newdata = base_total, type = "response")

anova(mod3, mod4)
summary(mod4)

new_data <- data.frame(DistPith2Bark_mm = base_total$DistPith2Bark_mm, Profile = pred_values, position = base_total$position)

# Plotar os dados originais com os valores ajustados
ggplot() +
 #geom_point(data = base_total, aes(x = DistPith2Bark_mm, y = Profile, color = position), alpha = 0.3) +  # Plotar os dados originais
  geom_point(data = new_data, aes(x = DistPith2Bark_mm, y = Profile, color = position)) +  # Adicionar a linha dos valores ajustados
  labs(title = "Valores ajustados vs. Dados originais", x = "DistPith2Bark_mm", y = "Profile")

