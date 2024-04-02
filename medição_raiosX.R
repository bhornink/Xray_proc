#Criado em 06 de fev de 2024 por B.Hornink
#Script para obtenção de perfis de densidade aparente da madeira, em imagens de raios X Não inclui marcação de anéis de crescimento.
#Artigo base: xRing—An R package to identify and measure tree-ring features using X-ray microdensity profiles - https://doi.org/10.1016/j.dendro.2018.11.002

rm(list = ls())

#1.Diretório####
wd <- "C:/Documentos/PhD/Projeto/Abordagem 1.b/mensuração densidade"
setwd(wd)

#gera a pasta onde ficarão os arquivos da espécie que estamos trabalhando. Deve ser rodado só uma vez 
dir.create("AL_output") #pasta da Astronium leicotei
dir.create("BP_output") #pasta do Brosimum parinarioides
dir.create("CO_output") #pasta da Cedrela odorata
dir.create("HC_output") #pasta da Cedrela odorata
dir.create("CV_output") #pasta da Caryocar villosum

dir()

#pasta onde será salvo os arquivos output
output_file <- "CV_output/"

#2.Pacotes####
#install.packages("xRing")
library(xRing)
library(tidyverse)

#3.Calibrações####
#a calibração deve ser feita para cada imagem, a menos que tenha mais de um raio na mesma imagem, dai usa a mesma calibração. 

directory_images <- "raios-x/CV_png"

name_images <- list.files(directory_images) #lista o nome das imagens na pasta
name_images

#o script é recomeçado para uma nova imagem sempre nessa linha de baixo. As linhas anteriores são rodadas uma única vez
image_selected <- name_images[27] #trocar o número para trocar de imagem!!!!!!!!
image_selected

img <- imRead(file.path(directory_images, image_selected)) #a imagem tem que estar em png, esse comando não aceita tiff

crop_img <- imCrop(img) #recorte a imagem para conseguir dar um zoom melhor. Faça o recorte mantendo as amostras e o acetato

cal_img <- calibrateFilm(crop_img, thickness = c(0,0.28,0.53,0.78,1.03,1.28,1.53,1.78,2.03), density= 1.274, plot = TRUE) 
#a calibração deve começar a área mais escura para a mais branca. São 9 steps, então tem que ter 9 quadrados na sua calibração. 

#4.Criando o perfil de densidade####
prof_img <- measureProfiles(crop_img, nPixel = 77.8, cal = cal_img) #nPixel é a espessura do path, 38.90 pixels para 1 mm de distância, ou seja, em 2 mm preciso de 77.8 pixels. Isso vai ser o mesmo para todas as imagens, pois todas tem a mesma dimensão

#O que levar em conta quando fizer o caminho: 
# 1) O path tem que ser feito seguindo o máximo o crescimento do raio; 
# 2) Começar pela medula e seguir até o alburno; 
# 3) Não passar por regiões dismorfas, que tenha alteração de densidade (ex.: região de galho) 
# 4) Não passar por rachaduras, quando possível pular essas regiões
# 5) Não começar o caminho fora da amostras, tente começar bem na borda da medula e terminar na borda do alburno

plot(prof_img)

#5. Salvando o perfil de densidade####

df <- data.frame()  # Cria um data frame vazio

# loop para juntar cada path criado
for (path in names(prof_img)) {
  # Acessa o perfil de cada path
  current_profile <- prof_img[[path]]$profile
  name_profile <- prof_img[[path]]$name
  
  # Cria um data frame temporário com os dados atuais
  temp_df <- data.frame(Profile = current_profile, Path_Name = name_profile)
  
  # Adiciona o data frame temporário (temp_df) ao data frame principal (df)
  df <- rbind(df, temp_df)
}

# Calcula a medida acumulada da distância medula a casca e adiciona no data frame temporário
Measure <- 0.026 #esse é o tamanho de cada pixel
df$DistPith2Bark_mm <- cumsum(rep(Measure, length(df[,1])))
df$Cod <- paste(sub("_30kv_5X3s.png","",sub("processed_wn_","", image_selected))) 
head(df)

Cod <- paste(sub("_30kv_5X3s.png","",sub("processed_wn_","", image_selected))) 

#6. Salvar o arquivo####
filename <- paste(output_file,Cod, sep = "")
write.table(df, file = paste(filename,".txt",sep = ""), sep = ";", row.names = FALSE) 

