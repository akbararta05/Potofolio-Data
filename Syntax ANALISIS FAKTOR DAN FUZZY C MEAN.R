#Load Packages
library(readr) # untuk membaca data
library(dplyr) # untuk data manipulation
library(GGally) # untuk membuat matriks korelasi
library(psych) # untuk melakukan analisis KMO
library(ggplot2) # untuk membuat plot
library(GPArotation) # untuk melakukan factor analysis
library(factoextra) # untuk mengekstrak dan visualisasi Hasil analisis data multivariat
library(gridExtra) # untuk membuat mixed plot dalam satu hasil
library(tidyr)
library(rmdformats)
library(BAMMtools) # untuk membuat natural breaks pada pemetaan
library(geojsonio) # untuk membaca file geojson
library(stringr) 
library(leaflet) # untuk visualisasi interaktif
library(ppclust) # untuk membuat Probabilistic and Possibilistic Cluster Analysis

#Data loading
data<-read.csv(file.choose(),header=T,sep=",",dec=".")
attach(data)
rmarkdown::paged_table(data)

#Data Cleaning
glimpse(data)

#Cek Missing Value
colSums(is.na(data))

####Mengatasi Missing value
data$AHH = ifelse(is.na(data$AHH),ave(data$AHH, FUN = function(x)
  mean(x, na.rm = 'TRUE')),data$AHH)

##Explorasi Data
#1. Gambaran umum variabel penyusun
summary(data)

#2. Melihat Visualisasi antar beberapa variabel
#Nilai korelasi antarvariabel
ggpairs(data[,c(2:9)], showStrips = F, upper = list(continuous = wrap("cor", size=2))) + 
  theme(axis.text = element_text(colour = "black", size = 7),
        strip.background = element_rect(fill = "#6e8f71"),
        strip.text = element_text(colour = "white", size = 6,
                                  face = "bold"))
#3. Melihat Diagram batang masing-masing variabel
#a.PDRB
ggplot(data = data, mapping = aes(x=  PDRB.Per.Kapita, y= reorder(Provinsi,PDRB.Per.Kapita))) +
  geom_col(aes(fill = PDRB.Per.Kapita))  + # untuk membuat barplot 
  labs(
    title = "PDRB Perkapita di Indonesia",
    x = "PDRB Perkapita",
    y = "Provinsi"
  ) +
  scale_fill_gradient(low = "#a9d1b1", high = "#4c614e") +
  geom_text(mapping = aes(label=round(PDRB.Per.Kapita, digits = 4)), # menambahkan informasi label masing-masing kategori dengan variasi box
            col = "white", # memberikan warna pada text
            nudge_x = -1, # menggeser text berdasarkan sumbu x
            label.size = 10) + 
  theme_minimal() +
  theme(legend.position = "none") # untuk menghilangkan legend.

#b.Penduduk Miskin
ggplot(data = data, mapping = aes(x=  Jumlah.Penduduk.Miskin, y= reorder(Provinsi,Jumlah.Penduduk.Miskin))) +
  geom_col(aes(fill = Jumlah.Penduduk.Miskin))  + # untuk membuat barplot 
  labs(
    title = "Jumlah Penduduk Miskin di Indonesia",
    x = "Jumlah Penduduk Miskin",
    y = "Provinsi"
  ) +
  scale_fill_gradient(low = "#a9d1b1", high = "#4c614e") +
  geom_text(mapping = aes(label=round(Jumlah.Penduduk.Miskin, digits = 4)), # menambahkan informasi label masing-masing kategori dengan variasi box
            col = "white", # memberikan warna pada text
            nudge_x = -1, # menggeser text berdasarkan sumbu x
            label.size = 10) + 
  theme_minimal() +
  theme(legend.position = "none") # untuk menghilangkan legend.

#c.Kepadatan penduduk
ggplot(data = data, mapping = aes(x=  Kepadatan.Penduduk, y= reorder(Provinsi,Kepadatan.Penduduk))) +
  geom_col(aes(fill = Kepadatan.Penduduk))  + # untuk membuat barplot 
  labs(
    title = "Kepadatan Penduduk di Indonesia",
    x = "Kepadatan Penduduk",
    y = "Provinsi"
  ) +
  scale_fill_gradient(low = "#a9d1b1", high = "#4c614e") +
  geom_text(mapping = aes(label=round(Kepadatan.Penduduk, digits = 4)), # menambahkan informasi label masing-masing kategori dengan variasi box
            col = "white", # memberikan warna pada text
            nudge_x = -1, # menggeser text berdasarkan sumbu x
            label.size = 10) + 
  theme_minimal() +
  theme(legend.position = "none") # untuk menghilangkan legend.

#d.Pengeluaran perkapita
ggplot(data = data, mapping = aes(x=  Pengeluaran.per.Kapita, y= reorder(Provinsi,Pengeluaran.per.Kapita))) +
  geom_col(aes(fill = Pengeluaran.per.Kapita))  + # untuk membuat barplot 
  labs(
    title = "Pengeluaran Perkapita di Indonesia",
    x = "Pengeluaran Perkapita",
    y = "Provinsi"
  ) +
  scale_fill_gradient(low = "#a9d1b1", high = "#4c614e") +
  geom_text(mapping = aes(label=round(Pengeluaran.per.Kapita, digits = 4)), # menambahkan informasi label masing-masing kategori dengan variasi box
            col = "white", # memberikan warna pada text
            nudge_x = -1, # menggeser text berdasarkan sumbu x
            label.size = 10) + 
  theme_minimal() +
  theme(legend.position = "none") # untuk menghilangkan legend.

#e.Angka harapan hidup
ggplot(data = data, mapping = aes(x=  AHH, y= reorder(Provinsi,AHH))) +
  geom_col(aes(fill = AHH))  + # untuk membuat barplot 
  labs(
    title = "Angka Harapan Hidup di Indonesia",
    x = "Angka Harapan Hidup",
    y = "Provinsi"
  ) +
  scale_fill_gradient(low = "#a9d1b1", high = "#4c614e") +
  geom_text(mapping = aes(label=round(AHH, digits = 4)), # menambahkan informasi label masing-masing kategori dengan variasi box
            col = "white", # memberikan warna pada text
            nudge_x = -1, # menggeser text berdasarkan sumbu x
            label.size = 10) + 
  theme_minimal() +
  theme(legend.position = "none") # untuk menghilangkan legend.

#f.Rata-rata lama sekolah
ggplot(data = data, mapping = aes(x=   Rata.Rata.Lama.Sekolah, y= reorder(Provinsi, Rata.Rata.Lama.Sekolah))) +
  geom_col(aes(fill =  Rata.Rata.Lama.Sekolah))  + # untuk membuat barplot 
  labs(
    title = "Rata-Rata Lama Sekolah di Indonesia",
    x = "Rata-Rata Lama Sekolah",
    y = "Provinsi"
  ) +
  scale_fill_gradient(low = "#a9d1b1", high = "#4c614e") +
  geom_text(mapping = aes(label=round( Rata.Rata.Lama.Sekolah, digits = 4)), # menambahkan informasi label masing-masing kategori dengan variasi box
            col = "white", # memberikan warna pada text
            nudge_x = -1, # menggeser text berdasarkan sumbu x
            label.size = 10) + 
  theme_minimal() +
  theme(legend.position = "none") # untuk menghilangkan legend.

#g.Gini ratio
ggplot(data = data, mapping = aes(x= Gini.Ratio, y= reorder(Provinsi,Gini.Ratio))) +
  geom_col(aes(fill = Gini.Ratio))  + # untuk membuat barplot 
  labs(
    title = "Gini Ratio di Indonesia",
    x = "Gini Ratio",
    y = "Provinsi"
  ) +
  scale_fill_gradient(low = "#a9d1b1", high = "#4c614e") +
  geom_text(mapping = aes(label=round(Gini.Ratio, digits = 4)), # menambahkan informasi label masing-masing kategori dengan variasi box
            col = "white", # memberikan warna pada text
            nudge_x = 0, # menggeser text berdasarkan sumbu x
            label.size = 10) + 
  theme_minimal() +
  theme(legend.position = "none") # untuk menghilangkan legend.

#h.Tingkat pengangguran terbuka
ggplot(data = data, mapping = aes(x=  Tingkat.Pengangguran.Terbuka, y= reorder(Provinsi,Tingkat.Pengangguran.Terbuka))) +
  geom_col(aes(fill = Tingkat.Pengangguran.Terbuka))  + # untuk membuat barplot 
  labs(
    title = "Tingkat Pengangguran Terbuka",
    x = "Tingkat Pengangguran Terbuka",
    y = "Provinsi"
  ) +
  scale_fill_gradient(low = "#a9d1b1", high = "#4c614e") +
  geom_text(mapping = aes(label=round(Tingkat.Pengangguran.Terbuka, digits = 4)), # menambahkan informasi label masing-masing kategori dengan variasi box
            col = "white", # memberikan warna pada text
            nudge_x = -1, # menggeser text berdasarkan sumbu x
            label.size = 10) + 
  theme_minimal() +
  theme(legend.position = "none") # untuk menghilangkan legend.

#Standarisasi Data
scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y)
  as.vector(scale(y)))
data1 <- data %>% scale_numeric()
data1

#Analisis
###1. Analisis Faktor
ggcorr(data1, label = T, hjust = 1, layout.exp = 2, size = 3, label_size = 2, label_round = 2, low = "#d7fc5b", mid = "white", high = "#5d8064")

#== Uji Multikolinieritas
diag(solve(cor(data1[2:9])))

##Kaiser-Meyer-Olkin (KMO) - Measure of Sampling Adequacy(MSA)
data_cor <- cor(data1[,2:9])
data_KMO<-KMO(data_cor)

data_KMO$MSA

rmarkdown::paged_table(as.data.frame(data_KMO$MSAi))

##The Bartlett's Test statistics
bartlett.test(data[,2:9])

##Membentuk faktor-faktor penyusun
fa_data <- fa(r=data_cor)
fa_data

nfactors <- fa.parallel(data1[,2:9], plot = T)
summary(nfactors)

# jumlah faktor optimal yang didapat
nfact <- nfactors$nfact

#factor analysis
fa_data_opt <- fa(r = data_cor, nfactors = nfact, rotate = "varimax")
fa_data_opt

#Faktor-Faktor Pembentuk Kesejahteraan Rakyat 
fa.diagram(fa_data_opt, rsize = 1)

#Melakukan PCA pada data
data_pca<-prcomp(data1[,2:9], scale. = T)
rmarkdown::paged_table(as.data.frame(data_pca$x))

pc_keep <- data_pca$x[,1:2] %>% 
  as.data.frame()
rmarkdown::paged_table(pc_keep)

#Melakukan perhitungan persamaan indeks kesejahteraan rakyat
ikr <- pc_keep %>% 
  mutate(ikr = 0.52*abs(PC1) + 0.48*abs(PC2))
rmarkdown::paged_table(ikr)

#Penggabungan nilai hasil perhitungan indeks kesejahteraan rakyat
data_new <- data %>% 
  select_if(~!is.numeric(.)) %>% # ambil kolom provinsi
  cbind(ikr = ikr[,3]) # gabungkan dengan kolom ikr

rmarkdown::paged_table(data_new)

# Membagi ikr ke dalam 3 kategori
natural_breaks <- function(df, var) {
  var_breaks <- BAMMtools::getJenksBreaks(df[[var]], k = 4)
  df[[paste0('Jenks_', var)]] <- findInterval(df[[var]], var_breaks)
  df
}

data_new <- natural_breaks(data_new, "ikr")

# Membuat Kategori
data_new$Kategori <- ordered(data_new$Jenks_ikr,
                               levels = c(1,2,3,4),
                               labels = c("Tidak Sejahtera","Kurang Sejahtera","Cukup Sejahterah", "Sejahtera"))
rmarkdown::paged_table(data_new)

##Fuzzy C-Means Clustering terhadap Kesejahteraan Rakyat
data_FCM <- data1[,2:9]
head(data_FCM)

rownames(data_FCM) <- data1$Provinsi

#Pemilihan K optimum
set.seed(123)

elbow <- 
  fviz_nbclust(
    x = data_FCM,
    FUNcluster = kmeans,
    method = "wss",
    k.max = 10
  ) + labs(subtitle = "Metode Elbow")
elbow

#Melakukan FCM
library(ppclust)
set.seed(123)
fcm.data <- fcm(data_FCM, 4, m=2, dmetric="sqeuclidean", pw = 2, 
                  alginitv="kmpp", alginitu="imembrand", 
                  nstart=1, iter.max=1000, con.val=1e-09)
summary(fcm.data)

fcm.data$csize

fcm_keep<-data.frame(data_FCM, fcm.data$cluster)
rmarkdown::paged_table(fcm_keep)

##Interpretasi: Cluster Profiling
v <- data.frame(fcm.data$v)
rmarkdown::paged_table(v)

##Visualisasi klaster
fcm.data2 <- ppclust2(fcm.data, "kmeans")
factoextra::fviz_cluster(fcm.data2, data = data_FCM, 
                         ellipse.type = "convex",
                         palette = "jco",
                         repel = TRUE)

#Uji Kruskall-walis terhadap kluster yang terbentuk
## kruskall walis
kruskal.test(fcm_keep)
