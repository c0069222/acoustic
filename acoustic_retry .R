install.packages("devtools")
devtools::install_github("https://github.com/DenaJGibbon/behaviouR")

# load necessary packages ####

library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)

# create a focal recordings subfolder to extract the sound files ####

# name the folder focal recordings ####

dir.create(file.path("FocalRecordings"), showWarnings = FALSE)

# load the sound files that are in the behaviouR package 

githubURL <- "https://github.com/DenaJGibbon/behaviouRdata/raw/master/data/FocalRecordings.rda"
FocalRecordings <- get(load(url(githubURL)))

# now save the recordings into the focal recordings folder ####

for (a in 1:length(FocalRecordings)) {
  FileName <- FocalRecordings[[a]][1][[1]]
  WaveFile <- FocalRecordings[[a]][2][[1]]
  writeWave(WaveFile, paste("FocalRecordings/", FileName, sep = ""))
}

# once completed, a .wav file will have been created, this is standard format for audio ####

# import and display the individual .wav file ####

GibbonWaveFile <- readWave("FocalRecordings/FemaleGibbon_1.wav")
GibbonWaveFile

# check the duration of the audio makes sense ####

duration(GibbonWaveFile) * GibbonWaveFile@samp.rate

# plot the amplitude to create an oscillogram ####

oscillo(GibbonWaveFile)

# zoom into a fraction of the graph to more closely analyse a section ####

oscillo(GibbonWaveFile, from = 0.1, to = 0.2)

# zoom in even further ####

oscillo(GibbonWaveFile, from = 0.15, to = 0.2)

# create a spectogram to show the spectrum of frequencies over time ####

SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav")

# zoom to the frequency of the female gibbon (lower) to ommit background noise ####

SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav", min.freq = 500, 
                  max.freq = 2500)

# hard to read in grey scale, change to colour ####

SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav", min.freq = 500, 
                  max.freq = 2500, Colors = "Colors")

# if preffered, a ggplot equivelant can be produced in ggplot (easier for futhur customization) ####

v <- ggspectro(GibbonWaveFile, flim=c(0,2.5)) + # y-axis limits in kHz
  geom_tile(aes(fill=amplitude)) +
  scale_fill_gradient2(name="Amplitude\n(dB)\n", limits=c(-60,0),
                       na.value="transparent",
                       low="green", mid="yellow", high="red", midpoint = -30)

v

# displaying multiple spectograms ####

# tell R to put 2x2 spectograms ####

par(mfrow = c(2, 2))

# create the spectograms ####

SpectrogramFunction(input.dir = "FocalRecordings", min.freq = 500, max.freq = 2500,
                    
                    Colors = "Colors")
par(mfrow = c(1,1))

# next stage is to simply the audio data to allow multivariate analysis ####

FeatureDataframe <- MFCCFunction(input.dir = "FocalRecordings")
dim(FeatureDataframe)

# analyse data frame usingP PCA ####

library(vegan)
source("nes8010.R")

# keep all rows but omit first column ####

acoustics_pca <- ordi_pca(FeatureDataframe[, -1], scale=TRUE)
summary(acoustics_pca)

ordi_plot(acoustics_pca, display="sites")

# use ggplot to plot instead 

acoustics_sco <- ordi_scores(acoustics_pca, display="sites")
acoustics_sco <- mutate(acoustics_sco, group_code = FeatureDataframe$Class)

ggplot(acoustics_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point()

# load necessary package again ####

library(warbleR)

# search for the recordings ####

blackbird_songs <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:song len:5-25', download = FALSE)

blackbird_alarm <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:alarm len:5-25', download = FALSE)

# create leaflet map with pop ups for locations 

map_xc(blackbird_songs, leaflet.map = TRUE)

# create subfolders for song calls and alarm calls

dir.create(file.path("blackbird_songs"))
dir.create(file.path("blackbird_alarm"))

# download the .MP3 files into the two subfolders 

query_xc(X = blackbird_songs, path="blackbird_songs")
query_xc(X = blackbird_alarm, path="blackbird_alarm")

# load more necessary packages

library(stringr)

old_files <- list.files("blackbird_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# minor changes for alarm calls ####

old_files <- list.files("blackbird_alarm", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-alarm_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# copy files into a new subfolder ####

dir.create(file.path("blackbird_audio"))
file.copy(from=paste0("blackbird_songs/",list.files("blackbird_songs")),
          to="blackbird_audio")
file.copy(from=paste0("blackbird_alarm/",list.files("blackbird_alarm")),
          to="blackbird_audio")

# check that the conversion has run OK

mp32wav(path="blackbird_audio", dest.path="blackbird_audio")
unwanted_mp3 <- dir(path="blackbird_audio", pattern="*.mp3")
file.remove(paste0("blackbird_audio/", unwanted_mp3))

#tack a single bird ####

blackbird_wav <- readWave("blackbird_audio/Turdusmerula-song_243908.wav")
blackbird_wav

#plot full frequency diagram ####

oscillo(blackbird_wav)

oscillo(blackbird_wav, from = 0.59, to = 0.60)

#spectogram of single bird ####

SpectrogramSingle(sound.file = "blackbird_audio/Turdusmerula-song_243908.wav",
                  Colors = "Colors")
# simplify the blackbird data through feature extraction using mel-frequency cepstral coefficient ####

blackbird_mfcc <- MFCCFunction(input.dir = "blackbird_audio",
                               max.freq=7000)
dim(blackbird_mfcc)

# produce PCA of previous data ####

source("nes8010.R")
library(vegan)

blackbird_pca <- ordi_pca(blackbird_mfcc[, -1], scale=TRUE)
summary(blackbird_pca)

blackbird_pca



#ggplot PCA blackbirds ####

blackbird_sco <- ordi_scores(blackbird_pca, display="sites")
blackbird_sco <- mutate(blackbird_sco, group_code = blackbird_mfcc$Class)

ggplot(blackbird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point()
v
