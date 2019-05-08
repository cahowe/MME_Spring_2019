#Colin Howe
#May 6th
#MME Final project
#Middlebury College

#requirements
require(tidyverse)
require(car)

#Read in virus data
virus <- read.csv(file.choose())
virus_save <- virus

#Read in bacteria data
bacteria <- read.csv(file.choose())
bact_save<- bacteria

#Read in symbiont data
symbiont <- read.csv(file.choose())
sym_save <- symbiont


######################################################################
######################################################################
#        Virus          #
#####################################################################
#####################################################################
#Graph of sample dist
virus %>%
  ggplot(aes(VLPml)) +
  geom_histogram(bins = 45) +
  facet_wrap(~ï..site)

#Normality of mermaid water
#Pass 0.563
virus %>%
  filter(ï..site == "Mermaidwater") %>%
  pull(VLPml) %>%
  shapiro.test()

#Normality of mermaid mucus
#A narrow pass p = 0.098
virus %>%
  filter(ï..site == "Mermaidmucus") %>%
  pull(VLPml) %>%
  shapiro.test()

#Normality of sandy water
#Bigggg pass (0.9552)
virus %>%
  filter(ï..site == "Sandywater") %>%
  pull(VLPml) %>%
  shapiro.test()

#Pass Levines test for variance
#p = 6.625 *10^-6
virus %>%
  leveneTest(VLPml ~ ï..site, data = .)

#Virus ANOVA
virus %>%
  aov(VLPml ~ ï..site, data = .) %>%
  summary()

#Virus Tukey
virus %>%
  aov(VLPml ~ ï..site, data = .) %>%
  TukeyHSD()

#Virus Graph
virus %>%
  ggplot(aes(y = VLPml, x = ï..site)) +
  geom_boxplot(colour = "dark blue", fill = "light blue") +
  scale_x_discrete(name = "Sampling Site", labels = c("Mermaid Reed (muscus)",
                                                "Mermaid Reed (water)", 
                                                "Sandy Cay (water)")) +
  labs(x = "Sampling Site", y = "VLP per ml") +
  annotate("text", x=1, y=1.7e08, label = "A") +
  annotate("text", x=2, y=5e07, label = "B") +
  annotate("text", x=3, y=5e07, label = "B")
  

######################################################################
######################################################################
#        Bacteria          #
#####################################################################
#####################################################################

#Graph of sample dist
bacteria %>%
  ggplot(aes(VLPml)) +
  geom_histogram(bins = 45) +
  facet_wrap(~ï..site)

#Normality of mermaid water
#P= 0.3283
bacteria %>%
  filter(ï..site == "Mermaidwater") %>%
  pull(VLPml) %>%
  shapiro.test()

#Normality of mermaid mucus
#p = 0.2884
bacteria %>%
  filter(ï..site == "Mermaidmucus") %>%
  pull(VLPml) %>%
  shapiro.test()

#Normality of sandy water
#p=0.9254
bacteria %>%
  filter(ï..site == "Sandywater") %>%
  pull(VLPml) %>%
  shapiro.test()

#Pass Levines test for variance
#p = 8.705*10^-7
bacteria %>%
  leveneTest(VLPml ~ ï..site, data = .)

#Bacteria Graph
bacteria %>%
  ggplot(aes(y = VLPml, x = ï..site)) +
  geom_boxplot(colour = "magenta", fill = "violet") +
  scale_x_discrete(name = "Sampling Site", labels = c("Mermaid Reed (muscus)",
                                                "Mermaid Reed (water)", 
                                                "Sandy Cay (water)")) +
  labs(x= "Sampling Site", y = "Bacterial Cells per ml") +
  annotate("text", x=1, y=9.7e06, label = "A") +
  annotate("text", x=2, y=3e06, label = "B") +
  annotate("text", x=3, y=3e06, label = "B")

#Bacteria ANOVA
bacteria %>%
  aov(VLPml ~ ï..site, data = .) %>%
  summary()

#Bacteria Tukey
bacteria %>%
  aov(VLPml ~ ï..site, data = .) %>%
  TukeyHSD()

######################################################################
######################################################################
#        Photosynthesizers        #
#####################################################################
#####################################################################

#Graph of sample dist
symbiont %>%
  ggplot(aes(VPLml)) +
  geom_histogram(bins = 45) +
  facet_wrap(~ï..site)

#Normality of mermaid water
#FAIL p =0.8.08e-7
symbiont %>%
  filter(ï..site == "Mermaidwater") %>%
  pull(VPLml) %>%
  shapiro.test()

#Normality of mermaid mucus
#pass p = 0.446
symbiont %>%
  filter(ï..site == "Mermaidmucus") %>%
  pull(VPLml) %>%
  shapiro.test()

#Normality of sandy water
#FAIL p = 1.77e-5
symbiont %>%
  filter(ï..site == "Sandywater") %>%
  pull(VPLml) %>%
  shapiro.test()

#Pass Levines test for variance
#p = 1.48e-9
symbiont %>%
  leveneTest(VLPml ~ ï..site, data = .)

#Photosynthesizer Graph
symbiont %>%
  ggplot(aes(y = VPLml, x = ï..site)) +
  geom_boxplot(colour = "orange", fill = "gold") +
  scale_x_discrete(name = "Sampling Site", labels = c(
                                                      "Mermaid Reed (mucus)",
                                                      "Mermaid Reed (water)", 
                                                      "Sandy Cay (water)")) +
  labs(x= "Sampling Site", y = "Photosynthesizing Cells per ml") +
  annotate("text", x=1, y=1.6e06, label = "A") +
  annotate("text", x=2, y=150000, label = "B") +
  annotate("text", x=3, y=150000, label = "B")
  
#Symbiont ANOVA
symbiont %>%
  aov(VPLml ~ ï..site, data = .) %>%
  summary()

#Symbiont Tukey
symbiont %>%
  aov(VPLml ~ ï..site, data = .) %>%
  TukeyHSD()

