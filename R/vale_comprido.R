
blades <- datasetlp %>% 
  filter(BlankType == "ElongatedProd" & MedWidth >= 12 & Phase == "Proto-Solutrean") %>% 
  select(Length, MedWidth, Thickness, BlankShape, PlatformThickness, PlatformWidth, OtherNotes, RawMaterial) %>% 
  mutate(Elongation = Length/MedWidth, Flattening = MedWidth/Thickness, PlatFlattening = PlatformWidth/PlatformThickness)

measur <- blades %>% 
  summarise(Elongmean = mean(Elongation), ElongSD = sd(Elongation),
            Flattmean = mean(Flattening), FlattSD = sd(Flattening),
            PlatFlatmean = mean(PlatFlattening), PlatFlatSD = sd(PlatFlattening),
            n = n())

shape <- blades %>% 
  group_by(BlankShape) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(freq = n/sum(n)*100)


VC <- dataset %>% 
  filter(OtherNotes == "TVC?") %>% 
  select(Length, MedWidth, Thickness, BlankShape, PlatformThickness, PlatformWidth, OtherNotes) %>% 
  mutate(Elongation = Length/MedWidth, Flattening = MedWidth/Thickness, PlatFlattening = PlatformWidth/PlatformThickness)


VCmeasur <- VC %>% 
  summarise(Elongmean = mean(Elongation), ElongSD = sd(Elongation),
            Flattmean = mean(Flattening), FlattSD = sd(Flattening),
            PlatFlatmean = mean(PlatFlattening), PlatFlatSD = sd(PlatFlattening),
            n = n())

VCshape <- VC %>% 
  group_by(BlankShape) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(freq = n/sum(n)*100)



blades_meas <- read_csv("index/data/blade_VC_measurements.csv")

elong <- ggplot(blades_meas, aes(x=Assemblage, y=ElongMean, fill=Class)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=ElongMean-ElongSD, ymax=ElongMean+ElongSD), width=.2,
                position=position_dodge(.9))+
  ggpubr::color_palette("jco")+
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))


flat <- ggplot(blades_meas, aes(x=Assemblage, y=FlatMean, fill=Class)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=FlatMean-FlatSD, ymax=FlatMean+FlatSD), width=.2,
                position=position_dodge(.9))+
  ggpubr::color_palette("jco")+
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))


plat_flat <- blades_meas %>% 
  filter(!Assemblage %in% c("Casal do Cepo", "Vale Almoinha"))

plat_flat <- ggplot(plat_flat, aes(x=Assemblage, y=PflatMean, fill=Class)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=PflatMean-PflatSD, ymax=PflatMean+PflatSD), width=.2,
                position=position_dodge(.9))+
  ggpubr::color_palette("jco")+
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

converg <- blades_meas %>% 
  filter(Class != "Points a face plan")

ggplot(converg, aes(Assemblage, ConvergentEdges)) +
  geom_linerange(
    aes(x = Assemblage, ymin = 0, ymax = ConvergentEdges), 
    color = "lightgray", size = 4
  ) +
  geom_point(aes(color = Class), size = 4)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggpubr::color_palette("jco")+
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))


