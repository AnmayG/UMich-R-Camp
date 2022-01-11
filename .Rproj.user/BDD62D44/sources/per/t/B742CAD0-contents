library(tidyverse)
library(readr)
library(skimr)
data <- read_tsv("C:/rcamp/r-camp-2021/tcga-bh-a18r/primary-tumor/b8a56619-9b99-4045-b747-b1e595c6f506.mirbase21.isoforms.quantification.txt")

clean_data <- data %>%
                mutate(isoform_coords = as.factor(isoform_coords)) %>%
                mutate(id = row_number()) %>%
                select("isoform_coords", "reads_per_million_miRNA_mapped", "id")

label_data <- clean_data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5)/number_of_bar
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

clean_data %>%
  ggplot(aes(x = "isoform_coords", y = "reads_per_million_miRNA_mapped")) +
  geom_segment(xend = "isoform_coords", yend=0) +
  coord_polar(start = 0) +
  geom_text(data=label_data,
            aes(x=id, y=reads_per_million_miRNA_mapped+10, 
                label=isoform_coords, 
                hjust=hjust),
            color="black", 
            fontface="bold",
            alpha=0.6, 
            size=2.5, 
            angle=label_data$angle, 
            inherit.aes=FALSE) 


view(read_tsv("small_data.txt"))
