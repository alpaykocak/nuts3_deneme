library(readxl)
pivot <- read_excel("/Users/necmettinalpaykocak/Documents/shiny_trials/nuts3_deneme/pivot.xls",skip = 1)
pivot <- pivot[,3:6]
colnames(pivot) <- c("tip", "il","yıl","veri")
i1 <- !is.na(unique(pivot$tip))
tip <- unique(pivot$tip)[i1]
i2 <- !is.na(unique(pivot$il))
il <- unique(pivot$il)[i2]
i3 <- !is.na(unique(pivot$yıl))
yıl <- unique(pivot$yıl)[i3]
data <- data.frame("tip" = as.factor(rep(tip, each = length(il)*length(yıl))),
           "il" = as.factor(rep(il, each = length(yıl), times = length(tip))),
           "yıl" = as.numeric(rep(yıl, times = length(tip)*length(il))),
           "veri" = as.numeric(pivot$veri)
           )
library(tidyr)
data <- separate(data,col = il,into = c("il_adi","il_kod"),sep = "-")
data$il_adi <- as.factor(data$il_adi)
data$il_kod <- as.factor(data$il_kod)
il_sinif <- read_excel("/Users/necmettinalpaykocak/Documents/shiny_trials/nuts3_deneme/il_sinif.xls")
datam <- merge(data,il_sinif,by = "il_kod",all.x = TRUE)
datam[,6:9] <- lapply(datam[,6:9], as.factor)
