
library(xlsx)

df <- read.xlsx("Copy of Copy of Bloomberg Govt 2017 Future Years Defense Program (FYDP) Database.xlsx", sheetIndex = 2)


df <- df[-1:-5]

names(df) <- NULL 


a <- df[6:1063, c(2, 5, 8:11, 28:32)]

sp <- data.frame(a)

write.xlsx(sp, "Bloom(test).xlsx", sheetName = "Sheet1")