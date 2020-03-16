lst <- list.files(path = "C:/Users/Siyu/Desktop/test_example",
           pattern = "*dashboard")

#lst <- as.list(lst)
df <- data.frame()
base_dir <-  'C:/Users/Siyu/Desktop/test_example'

for (i in lst){
  filename = i
  path <- file.path(base_dir,filename)
  print(path)
  df1 <- read_xlsx(path,sheet = "Stream")  
  df <- rbind(df,df1)
  }

half <- nrow(df)/2
df1 <- df[1:half,]
df2 <- df[half:nrow(df),]

save(df1,file = 'tweets1.RData')
save(df2,file = 'tweets2.RData')
