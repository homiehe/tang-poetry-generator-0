library(jiebaR)
library(tidyverse)
library(wordcloud2)

fileName <- "D:\\Master\\Work\\诗词分析\\tang_po_final.txt"
SC <- readChar(fileName, file.info(fileName)$size)
substr(SC, 1000, 1100)

cc = worker()

analysis <- as.data.frame(table(cc[SC]))
analysis <- as.tibble(analysis)
#rearrange the table
poetry<-analysis %>% arrange(desc(Freq))
poetry[-c(1:9),]
names(poetry) <- c("word","freq")
poetry$word <- as.character(poetry$word)

cipai <- "寥落古行宮，宮花寂寞紅。白頭宮女在，閒坐說玄宗。"
tagger <- worker("tag")
cipai_2 <- tagger <= cipai
cipai_2

example <- subset(poetry, freq >1 & nchar(word) <3 & freq < 300)
cixing <- attributes(cipai_2)$names
example_2 <- tagger <= example$word



write_songci <- function(m){
  set.seed(m)
  empty <- ""
  for (i in 1:length(cipai_2)){
    temp_file <- example_2[attributes(example_2)$name == cixing[i]]
    temp_file <- temp_file[nchar(temp_file) == nchar(cipai_2[i])]
    empty <- paste0(empty, sample(temp_file,1))
  }
  
  result <- paste0(substr(empty, 1,5), ",", substr(empty,5,9),"。", 
                   substr(empty, 10,14), ",", substr(empty, 15,19),"。")
  
}

data_p <- lapply(1:1000, write_songci)
                 
random_x <- sample(1:1000, size = 1)

data_p[random_x]
