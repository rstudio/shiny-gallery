gen_word <- function(input, output, session) {
  
  dt = read.csv('russian_words.csv', stringsAsFactors = FALSE)
   
  words = toupper(dt[nchar(as.character(dt[,2])) > 2,2])
  
  word_ = sample(words,1)
  
  dt_word = data.frame(letter = unlist(strsplit(word_, "")),
                       guessed = '_',
                       stringsAsFactors = FALSE)
  
  return(dt_word)
}

