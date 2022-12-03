# part 1 - https://adventofcode.com/2022/day/3

x1 <- "vJrwpWtwJgWrhcsFMMfFFhFp"
x2 <- "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
x3 <- "PmmdzqPrVvPwwTWBwg"
x4 <- "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
x5 <- "ttgJtRGJQctTZtZT"
x6 <- "CrZsJsPPZsGzwwsLwLmpwMDw"



rucksack <- function(x){
  n <- nchar(x)
  
  h1 <- substr(x, 1, n/2)
  h2 <- substr(x, n/2+1, n)
  
  char_h1 <- unlist(strsplit(h1, ""))
  char_h2 <- unlist(strsplit(h2, ""))
  
  for (i in char_h1){
    for (j in char_h2){
      if (i == j){
        item <- i
      }
    }
  }
  
  return(item)
}

rucksack(x1)

dados <- read.table(file = "Puzzle03/data/dados.csv", header = FALSE)

letras <- apply(dados, 1, rucksack)

letras <- factor(letras, levels = c(letters, LETTERS))

sum(as.numeric(letras))


# part 2 - https://adventofcode.com/2022/day/3#part2

x1 <- "vJrwpWtwJgWrhcsFMMfFFhFp"
x2 <- "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
x3 <- "PmmdzqPrVvPwwTWBwg"


common <- function(x1, x2, x3){
  intersect(
    intersect(unlist(strsplit(x1, "")), 
              unlist(strsplit(x2, ""))),
    unlist(strsplit(x3, ""))
  )
}

common(x1, x2, x3)

result <- 0

for (j in 1:(nrow(dados)/3)){
  result[j] <- 
    common(dados[j*3-2, ],
           dados[j*3-1, ],
           dados[j*3, ])
}





