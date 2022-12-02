# https://adventofcode.com/2022/day/1

library(tidyverse)

puzzle01 <- read.table(file = "data/puzzle01.csv", 
                       header = FALSE, 
                       blank.lines.skip = FALSE)

colnames(puzzle01) <- "calories"

puzzle01 <- 
  puzzle01 |> 
  mutate(calories2 = replace_na(calories, 0), 
         elf = 1)

k <- 1

for (j in 1:length(puzzle01$calories2)) {
  if(puzzle01$calories2[j] > 0){
    puzzle01$elf[j] = k
  } else {
    puzzle01$elf[j] = k
    k <- k+1
  }
}

puzzle01 |> 
  na.omit() |> 
  group_by(elf) |> 
  summarise(total = sum(calories)) |> 
  arrange(desc(total)) |> 
  head(1)

puzzle01 |> 
  na.omit() |> 
  group_by(elf) |> 
  summarise(total = sum(calories)) |> 
  arrange(desc(total)) |> 
  head(3) |> 
  summarise(sum(total))
