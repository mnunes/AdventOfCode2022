# puzzle 04 - https://adventofcode.com/2022/day/4

library(tidyverse)

dados <- read.csv(file = "data/input.dat")

split_ <- function(x, p = 1){
  return(
    as.numeric(unlist(strsplit(x, split = "-"))[seq(p, 2*length(x), 2)])
    )
}

dados |> 
  mutate(elf_1_1 = split_(elf_1, 1),
         elf_1_2 = split_(elf_1, 2),
         elf_2_1 = split_(elf_2, 1),
         elf_2_2 = split_(elf_2, 2)) |> 
  mutate(contem = ifelse( (elf_1_1 <= elf_2_1 & elf_1_2 >= elf_2_2) | (elf_2_1 <= elf_1_1 & elf_2_2 >= elf_1_2), 1, 0) ) |> 
  summarise(n = sum(contem))


# https://adventofcode.com/2022/day/4#part2

dados |> 
  mutate(elf_1_1 = split_(elf_1, 1),
         elf_1_2 = split_(elf_1, 2),
         elf_2_1 = split_(elf_2, 1),
         elf_2_2 = split_(elf_2, 2)) |> 
  mutate(nao_contem = ifelse( (elf_1_2 < elf_2_1 ) | (elf_2_2 < elf_1_1 ), 1, 0) ) |> 
  summarise(n = nrow(dados)-sum(nao_contem))




