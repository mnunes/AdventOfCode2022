library(tidyverse)

dados <- read.table(file = "Puzzle02/data.dat", sep = " ")

colnames(dados) <- c("opponent", "myself")

### parte 1 - https://adventofcode.com/2022/day/2

#dados <- data.frame(opponent = c("A", "B", "C"),
#                    myself = c("X", "Y", "Z"))

# preparar dados

dados <-
  dados |> 
  mutate(opponent = case_when(
    opponent == "A" ~ "rock",
    opponent == "B" ~ "paper",
    opponent == "C" ~ "scissors"
  )) |> 
  mutate(myself = case_when(
    myself == "X" ~ "rock",
    myself == "Y" ~ "paper",
    myself == "Z" ~ "scissors"
  ))

# calculo dos pontos

dados <- 
  dados |> 
  mutate(pontos = case_when(
    opponent == "rock"     & myself == "rock"     ~ 4, # 1 + 3,
    opponent == "rock"     & myself == "paper"    ~ 8, # 2 + 6,
    opponent == "rock"     & myself == "scissors" ~ 3, # 3 + 0,
    opponent == "paper"    & myself == "rock"     ~ 1, # 1 + 0,
    opponent == "paper"    & myself == "paper"    ~ 5, # 2 + 3,
    opponent == "paper"    & myself == "scissors" ~ 9, # 3 + 6,
    opponent == "scissors" & myself == "rock"     ~ 7, # 1 + 6,
    opponent == "scissors" & myself == "paper"    ~ 2, # 2 + 0,
    opponent == "scissors" & myself == "scissors" ~ 6  # 3 + 3
  )
)

sum(dados$pontos)

### parte 2 - https://adventofcode.com/2022/day/2#part2

dados <- read.table(file = "Puzzle02/data.dat", sep = " ")

colnames(dados) <- c("opponent", "myself")

#dados <- data.frame(opponent = c("A", "B", "C"),
#                    myself = c("X", "Y", "Z"))

# preparar dados

dados <-
  dados |> 
  mutate(opponent = case_when(
    opponent == "A" ~ "rock",
    opponent == "B" ~ "paper",
    opponent == "C" ~ "scissors"
  )) |> 
  mutate(myself = case_when(
    myself == "X" ~ "lose",
    myself == "Y" ~ "draw",
    myself == "Z" ~ "win"
  ))

# calculo dos pontos

dados <- 
  dados |> 
  mutate(pontos = case_when(
    opponent == "rock"     & myself == "lose" ~ 3, # 3 + 0,
    opponent == "rock"     & myself == "draw" ~ 4, # 1 + 3,
    opponent == "rock"     & myself == "win"  ~ 8, # 2 + 6,
    opponent == "paper"    & myself == "lose" ~ 1, # 1 + 0,
    opponent == "paper"    & myself == "draw" ~ 5, # 2 + 3,
    opponent == "paper"    & myself == "win"  ~ 9, # 3 + 6,
    opponent == "scissors" & myself == "lose" ~ 2, # 2 + 0,
    opponent == "scissors" & myself == "draw" ~ 6, # 3 + 3,
    opponent == "scissors" & myself == "win"  ~ 7  # 1 + 6 
  )
)

sum(dados$pontos)
