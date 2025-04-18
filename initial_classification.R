data(Zoo, package="mlbench")

head(Zoo)

library(tidyverse)
Zoo <- as_tibble(Zoo, rownames = "animal")
Zoo

Zoo <- Zoo |>
  mutate(across(where(is.logical), 
                function (x) factor(x, levels = c(TRUE, FALSE)))) |>
  mutate(across(where(is.character), factor))

summary(Zoo)

library(rpart)

Zoo <- Zoo |> select(-animal)

tree_default <- Zoo |> 
  rpart(type ~ ., data = _)
tree_default

