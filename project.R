vote_data <- read_csv("Documents/election data (copy2).csv", 
                      +     col_names = FALSE)
PVI <- read_csv("Documents/PVI.csv", col_names = FALSE)
CPI_inflation <- read_csv("Documents/CPI_inflation.csv")
census2 <- read_csv("Documents/census2.csv")

absvote = final.filler(0,-1)
relvote = final.filler(1,-1)

state_raws = final.filler(0,3)
kent_raws = final.filler(0,1)
ncc_raws = final.filler(0,0)
sussex_raws = final.filler(0,2)

pop = census.filler(1)
moved = census.filler(2)
degree = census.filler(3)
finance = census.filler(4)
income = census.filler(5)

state_census = data.table("pop" = pop$state, "finance"=finance$state, "moved" = moved$state, degree = degree$state, "income" = income$state)
sussex_census = data.table("pop" = pop$sussex, "finance"=finance$sussex, "moved" = moved$sussex, degree = degree$sussex, "income" = income$sussex)
kent_census = data.table("pop" = pop$kent, "finance"=finance$kent, "moved" = moved$kent, degree = degree$kent, "income" = income$kent)
ncc_census = data.table("pop" = pop$ncc, "finance"=finance$ncc, "moved" = moved$ncc, degree = degree$ncc, "income" = income$ncc)

state_vote = data.table("abs_mean" = absvote$state_mean, "abs_min" = absvote$state_min, 
                        "abs_med" = absvote$state_med, "rel_mean" = relvote$state_mean, "rel_min"= relvote$state_min, "rel_med" = relvote$state_med)
sussex_vote = data.table("abs_mean" = absvote$sussex_mean, "abs_min" = absvote$sussex_min, 
                         "abs_med" = absvote$sussex_med, "rel_mean" = relvote$sussex_mean, "rel_min"= relvote$sussex_min, "rel_med" = relvote$sussex_med)
kent_vote = data.table("abs_mean" = absvote$kent_mean, "abs_min" = absvote$kent_min, 
                       "abs_med" = absvote$kent_med, "rel_mean" = relvote$kent_mean, "rel_min"= relvote$kent_min, "rel_med" = relvote$kent_med)
ncc_vote = data.table("abs_mean" = absvote$ncc_mean, "abs_min" = absvote$ncc_min, 
                      "abs_med" = absvote$ncc_med, "rel_mean" = relvote$ncc_mean, "rel_min"= relvote$ncc_min, "rel_med" = relvote$ncc_med)
print(ncc_vote)

