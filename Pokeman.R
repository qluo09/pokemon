# STAT 344 Pokemon Project
# Stratified Random Sampling

# Step 1: Set random seed
set.seed(3442026)

# Step 2: Define population by generation
generations <- list(
  I    = 1:151,
  II   = 152:251,
  III  = 252:386,
  IV   = 387:493,
  V    = 494:649,
  VI   = 650:721,
  VII  = 722:809,
  VIII = 810:905,
  IX   = 906:1025
)

# Step 3: Sample sizes
n_g <- c(15, 10, 13, 10, 15, 7, 9, 9, 12)

# Step 4: Stratified SRSWOR
sample_list <- Map(
  function(g, ids, n) {
    data.frame(
      generation = g,
      pokemonID = sample(ids, n, replace = FALSE)
    )
  },
  names(generations),
  generations,
  n_g
)

# Step 5: Combine samples
pokemon_sample <- do.call(rbind, sample_list)

row.names(pokemon_sample) <- NULL

# Step 6: Check sample
print(pokemon_sample)

table(pokemon_sample$generation)

nrow(pokemon_sample)

length(unique(pokemon_sample$pokemonID))

# Step 7: Save results
write.csv(
  pokemon_sample,
  "pokemon_sample_100.csv",
  row.names = FALSE
)

#check
nrow(pokemon_sample)

length(unique(pokemon_sample$pokemonID))

table(pokemon_sample$generation)
