# ============================================================================
# Example 2: Relationship Types (cf. Figure 3, Leising et al. 2023)
#
# Shows all four additional relationship types using FIMM naming mode.
# Name nodes are defined but hidden automatically by naming_mode = "fimm".
# ============================================================================

library(vastR)

model <- vast_model(
  title       = "Relationship Types",
  naming_mode = "separated",
  rankdir     = "LR",
  nodesep     = 1.0,
  ranksep     = 1.5
) |>
  add_nodes(
    # Concepts (abstract labels)
    vast_concept("C_smk",  "C_smk"),
    vast_concept("C_lc",   "C_lc"),
    vast_concept("C_tic",  "C_tic"),
    vast_concept("C_tif",  "C_tif"),
    vast_concept("C_ht",   "C_ht"),
    vast_concept("C_ych",  "C_ych"),
    vast_concept("C_eq1",  "C_eq1"),
    vast_concept("C_eq2",  "C_eq2"),
    # Names (displayed in FIMM mode on the concept nodes)
    vast_name("n_smk",  "Smoking"),
    vast_name("n_lc",   "Lung Cancer"),
    vast_name("n_tic",  "Temperature\nin Celsius"),
    vast_name("n_tif",  "Temperature\nin Fahrenheit"),
    vast_name("n_ht",   "Height"),
    vast_name("n_ych",  "Number of Y\nChromosomes"),
    vast_name("n_eq1",  "X + 4 = 8"),
    vast_name("n_eq2",  "X = 4")
  ) |>
  add_edges(
    # Naming
    vast_naming("C_smk", "n_smk"),
    vast_naming("C_lc",  "n_lc"),
    vast_naming("C_tic", "n_tic"),
    vast_naming("C_tif", "n_tif"),
    vast_naming("C_ht",  "n_ht"),
    vast_naming("C_ych", "n_ych"),
    vast_naming("C_eq1", "n_eq1"),
    vast_naming("C_eq2", "n_eq2"),
    # Relationship types
    vast_causation("C_smk",      "C_lc"),
    vast_transformation("C_tic", "C_tif"),
    vast_prediction("C_ht",      "C_ych"),
    vast_reasoning("C_eq1",      "C_eq2")
  )

vast_render(model)
