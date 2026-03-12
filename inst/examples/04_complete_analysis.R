# ============================================================================
# Example 4: Complete VAST Analysis
#
# Argument: Does social media use cause depression in adolescents?
# Uses all major features together.
# ============================================================================

library(vastR)

model <- vast_model(
  title   = "Does Social Media Use Cause Depression in Adolescents?",
  naming_mode = "separated",
  rankdir = "TB",
  nodesep = 0.7,
  ranksep = 1.0,
  fontsize_edges = 12
) |>
  add_nodes(
    # Core concepts
    vast_concept("SMU",  "SMU"),
    vast_name("n_smu",   "Social Media\nUse (hours/day)"),
    vast_concept("DEP",  "DEP"),
    vast_name("n_dep",   "Depressive\nSymptoms"),
    vast_concept("SC",   "SC"),
    vast_name("n_sc",    "Social\nComparison"),
    vast_concept("SLEEP","SLP"),
    vast_name("n_sleep", "Sleep\nQuality"),
    vast_concept("LONE", "LONE"),
    vast_name("n_lone",  "Perceived\nLoneliness"),

    # Data
    vast_data("D1", "Cross-sectional\nSurvey (N=5000)"),
    vast_data("D2", "Longitudinal\nStudy (N=800)"),

    # Connective
    vast_diamond("med_or", "OR"),

    # Noise
    vast_noise_source("noise_dep"),

    # IS/OUGHT
    vast_is("is_smu_dep",   value = "?"),
    vast_ought("ought_smu", value = "low"),

    # Perspectives
    vast_perspective("p_res",  holder = "Researcher A", value = "0.7"),
    vast_perspective("p_crit", holder = "Critic B",     value = "0.3")
  ) |>
  add_edges(
    # Naming
    vast_naming("SMU",   "n_smu"),
    vast_naming("DEP",   "n_dep"),
    vast_naming("SC",    "n_sc"),
    vast_naming("SLEEP", "n_sleep"),
    vast_naming("LONE",  "n_lone"),

    # Causal pathways
    vast_causation("SMU",   "SC",    strength = "0.4"),
    vast_causation("SMU",   "SLEEP", strength = "-0.3"),
    vast_causation("SMU",   "LONE",  strength = "?"),
    vast_causation("SC",    "med_or"),
    vast_causation("SLEEP", "med_or"),
    vast_causation("LONE",  "med_or"),
    vast_causation("med_or","DEP"),
    vast_noise("noise_dep", "DEP"),

    # Prediction from data
    vast_prediction("D1", "SMU", strength = "0.35"),
    vast_prediction("D2", "SMU", strength = "0.20"),

    # Reasoning
    vast_reasoning("D1", "is_smu_dep", strength = "weak"),
    vast_reasoning("D2", "is_smu_dep", strength = "moderate"),

    # Structural
    vast_relation("SMU", "is_smu_dep", type = "", color = "gray60"),
    vast_relation("SMU", "ought_smu",  type = "", color = "gray60"),
    vast_relation("p_res",  "is_smu_dep", type = "", color = "gray50",
                  style = "dashed"),
    vast_relation("p_crit", "is_smu_dep", type = "", color = "gray50",
                  style = "dashed")
  ) |>
  add_groups(
    vast_group("mechanisms", label = "Proposed Mechanisms",
               node_ids = c("SC", "n_sc", "SLEEP", "n_sleep",
                            "LONE", "n_lone", "med_or"),
               fillcolor = "#FFF8E1", bordercolor = "#F57F17",
               penwidth = 2.5),
    vast_group("evidence", label = "Empirical Evidence",
               node_ids = c("D1", "D2"),
               fillcolor = "#E8F5E9", bordercolor = "#2E7D32",
               penwidth = 2.5)
  )

print(model)
vast_render(model)
