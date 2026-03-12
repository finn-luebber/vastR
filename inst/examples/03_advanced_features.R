# ============================================================================
# Example 3: Advanced Features
#
# Strengths, custom-function diamonds, noise, higher-order concepts,
# nested groups, IS/OUGHT, perspectives
# ============================================================================

library(vastR)

# ---- Part A: Custom-function diamond + noise ----

model_a <- vast_model(
  title       = "Strengths, Custom Functions, and Noise",
  naming_mode = "separated",
  rankdir     = "LR"
) |>
  add_nodes(
    vast_concept("X3", "X"),
    vast_concept("Z3", "Z"),
    vast_concept("Y3", "Y"),
    vast_diamond("fn1", "Y = 2.45X + Z"),
    vast_concept("X4", "X"),
    vast_concept("Y4", "Y"),
    vast_noise_source("noise1")
  ) |>
  add_edges(
    vast_causation("X3", "fn1"),
    vast_causation("Z3", "fn1"),
    vast_causation("fn1", "Y3"),
    vast_causation("X4", "Y4", strength = 0.3),
    vast_noise("noise1", "Y4")
  )

vast_render(model_a)


# ---- Part B: Nested higher-order concepts ----

model_b <- vast_model(
  title       = "Nested Higher-Order Concepts",
  naming_mode = "separated",
  rankdir     = "TB"
) |>
  add_nodes(
    vast_concept("genes",    "Genetic\nPredisposition"),
    vast_concept("hormones", "Hormonal\nBalance"),
    vast_concept("diet",     "Diet"),
    vast_concept("exercise", "Exercise"),
    vast_concept("stress",   "Chronic Stress"),
    vast_concept("bmi",      "BMI"),
    vast_diamond("and_bmi",  "AND")
  ) |>
  add_edges(
    vast_causation("genes",    "and_bmi"),
    vast_causation("hormones", "and_bmi"),
    vast_causation("diet",     "and_bmi"),
    vast_causation("exercise", "and_bmi"),
    vast_causation("stress",   "and_bmi"),
    vast_causation("and_bmi",  "bmi", ltail = "all_factors")
  ) |>
  add_groups(
    vast_group("bio", label = "Biological Factors",
               node_ids = c("genes", "hormones"),
               fillcolor = "#FCE4EC", bordercolor = "#C62828"),
    vast_group("env", label = "Environmental Factors",
               node_ids = c("diet", "exercise", "stress"),
               fillcolor = "#E8F5E9", bordercolor = "#2E7D32"),
    vast_nested_group("all_factors",
                      label = "Determinants of Body Weight",
                      node_ids = c("and_bmi"),
                      child_group_ids = c("bio", "env"),
                      fillcolor = "#F5F5F5", bordercolor = "gray30")
  )

vast_render(model_b)


# ---- Part C: IS / OUGHT with Perspectives ----

model_c <- vast_model(title = "IS and OUGHT with Perspectives", naming_mode = "separated", rankdir = "TB") |>
  add_nodes(
    vast_concept("INC", "INC"),
    vast_name("n_inc", "Expected increase of average\nglobal temperature in the\nnext 50 years (Kelvin)"),
    vast_is("is_inc",       value = "2"),
    vast_ought("ought_inc", value = "0"),
    vast_perspective("p_daniel", holder = "Daniel", value = "1.0"),
    vast_perspective("p_marcos", holder = "Marcos", value = "0.8")
  ) |>
  add_edges(
    vast_naming("INC", "n_inc"),
    vast_relation("INC", "is_inc",    type = "", color = "gray50"),
    vast_relation("INC", "ought_inc", type = "", color = "gray50"),
    vast_relation("p_daniel", "is_inc", type = "", color = "gray50", style = "dashed"),
    vast_relation("p_marcos", "is_inc", type = "", color = "gray50", style = "dashed")
  )

vast_render(model_c)


# ---- Part D: Compound Edges (lhead / ltail) ----
#
# Arrows can point to/from the border of a higher-order concept
# (group) instead of a specific node inside it. The `from`/`to`
# must still reference a node that lives inside the target group
# (Graphviz uses it for layout), but the arrow visually starts or
# ends at the group border.

model_d <- vast_model(
  title       = "Compound Edges: Arrows to/from Groups",
  naming_mode = "separated",
  rankdir     = "LR"
) |>
  add_nodes(
    # Biological factors (inside group "bio")
    vast_concept("genes",    "Genes"),
    vast_concept("hormones", "Hormones"),

    # Environmental factors (inside group "env")
    vast_concept("diet",     "Diet"),
    vast_concept("exercise", "Exercise"),

    # External concepts (not in any group)
    vast_concept("health",   "Health\nOutcomes"),
    vast_concept("policy",   "Public Health\nPolicy")
  ) |>
  add_groups(
    vast_group("bio", label = "Biological Factors",
               node_ids = c("genes", "hormones"),
               fillcolor = "#FCE4EC", bordercolor = "#C62828"),
    vast_group("env", label = "Environmental Factors",
               node_ids = c("diet", "exercise"),
               fillcolor = "#E8F5E9", bordercolor = "#2E7D32")
  ) |>
  add_edges(
    # Arrow FROM the bio group border TO health
    # (uses "genes" as anchor node, but arrow starts at group border)
    vast_causation("genes", "health", strength = 0.5,
                   ltail = "bio"),

    # Arrow FROM the env group border TO health
    vast_causation("diet", "health", strength = 0.4,
                   ltail = "env"),

    # Arrow FROM policy TO the env group border
    # (uses "diet" as anchor node, but arrow ends at group border)
    vast_causation("policy", "diet",
                   lhead = "env"),

    # Arrow FROM one group TO another group
    vast_prediction("genes", "diet", strength = "?",
                    ltail = "bio", lhead = "env")
  )

vast_render(model_d)

