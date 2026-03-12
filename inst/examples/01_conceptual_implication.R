# ============================================================================
# Example 1: Conceptual Implications (cf. Figure 2, Leising et al. 2023)
#
# Demonstrates the three naming modes using the SAME model definition.
# The only thing that changes is naming_mode on the vast_model().
# ============================================================================

library(vastR)

# Define the model content once: concepts, names, and their relationships.
# This is the canonical way to work with vastR — you always define both
# concepts (with abstract labels) and name nodes, then let the naming_mode
# control how they are displayed.

build_sun_model <- function(naming_mode) {
  vast_model(
    title       = paste0("Conceptual Implications (", naming_mode, ")"),
    naming_mode = naming_mode
  ) |>
    add_nodes(
      vast_concept("S", "S"),
      vast_concept("H", "H"),
      vast_concept("B", "B"),
      vast_name("n_sun",    "Sun"),
      vast_name("n_hot",    "Hot"),
      vast_name("n_bright", "Bright")
    ) |>
    add_edges(
      # Conceptual implications
      vast_implication("S", "H"),
      vast_implication("S", "B"),
      # Naming relationships
      vast_naming("S", "n_sun"),
      vast_naming("H", "n_hot"),
      vast_naming("B", "n_bright")
    )
}


# --- Integrated mode (default) ---
# Concepts and names shown together in the diagram, connected by n arrows.
model_integrated <- build_sun_model("separated")
vast_render(model_integrated)

# --- Separated mode ---
# Main diagram shows only concepts with abstract labels (S, H, B).
# A separate legend box shows the concept-to-name mappings.
model_separated <- build_sun_model("separated")
vast_render(model_separated)

# --- FIMM mode (Finger-is-Moon-Mode) ---
# Concept nodes display the name label directly (Sun, Hot, Bright).
# Name nodes and naming edges are hidden.
model_fimm <- build_sun_model("fimm")
vast_render(model_fimm)
