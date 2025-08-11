###############################################################################
# 0) Libraries ----------------------------------------------------------------
###############################################################################
pkgs <- c("jsonlite", "purrr", "dplyr", "tidyr", "stringr", "tibble", "readr")
install <- setdiff(pkgs, rownames(installed.packages()))
if (length(install)) install.packages(install)
invisible(lapply(pkgs, library, character.only = TRUE))
base <- "C:/Users/Julius/Desktop/OPAL/Data integration/Data from jupyter"
org_file <- file.path(base, "pure_organisations.json")
ro_file  <- file.path(base, "pure_research_output.json")

###############################################################################
# 2) Organisations (uuid → Name) ----------------------------------------------
###############################################################################
org_tbl <- fromJSON(org_file, simplifyVector = FALSE)$items |>
  map_dfr(\(o) {
    vals <- map_chr(o$name$text, "value")
    locs <- map_chr(o$name$text, "locale")
    name <- vals[match("en_GB", locs)] %||% vals[1]
    tibble(inst_id = o$uuid, inst_label = name)
  })

###############################################################################
# 3) Research Output  ---------------------------------------------------------
###############################################################################
ro_items <- fromJSON(ro_file, simplifyVector = FALSE)$items

author_long <- map_dfr(ro_items, function(item) {
  
  if (is.null(item$contributors) || length(item$contributors) == 0)
    return(NULL)
  
  # UUID‑Liste der Org‑Einheiten dieser Publikation
  orgs_pub <- if (length(item$organizations))
    map_chr(item$organizations, "uuid", .default = NA_character_)
  else NA_character_
  
  # Autor‑Infos
  map_dfr(item$contributors, function(c) {
    tibble(
      pub_id      = item$uuid,
      author_id   = c$person$uuid %||% NA_character_,
      first_name  = c$name$firstName %||% NA_character_,
      last_name   = c$name$lastName  %||% NA_character_,
      inst_ids    = list(orgs_pub)  # List‑Spalte (evtl. mehrere)
    )
  })
})

###############################################################################
# 4) Author Nodes -------------------------------------------------------------
###############################################################################
author_nodes <- author_long |>
  group_by(author_id, first_name, last_name) |>
  summarise(
    publications = n_distinct(pub_id),
    inst_ids     = unique(unlist(inst_ids)),
    .groups      = "drop"
  ) |>
  mutate(
    label      = paste(first_name, last_name),
    institutes = map_chr(inst_ids, \(v)
                         str_c(org_tbl$inst_label[match(v, org_tbl$inst_id)],
                               collapse = "; "))
  ) |>
  select(author_id, label, first_name, last_name,
         institutes, publications)

###############################################################################
# 5) Author Edges -------------------------------------------------------------
###############################################################################
author_edges <- author_long |>
  distinct(pub_id, author_id) |>
  group_by(pub_id) |>
  filter(n() >= 2) |>
  summarise(pairs = list(combn(author_id, 2, simplify = FALSE)),
            .groups = "drop") |>
  unnest(pairs) |>
  mutate(source = map_chr(pairs, 1),
         target = map_chr(pairs, 2)) |>
  count(source, target, name = "weight")

###############################################################################
# 6) Institute Edges ----------------------------------------------------------
###############################################################################
inst_edges <- author_long |>
  select(pub_id, inst_ids) |>
  unnest(inst_ids) |>
  distinct() |>
  group_by(pub_id) |>
  filter(n() >= 2) |>
  summarise(pairs = list(combn(inst_ids, 2, simplify = FALSE)),
            .groups = "drop") |>
  unnest(pairs) |>
  mutate(source = map_chr(pairs, 1),
         target = map_chr(pairs, 2)) |>
  count(source, target, name = "weight") |>
  filter(source != target)

###############################################################################
# 7) Institute‑Node
###############################################################################
inst_nodes <- org_tbl |>
  semi_join(inst_edges, by = c("inst_id" = "source"))  # jetzt korrekt


readr::write_csv2(author_nodes, file.path(base, "author_nodes.csv"))
readr::write_csv2(author_edges, file.path(base, "author_edges.csv"))

readr::write_csv2(inst_nodes,   file.path(base, "institute_nodes.csv"))
readr::write_csv2(inst_edges,   file.path(base, "institute_edges.csv"))

