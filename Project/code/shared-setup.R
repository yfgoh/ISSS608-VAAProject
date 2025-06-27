pacman::p_load(jsonlite, tidyverse, ggtext,
               knitr, lubridate, patchwork,
               ggraph, tidygraph, igraph,
               ggiraph, kableExtra, plotly, fmsb)

mc1_data <- fromJSON("data/MC1_graph.json")

glimpse(mc1_data)

mc1_nodes_raw <- as_tibble(mc1_data$nodes)

mc1_edges_raw <- as_tibble(mc1_data$links)


kable(mc1_nodes_raw %>%
        filter(grepl("Sailor Shift", name))) #Sailor Shift is in name column and not in stage_name column

kable(mc1_nodes_raw %>%
        filter(grepl("Ivy Echos", name))) #Ivy Echos is also in name column and not in stage_name column

mc1_nodes_raw <- mc1_nodes_raw %>%
  mutate(
    is_sailor = (
      str_detect(name, regex("sailor shift", ignore_case = TRUE))
    ) %>% replace_na(FALSE),
    is_ivy = (
      str_detect(name, regex("ivy echos", ignore_case = TRUE))
    ) %>% replace_na(FALSE),
    is_oceanus_folk = str_detect(genre, regex("oceanus folk", ignore_case = TRUE)) %>% #na/not oceanus folk = false
      replace_na(FALSE)
  )

kable(head(mc1_nodes_raw,5))

mc1_nodes_raw <- mc1_nodes_raw %>%
  mutate(across(c(release_date, notoriety_date, written_date),
                ~as.integer(if_else(`Node Type` %in% c("Song", "Album"), ., NA_character_))))

mc1_nodes_raw %>%
  count(id) %>%
  filter(n > 1)


duplicated_name <- mc1_nodes_raw %>%
  count(name) %>%
  filter(n > 1)

kable(head(duplicated_name,5))

cat("Total number of duplicated name:", sum(duplicated_name$n), "\n")

mc1_nodes_raw %>%
  filter(name %in% duplicated_name$name) %>%
  arrange(name) %>%
  kable() %>%
  kable_styling("striped", full_width = F) %>% 
  scroll_box(height = "200px")


mc1_nodes_tagged <- mc1_nodes_raw %>%
  mutate(group_key = paste(`Node Type`, name, single, release_date, genre,
                           notable, written_date, notoriety_date, is_sailor,
                           is_oceanus_folk, sep = "|"))

kable(head(mc1_nodes_tagged,5))


# Step 2: De-duplicate and keep the preferred (with stage_name if available)
mc1_nodes_dedup <- mc1_nodes_tagged %>%
  group_by(group_key) %>%
  arrange(desc(!is.na(stage_name))) %>%
  slice(1) %>%
  ungroup()

duplicated_name <- mc1_nodes_dedup %>%
  count(name) %>%
  filter(n > 1)

mc1_nodes_dedup %>%
  filter(name %in% duplicated_name$name) %>%
  arrange(name) %>%
  kable() %>%
  kable_styling("striped", full_width = F) %>% 
  scroll_box(height = "200px")


###  2.5.2 Duplicates in Edges


# Step 1: Identify duplicate combinations
duplicate_summary <- mc1_edges_raw %>%
  count(source, target, `Edge Type`) %>%
  filter(n > 1)

# Step 2: Join back to get all original duplicate rows
mc1_edges_raw %>%
  inner_join(duplicate_summary, by = c("source", "target", "Edge Type")) %>%
  kable()


mc1_edges_raw <- mc1_edges_raw %>%
  distinct(source, target, `Edge Type`, .keep_all = TRUE) %>%
  select(!key)

mc1_edges_raw %>%
  count(source, target, `Edge Type`) %>%
  filter(n > 1)


range(mc1_nodes_dedup$id)

range(mc1_edges_raw$source)

range(mc1_edges_raw$target)


mc1_nodes_clean <- mc1_nodes_dedup %>%
  rename(node_name = name, name = id) %>%
  mutate(name = as.character(name)) %>%
  select(`Node Type`, node_name, release_date, genre, notable, name, single, written_date, stage_name, notoriety_date, is_sailor, is_ivy, is_oceanus_folk)

kable(head(mc1_nodes_clean,5))


###  2.7.1 Create Edge Mapping from original id to kept id


# Step 1: Create mapping of all group_key → kept id
key_to_id_map <- mc1_nodes_dedup %>%
  select(group_key, kept_id = id)

# Step 2: Map all original rows to the retained ID
id_remap <- mc1_nodes_tagged %>%
  left_join(key_to_id_map, by = "group_key") %>%
  select(original_id = id, kept_id)

kable(head(id_remap,5))



# Step 3: Replace edges' source and target with mapped kept_id
mc1_edges_mapped <- mc1_edges_raw %>%
  left_join(id_remap, by = c("source" = "original_id"))

kable(head(mc1_edges_mapped,5))


mc1_edges_mapped <- mc1_edges_mapped %>%
  mutate(source = kept_id) %>%
  select(-kept_id) %>%
  left_join(id_remap, by = c("target" = "original_id")) %>%
  mutate(target = kept_id) %>%
  select(-kept_id) %>%
  rename(from = source, to = target) %>%
  mutate(from = as.character(from), to = as.character(to))

kable(head(mc1_edges_mapped,5))



mc1_edges_clean <- mc1_edges_mapped %>%
  filter(!is.na(from), !is.na(to))


setdiff(
  unique(c(mc1_edges_clean$from, mc1_edges_clean$to)),
  mc1_nodes_clean$name
)


# Define valid source and destination types for each edge type
edge_rules <- list(
  PerformerOf = list(source = c("Person", "MusicalGroup"), target = c("Song", "Album")),
  ComposerOf = list(source = c("Person"), target = c("Song", "Album")),
  ProducerOf = list(source = c("Person", "RecordLabel"), target = c("Song", "Album", "Person", "MusicalGroup")),
  LyricistOf = list(source = c("Person"), target = c("Song", "Album")),
  RecordedBy = list(source = c("Song", "Album"), target = c("RecordLabel")),
  DistributedBy = list(source = c("Song", "Album"), target = c("RecordLabel")),
  InStyleOf = list(source = c("Song", "Album"), target = c("Song", "Album", "Person", "MusicalGroup")),
  InterpolatesFrom = list(source = c("Song", "Album"), target = c("Song", "Album")),
  CoverOf = list(source = c("Song", "Album"), target = c("Song", "Album")),
  LyricalReferenceTo = list(source = c("Song", "Album"), target = c("Song", "Album")),
  DirectlySamples = list(source = c("Song", "Album"), target = c("Song", "Album")),
  MemberOf = list(source = c("Person"), target = c("MusicalGroup"))
)



# Create a lookup for node types
node_type_lookup <- mc1_nodes_clean %>%
  select(name, `Node Type`) %>%
  deframe()

# Add source and target node types to the edge table
mc1_edges_checked <- mc1_edges_clean %>%
  mutate(
    source_type = node_type_lookup[from],
    target_type = node_type_lookup[to]
  )

mc1_edges_tagged <- mc1_edges_checked %>%
  rowwise() %>%
  mutate(
    valid = {
      rule <- edge_rules[[`Edge Type`]]
      if (is.null(rule)) TRUE
      else {
        source_type %in% rule$source && target_type %in% rule$target
      }
    }
  ) %>%
  ungroup()

# Count and display invalid edge combinations
invalid_edge_summary <- mc1_edges_tagged %>%
  filter(!valid) %>%
  count(`Edge Type`, source_type, target_type, sort = TRUE)

kable(head(invalid_edge_summary,5))

# Check total invalid edge count
cat("Total invalid edges:", sum(!mc1_edges_tagged$valid), "\n")





# Keep only valid edges
mc1_edges_clean <- mc1_edges_tagged %>%
  filter(valid) 

cat("Total invalid edges:", sum(!mc1_edges_clean$valid), "\n")

mc1_edges_clean <- mc1_edges_clean %>%
  select(from, to, `Edge Type`)  # drop helper columns


##  2.11 Temporal Validation


# Define which edge types represent influence relationships between songs/albums
influence_edge_types <- c("InStyleOf", "InterpolatesFrom", "CoverOf", 
                         "LyricalReferenceTo", "DirectlySamples")

# Check for temporal inconsistencies in song/album influence relationships
temporal_check <- mc1_edges_clean %>%
  # Focus only on influence edge types
  filter(`Edge Type` %in% influence_edge_types) %>%
  # Add source node info (the influenced song/album)
  left_join(mc1_nodes_clean %>% 
            select(name, source_type = `Node Type`, source_release = release_date), 
            by = c("from" = "name")) %>%
  # Add target node info (the influencer song/album)
  left_join(mc1_nodes_clean %>% 
            select(name, target_type = `Node Type`, target_release = release_date), 
            by = c("to" = "name")) %>%
  # Only check Song/Album -> Song/Album relationships
  filter(source_type %in% c("Song", "Album"),
         target_type %in% c("Song", "Album")) %>%
  # Check for temporal violations
  mutate(
    temporal_violation = case_when(
      is.na(source_release) | is.na(target_release) ~ FALSE,  # Skip if dates missing
      source_release < target_release ~ TRUE,  # Influenced before influencer
      TRUE ~ FALSE
    )
  )

# Summary of temporal violations
violation_summary <- temporal_check %>%
  filter(temporal_violation) %>%
  count(`Edge Type`, sort = TRUE)

kable(head(violation_summary))



# Show examples of violations with node names
violation_examples <- temporal_check %>%
  filter(temporal_violation) %>%
  left_join(mc1_nodes_clean %>% select(name, source_name = node_name), 
            by = c("from" = "name")) %>%
  left_join(mc1_nodes_clean %>% select(name, target_name = node_name), 
            by = c("to" = "name")) %>%
  select(source_name, target_name, `Edge Type`, source_release, target_release) %>%
  arrange(source_release)
  

kable(head(violation_examples,5))



total_violations <- sum(temporal_check$temporal_violation, na.rm = TRUE)
total_checked <- nrow(temporal_check)
cat("\nTotal song/album influence relationships checked:", total_checked, "\n")
cat("Total temporal violations found:", total_violations, "\n")


#### 2.11.1 Removing all edges that have temporal issues


# Get all edges that are not influence relationships (keep as-is)
non_influence_edges <- mc1_edges_clean %>%
  filter(!(`Edge Type` %in% influence_edge_types))

# Get temporally consistent influence edges
consistent_influence_edges <- temporal_check %>%
  filter(!temporal_violation) %>%
  select(from, to, `Edge Type`)

# Combine all valid edges
mc1_edges_clean <- bind_rows(non_influence_edges, consistent_influence_edges)




total_cleaned_influences <- consistent_influence_edges %>% nrow()
cat("Total song/album influence relationships after cleaning:", total_cleaned_influences, "\n")


##  2.12 Check for orphan nodes


# Get all node IDs that appear in edges (either as source or target)
nodes_with_edges <- unique(c(mc1_edges_clean$from, mc1_edges_clean$to))

# Find nodes that don't appear in any edges
orphan_nodes <- mc1_nodes_clean %>%
  filter(!name %in% nodes_with_edges)

# Summary of orphan nodes
cat("Total nodes:", nrow(mc1_nodes_clean), "\n")
cat("Nodes with edges:", length(nodes_with_edges), "\n")
cat("Orphan nodes (no edges):", nrow(orphan_nodes), "\n")


##  2.13 Creating Consistent Visualisation Colouring



mc1_edges_clean <- mc1_edges_clean %>%
  mutate(`Edge Colour` = case_when(
    `Edge Type` %in% c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf", "RecordedBy", "DistributedBy") ~ "Creator Of",
    `Edge Type` %in% c("InStyleOf", "InterpolatesFrom", "CoverOf", "LyricalReferenceTo", "DirectlySamples") ~ "Influenced By",
    `Edge Type` == "MemberOf" ~ "Member Of",
    TRUE ~ "Other"
  ))

mc1_nodes_clean <- mc1_nodes_clean %>%
  mutate(
    `Node Colour` = case_when(
      `Node Type` %in% c("Person", "MusicalGroup", "RecordLabel") ~ "Musician",
      genre == "Oceanus Folk" ~ "Oceanus Folk",
      TRUE ~ "Other Genre"
    )
  )


##  2.14 Creating the Global Knowledge Graph





graph = tbl_graph(edges = mc1_edges_clean,
                             nodes = mc1_nodes_clean,
                             directed = TRUE)



set.seed(1234)
