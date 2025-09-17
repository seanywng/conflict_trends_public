acled_filtered |> 
  group_by(event_type) |> 
  summarise(fatalities = sum(fatalities, na.rm = TRUE)) |> 
  arrange(desc(fatalities)) |> 
  mutate(fatalities = format(fatalities, big.mark = ",")) |> 
  kable(caption = "Fatalities by event type, ACLED") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                font_size = 12, full_width = FALSE) 

acled_actors |> 
  group_by(country, year, actor_description) |> 
  summarise(num_actors = n_distinct(actor), .groups = "drop") |> 
  filter(!is.na(actor_description)) |> 
  mutate(country = fct_relevel(country, acled_actors_order)) |> 
  ggplot(aes(x = year, y = num_actors, group = actor_description)) + 
  geom_col(aes(fill = actor_description)) + 
  facet_wrap(~country, scales = "free_y") + 
  scale_fill_viridis_d(option = "turbo") + 
  scale_x_continuous(breaks = c(2014, 2016, 2018, 2020, 2022)) + 
  scale_y_continuous(labels = number_format(scale = 1)) + 
  theme(axis.text.x = element_text(hjust = 1, angle = 45), 
        legend.position = "top", 
        legend.text = element_text(size = 5), 
        legend.key.width = unit(.3, "cm"), 
        legend.key.height = unit(.3, "cm"), 
        legend.margin=margin(t = 0, unit='cm')) + 
  labs(title = "ACLED: Breakdown of conflict actor types in the Asia-Pacific, 2014-2023", 
       x = "", y = "Number of actors", 
       fill = "") + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme(
    axis.text.y = element_text(size = 5)
  )

tracker_words <- tracker |> 
  filter(start_date >= "2014-01-01") |> 
  select(id, incident_description) |> 
  unnest_tokens(word, incident_description) |> 
  anti_join(stop_words, by = "word") |>
  add_count(word) |> 
  arrange(desc(n)) |>
  left_join(
    tracker |> 
      select(id, province, district, llg,
             status, total_dead, total_injured, 
             conflict_name),
    by = "id")

ged_png |> 
  filter(year >= 1995) |> 
  group_by(year) |>
  summarise(events = n()) |> 
  ggplot(aes(y = events, x = year)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1996, 2024, 2))
scale_x_date(d
             ate_breaks = "2 years", 
             date_label = "%Y")

lashio_captured<- tribble(~x, ~y, ~year,
                          97.7525, 22.9665, 2024, 
                          92.6, 28, 2024
)

lashio_captured_text <- tribble(~x, ~y, ~year, ~label,
                                92.9, 28, 2024, "Captured")
# Lashio captured
geom_point(aes(x = x, y = y), size = 2.5, data = lashio_captured,
           colour = "mediumseagreen", pch = 15) +
  geom_text(aes(x = x, y = y), 
            label = "Captured",
            size = 2, data = lashio_captured_text, 
            face = "bold", 
            colour = "seashell")
  
  
  protest_country_words |>
  # filter(covid == "post-covid") |> 
  distinct(event_id_cnty, word, covid) |> 
  add_count(word) |> 
  pivot_wider(names_from = covid, values_from = n, values_fill = 0) |> 
  janitor::clean_names() |> 
  mutate(n = post_covid + pre_covid) |> 
  arrange(desc(n))

  acled_actors_indonesia |>
    mutate(covid = ifelse(event_date >= "2020-03-11", "Post-covid", "Pre-covid"), 
           covid = fct_relevel(covid, "Pre-covid", "Post-covid")) |>
    group_by(actor_description, admin1, admin1_label, covid) |> 
    summarise(events = n_distinct(event_id_cnty), 
              fatalities = sum(fatalities, na.rm = TRUE), 
              .groups = "drop") |> 
    mutate(actor_description = fct_relevel(actor_description, 
                                           c("Civilians", 
                                             "State Forces",  
                                             "Protesters",
                                             "Identity Militias", 
                                             "Rebel Groups", 
                                             "Rioters", 
                                             "Political Militias", 
                                             "Other Forces"
                                           ))) |> 
    left_join(
      indo_adm1_pop |>  
        mutate(
          adm1_en = str_trim(adm1_en), 
          adm1_en = case_when(
            str_detect(adm1_en, "Jakarta") ~ "Jakarta", 
            str_detect(adm1_en, "Yogyakarta") ~ "Yogyakarta",
            TRUE ~ adm1_en)),
      by = c("admin1" = "adm1_en")
    ) |> 
    mutate(
      events_100k = events / population * 100000, 
      fatalities_100k = fatalities / population * 100000, 
      covid = fct_relevel(covid, c("Pre-covid", "Post-covid"))
    ) |>
    ggplot(aes(x = events_100k + 0.0001, y = fatalities_100k + 0.0001)) + 
    geom_hline(yintercept = 0.0001, lwd = .2, linetype = "dashed", alpha = .5) + 
    geom_vline(xintercept = 0.0001, lwd = .2, linetype = "dashed", alpha = .5) + 
    geom_point(alpha = 0, aes(size = fatalities, colour = actor_description)) + 
    geom_text_repel(aes(label = admin1_label,  size = fatalities, colour = actor_description), 
                    show.legend = FALSE, max.overlaps = 20, 
                    box.padding = .1, 
                    label.padding = .1, 
                    label.r = .1, 
                    force = .5,
                    force_pull = .5, 
                    vjust = "inward") + 
    facet_wrap(~ covid) + 
    scale_x_log10() +
    scale_y_log10() + 
    scale_color_manual(values = c(
      "Civilians" = "#fde725ff",
      "State Forces" = "#7A0403FF",
      "Protesters" = "#4AC16DFF",
      "Identity Militias" = "#4777EFFF",
      "Rebel Groups" = "#FE9B2DFF",
      "Rioters" = "#1BD0D5FF",
      "Political Militias" = "#D3436EFF",
      "Other Forces" = "grey30")) +
    scale_size_continuous(range = c(1, 5)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1, 
                                                     size = 1.8)), 
           size = guide_legend(override.aes = list(alpha = 1, 
                                                   colour = "grey"))) + 
    labs(x = "No. of Events per 100k", 
         y = "No. of Fatalities per 100k",
         title = "Conflict actor types in Indonesia by number of events and fatalities (2014-2024)", 
         size = "Fatalities", 
         colour = "Actor type", 
         caption = "Source: www.acleddata.com") + 
    theme(plot.caption = element_text(hjust = .5), 
          strip.background = element_rect(fill = "black"), 
          strip.text = element_text(face = "bold"), 
          legend.text = element_text(size = 6), 
          legend.title = element_text(size = 7)) 
  
  ggsave(here("plots", "conflict_actors_indonesia.png"), dpi = 300, 
         height = 7, width = 11, units = "in")
  
  
  
  acled_filtered |> 
    group_by(country, event_type, sub_event_type) |> 
    summarise(events = n(), 
              fatalities = sum(fatalities), .groups = "drop") |>
    filter(country %in% top_10_annual_fatalities) |>
    mutate(country = fct_relevel(country, top_10_annual_fatalities)) |> 
    ggplot(aes(x = events + 0.01, 
               y = fatalities + 0.01)) + 
    geom_hline(yintercept = .01, alpha = .5, linetype = "dashed", linewidth = .5) +
    geom_vline(xintercept = .01, alpha = .5, linetype = "dashed", linewidth = .5) +
    geom_point(aes(colour = event_type)) + 
    geom_text_repel(aes(label = sub_event_type), 
                    size = 1.5, 
                    max.overlaps = 16) + 
    facet_wrap(~ country, scales = "free", ncol = 3) +
    scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 60000)) + 
    scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 60000)) + 
    theme(strip.background = element_rect(fill = "black"))
  
  
  acled_actors_indonesia <- acled_actors |> 
    filter(country == "Indonesia") |> 
    left_join(
      acled_filtered |> 
        filter(country == "Indonesia") |> 
        select(event_id_cnty, admin1), 
      by = "event_id_cnty"
    ) |> 
    # This matches the ACLED and location gazette and pcode datasets
    mutate(admin1 = ifelse(
      str_detect(admin1, "West|East|South|North|Southeast|Southwest|Central|Highland") & 
        !str_detect(admin1, "Nusa"), 
      paste0(stringr::word(admin1, 2), " ", stringr::word(admin1, 1)), 
      admin1
    )) |> ungroup() |> 
    mutate(admin1 = case_when(
      str_detect(admin1, "Southwest") ~ str_replace(admin1, "Southwest", "Barat Daya"), 
      str_detect(admin1, "Southeast") ~ str_replace(admin1, "Southeast", "Tenggara"), 
      str_detect(admin1, "North") ~ str_replace(admin1, "North", "Utara"), 
      str_detect(admin1, "South") ~ str_replace(admin1, "South", "Selatan"), 
      str_detect(admin1, "East") ~ str_replace(admin1, "East", "Timur"), 
      str_detect(admin1, "West") ~ str_replace(admin1, "West", "Barat"), 
      str_detect(admin1, "Central") ~ str_replace(admin1, "Central", "Tengah"), 
      str_detect(admin1, "Highland") ~ str_replace(admin1, "Highland", "Pegunungan"),
      TRUE ~ admin1
    )) |> 
    mutate(
      admin1 = case_when(
        str_detect(admin1, "Java") ~ str_replace_all(admin1, "Java", "Jawa"), 
        admin1 == "Barat Nusa Tenggara" ~ "Nusa Tenggara Barat", 
        admin1 == "Timur Nusa Tenggara" ~ "Nusa Tenggara Timur",
        str_detect(admin1, "Bangka") ~ "Kepulauan Bangka Belitung", 
        str_detect(admin1, "Sumatra") ~ str_replace(admin1, "Sumatra", "Sumatera"), 
        admin1 == "Riau Islands" ~ "Kepulauan Riau", 
        str_detect(admin1, "Jakarta") ~ "Jakarta",
        str_detect(admin1, "Yogyakarta") ~ "Yogyakarta",
        admin1 == "Papua Tengah" ~ "Papua", 
        admin1 == "Papua Selatan" ~ "Papua", 
        admin1 == "Papua Pegunungan" ~ "Papua", 
        admin1 == "Papua Barat Daya" ~ "Papua Barat", 
        TRUE ~ admin1),
      admin1 = str_trim(admin1)) |> 
    left_join(
      indo_adm1_pop |>  
        mutate(
          adm1_en = str_trim(adm1_en), 
          adm1_en = case_when(
            str_detect(adm1_en, "Jakarta") ~ "Jakarta", 
            str_detect(adm1_en, "Yogyakarta") ~ "Yogyakarta",
            TRUE ~ adm1_en)),
      by = c("admin1" = "adm1_en")
    ) |> 
    mutate(admin1_label = str_replace_all(admin1, 
                                          c("Timur" = "Ti", 
                                            "Barat" = "B", 
                                            "Tengah" = "Tgh", 
                                            "Tenggara" = "Tgg", 
                                            "Utata" = "U", 
                                            "Selatan" = "S")))  
  
  actor_list_indonesia <- acled_actors_indonesia |>   
    mutate(quarter = floor_date(event_date, unit = "quarter")) |>
    group_by(admin1, quarter) |> 
    summarise(actors = n_distinct(actor), .groups = "drop") |> 
    group_by(admin1) |> 
    summarise(actors = mean(actors)) |> 
    arrange(desc(actors)) |> 
    pull(admin1)
  
  lbn_food |> 
    mutate(quantity = parse_number(unit),
           unit = str_extract(unit, "[A-Za-z]+"), 
           usdprice = as.numeric(usdprice), 
           price = as.numeric(price)) |> 
    filter(pricetype == "Retail") |> 
    mutate(unit_price = price / quantity, 
           category_unit = paste0(category, " ", date)) |> 
    group_by(commodity, category, unit, date) |>
    summarise(unit_price = mean(unit_price, na.rm = TRUE), .groups = "drop") |> 
    filter(!is.nan(unit_price)) |> 
    group_by(commodity, category, date, unit) |> 
    summarise(unit_price = mean(unit_price), .groups = "drop") |> 
    filter(commodity %in% 
             c("Eggs", "Oil (sunflower)", "Cheese (picon)", 
               "Fuel (diesel)", "Fuel (gas)", "Fuel (petrol-gasoline, 95 octane)", 
               "Beans (white)", "Bread (pita)", "Bulgur (brown)") | 
             str_detect(commodity, "beef|tuna|sardine")) |>  
    ggplot(aes(x = date, 
               y = unit_price)) + 
    geom_line(aes(colour = category), 
              linewidth = .7, 
              alpha = .9) + 
    facet_wrap(~ commodity, scales = "free_y") + 
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") + 
    labs(y = "LBP price per unit (g, L, pc)", 
         x = "", 
         title = "Mean commodity prices in Lebanon, from WFP", 
         colour = "") + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1), 
          strip.background = element_rect(fill = "black")) + 
    guides(colour = guide_legend(override.aes = list(linewidth = 1.5)))
 
lebanon_actors |> 
  filter(country == "Lebanon") |> 
  group_by(actor_description) |> 
  summarise(events = n_distinct(event_id_cnty), 
            fatalities = sum(fatalities, na.rm = TRUE)) |> 
  pivot_longer(cols = -actor_description, 
               names_to = "type", 
               values_to = "value") |> 
  ggplot(aes(y = fct_rev(actor_description), x = value)) + 
  geom_col() + 
  facet_wrap(~type, scales = "free_x")
  count(actor_description)



Major protests topics include: political instability and dissatisfaction with the government, economic woes and the subsequent neglect of public infrastructure, the Israel-Palestinian conflict, legal and illegal detainees, and the port of Beirut explosion. 

Protests in Lebanon have largely revolved around the severe economic and political instability which has plagued the country. We also note few descriptors related to sectarian violence (i.e ), indicating that none of the various political and communal militias and groups are currently agitating (at a national level) for separatism, 


In this section, we explore the concerns of protesters in Lebanon. Below is a network graph of event descriptions of protests in Lebanon since 2016. Network graphs shows the connections between various observations (in this case, protest descriptors from Lebanon) and are commonly used to visualise social networks. 

The links between each of the words indicate the strength of the relationship (transparency) and the number of times this word pair has occurred (thickness). Only the most common word pairs with correlations above 0.2 are included (for legibility). 

The following major protest topics (from 2016-) have been identified from descriptions of protests events in Lebanon: 
  
  * Political instability and dissatisfaction with the government 
* Economic woes and the subsequent neglect of public infrastructure
* The Israel-Palestine conflict
* Legal and illegal detentions, calls for release
* Port of Beirut explosion 

Relatively fewer descriptors related to sectarian violence (i.e. Islam, Muslim, Sunni, Shiite, Christian) were noted. 

forecasts |> 
  filter(str_detect(indicator, "Aggreg") & 
           !str_detect(indicator, "Rank")) |> 
  mutate(indicator = str_remove(indicator, "AggregMetric-"), 
         indicator = str_replace(indicator, "m", " months")) |> 
  arrange(indicator) |> 
  filter(iso != "IRN") |> 
  group_by(indicator) |> 
  mutate(range = range_wna(value)) |> 
  ungroup() |> 
  left_join(world_shape |> filter(iso3 %in% c("ISR", "JOR", "LBN", "PSE", "SYR")), 
            by = c("iso" = "iso3"), 
            relationship = "many-to-many") |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = value)) + 
  facet_wrap(~ factor(indicator, 
                      levels = c("3 months", "6 months", "12 months")), nrow = 1) + 
  scale_fill_viridis_c(option = "magma", direction = -1) + 
  theme(strip.background = element_rect(fill = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        plot.caption = element_text(hjust = .5, size = 6)) + 
  labs(fill = "Normalised\nvalue", 
       title = "Conflict forecasts -- RAH Aggregates", 
       subtitle = "From September 2024", 
       caption = "The designations employed and the presentation of the material on this map do not imply the expression of any opinion whatsoever on the part of the Secretariat of the United Nations concerning the legal status\nof any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. This map is provided without any warranty of any kind whatsoever, either express or implied.")

forecasts |> 
  filter(str_detect(indicator, "6m") & 
           indicator %out% c("CAST-VaC_events-6m", 
                             "CAST-Battles_events-6m", 
                             "AggregMetric-6m", 
                             "AggregRank-6m")) |> 
  filter(iso != "IRN") |> 
  select(iso, country, indicator, value) |> 
  group_by(indicator) |> 
  mutate(range = range_wna(value), 
         indicator = str_replace(indicator, "ConfInt", "ConflictIntensity")) |> 
  ungroup() |> 
  left_join(world_shape |> filter(iso3 %in% c("ISR", "JOR", "LBN", "PSE", "SYR")), 
            by = c("iso" = "iso3"), 
            relationship = "many-to-many") |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = range)) + 
  facet_wrap(~indicator, nrow = 2) + 
  scale_fill_viridis_c(option = "magma", direction = -1) + 
  theme(strip.background = element_rect(fill = "black")) + 
  labs(fill = "Normalised\nvalue", 
       title = "Conflict forecasts -- 6 months from 2024-09")


"Civilians" = "#fde725ff",
"State Forces" = "#7A0403FF",
"Protesters" = "#4AC16DFF",
"Identity Militias" = "#4777EFFF",
"Rebel Groups" = "#FE9B2DFF",
"Rioters" = "#1BD0D5FF",
"Political Militias" = "#D3436EFF",
"Other Forces" = "grey30"

bng_protests <- bangladesh |> 
  filter(event_type %in% c("Protests", "Riots")) |> 
  mutate(covid = ifelse(event_date >= "2020-03-11", "post-covid", "pre-covid")) |> 
  select(event_id_cnty, notes, covid) |> 
  unnest_tokens(word, notes) |> 
  anti_join(stop_words, by = "word") |> 
  filter(str_detect(word, "[a-z]")) |> 
  filter(word %out% c("township", "district", "city", "village", 
                      "region", "town", "coded", "province", "county", 
                      "pradesh", "prefecture", "regency", 
                      "barangay", "upazila", "january", "february", 
                      "march", "april", "may",
                      "june", "july", "august", "september", "october",
                      "november", "december", 
                      "report")) |> 
  mutate(stem = SnowballC::wordStem(word, language = "porter")) 


set.seed(234)

protest_riot_network_graph_bangladesh <- bng_protests |>
  filter(str_detect(event_id_cnty, "IDN")) |> 
  # filter(covid == "post-covid") |> 
  distinct(event_id_cnty, word) |> 
  add_count(word) |> 
  filter(n >= 100) |> 
  pairwise_cor(word, event_id_cnty, sort = TRUE) |> 
  filter(correlation >= .15) |> 
  left_join(
    acled_protests |> 
      distinct(event_id_cnty, word) |> 
      add_count(word) |> 
      filter(n >= 100) |> 
      pairwise_count(word, event_id_cnty, sort = TRUE), 
    by = c("item1", "item2")
  ) |> 
  igraph::graph_from_data_frame() %>% 
  ggraph(layout = "fr") + 
  geom_edge_link(aes(alpha = correlation, edge_width = n), colour = "lightskyblue2", check_overlap = TRUE) +
  scale_edge_width_continuous(range = c(.1, 2.5), trans = "log10") +
  scale_alpha_continuous(range = c(0.01, 0.08)) +
  geom_node_point(colour = "lightskyblue2", alpha = 0.2, size = .5) +
  geom_node_text(aes(label = ifelse(name %in% 
                                      c("myanmar", "laundering", "coronavirus",
                                        "citizenship", "union", "load", "farm", 
                                        "prices", "arrears", "palestine", "kashmir", 
                                        "pension", "fukushima", "majeste",
                                        "arrested", "oli", "manipur", "korea", 
                                        "football", "rights", 
                                        "incumbent", "drivers", "dispute", 
                                        "papua", "kpk"),
                                    str_to_title(name),
                                    "")), 
                 size = 4.3, 
                 alpha = .7, 
                 colour = "goldenrod2") + 
  geom_node_text(aes(label = name), size = 1.5) + 
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = .5)) + 
  labs(title = "Network graph of protest descriptions in Indonesia 2014-2024", 
       subtitle = "Line thickness indicates number of events involving those words, line transparency indicates the correlation between words. Selected words highlighted.", 
       caption = "Source: www.acleddata.com")


actor_order_list <- acled_actors |> 
  mutate(quarter = floor_date(event_date, unit = "quarter")) |>
  group_by(country, quarter) |> 
  summarise(actors = n_distinct(actor), .groups = "drop") |> 
  group_by(country) |> 
  summarise(actors = mean(actors)) |> 
  arrange(desc(actors)) |> 
  pull(country)

acled_actors |>
  mutate(covid = ifelse(event_date >= "2020-03-11", "Post-covid", "Pre-covid"), 
         covid = fct_relevel(covid, "Pre-covid", "Post-covid")) |>
  group_by(actor_description, country, covid) |> 
  summarise(events = n_distinct(event_id_cnty), 
            fatalities = sum(fatalities, na.rm = TRUE), 
            .groups = "drop") |> 
  mutate(actor_description = fct_relevel(actor_description, 
                                         c("Civilians", 
                                           "State Forces",  
                                           "Protesters",
                                           "Identity Militias", 
                                           "Rebel Groups", 
                                           "Rioters", 
                                           "Political Militias", 
                                           "Other Forces"
                                         ))) |>
  left_join(
    population_estimates |>
      mutate(covid = ifelse(year >= 2020, "Post-covid", "Pre-covid")) |>
      group_by(country, covid) |>
      summarise(population = mean(population, na.rm = TRUE),
                .groups = "drop"), 
    by = c("country", "covid")
  ) |> 
  mutate(
    events_100k = events / population * 100000, 
    fatalities_100k = fatalities / population * 100000, 
    covid = fct_relevel(covid, c("Pre-covid", "Post-covid"))
  ) |>
  
  left_join(country_iso3, by = c("country" = "name")) |> 
  ggplot(aes(x = events_100k + 0.0001, y = fatalities_100k + 0.0001)) + 
  geom_hline(yintercept = 0.0001, lwd = .2, linetype = "dashed", alpha = .5) + 
  geom_vline(xintercept = 0.0001, lwd = .2, linetype = "dashed", alpha = .5) + 
  geom_point(alpha = 0, aes(size = fatalities, colour = actor_description)) + 
  geom_text_repel(aes(label = iso3,  size = fatalities, colour = actor_description), 
                  show.legend = FALSE, max.overlaps = 17, 
                  box.padding = .1, 
                  label.padding = .1, 
                  label.r = .1, 
                  force = .5,
                  force_pull = .5, 
                  vjust = "inward") + 
  facet_wrap(~ covid) + 
  scale_x_log10() +
  scale_y_log10() + 
  scale_color_manual(values = c(
    
    "Civilians" = "#FCFFA4FF",
    "State Forces" = "#000004FF",
    "Protesters" = "#FAC127FF",
    "Identity Militias" = "#9F2A63FF",
    "Rebel Groups" = "#F57D15FF",
    "Rioters" = "#65156EFF",
    "Political Militias" = "#D44842FF",
    "Other Forces" = "#280B54FF")) +
  scale_size_continuous(range = c(1, 5)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, 
                                                   size = 1.8)), 
         size = guide_legend(override.aes = list(alpha = 1, 
                                                 colour = "grey"))) + 
  labs(x = "No. of Events per 100k", 
       y = "No. of Fatalities per 100k",
       title = "Conflict actor types by number of events and fatalities", 
       size = "Fatalities", 
       colour = "Actor type", 
       caption = "Source: www.acleddata.com") + 
  theme(plot.caption = element_text(hjust = .5), 
        strip.background = element_rect(fill = "black"), 
        strip.text = element_text(face = "bold"), 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 7)) 

Actors 


"The plot below shows the activity (events per 100,000 persons) and lethality (fatalities per 100,000 persons) of the various conflict actor types in each country. The total size of each country's ISO code indicates the total amount of fatalities from events where each actor type was involved. 
       
Judging by the frequency of protest events in countries not at war, pre-covid, the countries and territories with the most dissatisfied populations were Nepal, Hong Kong, Pakistan, Sri Lanka, Cambodia and Bangladesh (with its predominance of riots). Post-covid, populations in Pakistan and Sri Lanka are still voicing their dissatisfaction. For the most part, protests seem to be less lethal after covid, though Pakistani protesters had almost as high a fatality rate as protesters in Myanmar.  

Protests have increased in India and Indonesia, though riots are still at the roughly the same levels as before. Maldivians (at the bottom right) are protesting at greater levels than even pre-covid Hong Kong. Pre-covid, Cambodian protesters died at the highest rate of al lother protesters. 

Pre-covid, the most prominent actors in Afghanistan, Pakistan and Myanmar indicated that these countries were experiencing open warfare, given the high activity and lethality experienced amongst state forces, rebel groups, political militias and civilians (any event involving a combatant and a civilian ends predictably). Bangladeshi and Thai actors border these most prolific actors, indicating significant instability and unrest in those countries. 

Post-covid, we see a rapid increase of deaths in Afghanistan and Myanmar, due to intensifying battles leading to victory for the Taliban and the post-coup civil war respectively. 

Papua New Guinea, where data collection began in `r acled_filtered |> filter(country == "Papua New Guinea")  %>% {min(.$event_date)}`, has showed itself to be one of the most violent countries in the Asia-Pacific." 

As noted earlier, with most countries moving up and down the axis of state control 
When faced with economic woes 

Despite all the violence in Pakistan, `load shedding` still appears in the words most likely to be associated with protests in Pakistan, perhaps indicating that their infrastructure is in a truly dire state, even when compared to countries of similar income levels.


actors_conf_int %>% 
  left_join(acled_actors %>% 
              group_by(actor, actor_description, country) %>% 
              summarise(fatalities = sum(fatalities), .groups = "drop"), 
            by = "actor") %>%
  mutate(actor = fct_reorder(actor, estimate), 
         actor = str_replace_all(actor, "Forces of ", ""), 
         actor = str_replace_all(actor, "People's Defense Force", "PDF"),
          actor = str_replace_all(actor, "the Philippines", "Philippines"),
         actor = str_sub(actor, start = 1L, end = 40L), 
         country = fct_relevel(country, most_fatalities_list), 
         sort = (estimate / sum(estimate)) + (fatalities / sum(fatalities))) %>% 
  mutate(actor = ifelse(str_detect(actor, "Myanmar|Arakan|Kachin") | 
                          country == "Myanmar", 
                        paste0("<span style='color:#FF0000'>", actor, "</span>"),
                        actor)) |> 
  arrange(desc(sort)) |> 
  group_by(country) |> 
  slice(1:10) |> 
  ungroup() |> 
  filter(country %out% c("Bhutan", "Fiji", "Timor-Leste",
                         "Singapore", "New Zealand", "Mongolia", 
                         "Japan", "Hong Kong", "Maldives", "South Korea",
                         "Australia", "Solomon Islands")) |> 
  mutate(actor_description = fct_relevel(actor_description,
                                         c(
                                           "Civilians", "State Forces", 
                                           "Protesters", "Identity Militias", 
                                           "Rebel Groups", "Rioters", 
                                           "Political Militias", "Other Forces")))  |> 
  ggplot(aes(x = estimate, y = reorder_within(actor, estimate, country), colour = actor_description)) + 
  geom_point(aes(size = fatalities)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15) + 
  scale_x_continuous(trans = "log10") +
  scale_size_continuous(range = c(1, 5)) + 
  scale_colour_manual(
    values = c(
      "Civilians" = "#FCFFA4FF",
      "State Forces" = "#000004FF",
      "Protesters" = "#FAC127FF",
      "Identity Militias" = "#9F2A63FF",
      "Rebel Groups" = "#F57D15FF",
      "Rioters" = "#65156EFF",
      "Political Militias" = "#D44842FF",
      "Other Forces" = "#280B54FF")) +
  # scale_colour_brewer(palette = "Dark2") + 
  scale_y_reordered() + 
  facet_wrap(~country, scales = "free", ncol = 4) +
  labs(x = "Estimate: Fatalities per conflict event (log scale)",
      y = "", 
      title = "Top 10 'deadliest' actors per country in order of fatalities per event, 2014-2023", 
      subtitle = "Only actors involved more than 20 conflict events. Size indicates number of fatalities associated with actor. Red actors are from Myanmar.", 
      caption = "Data source: Armed Conflict Location & Event Data Project (ACLED); acleddata.com",
      colour = "", size = "") + 
  guides(size = "none") + 
  theme(plot.caption = element_text(hjust = 0.5), 
        legend.text = element_text(size = 9), 
        legend.position = "top",
        axis.text = element_text(size = 4), 
        axis.text.y = element_markdown(size = 6)) +
  guides(colour = guide_legend(nrow = 1)) 

"The Philippine police is more lethal than anti-drug vigilantes and the New People's Army. Similarly, in China, civilians seem to be involved in more fatalities per event than even the police, or rioters, indicating problems with violence against civilians (as fatalities from an incident where one party were civilians and the other combatants).

South Korea here should be counterpoint to the Philippines, Vietnam and China, especially in terms of the lethality of state actors. In spite of how much Koreans protest, neither their protesters nor their state actors are amongst those who have suffered the most casualties."


filter(!is.na(longitude) & !is.na(latitude) & 
             country %in% c("Afghanistan", "Pakistan")) |> 
    select(event_id_cnty, event_type, longitude, latitude, event_type, fatalities) |>
    ggplot() +
    geom_sf(data = world_shape, size = .7, colour = "gold", fill = "grey90", alpha = .5) + 
    geom_sf(data = afpak_buffer, colour = "white", fill = "white", alpha = .5) +
    coord_sf(xlim = c(60.41, 75.02), 
             ylim = c(25.04, 39.11)) +
    geom_point(aes(x = 73.0363, y = 33.6995), colour = "cornflowerblue", pch = 15) +
    geom_point(aes(x = 69.2075, y = 34.5553), colour = "cornflowerblue", pch = 15) +
    geom_point(aes(x = longitude, y = latitude, colour = event_type, size = fatalities), alpha = .05) +
    scale_colour_manual(values = c(
      "Battles" = "#9b2226",
      "Violence against civilians" = "#001219",
      "Explosions/Remote violence" = "#ee9b00",
      "Protests" = "#94d2bd",
      "Strategic developments" = "#e9d8a6",
      "Riots" = "#005f73"
    )) +
    theme(plot.background = element_rect(fill = NA, colour = NA),
          panel.background = element_rect(fill = NA, colour = NA),
          rect = element_rect(fill = NA, colour = NA),
          legend.title = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0.2)) + 
    guides(colour = guide_legend(override.aes = list(alpha = 1)), 
           size = guide_legend(override.aes = list(alpha = 1, 
                                                   colour = "grey"))) + 
    theme_void() + 
    labs(colour = "", size = "Fatalities")

genocide_t_test <- bangladesh_summary |> 
  mutate(rohingya = ifelse(quarter < "2017-08-25", 
                           "pre_genocide", 
                           "post_genocide")) |> 
  filter(type == "fatalities") |> 
  select(-type) |> 
  group_by(admin2, rohingya) |> 
  summarise(mean_value = mean(value, na.rm = TRUE), 
            .groups = "drop") |> 
  pivot_wider(values_from = mean_value, 
              names_from = rohingya)  %>%
  t.test(.$post_genocide, .$pre_genocide, data = .) |> 
  broom::tidy()


# Macroeconomics and conflict 

```{r}
wdi |> count(indicator)

wdi |> filter(indicator == "Multidimensional poverty headcount ratio (World Bank) (% of population)")
```

```{r}
wdi_wide_events |> 
  ggplot(aes(x = `Prevalence of underweight, weight for age (% of children under 5)`, 
             y = `Voice and Accountability: Estimate`)) +
  
  geom_point() + 
  geom_smooth(method = "lm")


```


```{r}
wdi_wide_events <- wdi |> 
  filter(indicator %in% 
           c("Total debt service (% of GNI)", 
             "Military expenditure (% of GDP)",
             "Prevalence of underweight, weight for age (% of children under 5)", 
             "NEET, total (% of youth population) (modeled ILO estimate)", 
             "People using at least basic sanitation services (% of population)", 
             "Multidimensional poverty headcount ratio (World Bank) (% of population)")) |>
  select(-series_code) |>
  filter(year >= 2010 & !is.na(value)) |>
  pivot_wider(names_from = indicator, values_from = value) |> 
  group_by(country, year) %>% 
  reframe(across(everything(), ~na.omit(.))) |> 
  ungroup() |> 
  left_join(
    acled_filtered |> filter(year >= 2010 & year < 2024) |>
      group_by(country, year) |>
      summarise(events = n_distinct(event_id_cnty),
                .groups = "drop"), 
    by = c("country", "year")
  ) |> 
  mutate(events = log10(events)) |>  
  select(-country, -country, -country_iso, -country_code, -year)

set.seed(145)

wdie_split <- initial_split(wdi_wide_events, prop = 0.80)
wdie_train <- training(wdie_split)
wdie_test <- testing(wdie_split)

lm(events ~ ., data = (wdie_train)) |> 
  summary()
```

The best performing indicators 

```{r}
wdi_wide_fatalities <- wdi |> 
  filter(indicator %in% 
           c("Total debt service (% of GNI)", 
             "Control of Corruption: Estimate", 
             "Military expenditure (% of GDP)",
             "Prevalence of underweight, weight for age (% of children under 5)", 
             "NEET, total (% of youth population) (modeled ILO estimate)")) |>
  select(-series_code) |>
  filter(year >= 2010 & !is.na(value)) |>
  pivot_wider(names_from = indicator, values_from = value) |> 
  group_by(country, year) %>% 
  reframe(across(everything(), ~na.omit(.))) |> 
  ungroup() |> 
  left_join(
    acled_filtered |> filter(year >= 2010 & year < 2024) |>
      group_by(country, year) |>
      summarise(fatalities = sum(fatalities, na.rm = TRUE),
                .groups = "drop"), 
    by = c("country", "year")
  ) |> 
  mutate(fatalities = log10(fatalities)) |> 
  filter(!is.na(fatalities) & !is.nan(fatalities) & !is.infinite(fatalities)) |> 
  select(-country, -country, -country_iso, -country_code, -year)

set.seed(135)

wdif_split <- initial_split(wdi_wide_fatalities, prop = 0.80)
wdif_train <- training(wdif_split)
wdif_test <- testing(wdif_split)

lm(fatalities ~ ., data = (wdif_train)) |> 
  summary()
```


```{r}
set.seed(2021) 


rf_model <- 
  rand_forest(trees = 1000, min_n = 4) |> 
  set_mode("regression") |> 
  set_engine("ranger", importance = "impurity")

rf_recipe <- recipe(events ~ ., data = wdi_train)

rf_prep <- prep(rf_recipe)

juiced <- juice(rf_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

tune_workflow <- 
  workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(tune_spec)


rf_workflow %>%
  fit(wdi_train) %>% 
  extract_fit_parsnip() %>% 
  vip::vip(num_features = 12)

```

```{r}
set.seed(234)
trees_folds <- vfold_cv(wdi_train)



```

```{r}
set.seed(345)
tune_res <- tune_grid(
  tune_workflow,
  resamples = trees_folds,
  grid = 20
)

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE")

rf_grid <- grid_regular(
  mtry(range = c(10, 30)),
  min_n(range = c(2, 8)),
  levels = 5
)

rf_grid

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "rmse")

set.seed(456)

regular_res <- tune_grid(
  tune_workflow,
  resamples = trees_folds,
  grid = rf_grid
)

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "rmse")
```


```{r}

```



```{r}
events_wdi <- wdi |> 
  filter(indicator %in% 
           c("Consumer price index (2010 = 100)", 
             "Inflation, consumer prices (annual %)", 
             "Total debt service (% of GNI)", 
             "GDP per capita (current US$)", 
             "People using at least basic sanitation services (% of population)", 
             "Voice and Accountability: Estimate")) |>
  filter(year >= 2010) |> 
  left_join(
    acled_filtered |> filter(year >= 2010 & year < 2024) |>
      group_by(country, year) |>
      summarise(events = n_distinct(event_id_cnty)), 
    by = c("country", "year")
  ) |> filter(!is.na(events) & !is.na(value))


nest(-indicator) %>%
  mutate(model = map(data, ~ lm(.$events ~ .$value))) |> 
  unnest_legacy(map(model, tidy)) |> 
  filter(term != "(Intercept)")


fatalities_model <- wdi |> 
  filter(indicator %in% 
           c("Consumer price index (2010 = 100)", 
             "Inflation, consumer prices (annual %)", 
             "Total debt service (% of GNI)", 
             "GDP per capita (current US$)", 
             "People using at least basic sanitation services (% of population)", 
             "Voice and Accountability: Estimate")) |>
  filter(year >= 2010) |> 
  left_join(
    acled_filtered |> filter(year >= 2010 & year < 2024) |>
      group_by(country, year) |>
      summarise(events = n_distinct(event_id_cnty), 
                fatalities = sum(fatalities, na.rm = TRUE)), 
    by = c("country", "year")
  ) |> 
  filter(!is.na(fatalities) & !is.na(value)) |> 
  nest(-indicator) %>%
  mutate(model = map(data, ~ lm(.$events ~ .$value))) |> 
  unnest_legacy(map(model, tidy)) |> 
  filter(term != "(Intercept)")



```


```{r}
# Initialize file path
png(height=1800, width=1800, file=here("plots", "correlation_plot.png"), type = "cairo")

# Your function to plot image goes here
islands |> 
  filter(index == "Demographic statistics") |> 
  filter(indicator_full %out% c("Population density Resident Maldivian Population",
                                "Median Age Resident Maldivians") & 
           !str_detect(indicator_full, "International|\\%")) |>
  select(Atoll, island, concat, indicator_full, value) |> 
  pivot_wider(names_from = indicator_full, 
              values_from = value) |> 
  select(-Atoll, -island, -concat) |>
  cor(method = c("pearson")) %>%
  corrplot.mixed(order = "AOE", 
                 title = "Demographic statistics",
                 mar = c(0,0,1,0),
                 number.cex = 5,
                 number.digits = 2, 
                 diag = "l", tl.pos = "lt")

# Then
dev.off()  
```

## Pakistan

Pakistan is party to numerous geopolitical conflicts. It also entered into a massive political crisis following the [ouster](https://www.scmp.com/news/asia/south-asia/article/3217401/pakistan-speeds-towards-full-blown-crisis-imran-khans-foot-accelerator) of Prime Minister Imran Khan, though the situation has since stabilised. However, the Pakistani state remains unstable enough for long-time allies like China to be [very publicly concerned](https://thediplomat.com/2023/05/behind-chinas-friendly-advice-for-pakistan/). Furthermore, state and non-state actors in Pakistan have a history of using [deadly force](https://acleddata.com/2024/02/01/political-repression-and-militant-targeting-set-the-stage-for-pakistans-2024-elections/) against protesters and other civilians, including militia attacks on political parties.

Even as Pakistan reels from its constitutional crisis, its relationship with the Taliban has soured since the US withdrawal. According to [Radio Free Europe](https://www.rferl.org/a/afghanistan-pakistan-taliban-ttp-terrorism/33078685.html), the Taliban has not reined in the terror group Tehrik-e Taliban (TTP), which has killed hundreds of Pakistani soldiers since 2021. Michael Semple, a former UN and EU Advisor, believes that Afghanistan would not likely give up on TTP, given that Pakistan is reaching a "tipping point given the political and economic crises", creating quite the opportunity for Afghanistan.

Pakistan's economic woes

https://acleddata.com/2024/02/14/acled-insight-election-related-violence-in-pakistan/

Pakistan has been often leaning on China for assistance to keep its economy intact. Last year in July, when Islamabad’s foreign exchange reserves were depleting at a massive pace, it asked Beijing to delay extending the maturity of debt for nine power plants built by Chinese companies under the multibillion-dollar Pakistan China Economic Corridor (CPEC). https://www.firstpost.com/world/pakistan-seeks-additional-10-billion-yuan-loan-from-china-13829881.html

https://finance.yahoo.com/news/china-pakistan-economic-ties-could-093000794.html

Ongoing tensions between Khan and the political establishment spilled over in May 2023, when Khan was arrested on corruption charges.16 PTI supporters mobilized across the country, organizing demonstrations in hundreds of locations. Of these, 21% of demonstrations turned violent, with demonstrators attacking government and military properties, including the residences of high-ranking military officers.

https://acleddata.com/2024/02/01/political-repression-and-militant-targeting-set-the-stage-for-pakistans-2024-elections/

Political and economic development can be characterized as the conflict between states' efforts to increase their capabilities and existing social forces that they must accommodate and to which they must be accountable (Acemoglu and Robinson 2019). This conflict is definitional at the frontiers of the formal state. The combination of a law that clearly demarcates the end of the formal state combined with the agriculture transformation precipitated by the Green Revolution makes 20th century Pakistan an ideal setting to study these processes.

We show that the trajectory of state presence within the borders of modern-day Pakistan is consistent with a theoretical framework in which states extend governance to areas where the economic benefits of investing in institutions for taxation and resource extraction outweigh the costs of doing so. Using crop-suitability data from the Food and Agriculture Organization of the United Nations, we demonstrate first that the choice by the British to apply the Frontier Crimes Regulation (FCR) to over half of Pakistan in 1901 was (conditionally) unrelated to crop suitability. We then exploit the fact that the Green Revolution created more fundamental agricultural transformation in areas of low crop suitability to understand Pakistan's selective roll-back of the FCR throughout the 1960s and 1970s. Subdistricts made suitable to agriculture by the Green Revolution were more likely to see the FCR removed.

https://onlinelibrary.wiley.com/doi/10.1111/ecca.12527

Kabul was also disturbed that the US also backed down on its longstanding demand that the Taliban break publicly with al-Qaeda. Instead the Taliban made vague statements about never letting ‘their country’ be used for terrorism against another. That echoes the Taliban’s statements before and after 9/11 that al-Qaeda was not engaged in attacks on American targets despite all the obvious evidence. al-Qaeda fighters are still on the battlefield in Afghanistan fighting with the Taliban.

The Taliban’s Pakistani patrons, the army and the ISI were very pleased with the outcome. They control the lives of the leadership in Pakistan and the lives of the Taliban team in Doha. As the former head of Afghan intelligence, Amrullah Saleh, likes to point out that the Taliban negotiators fly home to Karachi from Doha whenever they want to see their boss or their families. They are not independent players.

https://www.brookings.edu/articles/pakistan-taliban-and-the-afghan-quagmire/

On August 14, Torkham, the main border crossing connecting Pakistan and Afghanistan, remained closed for a third day after a firefight that injured several Taliban fighters and at least three Pakistani soldiers on August 12. The Taliban said three Afghan civilians were killed in the cross fire.

Such clashes are almost a weekly occurrence along their 2,500-kilometer border.

But Michael Semple, a former EU and UN adviser to Afghanistan, told RFE/RL it would be difficult for the Taliban to give up on the TTP's nearly two-decade campaign when it sees a tipping point given the grave political and economic crises engulfing Pakistan.

https://www.rferl.org/a/afghanistan-pakistan-taliban-ttp-terrorism/33078685.html

Even as it is embroiled in a political conflict, Pakistan also remains involved in several regional conflicts -- namely, with India and with Afganistan.

<br>
  
  ```{r fig.height=7}


afpakind_capitals <- tribble(~x, ~y, ~name, 
                             77.2088, 28.6139, "New Delhi", 
                             73.0363, 33.6995, "Islamabad", 
                             69.2075, 34.5553, "Kabul") |> 
  st_as_sf(coords = c("x", "y"), crs = 4326)

acled_filtered |> 
  filter(year > 2015) |> 
  filter(!is.na(longitude) & !is.na(latitude) & 
           country %in% c("Pakistan", "India", "Afghanistan")) |> 
  select(event_id_cnty, event_type, longitude, latitude, event_type, fatalities) |>
  ggplot() + 
  geom_sf(data = world_shape, size = 1, colour = "darkblue", fill = "grey85", alpha = .5) +
  geom_sf_text(data = world_shape, aes(label = name)) + 
  geom_text(aes(, x = 76, y = 27), label = "India") + 
  geom_point(data = afpakind_capitals, aes(geometry = geometry), 
             size = 3, alpha = .8, pch = 15, alpha = .8, stat = "sf_coordinates",
             colour = "cornflowerblue") + 
  geom_sf_text(data = afpakind_capitals, aes(geometry = geometry, label = name), 
               size = 3, alpha = .8) + 
  geom_sf(data = kash_buffer, colour = "white", fill = "white", alpha = .65) + 
  geom_sf(data = afpak_buffer, colour = "white", fill = "white", alpha = .65) + 
  coord_sf(xlim = c(60, 82), ylim = c(23.1, 37)) +
  geom_point(aes(x = longitude, y = latitude, colour = event_type, size = fatalities), alpha = .05) +
  scale_colour_manual(values = c(
    "Battles" = "#9b2226",
    "Violence against civilians" = "#001219",
    "Explosions/Remote violence" = "#ee9b00",
    "Protests" = "#94d2bd",
    "Strategic developments" = "#e9d8a6",
    "Riots" = "#005f73"
  )) +
  scale_size_continuous(range = c(.1, 5)) + 
  theme(plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        rect = element_rect(fill = NA, colour = NA),
        legend.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0.2)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)), 
         size = guide_legend(override.aes = list(alpha = 1, 
                                                 colour = "grey"))) + 
  theme_void() +
  labs(title = "Political conflict in Pakistan, including border areas with Afghanistan and India",
       subtitle = "Border areas (within 100km) and Kashmir marked in white. Country capitals are the blue squares.", 
       colour = "", size = "Fatalities")


```

<br>
  
  ```{r}
acled_filtered |> 
  filter(country == "Pakistan") |> 
  group_by(event_type, quarter = floor_date(event_date, unit = "quarter")) |> 
  summarise(Events = n_distinct(event_id_cnty), 
            Fatalities = sum(fatalities, na.rm = TRUE), 
            .groups = "drop") |> 
  pivot_longer(cols = c(Events, Fatalities), 
               names_to = "type", 
               values_to = "value") |>
  ggplot(aes(x = quarter, y = value)) + 
  geom_vline(aes(xintercept=as.numeric(as.Date(dmy("17/2/2018")))), linetype=4, colour="black", 
             alpha = .5, size = .5) +
  geom_vline(aes(xintercept=as.numeric(as.Date(dmy("29/2/2020")))), linetype=4, colour="black", 
             alpha = .5, size = .5) +
  geom_area(aes(fill = event_type), 
            size = .75) + 
  scale_fill_manual(values = c(
    "Battles" = "#9b2226",
    "Violence against civilians" = "#001219",
    "Explosions/Remote violence" = "#ee9b00",
    "Protests" = "#94d2bd",
    "Strategic developments" = "#e9d8a6",
    "Riots" = "#005f73"
  )) +
  facet_wrap(~ type, scales = "free_y", ncol = 1) + 
  scale_y_continuous(labels = comma) + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") + 
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(size = 10), 
        legend.position = "top") + 
  labs(x = "", y = "",
       title = "Conflict events and fatalities in Pakistan, 2010-2024", 
       fill = "") + 
  guides(fill = guide_legend(nrow = 1))
```

Apart from Jammu and Kashmir, the border region between India and Pakistan is sparsely populated. Though, with reference to the graphs below, the number of violent incidents in Kashmir has decreased since 2020, over the time period monitored by ACLED below, there were more battles in Kashmir than the rest of India.

Pakistan's recent increase in conflict will be discussed in the next section.

<br>

```{r}

afpakind_border <- acled_filtered |> 
  filter(!is.na(longitude) & !is.na(latitude) & 
             country %in% c("India", "Pakistan", "Afghanistan")) |> 
  left_join(
    rbind(acled_filtered |>
              filter(!is.na(longitude) & !is.na(latitude) &
                       country %in% c("Afghanistan", "Pakistan")) |>
              st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
              st_intersection(ind_pak_buffer) |>
              st_drop_geometry() |>
              mutate(border = "Afpak border") |>
              select(event_id_cnty, border), 
    acled_filtered |>
      filter(!is.na(longitude) & !is.na(latitude) &
                       country %in% c("India", "Pakistan")) |>
              st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
              st_intersection(ind_pak_buffer) |>
              st_drop_geometry() |>
              mutate(border = "Indpak border") |>
              select(event_id_cnty, border)), 
            by = "event_id_cnty") |> 
  mutate(border = case_when(is.na(border) & country == "Pakistan" ~ "other Pakistan", 
                            is.na(border) & country == "India" ~ "other India"))

```

```{r}
acled_filtered |> 
  filter(year < 2024 & year > 2015) |> 
  filter(country %in% c("India", "Pakistan")) |> 
  mutate(kashmir = case_when(
    admin1 %in% c("Azad Jammu and Kashmir", "Jammu and Kashmir", "FATA", "Khyber Pakhtunkhwa") ~ "Kashmir", 
    country == "Pakistan" & 
       admin1 %out% c("Azad Jammu and Kashmir", "Jammu and Kashmir", "FATA", "Khyber Pakhtunkhwa") ~ "other Pakistan", 
    country == "India" & 
       admin1 %out% c("Azad Jammu and Kashmir", "Jammu and Kashmir", "FATA", "Khyber Pakhtunkhwa") ~ "other India"
  )) |> 
  group_by(event_type, year, kashmir) |> 
  summarise(events = n_distinct(event_id_cnty), .groups = "drop") |> 
  ggplot(aes(x = year, y = events)) + 
  geom_line(aes(colour = event_type), lwd = 1) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) + 
  scale_colour_manual(values = c(
      "Battles" = "#9b2226",
      "Violence against civilians" = "#001219",
      "Explosions/Remote violence" = "#ee9b00",
      "Protests" = "#94d2bd",
      "Strategic developments" = "#e9d8a6",
      "Riots" = "#005f73"
    )) +
  facet_wrap(~kashmir) + 
  theme(strip.background = element_rect(fill = "black")) + 
  labs(colour = "Event type", 
       x = "", 
       y = "Number of political incidents",
       title = "Political incidents in India and Pakistan, by year and border area (2016-2023)")

```

<br>

```{r}
acled_filtered |> 
  filter(country %in% c("India", "Pakistan")) |> 
  mutate(kashmir = case_when(
    admin1 %in% c("Azad Jammu and Kashmir", "Jammu and Kashmir") ~ "Kashmir", 
    country == "Pakistan" ~ "other Pakistan", 
    country == "India" ~ "other India"
  )) |> 
  filter(country == "India") |> 
  group_by(event_type, kashmir) |> 
   summarise(events = n_distinct(event_id_cnty),
             fatalities = sum(fatalities),
             .groups = "drop") |> 
  pivot_longer(cols = c(events, fatalities), 
               names_to = "variable", 
               values_to = "value") |> 
  ggplot(aes(x = value, y = fct_rev(event_type), fill = kashmir)) +
  geom_col(position = position_dodge(width = .9)) + 
  geom_text(aes(label = comma(value)), 
            position = position_dodge(width = .9), 
            hjust = "inward") + 
  scale_x_continuous(label = comma, trans = "sqrt") + 
  scale_fill_viridis_d(begin = .3) + 
  facet_wrap(~ variable, scales = "free_x") + 
  theme(strip.background = element_rect(fill = "black"), 
        legend.position = "top",
        plot.title = element_text(size = 11)) + 
  labs(x = "", y = "", fill = "", 
       title = "Conflict events and fatalities in Indian-adminstered Kashmir vs the rest of the country, 2016-01-01 to 2024-05-30")

```

<br>

Fatalities in Kashmir have also decreased since their 2018 highs. Kashmir experiences violence against civilians and riots at a lower rate than the rest of India and Pakistan.

<br>

```{r}
acled_filtered |> 
  filter(year < 2024 & year > 2015) |> 
  filter(country %in% c("India", "Pakistan")) |> 
  mutate(kashmir = case_when(
    admin1 %in% c("Azad Jammu and Kashmir", "Jammu and Kashmir") ~ "Kashmir", 
    country == "Pakistan" ~ "other Pakistan", 
    country == "India" ~ "other India"
  )) |> 
  group_by(event_type, year, kashmir) |> 
  summarise(fatalities = sum(fatalities), .groups = "drop") |> 
  ggplot(aes(x = year, y = fatalities)) + 
  geom_line(aes(colour = event_type), lwd = 1) +
  scale_colour_manual(values = c(
      "Battles" = "#9b2226",
      "Violence against civilians" = "#001219",
      "Explosions/Remote violence" = "#ee9b00",
      "Protests" = "#94d2bd",
      "Strategic developments" = "#e9d8a6",
      "Riots" = "#005f73"
    )) + 
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) + 
  facet_wrap(~kashmir) + 
  theme(strip.background = element_rect(fill = "black")) + 
  labs(colour = "Event type", 
       x = "", 
       y = "Number of conflict fatalities per event type",
       title = "Political conflict fatalities by year")
```

<br><br><br>