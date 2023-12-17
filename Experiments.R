# Question 2.6 ----

## Method 1: line and bar ----

library(ggplot2)

# Create a sample dataset
states <- c("State A", "State B", "State C", "State D")
n_fraud <- c(10, 20, 15, 5)
p_fraud <- c(2.5, 4.8, 3.6, 1.2)

data <- data.frame(State = states,
                   Number_of_Fraud = n_fraud,
                   Percentage_of_Fraud = p_fraud)

# Create the plot
ggplot() +
  geom_bar(aes(x = State, y = Number_of_Fraud),
           stat = "identity",
           fill = "steelblue",
           width = 0.5) +
  geom_line(aes(x = State, y = Percentage_of_Fraud * 10),
            group = 1,
            color = "red",
            size = 1.2) +
  geom_point(aes(x = State, y = Percentage_of_Fraud * 10),
             group = 1,
             color = "red",
             size = 3) +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Percentage of Fraud")) +
  labs(title = "Comparison of Number of Fraud and Percentage of Fraud",
       x = "State",
       y = "Number of Fraud") +
  theme_minimal()


## Method 2: dot plot ----

d_tally |>
  ggplot() +
  geom_point(aes(y = reorder(state, prop_fraud),
                 x = prop_fraud, size = n_fraud)) +
  geom_segment(aes(x = 0,
                   xend = prop_fraud,
                   y = reorder(state, prop_fraud),
                   yend = reorder(state, prop_fraud))) +
  scale_size(range = c(1,8)) +
  theme_classic()


d_tally |>
  ggplot(aes(x = reorder(state, prop_fraud), y = prop_fraud, fill = n_fraud)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightskyblue", high = "blue") +
  coord_flip() +
  labs(title = "Prevalence of Fraud",
       x = "State",
       y = "Percentage of Fradulent Tally Sheets") +
  theme_bw()

d_tally |>
  ggplot(aes(x = reorder(state, n_fraud))) +
  geom_col(aes(y = n_fraud,
               #fill = "Number of Incidents"
               ),
           position = "dodge", width = 0.4,
           
           alpha = 0.5) +
  geom_col(aes(y = prop_fraud,
               #fill = "Number of Incidents"
  ),
  position = "dodge", width = 0.4)


# side to side (unsuccessful)

d_tally |>
  ggplot() +
  geom_bar(aes(x = reorder(state, n_fraud),
               y = n_fraud),
           stat = "identity",
           fill = "blue",
           alpha = 0.5, width = 0.4,
           position = position_dodge()) +
  geom_bar(aes(x = reorder(state, n_fraud),
               y = prop_fraud*20),
           stat = "identity",
           fill = "red",
           alpha = 0.5, width = 0.4,
           position = position_dodge())



# Question 4.3 ----

## jitter ----

d |>
  ggplot(aes(x = salinas_prop, y = fraud_bin)) +
  geom_jitter(size = 0.1) # too dense

d_long |> ggplot(aes(x = Value, y = fraud_bin, color = fraud_bin)) +
  geom_jitter(size = 0.1, alpha = 0.5) +
  facet_wrap(~ factor(Candidates, level = c("Salinas (PRI)",
                                            "Cardenas (FDN)",
                                            "Clouthier (PAN)")),
             scales = "free_y", ncol = 1, strip.position = "right") +
  scale_color_manual(values = c("red", "black")) +
  theme_classic() +
  theme(legend.position = "none")


## bar
d_long$fraud_bin <- factor(d_long$fraud_bin, levels = c("FALSE", "TRUE"))
d_long |> 
  ggplot(aes(x = Value, fill = fraud_bin)) +
  geom_histogram(color = "white") +
  facet_wrap(~ factor(Candidates, level = c("Salinas (PRI)",
                                            "Cardenas (FDN)",
                                            "Clouthier (PAN)")),
             scales = "free_y", ncol = 1, strip.position = "right") +
  scale_fill_manual(values = c("lightgrey", "darkred")) +
  theme_classic()

# density

d_long |>
  ggplot(aes(x = Value)) +
  geom_density(aes(color = Candidates)) +
  facet_wrap(~ factor(fraud_bin),
             scales = "free_y", ncol = 1, strip.position = "right") +
  theme_minimal()

d_long |> filter(Candidates == "Cardenas (FDN)") |>
  ggplot(aes(x = Value)) + geom_density()


# Task 5.3 ----

## scatter plot w/ smooth line ----

sum_fraud_by_district |>
  ggplot(aes(x = vote_diff, y = prop_fraud)) +
  geom_point() +
  geom_smooth(data=subset(sum_fraud_by_district,
                          vote_diff < 40000),
              method = "lm", formula = y ~ poly(x, 2), se = FALSE)

geom_smooth(data=subset(sum_fraud_by_district,
                        prop_fraud > 0.5),
            method = "lm", orientation = "y", se = FALSE) +

## scatter plot w color

sum_fraud_by_district |>
  mutate(num = ifelse(vote_diff < 20000, "low", "high"),
         poss = ifelse(prop_fraud < 0.5, "no", "yes"),
         color = paste(num, poss, sep = "-")) |>
  ggplot(aes(x = vote_diff, y = prop_fraud)) +
  geom_point(aes(color = color), size = 3, alpha = 0.5) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position = "none")

# other plots with vote diff

sum_fraud_by_district |>
  ggplot(aes(x = vote_legislature, y = vote_president)) +
  geom_point(aes(color = prop_fraud), size = 3, alpha = 0.8) +
  scale_color_gradient(low = "azure2", high = "red") +
  geom_smooth(method = "lm", color = "grey", alpha = 0.5, se = FALSE) +
  theme_minimal()


  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position = "none")


# task 6.1 ----
  

map_data <- sum_fraud_by_district |>
    group_by(state) |>
    summarise(fraud = mean(prop_fraud)) |>
    arrange(fraud)

setdiff(map_data$state, map_mex$state_name)
setdiff(map_mex$state_name, map_data$state)

map_data <- map_data |>
  mutate(state = ifelse(state == "Yucatan", "Yucatán", state),
         state = ifelse(state == "Nuevo Leon", "Nuevo León", state),
         state = ifelse(state == "Michoacan", "Michoacán", state),
         state = ifelse(state == "Queretaro", "Querétaro", state),
         state = ifelse(state == "San Luis Potosi", "San Luis Potosí", state),
         state = ifelse(state == "Distrito Federal", "Ciudad de México", state),
         state = ifelse(state == "Edomex", "México", state))

setdiff(map_data$state, map_mex$state_name)

map_combined <- map_mex |>
  left_join(map_data, by = c("state_name" = "state"))

map_combined |>
  ggplot(aes(x = long, y = lat)) +
  geom_map(
    map = map_combined,
    aes(map_id = id, fill = fraud),
    color = "black", linewidth = 0.1
  ) +
  theme_classic() +
  scale_fill_gradient(low = "white", high = "grey17") +
  coord_map()

# task 6.2 ----

setdiff(map_data$state, map_mex_sf$NAME_1)
setdiff(map_mex_sf$NAME_1, map_data$state)

map_data <- map_data |>
  mutate(state = ifelse(state == "Ciudad de México", "Distrito Federal", state))
setdiff(map_data$state, map_mex_sf$NAME_1)

map_combined1 <- map_mex_sf |>
  left_join(map_data, by = c("NAME_1" = "state"))

map_combined1 |> ggplot() +
  geom_sf(aes(fill = fraud), color = "black") +
  scale_fill_gradient(low = "white", high = "grey17") +
  theme_void() +
  labs(fill = "Proportion\nof altered\ntallies") +
  theme(legend.position = c(.1,.25)) +
  coord_sf()


# task 6.3 ----

map_com_sf_cart <- map_combined_sf |>
  mutate(geometry = st_transform(geometry, 3857)) |>
  cartogram_cont(weight = "fraud")

map_com_sf_do <- map_combined_sf |>
  mutate(geometry = st_transform(geometry, 3857)) |>
  cartogram_dorling(weight = "fraud")

map_com_sf_do |> ggplot() +
  geom_sf(aes(fill = fraud), color = "black") +
  scale_fill_gradient(low = "white", high = "red") +
  geom_sf_text(aes(label = NAME_1), size = 2.5) +
  #geom_sf_text(aes(label = fraud), size = 2.5, nudge_y = 5) +
  theme_void() +
  labs(fill = "Proportion\nof altered\ntallies") +
  theme(legend.position = c(.1,.25)) +
  coord_sf()


map_com_sf_severe <- map_combined_sf |>
  mutate(fraud = ifelse(fraud < 33.3, NA, fraud),
         NAME_1 = ifelse(fraud < 33.3, NA, NAME_1))

map_com_sf_severe |>
  ggplot() +
  geom_sf(aes(fill = fraud), color = "black") +
  scale_fill_gradient(low = "rosybrown1", high = "darkred") +
  geom_sf_text(data = subset(map_com_sf_severe,
                             NAME_1 != "Tlaxcala" & NAME_1 != "Veracruz"),
                             aes(label = NAME_1), size = 2.5) +
  ggrepel::geom_text_repel(data = subset(map_com_sf_severe,
                                         NAME_1 %in% c("Tlaxcala", "Veracruz")),
                           aes(label = NAME_1, geometry = geometry),
                           stat = "sf_coordinates",
                           min.segment.length = 0, size = 2.5, direction = "x") +
  theme_void() +
  labs(fill = "Proportion\nof altered\ntallies") +
  theme(legend.position = c(.1,.25)) +
  coord_sf()

