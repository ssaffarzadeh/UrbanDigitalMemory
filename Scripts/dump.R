# synth <- read.csv("data/Newest_tehran_city_theater_square_dataset.csv", stringsAsFactors = FALSE)

# Define the emoji-to-text map
# emoji_map <- c(
#   "😊" = "happy",
#   "😢" = "sad",
#   "😠" = "angry",
#   "😡" = "super angry",
#   "😍" = "love",
#   "🌿" = "nature",
#   "😂" = "laughing",
#   "😞" = "unhappy",
#   "😎" = "cool",
#   "😨" = "scared",
#   "😤" = "frustrated",
#   "💔" = "sad",      # Cracked heart emoji for sadness
#   "❤️" = "love",    # Normal heart emoji for love
#   "💖" = "love",     # Heart with sparks emoji for love
#   "😐" = "neutral",
#   "😕" = "confused",
#   "😌" = "content",
#   "😔" = "sorrowful",
#   "😏" = "smirking",
#   "😒" = "unamused",
#   "😣" = "worried",
#   "😥" = "disappointed",
#   "😇" = "innocent",
#   "😈" = "devilish",
#   "🤔" = "uncertain",
#   "🙄" = "awkward"
# )




# # Local emoji dictionary
# emoji_map <- c(
#   # Happy
#   "😊" = "happy", "😃" = "happy", "😄" = "happy", "😁" = "happy", "😆" = "happy",
# 
#   # Sad
#   "😢" = "sad", "😭" = "sad", "😞" = "sad", "😟" = "sad", "😥" = "sad", "😔" = "sad",
#   "😣" = "sad", "😖" = "sad", "💔" = "sad",
# 
#   # Angry
#   "😠" = "angry", "😡" = "super angry",
# 
#   # Love
#   "😍" = "love", "❤️" = "love", "💖" = "love", "💘" = "love", "💝" = "love",
#   "💗" = "love", "💓" = "love",
# 
#   # Nature
#   "🌿" = "nature", "🌳" = "nature", "🌲" = "nature", "🌴" = "nature",
#   "🌵" = "nature", "🍃" = "nature", "🌷" = "nature", "🌸" = "nature",
#   "🌺" = "nature", "🌼" = "nature", "🌻" = "nature",
# 
#   # Laughing
#   "😂" = "laughing",
# 
#   # Neutral
#   "😐" = "neutral", "😑" = "neutral",
# 
#   # Confused
#   "😕" = "confused", "🤔" = "confused",
# 
#   # Cool
#   "😎" = "cool",
# 
#   # Scared
#   "😨" = "scared", "😰" = "scared",
# 
#   # Frustrated
#   "😤" = "frustrated",
# 
#   # Content
#   "😌" = "content",
# 
#   # Sorrowful
#   "😢" = "sorrowful",
# 
#   # Smirking
#   "😏" = "smirking",
# 
#   # Unamused
#   "😒" = "unamused",
# 
#   # Disappointed
#   "😞" = "disappointed",
# 
#   # Innocent
#   "😇" = "innocent",
# 
#   # Devilish
#   "😈" = "devilish",
# 
#   # Uncertain
#   "🤨" = "uncertain",
# 
#   # Awkward
#   "🙄" = "awkward",
# 
#   # Excited
#   "😃" = "excited",
# 
#   # Surprised
#   "😲" = "surprised", "😮" = "surprised",
# 
#   # Tired
#   "😪" = "tired", "🥱" = "tired",
# 
#   # Sick
#   "🤒" = "sick", "🤢" = "disgusted",
# 
#   # Celebrate
#   "🎉" = "celebrate",
# 
#   # Party
#   "🎊" = "party",
# 
#   # Fire
#   "🔥" = "fire",
# 
#   # Star
#   "⭐" = "star", "🌟" = "star",
# 
#   # Weather
#   "☀️" = "sunny", "🌧️" = "rainy", "❄️" = "snowy", "🌈" = "rainbow",
# 
#   # Travel
#   "✈️" = "travel", "🌍" = "travel",
# 
#   # Food
#   "🍕" = "food", "🍔" = "food", "🍟" = "food", "🍝" = "food",
# 
#   # Drink
#   "☕" = "coffee", "🍷" = "wine", "🍹" = "cocktail",
# 
#   # Animals
#   "🐶" = "dog", "🐱" = "cat", "🐦" = "bird", "🐟" = "fish"
# )


# # Function to replace emojis with text
# replace_emojis <- function(text) {
#   for (emoji in names(emoji_map)) {
#     text <- stringr::str_replace_all(text, fixed(emoji), emoji_map[[emoji]])
#   }
#   return(text)
# }
# 
# # Apply emoji replacement
# synth$text <- sapply(synth$text, replace_emojis)

oldspatialtopic_names <- c( "0" = "General Street Life & Nostalgia",
                            "1" = "Neighborhood Hubs & Squares (Nazi Abad)",
                            "2" = "Grand Bazaar & Market Shopping",
                            "4" = "Toopkhaneh Square Naming & War",
                            "5" = "City Theater Metro & Underpass",
                            "11" = "Metro Infrastructure & Tajrish Station",
                            "12" = "Avenues, Gardens & Boulevards",
                            "13" = "Performing Arts & Festivals (City Theater)",
                            "16" = "Valiasr Street Connectivity & Length",
                            "18" = "Tree Preservation & Urban Ecology",
                            "19" = "Urban Management & Heritage Protection(CT)",
                            "21" = "Atmospheric Streets & Alleys",
                            "29" = "Architectural Heritage (Hassan Abad)",
                            "31" = "Metro Station Entrances",
                            "32" = "Seasonal Walks (Autumn in Valiasr)",
                            "43" = "Urban Decay & Street Dogs (Vanak)",
                            "46" = "Hassan Abad Heritage Houses",
                            "52" = "Historical Renaming of Squares",
                            "82" = "Cafe Culture & Solitude",
                            "88" = "Historical Gates (Bab Homayoun)")

oldspatial_ids <- c(0, 1, 2, 4, 5, 11, 12, 13, 16, 18, 19, 21,
                    31, 32, 46, 52, 82, 88)

old_spatial_tweets <- tweets |>
  select(-X)|>
  filter(final_topic %in% spatial_ids)|>
  mutate(Topic = final_topic)|>
  mutate(eng_name = spatialtopic_names[as.character(Topic)]) |>
  # filter(!is.na(eng_name))|>
  left_join(topic_count, by = "Topic")|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year != "2021")|>
  mutate(timeline = if_else(year < 2024, "Before_24", "After_24"))|>
  select(doc_id, text, txtenglish, year, Topic, eng_name, Count, 
         sentiment_label, sentiment_score, type, timeline, createdAt, 
         all_of(spatial_columns))

alltopic_for24 <- tweets|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year < 2024 & type == "ct")|>
  select(doc_id, text, year, type, final_topic, txtenglish, sentiment_label)|>
  view()

notct_alltopic_for24 <- tweets|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year < 2024 & type == "not_ct")|>
  select(doc_id, text, year, type, final_topic, txtenglish, sentiment_label)|>
  view()

notct_alltopic_after24 <- tweets|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year >= 2024 & type == "not_ct")|>
  select(doc_ic, text, year, type, final_topic, txtenglish, sentiment_label)|>
  view()

alltopic_after24 <- tweets|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year >= 2024 & type == "ct")|>
  select(doc_id, text, year,type, final_topic, txtenglish, sentiment_label)|>
  view()


