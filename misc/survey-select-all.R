library(tidyverse)

museum_survey <-
    tribble(
        ~respondent_id , ~exhibits_visited                                                           , ~how_heard_about_museum                     , ~amenities_used                            ,
                     1 , "Ancient Egypt, Dinosaur Hall"                                              , "Friend or family"                          , "Gift shop, Cafe"                          ,
                     2 , "Space and Astronomy"                                                       , "Social media, Museum website"              , "Audio guide"                              ,
                     3 , "Dinosaur Hall, Modern Art, Ocean Life"                                     , "Friend or family, School or work"          , "Cafe, Coat check, Guided tour"            ,
                     4 , "Modern Art"                                                                , "Newspaper"                                 , "Gift shop"                                ,
                     5 , "Ancient Egypt, Ocean Life, Space and Astronomy"                            , "Social media"                              , "Cafe, Audio guide"                        ,
                     6 , "Dinosaur Hall"                                                             , "Friend or family, Social media, Newspaper" , "Gift shop, Cafe, Coat check, Audio guide" ,
                     7 , "Ancient Egypt, Dinosaur Hall, Modern Art, Ocean Life, Space and Astronomy" , "Museum website"                            , "Guided tour"                              ,
                     8 , "Ocean Life, Space and Astronomy"                                           , "School or work"                            , "Gift shop, Coat check"                    ,
                     9 , "Ancient Egypt, Modern Art"                                                 , "Friend or family, Museum website"          , "Cafe"                                     ,
                    10 , "Dinosaur Hall, Ocean Life"                                                 , "Social media, School or work"              , "Gift shop, Cafe, Guided tour"
    )

# One column per option, with 1 if the respondent selected it and 0 if not
museum_survey_wide <-
    museum_survey |>
    pivot_longer(
        cols = -respondent_id,
        names_to = "question",
        values_to = "response"
    ) |>
    separate_longer_delim(response, delim = ", ") |>
    mutate(
        response = str_to_lower(response) |> str_replace_all(" ", "_"),
        selected = 1
    ) |>
    pivot_wider(
        names_from = c(question, response),
        values_from = selected,
        values_fill = 0,
        names_sort = TRUE
    )

museum_survey_wide |>
    pivot_longer(
        cols = -respondent_id,
        names_to = "question"
    ) |>
    filter(value == 1) |>
    select(respondent_id, question) |>
    count(question)
