library(tidyverse)
library(bibliometrix)
library(ggwordcloud)
library(ggsci)

# Read metadata
metadata <- bibliometrix::convert2df(here::here("data/my_publication.bib"), format = "bibtex", dbsource = "wos") %>%
    as_tibble()

if (FALSE) {
# Retrieve keywords from metadata
df <- metadata$ID %>%
    tolower() %>%
    str_split("; ") %>% 
    unlist() %>%
    str_replace(";", "") %>%
    tibble(Word = .) %>%
    group_by(Word) %>%
    count(name = "Count") %>%
    arrange(desc(Count)) %>%
    ungroup()
    
}

# Retrieve abstract words
df <- metadata$AB %>%
    tolower() %>%
    str_split(" ") %>%
    unlist() %>%
    str_replace("\\.", "") %>% str_replace(",", "") %>% 
    str_replace("'", "") %>% str_replace("' ", "") %>% str_replace("`", "") %>% str_replace(" `", "") %>%
    str_replace("ies", "y") %>%
    str_replace("ms$", "m") %>%
    str_replace("ns$", "n") %>% 
    str_replace("ts$", "t") %>% 
    str_replace("ls$", "l") %>% 
    tibble(Word = .) %>%
    group_by(Word) %>%
    count(name = "Count") %>%
    arrange(desc(Count)) %>%
    ungroup() %>% 
    filter(!Word %in% c("the", "of", "to", "a", "and", "in", "we", 
                        "for", "are", "that", "as", "be", "by", "used", 
                        "with", "an", "is", "may", "first", "was", "at", 
                        "do", "have", "how", "they", "this", "any", "were",
                        "or", "our", "into", "from", "new", "where",
                        "here", "on", "both", "show", "can", "when", "been", 
                        "let", "there", "under", "will", "some", "which", "not",
                        "know", "find", "even", "date", "from", "has", "us", "well",
                        "allow", "suited", "allows", "then", "these", "than",
                        "their", "form", "set", "main", "result", "particularly",
                        "second", "range", "identify", "review", "understanding", "number",
                        "decades", "large"
                        )) %>%
    filter(Count != 1)

# 
set.seed(123)
p <- df %>%
    mutate(Angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(80, 20))) %>%
    mutate(Color = factor(sample.int(10, nrow(df), replace = TRUE))) %>% 
    ggplot(aes(label = Word, size = Count, color = Color, angle = Angle)) +
    #geom_text_wordcloud() +
    geom_text_wordcloud_area(rm_outside = TRUE) +
    scale_size_area(max_size = 10) +
    scale_color_npg() +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA))

ggsave(here::here("plots/wordcloud.png"), p, width = 8, height = 4)





