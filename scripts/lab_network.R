library(tidygraph)
library(ggraph)
library(tidyverse)
library(rvest)


pub_xpath <- '//*[@id="block-yui_3_17_2_1_1567798471690_21270"]/div/p'

journals <- c("PLoS Biology", "PNAS", "Cell Systems", "eLife", "ISME", 
              "Frontiers", "Annual", "Natur", "Current", "Science", "Scientific",
              "Evolution", "PL", "Lecture", "Bio", "mS", "Comm", "Journal")

coauthor_list <- read_html("http://www.sanchezlaboratory.com/pubs") %>%
    # Select the line of publication list
    html_elements("section") %>%
    # Separate individual publications
    html_elements("p") %>%
    html_text() %>%
    # Separate titles and authors
    str_split("\\(\\d{4}\\)") %>% sapply(function(x) x[2]) %>% 
    # Separate authors and journals
    str_split(paste0("(", paste(journals, collapse = ")|("), ")")) %>% sapply(function(x) x[1]) %>%
    `[`(!is.na(.)) %>%
    # Separate names
    str_split(",|( and )|(,and )|&")  %>%
    # Make a tibble
    lapply(function(x) tibble(Author = x)) %>%
    bind_rows(.id = "Paper") 

sanchez_lab <- c(
    "Chang-Yu Chang",
    "Jean Vila",
    "Maria Rebolleda-Gomez",
    "Nanxi Lu",
    "Sylvie Estrela",
    "Alicia Sanchez-Gorostiaga",
    "Djordje Bajic",
    "Alvaro Sanchez",
    "Joshua Goldford",
    "Madeline Bender",
    "Felipe Lino",
    "Juan Diaz-Colunga",
    "Nora Pyenson",
    "Xin Sun",
    "Abby Skwara",
    "Jackie Folmar"
)
sanchez_lab <- rev(sanchez_lab)


sanchez_lab_g1 <- c("Chang-Yu Chang",
                    "Jean Vila",
                    "Maria Rebolleda-Gomez",
                    "Nanxi Lu",
                    "Sylvie Estrela",
                    "Alicia Sanchez-Gorostiaga",
                    "Djordje Bajic",
                    "Alvaro Sanchez",
                    "Joshua Goldford",
                    "Madeline Bender")

# Clean up author names
paper_author_list <- coauthor_list %>%
    mutate(Author = str_replace(Author, "^\\s", "")) %>%
    mutate(Author = str_replace(Author, "\\s$", "")) %>%
    mutate(Author = str_replace(Author, "^\\.", "")) %>%
    mutate(Author = str_replace(Author, "\\.$", "")) %>%
    mutate(Author = str_replace(Author, "\\#$", "")) %>%
    mutate(Author = str_replace(Author, "\\*$", "")) %>% 
    mutate(Author = str_replace(Author, "^\\s", "")) %>%
    mutate(Author = str_replace(Author, "\\s$", ""))

# Clean up the name ambiguity
paper_author_list_sanchez <- paper_author_list %>%
    mutate(Author = ifelse(Author %in% c("Jean CC Vila", "Jean C.C. Vila", "Jean C. C. Vila"), "Jean Vila", Author)) %>%
    mutate(Author = ifelse(Author %in% c("María Rebolleda-Gomez"), "Maria Rebolleda-Gomez", Author)) %>%
    mutate(Author = ifelse(Author %in% c("Djordje Bajić", "Djordje Bajic"), "Djordje Bajic", Author)) %>%
    mutate(Author = ifelse(Author %in% c("Sanchez A"), "Alvaro Sanchez", Author)) %>%
    filter(Author %in% sanchez_lab)

# Nodes
df_nodes <- tibble(name = sanchez_lab) %>% mutate(Gen1 = name %in% sanchez_lab_g1)


# Edges
df_temp <- paper_author_list_sanchez %>%
    group_by(Paper) %>%
    mutate(AuthorNumber = n()) %>%
    filter(AuthorNumber >= 2)

df_edges <- df_temp %>%
    split.data.frame(f = .$Paper) %>%
    lapply(function(x) {
        temp <- t(combn(x$Author, 2))
        tibble(from = temp[,1], to = temp[,2])
    }) %>%
    bind_rows(.id = "Paper") %>%
    mutate(from = match(from, sanchez_lab), to = match(to, sanchez_lab))

df_edges[,c("from", "to")] <- df_edges %>%
    select(from, to) %>%
    apply(1, sort) %>% t()

df_edges <- df_edges %>%
    arrange(from, to) %>%
    group_by(from, to) %>%
    summarise(`Number of coauthored papers` = n())

# Graph
graph <- tbl_graph(nodes = df_nodes, edges = df_edges)

node_size = 3
p1 <- graph %>%
    ggraph(layout = "linear") +
    geom_edge_arc(aes(edge_width = `Number of coauthored papers`), color = "grey20", alpha = 0.5,
                  start_cap = circle(node_size/2+1, "mm"),
                  end_cap = circle(node_size/2+1, "mm")) +
    geom_node_text(aes(label = name, color = Gen1), size = 3, hjust = 1) +
    coord_flip() +
    scale_edge_width(breaks = c(1,4,9)) +
    scale_y_continuous(limits = c(-2, 5)) +
    scale_color_manual(values = c(`TRUE` = "#DB7469", `FALSE` = "#557BAA")) +
    guides(color = "none") +
    theme_classic() +
    theme(legend.position = "top", panel.background = element_rect(fill = "white"),
          axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
p1
ggsave(here::here("plots/lab_network_linear.png"), plot = p1, width = 6, height = 4)

node_size = 3
p2 <- graph %>%
    activate(nodes) %>% filter(name %in% sanchez_lab_g1) %>%
    ggraph(layout = "circle") +
    geom_edge_link(aes(edge_width = `Number of coauthored papers`), color = "grey20", alpha = 0.5,
                   start_cap = circle(node_size/2+1, "mm"),
                   end_cap = circle(node_size/2+1, "mm")) +
    geom_node_text(aes(label = name), size = 3, color = "black") +
    theme_classic() +
    scale_edge_width(breaks = c(1,4,9)) +
    scale_x_continuous(limits = c(-1.2, 1.2)) +
    scale_y_continuous(limits = c(-1.2, 1.2)) +
    theme(legend.position = "top", panel.background = element_rect(fill = "white"),
          axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

p2
ggsave(here::here("plots/lab_network.png"), plot = p2, width = 5, height = 5)






