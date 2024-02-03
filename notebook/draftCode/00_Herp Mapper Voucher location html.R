
read_html("https://www.herpmapper.org/record/127244",
          encoding = "UTF-8") %>% 
  html_elements("body") %>% 
  html_elements("div.thumbnail")