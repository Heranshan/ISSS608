# Load packages
packages = c('ggiraph', 'plotly', 
             'DT', 'patchwork',
             'gganimate', 'tidyverse',
             'readxl', 'gifski', 'gapminder',
             'treemap', 'treemapify',
             'rPackedBar')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

# Read data
exam_data <- read_csv("data/Exam_data.csv")

# Tooltip effect
p1 <- ggplot(data=exam_data, 
             aes(x = MATHS)) +
  geom_dotplot_interactive(              
    aes(data_id = ID),              
    stackgroups = TRUE,                  
    binwidth = 1,                        
    method = "histodot") +  
  coord_cartesian(xlim=c(0,100)) +
  scale_y_continuous(NULL,               
                     breaks = NULL)
p2 <- ggplot(data=exam_data, 
             aes(x = ENGLISH)) +
  geom_dotplot_interactive(              
    aes(data_id = ID),              
    stackgroups = TRUE,                  
    binwidth = 1,                        
    method = "histodot") + 
  coord_cartesian(xlim=c(0,100)) +
  scale_y_continuous(NULL,               
                     breaks = NULL)
girafe(code = print(p1 / p2),
       width_svg = 6,
       height_svg = 6,
       options = list(
         opts_hover(css = "fill: #202020;"),
         opts_hover_inv(css = "opacity:0.2;")
       )
)

# Plotly
plot_ly(data = exam_data, 
        x = ~ENGLISH, 
        y = ~MATHS,
        text = ~paste("Student ID:", ID,     
                      "<br>Class:", CLASS),  
        color = ~RACE, 
        colors = "Set1") %>%
  layout(title = 'English Score versus Maths Score ',
         xaxis = list(range = c(0, 100)),
         yaxis = list(range = c(0, 100)))