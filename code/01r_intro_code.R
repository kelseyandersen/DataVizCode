
## --------------------------------------------------------------------------------------------
x <- 1
y <- 2
x + y


## --------------------------------------------------------------------------------------------
1+3


## ----custom_functions------------------------------------------------------------------------
# I am a comment!!! Just here to help jog the memory later on...
# Let us make a function!
addition <- function(argument_one,
                     argument_two){ 
  argument_one + argument_two # operations
} # curly brackets define operations

ls() # check content of the environment
addition(argument_one = x,
         argument_two = y)




addition(x, y)# Notice the difference?!
addition(x, y) == x+y #notice double "="
all.equal(addition(x, y), x+y) #Same as above, but pre-made




## --------------------------------------------------------------------------------------------
num <- c(50, 60, 65) 

char <- c("mouse", "rat", "dog") 

fct <- factor("low", "med", "high")

dates <- as.Date(c("02/27/92", "02/27/92", "01/14/92"), "%m/%d/%y")

logical <-  c(FALSE, FALSE, TRUE) # only TRUE or FALSE


## --------------------------------------------------------------------------------------------
num[1] # 1st element
num[num >= 60] # More than or equal
char == "dog" # see logical on the left
char[logical]
char[char == "dog"]


## --------------------------------------------------------------------------------------------
df <- data.frame(col_one = num,
                 col_two = char)
print(df)
head(df,1)


## --------------------------------------------------------------------------------------------
df[1, 1] # [rows, columns]
df[, 1] # 1st column in the data frame
df[, -2] # Exclude 2nd column
df[2:3, "col_two"] 
df$col_two


## ----libs, echo = FALSE----------------------------------------------------------------------
library("tidyverse")
library("here")


## --------------------------------------------------------------------------------------------
dt <- survey_data <- read_csv("https://raw.githubusercontent.com/emdelponte/paper-coffee-rust-Ethiopia/master/data/survey_clean.csv")
tibble::glimpse(dt, 70)



## --------------------------------------------------------------------------------------------
(dt_small <- 
dt %>%
  select(cultivar, zone, inc) %>% 
  group_by(cultivar, zone) %>%
  slice(head(row_number(), 1)) %>% 
  filter(
    zone =="Sheka" | zone ==  "Sidama") %>% 
  ungroup())


## ----select----------------------------------------------------------------------------------
dt_small %>% 
  select(cultivar, inc) %>% 
  filter(inc <= 17)


## --------------------------------------------------------------------------------------------
dt_small %>%
  group_by(cultivar) %>%
  summarize(mean_inc = mean(inc),
            min_weight = min(inc)) %>%
    arrange(desc(mean_inc))



## --------------------------------------------------------------------------------------------
dt_small


## --------------------------------------------------------------------------------------------
(dt_small_wide <- 
dt_small %>%
  pivot_wider(names_from = "zone", 
              values_from = "inc"))


## --------------------------------------------------------------------------------------------
dt_small_wide 


## --------------------------------------------------------------------------------------------
dt_small_wide %>% 
  pivot_longer(cols = 
                 c("Sheka", "Sidama"), 
               names_to = "zone",
               values_to = "inc")

