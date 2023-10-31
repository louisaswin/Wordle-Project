wordle_list = readLines("wordle-answers-alphabetical.txt")

?strsplit
?substr

# want to separate each word by its letter 
# to do analysis on letter placement and letter frequencies

letters_list = lapply(wordle_list, function(word) strsplit(word, split = "")[[1]])
head(letters_list)

# go with this method instead
first_letter1 = sapply(wordle_list, function(word) substr(word, 1, 1))

wordle_df1 = data.frame(first_letter1)
head(wordle_df1)
# like this method
# want to automate it to do this for all 5 letters

letter_vector = function(word_list, position){
  sapply(wordle_list, function(word) substr(word, start=position, stop=position))
}

first_letter = letter_vector(wordle_list, 1)
sum(first_letter != first_letter1)
# check shows function works

second_letter = letter_vector(wordle_list, 2)
third_letter = letter_vector(wordle_list, 3)
fourth_letter = letter_vector(wordle_list, 4)
fifth_letter = letter_vector(wordle_list, 5)

# put into dataframe
wordle_df = data.frame(first_letter, second_letter, third_letter, fourth_letter, fifth_letter)
head(wordle_df)

class(first_letter)
wordle_df[] = lapply(wordle_df, factor)
summary(wordle_df)


## first letter
library(ggplot2)
ggplot(wordle_df, aes(x=first_letter)) +
  geom_bar(fill = "skyblue") +
  labs(x = "First Letter", y = "Frequency", title = "Frequency of First Letters") +
  theme_minimal()

ggplot(wordle_df, aes(x=first_letter)) +
  geom_bar(fill = "skyblue") +
  coord_polar(theta = "x") +
  labs(x = "First Letter", y = "Frequency", title = "Frequency of First Letters")

first = wordle_df %>%
  group_by(first_letter) %>%
  summarize(count = n()) %>%
  mutate(freq = count/sum(count),
         position = "1st") %>%
  arrange(desc(count))
first

ggplot(first, aes(x=first_letter, y=freq)) +
  geom_bar(stat= "identity")
#0.05 cutoff 

# 7 most common
head(first, 7)
# top 7 are s, c, b, t, p, a, & f
# where s is twice as common as runner up c
# only one vowel 

# 5 least common
tail(first, 5)
# bottom 5 are z, y, k, j, & q

# clear winners in first letter, specific letters are much more commonly used than many others
  # occurring more than 5% of the time 
# clear losers as well, occuring less than 1% of the time 



## second letter

second = wordle_df %>%
  group_by(second_letter) %>%
  summarize(count = n()) %>%
  mutate(freq = count/sum(count),
         position = "2nd") %>%
  arrange(desc(count))
second

ggplot(second, aes(x=second_letter, y=freq)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(x = "Second Letter", y = "Frequency", title = "Frequency of Second Letters") +
  theme_minimal()
# looks very different from 1st letter frequencies
# looks like about 1/3 of the letters are very common and then the other 2/3 aren't very common at all
# a lot more competition among letters for most popular letter

ggplot(wordle_df, aes(x=second_letter)) +
  geom_bar(fill = "lightgreen") +
  coord_polar(theta = "x") +
  labs(x = "Second Letter", y = "Frequency", title = "Frequency of Second Letters")

# 8 most common
head(second, 8)
# top 8 are a, o, r, e, i, l, u, h
# where top 4 occur over 10% of the time and all occur over 6% of the time 
# includes all vowels 

# 5 least common
tail(second, 5)
# bottom 5 are z, j, q, f, k
# same letters in bottom 5 as first letters with the exception of f replacing y 
# all occur less than 0.5% of the time



# maybe stretch these 



## third letter

third = wordle_df %>%
  group_by(third_letter) %>%
  summarize(count = n()) %>%
  mutate(freq = count/sum(count),
         position = "3rd") %>%
  arrange(desc(count))
third

ggplot(third, aes(x=third_letter, y=freq)) +
  geom_bar(stat = "identity", fill = "lightsalmon") +
  labs(x = "Third Letter", y = "Frequency", title = "Frequency of Third Letters") +
  theme_minimal()
# much more varied distribution 
# looks like if they were ordered in frequency it would a gradual descent

# 7 most common
head(third, 7)
# top 7 are a, i, o, e, u, r, n
# where top 3 occur over 10% of the time and all occur over 6% of the time 
# predictably, all vowels take the top 5 spots

# 6 least common
tail(third, 10)
# bottom 5 are q, j, h, z, x, k
# appearance of new least common letter: h 
# q beats z out for most least common letter for the first time 
# all occur less than 0.6% of the time




## fourth letter

fourth = wordle_df %>%
  group_by(fourth_letter) %>%
  summarize(count = n()) %>%
  mutate(freq = count/sum(count),
         position = "4th") %>%
  arrange(desc(count))
fourth

ggplot(fourth, aes(x=fourth_letter, y=freq)) +
  geom_bar(stat = "identity", fill = "lightpink") +
  labs(x = "Fourth Letter", y = "Frequency", title = "Frequency of Fourth Letters") +
  theme_minimal()
# it looks like all of the letters are used a lot more frequently than some of the other letter placements
  # such that there is only one stand out winner for most common letter, and only 3 stand out losers 
# betting the mean frequency for all of the letters is more representative for this graph than the other 3
# probably around 3.5%

#let's see
ggplot(fourth, aes(x=fourth_letter, y=freq)) +
  geom_bar(stat = "identity", fill = "lightpink") +
  labs(x = "Fourth Letter", y = "Frequency", title = "Frequency of Fourth Letters") +
  theme_minimal() + 
  geom_hline(yintercept = mean(fourth$freq), linetype = "dashed", color = "darkblue") + 
  annotate("text", x = "y", y = mean(fourth$freq) + .0025, label = "Mean", color = "black") +
  geom_hline(yintercept = median(fourth$freq), linetype = "dashed", color = "darkgreen") +
  annotate("text", x = "y", y = median(fourth$freq) + .0025, label = "Median", color = "black")

ggplot(fourth, aes(y = freq)) +
  geom_boxplot()

# 9 most common
head(fourth, 9)
# top 9 are e, n, s, a, l, i, c, r, t 
# where most common letter, e, occurs almost double as 2nd most common letter, n 
top 5 occur over 7% of the time and all occur over 6% of the time 
# u and o are the missing vowels

# 5 least common
tail(fourth, 5)
# bottom 5 are j, y, x , z, b
# appearance of new least common letter: b
# all occur less than 1% of the time
# bottom 3 occur less than 0.2% of the time







## fifth letter

fifth = wordle_df %>%
  group_by(fifth_letter) %>%
  summarize(count = n()) %>%
  mutate(freq = count/sum(count),
         position = "5th") %>%
  arrange(desc(count))
fifth

ggplot(fifth, aes(x=fifth_letter, y=freq)) +
  geom_bar(stat = "identity", fill = "plum") +
  labs(x = "Fifth Letter", y = "Frequency", title = "Frequency of Fifth Letters") +
  theme_minimal()
# very split, would look like an exponential curve 
# looks like we have the most popular letter yet for first place 
# and a high runner up

ggplot(fifth, aes(x= reorder(fifth_letter, -freq), y=freq)) +
  geom_bar(stat = "identity", fill = "plum") +
  labs(x = "Fifth Letter", y = "Frequency", title = "Frequency of Fifth Letters") +
  theme_minimal()


ggplot(fifth, aes(x=fifth_letter, y=freq)) +
  geom_bar(stat = "identity", fill = "plum") +
  labs(x = "Fifth Letter", y = "Frequency", title = "Frequency of Fifth Letters") +
  theme_minimal() + 
  geom_hline(yintercept = mean(fifth$freq), linetype = "dashed", color = "darkblue") + 
  annotate("text", x = "y", y = mean(fifth$freq) + .0025, label = "Mean", color = "black") +
  geom_hline(yintercept = median(fifth$freq), linetype = "dashed", color = "darkgreen") +
  annotate("text", x = "y", y = median(fifth$freq) + .0025, label = "Median", color = "black")


# 8 most common
head(fifth, 8)
# top 8 are e, y, t, r, l, h, n, d
# top 2 letters are very high
top 3 occur over 10% of the time and all occur over 5% of the time 
# u and o are the missing vowels

# 6 least common
tail(fifth, 6)
# bottom 6 are u, z, x, i, b, w
# first time that vowels are in the bottom 6, but makes sense
# appearance of new least common letter: b
# all occur less than 1% of the time
# all except w occur less than 0.05% of the time



# MISSING LETTERs
length(first$first_letter)
length(second$second_letter)
length(third$third_letter)
length(fourth$fourth_letter)
length(fifth$fifth_letter)
#missing letters in 1st, 4th, and 5th
missing_1 <- setdiff(letters, unique(as.character(first$first_letter)))
print(missing_letter_first)
missing_4 <- setdiff(letters, unique(as.character(fourth$fourth_letter)))
print(missing_4)
missing_5 <- setdiff(letters, unique(as.character(fifth$fifth_letter)))
print(missing_5)
#presumably this would suggest that a user shouldn't be able to these letters in these positions 
#therefore they are not necessarily deserving to be on the bottom list to avoid using 
# since it shouldn't be possible
# of course there are likely some exceptions 
# go back and add these 


# combine 1st 2nd through 5th into one data frame to see the frequencies of each letter 
  # for each letter placement

new_df = bind_rows(first, second, third, fourth, fifth, .id = "position")
(new_df)

new_df <- new_df %>%
  mutate(letter = coalesce(first_letter, second_letter, third_letter, fourth_letter, fifth_letter)) %>%
  select(-first_letter, -second_letter, -third_letter, -fourth_letter, -fifth_letter) 

new_df <- new_df %>%
  select(letter, position, freq, count)

new_df


# need to be added to data for more comprehensive stats
# first letter - missing x
# fourth letter - missing q
# fifth letter - missing j,q,v
missingletters <- tibble(letter = factor(c("x", "q", "j", "q", "v")), position = as.character(c(1, 4, 5, 5, 5)),
                         freq = c(0, 0, 0, 0, 0), count = as.integer(c(0, 0, 0, 0, 0)))

new_df <- new_df %>%
  bind_rows(missingletters)


#stacked graph
custom_colors = c("1" = "skyblue", "2" = "lightgreen", "3" = "lightsalmon", 
                  "4" = "lightpink", "5" = "plum")

ggplot(new_df, aes(letter, freq, fill = position)) +
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_manual(values = custom_colors, name = "Position") + 
  labs(x = "Letter", y = "Frequency", title = "Frequency of Letters") +
  theme_minimal()

ggplot(new_df, aes(position, freq, fill = position)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors) + 
  labs(x = "Position", y = "Frequency", title = "Frequency of Letters by Position") +
  theme_minimal() + 
  theme(legend.position = "none")

# outliers in 1, 3, 4, & 5



# barcharts ordered
ggplot(new_df, aes(x = letter, y = freq, fill = position)) +
  geom_bar(stat = "identity") +
  facet_grid(~ position) +
  scale_fill_manual(values = custom_colors) + 
  labs(x = "Letter", y = "Frequency", title = "Frequency of Letters by Position") +
  theme_minimal() 

ggplot(new_df, aes(x = letter, y = freq, fill = position)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ position) +
  scale_fill_manual(values = custom_colors) + 
  labs(x = "Letter", y = "Frequency", title = "Frequency of Letters by Position") +
  theme_minimal() +
  theme(panel.spacing = unit(1, "cm"))   # adjust for more space btwn graphs

ggplot(new_df, aes(x = letter, y = freq, fill = position)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ position, nrow = 3) + # graphs per row
  scale_fill_manual(values = custom_colors) + 
  labs(x = "Letter", y = "Frequency", title = "Frequency of Letters by Position") +
  theme_minimal() +
  theme(panel.spacing = unit(1, "cm"))   # adjust for more space btwn graphs

# graphs arent working
# not showing x labels


ggplot(new_df, aes(x = reorder(letter, -freq), y = freq, fill = position)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ position) +
  scale_fill_manual(values = custom_colors) + 
  labs(x = "Letter", y = "Frequency", title = "Frequency of Letters by Position") +
  theme_minimal() +
  theme(panel.spacing = unit(1, "cm"))   # adjust for more space btwn graphs

#doesnt work 




pos1 = new_df %>%
  filter(position == "1")
pos2 = new_df %>%
  filter(position == "2")
pos3 = new_df %>%
  filter(position == "3")
pos4 = new_df %>%
  filter(position == "4")
pos5 = new_df %>%
  filter(position == "5")

library(patchwork)
  
p1 = ggplot(pos1, aes(x= reorder(letter, -freq), y=freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Letter", y = "Frequency", title = "Frequency of Letters in 1st Position") +
  theme_minimal()
p2 = ggplot(pos2, aes(x= reorder(letter, -freq), y=freq)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(x = "Letter", y = "Frequency", title = "Frequency of Letters in 2nd Position") +
  theme_minimal()
p3 = ggplot(pos3, aes(x= reorder(letter, -freq), y=freq)) +
  geom_bar(stat = "identity", fill = "lightsalmon") +
  labs(x = "Letter", y = "Frequency", title = "Frequency of Letters in 3rd Position") +
  theme_minimal()
p4 = ggplot(pos4, aes(x= reorder(letter, -freq), y=freq)) +
  geom_bar(stat = "identity", fill = "lightpink") +
  labs(x = "Letter", y = "Frequency", title = "Frequency of Letters in 4th Position") +
  theme_minimal()
p5 = ggplot(pos5, aes(x= reorder(letter, -freq), y=freq)) +
  geom_bar(stat = "identity", fill = "plum") +
  labs(x = "Letter", y = "Frequency", title = "Frequency of Letters in 5th Position") +
  theme_minimal()
combined_plot <- p1 + p2 + p3 + p4 + p5
print(combined_plot)



new_df

stats_df = new_df %>%
  group_by(letter) %>%
  summarize(count = sum(count)) %>%
  mutate(freq = count/sum(count))

stats_df1 = stats_df %>%
  arrange(desc(freq))

#u messed up 
sum(stats_df1$count)
length(wordle_list)*5

ggplot(stats_df, aes(y=freq)) +
  geom_boxplot()
#no outliers for all of them 



1s3a5en//2a4e
overall : e/a/r/o/t/l/i/s/n/c

slane
aoreilu
  
  
  
which(wordle_list == "slane")
  
head(wordle_df)

no_e_rows <- wordle_df %>%
  filter(!(first_letter == "e" | 
         second_letter == "e" | 
         third_letter == "e" |
         fourth_letter == "e" | 
         fifth_letter == "e"))

nrow(no_e_rows)
nrow(wordle_df)
1259/2315

# for overall stats you are counting letter's twice
letters
length(letters)

props = 0
for (i in 1:26){
  prop = length(wordle_list[!grepl((wordle_list[i]), wordle_list)])/length(wordle_list)
  props = c(prop, props)
  
}
props
no_e_words <- wordle_list[!grepl("a", wordle_list)]
length(wordle_list[!grepl(letters[1], wordle_list)])

sum(no_e_words == noe)


