#####################################################################
### Course: Text Mining                                         #####
### Subject: 2024-2                                             #####
### Title: Beyond the Questions                                 #####
### Topic: Analyzing Interview Data for Strategic Preparation   #####
### TEAM: 05                                                    #####   
### Member: 21900707 YoungWoo Cho                               #####
###         22200022 Jiho Kang                                  #####
###         22200130 Yewon Kim                                  #####
#####################################################################

library(jsonlite)
library(zip)
library(dplyr)
library(magrittr)
library(stringr)
library(tidyr)
library(tm)
library(tidytext)
library(KoNLP) 
library(extrafont)
library(showtext)
library(RmecabKo)
library(reticulate)
library(ggplot2)
library(wordcloud2)
library(ggwordcloud)
library(patchwork)
library(topicmodels)
library(gridExtra)
library(LDAvis)
library(stm)
library(keras)
library(igraph)
library(NLP)
library(ggraph)
library(widyr)

#######################################################
###############     0. Data loading     ###############
#######################################################

# Step1. Set the data file direction
directory_path <- "/Users/joy/Desktop/2024-2/Text\ Mining/TeamProject/TM\ Team\ Project/data\ 3"

# Step2. Get list of all folders in the directory
subfolders <- list.dirs(path = directory_path, recursive = TRUE, full.names = TRUE)[21:48]

# Step3. Save JSON file in each subfolder
subfolder_json_data <- lapply(subfolders, function(subfolder) {
  # Get JSON file names
  json_files <- list.files(path = subfolder, pattern = "\\.json$", full.names = TRUE)
  # Read JSON file
  json_data <- lapply(json_files, function(file) {
    tryCatch({
      fromJSON(file, flatten = TRUE)
    }, error = function(e) {
      # When an error occurs, print a message and return NULL
      message(sprintf("Error occurred: %s\nSkipping the file. (%s)", e$message, file))
      NULL
    })
  })
  # Transform to list
  list(subfolder = subfolder, json_data = json_data)
})

# Step4. Create data_frame containing data we need
data_frame <- do.call(rbind, lapply(subfolder_json_data, function(folder) {
  # Extract file path
  subfolder_path <- folder$subfolder
  # Extract question text
  questions <- unlist(lapply(folder$json_data, function(json) {
    tryCatch({
      json$dataSet$question$raw$text
      # Error case
    }, error = function(e) {
      NULL
    })
  }), use.names = FALSE)
  # Combine subfolder path and question text
  if (length(questions) > 0) {
    data.frame(Subfolder = subfolder_path, Question = questions, stringsAsFactors = FALSE)
    # Return NULL if there is no question
  } else {
    NULL  
  }
}))

# Step5. Create new column containing new information using file path column
data_frame %<>%
  mutate(
    Category = str_match(Subfolder, "TL_\\d+\\.([A-Za-z]+)_([A-Za-z]+)_([A-Za-z]+)")[, 2],
    Gender = str_match(Subfolder, "TL_\\d+\\.([A-Za-z]+)_([A-Za-z]+)_([A-Za-z]+)")[, 3],
    Experience = str_match(Subfolder, "TL_\\d+\\.([A-Za-z]+)_([A-Za-z]+)_([A-Za-z]+)")[, 4]
  ) %>% select(-1) %>% distinct()

# Step6. Check the result
data_frame %>% head()

######## Font installation for Korean display ########

font_import(prompt = FALSE)
loadfonts(device = "pdf")
fonts()

font_add_google("Nanum Gothic", "nanum")
showtext_auto()
font_families()

###############   1. Data Exploration   ###############

# Check data structure
str(data_frame)

# tibble [58,787 × 7] (S3: tbl_df/tbl/data.frame)
# $ Question  : chr [1:58787] "요즘 논쟁이 많은 우버 택시 찬반에 대해서 논해 보시겠습니까" "요즘 논쟁이 많은 우버 택시 찬반에 대해서 논해 보시겠습니까" "요즘 논쟁이 많은 우버 택시 찬반에 대해서 논해 보시겠습니까" "요즘 논쟁이 많은 우버 택시 찬반에 대해서 논해 보시겠습니까" ...
# $ Category  : chr [1:58787] "Management" "Management" "Management" "Management" ...
# $ Gender    : chr [1:58787] "Female" "Female" "Female" "Female" ...
# $ Experience: chr [1:58787] "Experienced" "Experienced" "Experienced" "Experienced" ...
# $ char_count: int [1:58787] 33 33 33 33 33 89 89 89 89 89 ...
# $ word_count: int [1:58787] 9 9 9 9 9 22 22 22 22 22 ...
# $ words     : chr [1:58787] "논쟁" "우버" "택시" "찬반" ...

# Summary Statistics
summary(data_frame)

# Question           Category            Gender           Experience          char_count       word_count      words   
# Length:58787       Length:58787       Length:58787       Length:58787       Min.   :  9.00   Min.   : 2.00   Length:58787  
# Class :character   Class :character   Class :character   Class :character   1st Qu.: 61.00   1st Qu.:15.00   Class :character
# Mode  :character   Mode  :character   Mode  :character   Mode  :character   Median : 70.00   Median :18.00   Mode  :character  
# Mean   : 71.66   Mean   :17.96  
# 3rd Qu.: 82.00   3rd Qu.:20.00  
# Max.   :132.00   Max.   :39.00  
       

# Check NA
colSums(is.na(data_frame))

# [Result]
# Question   Category   Gender 
# 0          0          0 
# Experience char_count word_count 
# 0          0          0 

# Check the sample in the main text column
head(data_frame$Question, 5)
table(data_frame$Category)

# [Result]
# "지금 지원자분이 제일 가고 싶은 회사가 어딜까요"                                                                      
# "지원자님은 이전 직장을 그만둔 어떤 특별한 이유가 있습니까 그리고 현 직장도 혹시 같은 이유로 그만둘 가능성이 있을까요"
# "지원자님은 이전 직장을 그만둔 어떤 특별한 이유가 있습니까 그리고 현 직장도 혹시 같은 이유로 그만둘 가능성이 있을까요"
# "지원자님은 이전 직장을 그만둔 어떤 특별한 이유가 있습니까 그리고 현 직장도 혹시 같은 이유로 그만둘 가능성이 있을까요"
# "지원자님은 이전 직장을 그만둔 어떤 특별한 이유가 있습니까 그리고 현 직장도 혹시 같은 이유로 그만둘 가능성이 있을까요"

set.seed(123)
data_frame %>% filter(Category=="Management" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="Management" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Male" & Experience == "New") %>% nrow()

data_frame %>% filter(Category=="Management" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="Management" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Male" & Experience == "Experienced") %>% nrow()

#####################################
row_remove <- data_frame %>%
  filter(Category=="Management" & Gender=="Male" & Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 3900)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category=="Management" & Gender=="Female" & Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 7400)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category=="Management" & Gender=="Male" & Experience == "Experienced") %>%  # 조건 필터링
  slice_sample(n = 800)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category=="Management" & Gender=="Female" & Experience == "Experienced") %>%  # 조건 필터링
  slice_sample(n = 500)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")

row_remove <- data_frame %>%
  filter(Category=="Design" & Gender=="Female" & Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 3800)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category=="Design" & Gender=="Male" & Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 1050)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")


row_remove <- data_frame %>%
  filter(Category=="ICT" & Gender=="Male" & Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 2600)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category=="ICT" & Gender=="Female" & Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 1100)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")


row_remove <- data_frame %>%
  filter(Category == "ProductionManufacturing", Gender == "Female", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 2800)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "ProductionManufacturing", Gender == "Male", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 1800)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")


row_remove <- data_frame %>%
  filter(Category == "PublicService", Gender == "Female", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 9050)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "PublicService", Gender == "Male", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 3300)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "PublicService", Gender == "Female", Experience == "Experienced") %>%  # 조건 필터링
  slice_sample(n = 1100)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "PublicService", Gender == "Male", Experience == "Experienced") %>%  # 조건 필터링
  slice_sample(n = 400)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")


row_remove <- data_frame %>%
  filter(Category == "SalesMarketing", Gender == "Female", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 5700)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "SalesMarketing", Gender == "Male", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 2400)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "SalesMarketing", Gender == "Female", Experience == "Experienced") %>%  # 조건 필터링
  slice_sample(n = 300)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "SalesMarketing", Gender == "Male", Experience == "Experienced") %>%  # 조건 필터링
  slice_sample(n = 600)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")

row_remove <- data_frame %>%
  filter(Category == "RND", Gender == "Male", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 1600)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")
row_remove <- data_frame %>%
  filter(Category == "RND", Gender == "Female", Experience == "New") %>%  # 조건 필터링
  slice_sample(n = 1100)
data_frame <- data_frame %>%
  anti_join(row_remove, by = "Question")

data_frame %>% filter(Category=="Management" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="Management" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Male" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Female" & Experience == "New") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Male" & Experience == "New") %>% nrow()

data_frame %>% filter(Category=="Management" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="Management" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="Design" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ICT" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="ProductionManufacturing" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="PublicService" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="SalesMarketing" & Gender=="Male" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Female" & Experience == "Experienced") %>% nrow()
data_frame %>% filter(Category=="RND" & Gender=="Male" & Experience == "Experienced") %>% nrow()

# Check interview question length and distribution
data_frame$char_count <- nchar(data_frame$Question)
data_frame$word_count <- sapply(strsplit(data_frame$Question, "\\s+"), length)

summary(data_frame$char_count)
# Min. 1st Qu.  Median  Mean 3rd Qu.    Max.
# 9.00   60.00   70.00  71.25   82.00  131.00
 
summary(data_frame$word_count)
# Min. 1st Qu.  Median   Mean  3rd Qu.    Max. 
# 2.00   15.00   18.00  17.92   21.00   39.00 

hist(data_frame$char_count, main = "Character Count Distribution", xlab = "Character Count")
hist(data_frame$word_count, main = "Word Count Distribution", xlab = "Word Count")

# [Interpretation]
# * word 
# There are questions between at least 9 and 131 characters, with an average of 71 characters.
# Multiple questions are distributed in the range of 61 to 82 characters. Most questions are relatively constant in length. 
# It can be seen that the distribution of interview questions is similar to the normal distribution and is balanced.


###############   2. Data Preprocessing   ###############

# Unnecessary symbol removal
data_frame <- data_frame %>%
  mutate(Question = str_replace_all(Question, "[^가-힣\\s]", "")) %>%
  mutate(Question = str_squish(Question))

# Morphological analysis and noun extraction: using KoNLP
useNIADic()

data_frame <- data_frame %>%
  mutate(words = sapply(Question, extractNoun)) %>%
  unnest(words) # Spread the list to frame the data

# Check the frequency of words
word_frequency <- data_frame %>%
  count(words, sort = TRUE) %>%
  head(50)

print(word_frequency, n=50)

# [Result] 
# * We were able to confirm the existence of meaningless words consisting of Korean alphabet in frequently appearing words.
#    words      n
# 1  해      9714
# 2  지원자  7362
# 3  말씀    7134
# 4  님      6748
# 5  것      4701
# 6  생각    4433
# 7  한      4015
# 8  본인    3688
# 9  일      3519
# 10 무엇    3062
# 11 수      2857
# 12 설명    2716
# 13 이유    2597
# 14 때      2033
# 15 하시    1868
# 16 궁금    1830
# 17 하      1803
# 18 적      1791
# 19 회사    1783
# 20 업무    1722
# 21 경험    1716
# 22 하게    1641
# 23 그것    1530
# 24 해결    1516
# 25 할      1401
# 26 중      1246
# 27 노력    1183
# 28 상황    1149
# 29 번      1133
# 30 분      1130
# 31 공부    1091
# 32 들       996
# 33 직무     969
# 34 방법     956
# 35 지원     952
# 36 합       890
# 37 수행     825
# 38 하기     817
# 39 우리     808
# 40 분야     774
# 41 점       746
# 42 면접자   718
# 43 문제     701
# 44 가지     696
# 45 관리     696
# 46 입사     695
# 47 자신     693
# 48 지       676
# 49 시간     663
# 50 직장     645

# Remove one-character word 
data_frame <- data_frame %>%
  filter(str_length(words) > 1)

# Check the word frequency 
word_frequency <- data_frame %>%
  count(words, sort = TRUE) %>%
  head(50)
print(word_frequency, n=50)

# Load Stopword file
stopwords_ko <- readLines("stopwords-ko.txt", encoding = "UTF-8")
additional_stopwords <- c("면접자", "그것","지원자", "말씀", "하신", "있습니다", "있으세요", "생각", "본인", "이유",
                          "설명", "궁금", "하시", "하게", "그것", "하기", "구체", "주시", "가지", "해서", 
                          "면접", "부탁드리겠습니", "얘기", "평소", "주시겠습니", "좋겠습니", "주십시", "들이", "일하")

data_frame <- data_frame %>%
  filter(!words %in% c(stopwords_ko, additional_stopwords))

word_frequency <- data_frame %>%
  count(words, sort = TRUE) %>%
  head(50)
print(word_frequency, n=50)

# Check the top 20 words (based on frequency)
top_20_words <- word_frequency %>% head(20)
print(top_20_words)

#     words        n
# 1  회사      1783
# 2  업무      1722
# 3  경험      1716
# 4  해결      1516
# 5  노력      1183
# 6  상황      1149
# 7  공부      1091
# 8  직무       969
# 9  방법       956
# 10 지원       952
# 11 수행       825
# 12 분야       774
# 13 문제       701
# 14 관리       696
# 15 입사       695
# 16 직장       645
# 17 단점       643
# 18 장점       637
# 19 프로젝트   637
# 20 갈등       627

ggplot(top_20_words, aes(x = reorder(words, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Most Frequently Used Words in Job Interviews",
    x = "Words",
    y = "Frequency"
  ) +
  theme_minimal()

# [Result]
# Words such as "experience", "company", "solution", "work", "study", and "job" are ranked highly.
# Overall, interview questions tend to emphasize the <interviewee's career background, job-related competencies, and problem-solving abilities.>
# Specifically, keywords like "effort", 'method", and "application" indicate that many questions provide opportunities for candidates to explain <their own approaches> and <motivated attitudes>.
# Words like "stress" and "overcome" suggest that many questions focus on evaluating <the interviewee's coping skill and psychological stability>. 

# [Get some Insight Here]
# Interview questions appear to focus on evaluating <core competencies> related to actual job performance, such as experience, problem-solving, and stress management.
# Therefore, interviewees should prepare specific examples of their problem-solving experiences and the lessons learned from them.
# It also seems important for interviewees to effectively express their psychological resilience, including examples of how they handled <stressful situations>.
# Based on this data, it is expected to be useful for analyzing question trends by job category or generating tailored question examples.


###############   3. Data Analysis   ###############

# 1️⃣ TF-IDF

# 1. Analysis of Keywords by Job Category

# Word frequency calculation by job category
category_keywords <- data_frame %>%
  group_by(Category, words) %>%
  summarise(Frequency = n()) %>%
  arrange(Category, desc(Frequency))

# Delete Design stopword 
additional_stopword <- c("디자인","디자이너", "경영","마케팅","영업")
# Keywords that were too directly related to the field were removed.

# Removing duplicates and aggregating frequency counts for each document-word pair.

category_keywords <- category_keywords %>%
  filter(!words %in% additional_stopword) %>%
  ungroup()

# Calculate TF-IDF 
category_idf_df <- category_keywords %>%
  bind_tf_idf(words, Category, Frequency) %>%
  arrange(Category, desc(tf_idf))

# Visualization: Top 10 Keywords by TF-IDF
ggplot(category_idf_df %>% group_by(Category) %>% top_n(10, tf_idf), 
       aes(x = reorder(words, tf_idf), y = tf_idf, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ Category, scales = "free_y") +
  labs(title = "Top TF-IDF Keywords by Category", 
       x = "Keyword", 
       y = "TF-IDF") +
  theme_minimal()


# [Result]
# By Job Category (1 sentence each) :  

# Design : Keywords reflect creativity, trends, and branding, emphasizing innovation and aesthetic appeal.  
# ICT : Focus on coding, algorithms, and development highlights technical expertise and problem-solving.  
# Management : Terms like strategy, logistics, and teamwork suggest leadership and organizational focus.  
# Production/Manufacturing : Keywords like equipment and engineering underscore operational efficiency and technical knowledge.  
# Public Service : Emphasis on addressing complaints and serving the public indicates customer service and accountability.  
# R&D : Technical and scientific terms highlight analytical thinking and advanced problem-solving.  
# Sales/Marketing : Keywords reflect persuasion, branding, and channel management, focusing on driving revenue.

# [Insights]  
# Depending on the key skills emphasized in each role, 
# candidates should prepare experiences and portfolios that demonstrate technical expertise and professional skills. 
# For instance, candidates for design roles should highlight creativity and trend awareness, 
# while those in ICT roles should focus on programming skills and algorithm understanding. 

# 2. Alysis of key words by Gender

# Word frequency calculation by gender
gender_keywords <- data_frame %>%
  group_by(Gender, words) %>%
  summarise(Frequency = n()) %>%
  arrange(Gender, desc(Frequency))

# Delete Production Manufacture stopword 
male_stopword <- c("대해")

# Removing duplicates and aggregating frequency counts for each document-word pair.
gender_keywords <- gender_keywords %>%
  filter(!words %in% male_stopword) %>%
  ungroup()

# Calculate TF-IDF 
gender_idf_df <- gender_keywords %>%
  bind_tf_idf(words, Gender, Frequency) %>%
  arrange(Gender, desc(tf_idf))

# Visualization : Top 10 Keywords by TF-IDF 
ggplot(gender_idf_df %>% group_by(Gender) %>% top_n(10, tf_idf), 
       aes(x = reorder(words, tf_idf), y = tf_idf, fill = Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ Gender, scales = "free_y") +
  labs(title = "Top TF-IDF Keywords by Gender", 
       x = "Keyword", 
       y = "TF-IDF") +
  theme_minimal()

# Result
# [Female]
# Female interview questions often focus on social ethics 
# (e.g., corruption, public office) and social media or personal branding (e.g., Instagram, sales). 
# This suggests women may be more associated with roles emphasizing social responsibility and digital marketing.

# [Male]
# Male interview questions tend to focus on technical, economic, and business-related skills (e.g., strengths, profits, welding). 
# This suggests men are more likely to be in roles that prioritize technical expertise and economic outcomes.

# Gender-based Job Role Preferences :
# Women may be drawn to roles emphasizing social responsibility and digital media, 
# while men may prefer positions focusing on technical proficiency and economic performance.

# Female candidates can prepare to emphasize social responsibility, interpersonal skills, and digital media expertise. 
# Male candidates should focus on showcasing technical expertise, economic achievements, and practical problem-solving abilities.


# 3. Analysis of Key Keywords by Job Experienced

# Word frequency calculation by Experienced
experienced_keyword <- data_frame %>%
  group_by(Experience, words) %>%
  summarise(Frequency = n()) %>%
  arrange(Experience, desc(Frequency))

# Production Manufacture stopword Delete 
new_stopword <- c("없으셨습니", "조치하시겠습니")

# Removing duplicates and aggregating frequency counts for each document-word pair.

experienced_keyword <- experienced_keyword %>%
  filter(!words %in% new_stopword) %>%
  ungroup()

# Calculate TF-IDF 
experience_idf_df <- experienced_keyword %>%
  bind_tf_idf(words, Experience, Frequency) %>%
  arrange(Experience, desc(tf_idf))

# Visualization: Top 10 Keywords by TF-IDF
ggplot(experience_idf_df %>% group_by(Experience) %>% top_n(10, tf_idf), 
       aes(x = reorder(words, tf_idf), y = tf_idf, fill = Experience)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ Experience, scales = "free_y") +
  labs(title = "Top TF-IDF Keywords by Experience", 
       x = "Keyword", 
       y = "TF-IDF") +
  theme_minimal()

# Result
# [Experienced] 
# Interview questions for experienced candidates emphasize strengths, strategic decision-making, and problem-solving, 
# suggesting they are suited for roles requiring leadership and multitasking.

# [Inexperienced Candidates] 
# Questions for inexperienced candidates focus on basic concepts and personal traits, highlighting their potential for growth and suitability for entry-level roles.

# Experienced candidates should highlight examples of problem-solving and strengths demonstrated in real-world scenarios. Inexperienced candidates should showcase adaptability and learning potential through relevant examples.


# 2️⃣ Topic Modeling
# Objective: Extract latent topics from interview questions.
# [Method]: Use Latent Dirichlet Allocation (LDA) and Structural Topic Model (STM).

# ----------------------- LDA Model -----------------------

# Text preprocessing and DTM creation 
dtm <- DocumentTermMatrix(Corpus(VectorSource(data_frame$Question)))

cat("Number of documents:", nDocs(dtm), "\n")
cat("Number of terms:", nTerms(dtm), "\n")
# Number of documents -> 81589 
# Number of terms -> 4833

k_values <- 2:10
perplexity_values <- sapply(k_values, function(k) {
  lda_model <- LDA(dtm, k = k, control = list(seed = 123))
  perplexity(lda_model, dtm)
})

# Perplexity graph for k values
plot(2:10, perplexity_values, type = "b", xlab = "Number of Topics (k)", ylab = "Perplexity",
     main = "Optimal Number of Topics", pch = 19, col = "blue")

# [Result]
# * At k=2, the lowest perplexity is observed, with perplexity increasing as k increases.
#   This suggests that the data can be well-explained with a <small number of topics (k)>.

# * k=2 appears to be the most suitable number of topics for the data.
#   However, considering interpretability and the diversity of the data,
#   exploring slightly higher values like k=3 or k=4 may reveal additional insights.
#   The choice of k should align with the project objective.

# Testing k = 2 ~ 4

# Define range of k values
k_values <- 2:4

# Initialize list for storing results
lda_results <- list()

# Train LDA models and store results for each k
for (k in k_values) {
  # Train LDA model
  lda_model <- LDA(dtm, k = k, control = list(seed = 1234))
  
  # Extract topic-term matrix (beta)
  lda_topics <- tidy(lda_model, matrix = "beta")
  
  # Extract top terms for each topic
  top_terms <- lda_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    arrange(topic, desc(beta))
  
  # Store results
  lda_results[[as.character(k)]] <- list(
    model = lda_model,
    topics = top_terms
  )
}

# Visualize top keywords for each k value
plots <- lapply(k_values, function(k) {
  ggplot(lda_results[[as.character(k)]]$topics, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    coord_flip() +
    facet_wrap(~ topic, scales = "free_y") +
    labs(title = paste("LDA Topic Analysis (k =", k, ")"), x = "Terms", y = "Beta") +
    theme_minimal()
})

# Display visualizations in a single page
do.call(grid.arrange, plots)

# [Result]
# * k=2
#   - topic1: Keywords like 'solution,' 'explain,' 'question' focus on problem-solving.
#   - topic2: Keywords like 'say,' 'nowadays,' 'curious' reflect more observational or conversational questions.

# * k=3
#   - topic1: Keywords like 'solution,' 'experience,' 'candidate' highlight problem-solving and career-related questions.
#   - topic2: Keywords like 'say,' 'topic,' 'curious' emphasize observational/advisory questions.
#   - topic3: Keywords like 'challenge,' 'wish,' 'passion' focus on motivation and enthusiasm.

# * k=4
#   - topic1: Keywords like 'solution,' 'support,' 'problem' focus on practical problem-solving.
#   - topic2: Keywords like 'say,' 'nowadays,' 'curious' highlight neutral and conversational aspects.
#   - topic3: Keywords like 'experience,' 'candidate,' 'resume' emphasize career and background.
#   - topic4: Keywords like 'passion,' 'challenge,' 'wish' focus on evaluating attitudes and aspirations.

# [Insights]
# * Common Patterns:
#   - Across all k values, keywords like 'solution,' 'experience,' and 'candidate' are central to questions, 
#     indicating that interview questions focus on evaluating <problem-solving skills> and <job fit>.

# * Differences by k value:
#   - k=2: Broad classification of questions.
#   - k=3: Divides into problem-solving, observational questions, and motivation evaluation.
#   - k=4: Clearly distinguishes job skills, observational questions, career-related questions, and attitude/motivation evaluation.

# Based on these results, k=3 appears to be the most appropriate choice.
# k=3 effectively categorizes key interview evaluation areas, such as <job fit>, <interpersonal skills>, and <stress management and collaboration> .

# Adjust DTM sparsity to reduce dimensionality
dtm <- removeSparseTerms(dtm, 0.99)

# Remove empty documents
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]

# LDAVis visualization
k_values <- 3 
for (k in k_values) {
  cat("Generating LDAVis for k =", k, "\n")
  
  # Train LDA model
  lda_model <- LDA(dtm, k = k, control = list(seed = 1234))
  
  # Generate JSON
  json_lda <- createJSON(
    phi = posterior(lda_model)$terms,
    theta = posterior(lda_model)$topics,
    doc.length = rowSums(as.matrix(dtm)),
    vocab = colnames(as.matrix(dtm)),
    term.frequency = colSums(as.matrix(dtm))
  )
  
  # Visualization
  serVis(json_lda)
}

# [Interpretation] *k=3
# 1) Inter-topic Distance Map:
#    Topics 1, 2, and 3 are independently distributed, with no overlap.
#    (= Topics are clearly distinct and encapsulate specific interview question content.)
# 2) Set λ close to 0 to observe highly relevant terms:
#    - Topic1: job, role, work, entry
#    - Topic2: time, management, people, supervisor, surrounding, how, difficult, coping
#    - Topic3: stress, goal, challenging, hard, experience, management, collaboration

# [Insights]
# This analysis reveals <key evaluation areas in interviews>:
# - Interviews primarily focus on <job fit>, <interpersonal skills>, and <stress management and collaboration>.
# - Preparation recommendations by topic:
#   - Topic1: Interviewees should demonstrate <a deep understanding of the job> and explain their role in detail.
#   - Topic2: Interviewees should prepare examples of handling <difficult situations or interpersonal conflicts>, emphasizing time management and coping skills.
#   - Topic3: Interviewees should showcase their strategies for <stress management, goal achievement, and collaboration experience>.

# Additionally, companies can use this data to develop questions that better evaluate key competencies:
# Example Questions:
# - Topic1: "How do you understand and plan to perform your role in this job?"
# - Topic2: "Can you describe a challenging situation you faced and how you dealt with it?"
# - Topic3: "Can you share an experience of collaborating during a team conflict?"

# To address the limitations of LDA, 
# we plan to use the STM model for further analysis.


# ----------------------- STM Model -----------------------

# Analyze topic distributions more precisely using metadata such as job type, years of experience, and interviewee background from the interview question dataset.
# Compare STM results with LDA to examine changes or new patterns in topics when metadata is added.

# STM Model Training
# STM uses a document-term matrix and metadata together
# Metadata: Job Category, Gender, Experience

stm_data <- data_frame %>%
  select(Question, Category, Gender, Experience) %>%
  mutate(Question = as.character(Question))

# Text Processing
processed <- textProcessor(documents = stm_data$Question, metadata = stm_data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# Train STM Model
stm_model <- stm(documents = out$documents, vocab = out$vocab, 
                 K = 5, prevalence = ~ Category + Gender + Experience, 
                 data = out$meta, seed = 1234)

# Extract Top Keywords for STM Topics
stm_topics <- labelTopics(stm_model, n = 10)
print(stm_topics)

# [Results]
# Topic 1 Top Words:
#         Highest Prob: 어떻게, 있습니다, 회사에, 건가요, 궁금합니다, 지원자님께서는, 있는데요, 된다면, 업무를, 지원자님은 
#         FREX: 있습니다, 회사에, 건가요, 업무를, 목표를, 하시겠습니까, 회사의, 직무가, 상사가, 상황을 
#         Lift: 가보고, 가정합시다, 가져야, 가지와, 거예요, 공동체, 공동체에, 괜찮게, 근무하려면, 긍정적으로 
#         Score: 어떻게, 상사가, 건가요, 하시겠습니까, 회사에, 그래도, 있습니다, 영어로, 기차가, 서류를 
# Topic 2 Top Words:
#         Highest Prob: 그리고, 부탁드립니다, 그것을, 있나요, 무엇이라고, 궁금합니다, 어려운, 무엇을, 있으신지, 지금까지 
#         FREX: 그리고, 그것을, 무엇을, 있으신지, 대해서도, 생각하십니까, 힘들었던, 존경하는, 하셨는지, 생각하는지 
#         Lift: 거쳤는지, 과정들을, 노력하는, 노력하셨나요, 자사를, 활동에서, 궁금하고요, 노력은, 누구인지, 면접자분께서 
#         Score: 그것을, 그리고, 존경하는, 무엇을, 인물을, 무엇이라고, 글로벌, 인재에게, 요소는, 지금까지 
# Topic 3 Top Words:
#         Highest Prob: 바랍니다, 말씀해, 주시기, 지원자님께서, 지원자님께서는, 이유도, 본인이, 생각하는, 주시길, 그렇게 
#         FREX: 바랍니다, 주시기, 이유도, 생각하는, 주시길, 생각하시는, 공부하고, 이유와, 중요하게, 관심을 
#         Lift: 사용하시는지, 진출했으면, 콘트롤을, 가지에, 계기와, 나라로, 동기에, 동료를, 만들어, 모습은 
#         Score: 바랍니다, 생각하시는, 주시기, 공부하고, 생각하는, 그렇게, 분야에, 이유도, 중요하게, 가지게 
# Topic 4 Top Words:
#         Highest Prob: 있다면, 말씀해, 궁금합니다, 어떻게, 주시면, 협업을, 기억에, 스트레스를, 가지만, 방법으로 
#         FREX: 주시면, 협업을, 기억에, 스트레스를, 가지만, 방법으로, 공부를, 골라서, 본인만의, 지원자님만의 
#         Lift: 가능하시겠어요, 가정과, 가지만을, 갈등들, 갈등들을, 갈등들이, 갈등에, 강의도, 강점이, 개인의 
#         Score: 스트레스를, 기억에, 커뮤니케이션을, 골라서, 스트레스, 가지만, 시간이, 관리를, 방법으로, 어려웠던 
# Topic 5 Top Words:
#         Highest Prob: 주세요, 말씀해, 대해서, 설명해, 있다면, 본인의, 자세하게, 경험이, 본인이, 주시고 
#         FREX: 본인의, 자세하게, 주시고, 있는지, 의견을, 장점과, 특별한, 최대한, 극복하기, 단점을 
#         Lift: 가능하면, 가지거나, 갈생이, 감수하면서, 강점은, 강점이나, 갖춰서, 개념에, 개발에, 개발자는 
#         Score: 주세요, 의견을, 장점과, 대해서, 자세하게, 단점을, 본인의, 키우기, 설명해, 주시고 

# [Interpretation of Results]
# * Highest Prob: Words that appear most frequently in each topic
# * FREX (Frequency and Exclusivity): Words that are frequent and distinctive for a specific topic
# * Lift: Words that occur much more frequently in one topic compared to others
# * Score: Importance scores calculated based on the model's criteria

# [Topic-wise Interpretation]
# Topic 1: 어떻게, 회사에, 건가요, 지원자님께서는, etc.
#           Reflects questions about <interest in the company or role> and <motivation to apply>.
#           > Questions about how interviewees can contribute to the company or their attitude toward the role.
# Topic 2: 그리고, 무엇을, 있나요, 존경하는, etc.
#           Questions exploring <personal values, challenging experiences, or role models>.
#           > Focused on interviewees' <values>, <challenges>, and <inspirations>.
# Topic 3: 바랍니다, 말씀해, 생각하는, 주기, etc.
#           Reflects questions about <personal values, goals, and motivations>.
#           > Questions about <self-reflection and growth direction>.
# Topic 4: 있다면, 협업을, 스트레스를, 방법으로, etc.
#           Questions focusing on <collaboration, stress management, and conflict resolution>.
#           > Highlights <collaboration skills> and <problem-solving strategies>.
# Topic 5: 주세요, 경험이, 본인의, 극복하기, etc.
#           Questions centering on <personal experiences, overcoming challenges, and strengths/weaknesses>.
#           > Focused on interviewees' <strengths, weaknesses, and examples of overcoming difficulties>.


# STM Visualization: Topic-Word Probabilities
topic_words <- as.data.frame(stm_topics$prob)
topic_words_long <- tidyr::gather(topic_words, key = "Topic", value = "Probability", everything())

# STM Visualization: Document Proportion by Topic
stm_doc_topics <- tidy(stm_model, matrix = "gamma")

ggplot(stm_doc_topics, aes(x = factor(topic), y = gamma, fill = factor(topic))) +
  geom_boxplot() +
  labs(title = "Document Proportion by Topic (STM)", x = "Topic", y = "Document Proportion") +
  theme_minimal()

# [Results]
# * Topic1: The median document proportion is relatively high with a wide spread. 
#           Many documents have proportions above 0.5, making it the most <dominant topic> in the data.
# * Topic2: The median document proportion is close to 0, suggesting that it is a <unique topic> appearing in only a few documents.
# * Topic3~5: The median document proportions are similar across topics, appearing consistently in several documents with <balanced distributions>.


# 4️⃣ Word Network Analysis
# Objective: Analyze relationships between words in interview questions.
# [Method]: Generate and visualize a word network based on bigram data.

# Create a Network Graph Object (Undirected Graph)
keyword_network <- word_frequency %>% 
  filter(n > 50)

# Create a Network Graph Object (Undirected Graph)
network_graph <- graph_from_data_frame(
  d = keyword_network,
  directed = FALSE
)

# Detect Clusters Using Louvain Algorithm
clusters <- cluster_louvain(network_graph)
V(network_graph)$cluster <- as.factor(membership(clusters))

clusters %>% head()

# Node Size: Set the size of each node based on its degree
node_frequency <- degree(network_graph)
V(network_graph)$size <- node_frequency

# Add Edge Attribute for Frequency
E(network_graph)$frequency <- keyword_network$frequency

# Visualize Network Graph
ggraph(network_graph, layout = "fr") + 
  # Edge Style
  geom_edge_link(aes(alpha = frequency, width = frequency), color = "gray") + 
  # Node Style
  geom_node_point(aes(size = size, color = cluster)) + 
  # Add Node Text Labels
  geom_node_text(aes(label = name), repel = TRUE, size = 2.5) + 
  # Edge and Node Style Ranges
  scale_edge_alpha(range = c(0.4, 1), guide = "none") + 
  scale_edge_width(range = c(0.5, 2), guide = "none") + 
  scale_size_continuous(range = c(3, 10)) + 
  scale_color_viridis_d() + 
  theme_void() +
  ggtitle("Interview Keyword Network Graph") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "right"
  )

# 5️⃣ Keyword Comparison by Gender and Experience
# Objective: Compare key keywords based on Gender and Experience.
# [Method]: Group data by Gender and Experience, then compare keyword frequencies.

# 1) Gender
# Calculate keyword frequencies by gender
gender_keywords <- data_frame %>%
  group_by(Gender, words) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(Gender, desc(Frequency))

# Visualization
ggplot(gender_keywords %>% filter(Frequency > 1100), aes(x = reorder(words, Frequency), y = Frequency, fill = Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ Gender, scales = "free_y") +
  labs(title = "Top Keywords by Gender", x = "Keywords", y = "Frequency") +
  theme_minimal()

# [Results and Interpretation]
# - Both men and women focus on <work-related keywords> as the most frequent.
# - Differences in emphasis:
#   🌟 Men: Focus on <task execution and collaboration skills> (e.g., "collaboration," "project," "work").
#   🌟 Women: Focus on <emotional management and self-reflection> (e.g., "overcome," "emotion," "strengths," "weaknesses," "situations").
# - While overall similarities are strong, subtle differences exist.


# 2) Experience
# Count applicants by experience level
experience_count <- data_frame %>%
  group_by(Experience) %>%
  summarise(Count = n(), .groups = "drop")

print(experience_count)

#   Experience   Count
# 1 Experienced  44605
# 2 New         284667

# Visualization: Distribution of New vs Experienced Applicants
ggplot(experience_count, aes(x = reorder(Experience, Count), y = Count, fill = Experience)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribution of New vs Experienced Applicants", x = "Experience Level", y = "Number of Applicants") +
  theme_minimal()

# Calculate and Normalize Keyword Frequencies by Experience Level
experience_keywords_normalized <- data_frame %>%
  group_by(Experience, words) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  group_by(Experience) %>%
  mutate(NormalizedFrequency = Frequency / sum(Frequency)) %>%
  arrange(Experience, desc(NormalizedFrequency))

head(experience_keywords_normalized)

# Visualization (Normalized Frequencies)
ggplot(experience_keywords_normalized %>% filter(NormalizedFrequency > 0.005), 
       aes(x = reorder(words, NormalizedFrequency), y = NormalizedFrequency, fill = Experience)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ Experience, scales = "free_y") +
  labs(title = "Top Keywords by Experience (Normalized)", x = "Keywords", y = "Normalized Frequency") +
  theme_minimal()

# [Results and Interpretation]
# - Most key keywords are <similar across experience levels>, indicating that companies value both <results-oriented work capabilities> and <personal attitudes>.
# - Junior-level applicants: Likely to focus on learning, potential, and adaptability.
# - Experienced applicants: Emphasis on proven abilities, leadership, and past achievements.
# - Suggestion: Interview questions should be further tailored to differentiate clearly between experience levels, aligning with expected roles and competencies.


# Network Analysis

create_edge_list <- function(tokenized_data) {
  # Initialize an empty data frame to store the edges
  edge_list <- data.frame(from = character(0), to = character(0), weight = integer(0), stringsAsFactors = FALSE)
  
  # Iterate over all pairs of sentences
  for (i in 1:(length(tokenized_data) - 1)) {
    for (j in (i + 1):length(tokenized_data)) {
      
      # Find common words between sentence i and sentence j
      common_words <- intersect(tokenized_data[[i]], tokenized_data[[j]])
      
      # If there are common words, create edges for each common word
      if (length(common_words) > 0) {
        for (word in common_words) {
          # Add a new edge to the edge list
          edge_list <- rbind(edge_list, data.frame(from = tokenized_data[[i]], to = tokenized_data[[j]], weight = length(common_words)))
        }
      }
    }
  }
  # Return the edge list
  return(edge_list)
}

# Design case
data_frame$Category %>% table()
grouped_data <- data_frame %>% filter(Category=="Design") %>%
  group_by(Question) %>%
  summarise(Words_Vector = as.vector(strsplit(paste(words, collapse = " "), " ")), .groups = 'drop')
word_list <- grouped_data$Words_Vector %>% unique()
word_list <- word_list[sapply(word_list, length) == 10]
length(word_list)

# Create the edge list by calling the function with the tokenized word list
edge_list <- create_edge_list(word_list)
# Create an undirected graph from the edge list data frame
g <- graph_from_data_frame(edge_list, directed = FALSE)
# ggraph
ggraph(g, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.8, color = "skyblue") +  # edge
  geom_node_point(size = 5, color = "red") +  # node
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold") +  # node name
  theme_void() +
  theme(legend.position = "none")


# ICT case
grouped_data <- data_frame %>% filter(Category=="ICT") %>%
  group_by(Question) %>%
  summarise(Words_Vector = as.vector(strsplit(paste(words, collapse = " "), " ")), .groups = 'drop')
word_list <- grouped_data$Words_Vector %>% unique()
word_list <- word_list[sapply(word_list, length) == 11]
length(word_list)

# Create the edge list by calling the function with the tokenized word list
edge_list <- create_edge_list(word_list)
# Create an undirected graph from the edge list data frame
g <- graph_from_data_frame(edge_list, directed = FALSE)
# ggraph
ggraph(g, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.8, color = "skyblue") +  # edge
  geom_node_point(size = 5, color = "red") +  # node
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold") +  # node name
  theme_void() +
  theme(legend.position = "none")

# Management case
grouped_data <- data_frame %>% filter(Category=="Management") %>%
  group_by(Question) %>%
  summarise(Words_Vector = as.vector(strsplit(paste(words, collapse = " "), " ")), .groups = 'drop')
word_list <- grouped_data$Words_Vector %>% unique()
word_list <- word_list[sapply(word_list, length) == 13]
length(word_list)

# Create the edge list by calling the function with the tokenized word list
edge_list <- create_edge_list(word_list)
# Create an undirected graph from the edge list data frame
g <- graph_from_data_frame(edge_list, directed = FALSE)
# ggraph
ggraph(g, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.8, color = "skyblue") +  # edge
  geom_node_point(size = 5, color = "red") +  # node
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold") +  # node name
  theme_void() +
  theme(legend.position = "none")

# PM case
grouped_data <- data_frame %>% filter(Category=="ProductionManufacturing") %>%
  group_by(Question) %>%
  summarise(Words_Vector = as.vector(strsplit(paste(words, collapse = " "), " ")), .groups = 'drop')
word_list <- grouped_data$Words_Vector %>% unique()
word_list <- word_list[sapply(word_list, length) == 11]
length(word_list)

# Create the edge list by calling the function with the tokenized word list
edge_list <- create_edge_list(word_list)
# Create an undirected graph from the edge list data frame
g <- graph_from_data_frame(edge_list, directed = FALSE)
# ggraph
ggraph(g, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.8, color = "skyblue") +  # edge
  geom_node_point(size = 5, color = "red") +  # node
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold") +  # node name
  theme_void() +
  theme(legend.position = "none")


# PS case
grouped_data <- data_frame %>% filter(Category=="PublicService") %>%
  group_by(Question) %>%
  summarise(Words_Vector = as.vector(strsplit(paste(words, collapse = " "), " ")), .groups = 'drop')
word_list <- grouped_data$Words_Vector %>% unique()
word_list <- word_list[sapply(word_list, length) == 11]
length(word_list)

# Create the edge list by calling the function with the tokenized word list
edge_list <- create_edge_list(word_list)
# Create an undirected graph from the edge list data frame
g <- graph_from_data_frame(edge_list, directed = FALSE)
# ggraph
ggraph(g, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.8, color = "skyblue") +  # edge
  geom_node_point(size = 5, color = "red") +  # node
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold") +  # node name
  theme_void() +
  theme(legend.position = "none")


# RND case
grouped_data <- data_frame %>% filter(Category=="RND") %>%
  group_by(Question) %>%
  summarise(Words_Vector = as.vector(strsplit(paste(words, collapse = " "), " ")), .groups = 'drop')
word_list <- grouped_data$Words_Vector %>% unique()
word_list <- word_list[sapply(word_list, length) == 11]
length(word_list)

# Create the edge list by calling the function with the tokenized word list
edge_list <- create_edge_list(word_list)
# Create an undirected graph from the edge list data frame
g <- graph_from_data_frame(edge_list, directed = FALSE)
# ggraph
ggraph(g, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.8, color = "skyblue") +  # edge
  geom_node_point(size = 5, color = "red") +  # node
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold") +  # node name
  theme_void() +
  theme(legend.position = "none")


# SM case
grouped_data <- data_frame %>% filter(Category=="SalesMarketing") %>%
  group_by(Question) %>%
  summarise(Words_Vector = as.vector(strsplit(paste(words, collapse = " "), " ")), .groups = 'drop')
word_list <- grouped_data$Words_Vector %>% unique()
word_list <- word_list[sapply(word_list, length) == 12]
length(word_list)

# Create the edge list by calling the function with the tokenized word list
edge_list <- create_edge_list(word_list)
# Create an undirected graph from the edge list data frame
g <- graph_from_data_frame(edge_list, directed = FALSE)
# ggraph
ggraph(g, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.8, color = "skyblue") +  # edge
  geom_node_point(size = 5, color = "red") +  # node
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, fontface = "bold") +  # node name
  theme_void() +
  theme(legend.position = "none")

