install.packages("tidyverse")
library(tidyverse)
install.packages("palmerpenguins")
library(palmerpenguins)
install.packages("ggthemes")
library(ggthemes)

penguins #前面裝tidyverse了，所以可以用tibble；改良版的dataframe
glimpse(penguins)  #一樣是看資料集長相，但可以偷看到所有變數名稱

ggplot(data = penguins, 
       aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(aes(color = species, shape = species)) +  #如果丟在上一層的ggplt裡，就會是下面所有都被影響
  geom_smooth(method = 'lm') + 
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species" #內建的lab名稱是小寫
  ) +
  scale_color_colorblind() #改顏色

#1.2.5 Exercises
'How many rows are in penguins? How many columns?'
penguins
'rows:344, cols:8'

'What does the bill_depth_mm variable in the penguins data frame describe? Read the help for ?penguins to find out.'
?penguins
'a number denoting bill depth (millimeters)'

'Make a scatterplot of bill_depth_mm vs. bill_length_mm. That is, make a scatterplot with bill_depth_mm on the y-axis and bill_length_mm on the x-axis. Describe the relationship between these two variables.'
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point(na.rm = T) #預設是F 
'正相關'

'What happens if you make a scatterplot of species vs. bill_depth_mm? What might be a better choice of geom?'
ggplot(penguins, aes(x = bill_depth_mm, y = species)) + 
  geom_point() + 
  labs(title = 'Data come from the palmerpenguins package')

'點都疊在一起了。比較好的geom不是這個，是把數值資料放在y軸，類別資料放在X軸。'

ggplot(data = penguins, 
       aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(aes(color = bill_depth_mm)) +  
  geom_smooth(method = 'loess')  #loess是平滑曲線

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)  #se = FALSE 是指不要出現表示標準差的陰影區間

penguins |>
  ggplot(aes(x = fct_infreq(species))) +   #non-ordered levels=>reorder the bars based on their frequencies, 類別型資料要計數的話要先轉成factor
  geom_bar(color = "red", fill = "blue")

#更多geom: -----------
geom_histogram(binwidth = 200)
geom_density(alpha = 0.5) #調上一層fill的透明度
geom_boxplot()
geom_bar(position = "fill") #讓每個X bar的y軸高度都變成齊頭式的1(所以是比例，要把原本的Y lab=count改掉，加上  labs(y = "proportion"))
#---------------------

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = sex, shape = species)) +
 # geom_smooth(aes(linetype = species, method = 'lm', se = F)) +
  facet_wrap(~island) +
  labs(title = '三個島嶼，不同性別的body mass')
ggsave(filename = "penguin-plot.png")

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species)) 
  #facet_wrap(~island) +
  #labs(title = '三個島嶼，不同species的body mass')

# + scale_x_continuous(breaks = c(1999, 2000)) # x-axis breaks at 1999 and 2000
#可以指定我的圖只要x或y是哪個區間範圍

#1.5.5 Exercises
glimpse(mpg) #只有int, dbl(一種浮點數)是numerical
#--------------------------
#Data transformation
install.packages("nycflights13")
library(nycflights13)
library(tidyverse)

#發現head, glimpse都讀不了，用?去查，或是因為他是package
data(package = "nycflights13")
glimpse(flights)

to_save_the_result <- flights |> #or %<%
  filter(distance > 3000 & month == 6) #& 是 and，or是|；另有一樣功能的%in% (變數名稱前加!代表不要哪些)
#(e.g) day == 1 | day == 2 就是day %in% c(1,2)

flights |>
  arrange(desc(day), desc(month), desc(year)) #預設是由小到大，所以加上desc代表由大到小

#distinct() ...刪掉重複的row
#distinct(var1, var2) ...找到所有不重複、可能的組合
#承上，加上.keep_all = TRUE，就可以保留var1 var2以外的columns

#count(var1, var2) ...算某個組合的出現次數
#承上，加上sort = T，即可讓次數由大至小

#3.2.5 Exercises
flights |>
  distinct(origin)

'Sort flights to find the flights with the longest departure delays. Find the flights that left earliest in the morning.'
flights %>%
  arrange(desc(dep_delay)) |>
  arrange(dep_time) |>
  relocate(dep_delay, dep_time)

'fastest flight'
flights |>
  mutate(speed = distance/air_time )  |>
  arrange(desc(speed)) |>
  relocate(speed)

'Was there a flight on every day of 2013?'
flights |>
  filter(year == 2013) |>
  distinct(month, day) #|>
  #nrow()
'因為distinct出來有365個row，所以是的，每天都有班機。'

'Which flights traveled the farthest distance? Which traveled the least distance?'
flights |>
  arrange(desc(distance)) |>
  relocate(distance)
farthest_flights <- flights |>
  filter(distance == 4983)

flights |>
  arrange(distance) |>
  relocate(distance)
least_flights <- flights |>
  filter(distance == 80) 

#mutate(var1,var2, .before = 1) 
#加上.before = 1，可以讓新變數出現在開頭的col，跟relocate有點像(?)
#.after就是加在最後面

#.keep = "used"  =>  col只出現mutate裡有出現的變數

#select
#(!var2:var4) => 選var1, 5, 6...
#還可以用-
#(starts/ends_with/contains("abc"))、num_range("x", 1:3)
#all_of()：必須全有；any_of()：有誰選誰

#改欄位名稱:
#1. 只要改其中幾個，其他照舊:rename(new name= old name)
#2. 只要拿我改過名的出來，其他不要:select

#note: 不手動改名 janitor::clean_names() 

#3.3.5 Exercises
'Compare dep_time, sched_dep_time, and dep_delay'
flights |>
  select(contains("dep"))

flights |> select(contains("TIME", ignore.case = FALSE)) #在contains裡那個括號!
#加上ignore.case = FALSE，就能規定一定要大小寫也相同

'Rename air_time to air_time_min to indicate units of measurement and move it to the beginning'
flights |>
  rename(air_time_min = air_time) |>
  relocate(air_time_min)
flights |> 
  group_by(month) |> 
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE),  #na.rm
            count = n() ) #count是指那一組有幾筆資料

#用完group_by之後，搭配slice
#slice_head/tail(n = )
#slice_min/max(x, n =)  #x是指定的col
  #加上with_ties = FALSE，可以取出每組一個最大值。否則預設是會保留平手的所有rows
  #slice跟summarize差在一個會取整個row 一個只會取出指定的值
#slice_sample(n =)

#進行全體計算前 ungroup()，等於.groups = "drop"

#3.5.7 Exercises
'Which carrier has the worst average delays?'
flights |>
  group_by(carrier) |>
  summarize(average_delays = mean(dep_delay, na.rm = T)) |>
  arrange(desc(average_delays))

'Find the flights that are most delayed upon departure to each destination'
flights |>
  group_by(dest) |>
  summarize(max_dep_delay = max(dep_delay, na.rm = T)) |>
  relocate(dest, max_dep_delay) 

'How do delays vary over the course of the day? Illustrate your answer with a plot.'
dat <- flights |>
  select(hour, minute, dep_delay) |>
  mutate(time = as.POSIXct(paste0(hour, ':', minute), format = "%H:%M"))

ggplot(dat, aes(time, dep_delay)) +
  geom_smooth() +
  labs(x = "時間", y = "出發延誤", title = "一天中的延誤狀況")

#df %>% count(dest, sort = TRUE) #sort可以取代arrange(desc=T)

df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K"))

df |>
  group_by(y) #已分好組 但要看起來排列好要搭配其他(summarize,mutate...)

df |>
  arrange(y) #只是排列好了，後續計算還是分開的

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x)) #y:2組，z:2組 =>依照y,z分組，得到2*2=4個不同的組合

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x)) #(依照組別)壓縮資料

df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x)) #新增欄位

#3.6
install.packages('Lahman')
library(Lahman)
data(package ='Lahman') #很多個tibble組成package,package要用data()來看  (?)

batters <- Lahman::Batting |> 
  group_by(playerID) |> 
  summarize(performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
            n = sum(AB, na.rm = TRUE))
batters |>
  filter(n > 100) |> #避免打擊數太少的
  ggplot(aes(n, performance)) +
  geom_point(alpha = 0.1) +  #alpha:透明度
  geom_smooth(method = "loess", se = F)


# tidy --------------------------------------------------------------------

library(tidyverse)
bill_longer <- billboard |>
  pivot_longer(cols = starts_with('wk'),
             names_to = 'week',
             values_to = 'rank',
             values_drop_na = TRUE) |> #na.rm=T 的概念(?) 
  mutate(week = parse_number(week)) #parse_number(var name)，會只留下第一個數字
  
bill_longer |>
  ggplot(aes(week, rank, group = track)) +
  geom_line(alpha = 0.25) +
  scale_y_reverse()
# -------------------------------------------------------------------------

df <- tribble(  #tribble:用來手輸小型資料
  ~id,  ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
) 

df_longer <- df |>
  pivot_longer(cols = starts_with("bp"),
               names_to = "measurement",
               values_to = "value")

who2_longer <- who2 |>
  pivot_longer(cols = -c(country, year),
               names_to = c("diagnosis", "gender", "age"),
               names_sep = "_",  #new!
               values_to = "count")#,
               #values_drop_na = T)
who2_longer

distinct() #前要先select()，補充如下:
#範圍:-或!(var1:var3) => 範圍不用c
#各自扣掉:-c(var1, var2, var3) => 直接用變數名稱
# 或是 -any_of(c("var1", "var2", "var3")) => 變數名稱要用引號

# -------------------------------------------------------------------------

household_longer <- household |>
  pivot_longer(cols = -family,
               names_to = c(".value", "child"), 
               names_sep = "_", 
               values_drop_na = TRUE)
#.value  新用法
# -------------------------------------------------------------------------
cms_patient_experience

cms_patient_experience |> 
  pivot_wider(  #new!
    id_cols = starts_with("org"), #這是「不准動」的欄位
    names_from = measure_cd, #要拆開的col
    values_from = prf_rate  #值從哪個col來
  )

#看一個col總共有哪些可能性
df |> 
  distinct(要看的欄位名稱) |> 
  pull()  #new!

#看 id_cols
df |> 
  select(!measurement & !value) |> 
  distinct()


# --還是不知道在幹嘛的-----------------------------------------------------------------------

getwd()
install.packages("usethis")
library(usethis)
?usethis

# --讀檔案-----------------------------------------------------------------------
'csv'

#na = c("N/A", "", "無") :定義哪些是na

students |> 
  janitor::clean_names() #自動用snake原則 重新命名變數
  mutate(meal_plan = factor(meal_plan)) #把某個變數存成factor

read_csv( #，
    "# A comment I want to skip
  x,y,z
  1,2,3",
  comment = "#") #或是 skip = 1
#其他: col_names = FALSE
#      na = c("", "NA", ".")     #找出原資料哪些應該被視為na 

read_csv2() #；

#7.2.4 Exercises
read_delim("filename.txt", delim = "|")

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`)))

'Extracting the variable called 1.'
annoying |>
  select("1") #這樣取出來是dataframe,若要vector，用pull('1')

problems(df) #看讀取資料時有沒有錯誤的地方

read_csv("data",
         col_types = cols( #new!指定讀進來的col是哪種(logical/interger/number/character/date/factor/skip)
  phone = col_character(),    # 強制保留電話的 0
  salary = col_number(),      # 自動濾掉 $ 符號，變成純數字
  useless_1 = col_skip(),     # 這一欄不讀取，直接丟掉
))

read_csv('data',
         col_types = col(.default = col_character() #除了指定col name,也可以是.default，就會變成全部的
))

       # col_types = cols_only(x = col_character()) #只要讀某個col 並且指定型式

read_csv(sales_files, id = "file") #讀取多個檔案(用c串起來)，並且新增一個叫做file的col，告訴我們這筆資料來自哪個檔案

#存取檔案: ---------------------------------
install.packages('arrow')
library(arrow)

write_parquet(students, "students.parquet") #"要存成的新名字"
read_parquet("students.parquet") #讀取parquet

#tibble v.s. tribble  ------------------------------
tibble(
  x = c(1, 2, 5), 
  y = c("h", "m", "g"),
  z = c(0.08, 0.83, 0.60)
)
tribble(   # transposed tibble，直接是col的方式呈現了
  ~x, ~y, ~z,
  1, "h", 0.08,
  2, "m", 0.83,
  5, "g", 0.60
)
# reprex  ----------------------------------
y <- 1:4
mean(y)
reprex::reprex() #產生大家都能跑的程式跟結果

# use dput() to include data ---------------------------------
dput(mtcars)
#mtcars <- 我複製dput(mtcars)的執行結果


# 9 -----------------------------------------------------------------------
glimpse(mpg)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class), size = 3)
#color/shape/size/alpha/stroke(shape在21-24時的邊框粗細/fill)

#menu of shape
#1
shape_dat <- tibble(
  x = (0:25%/%5), 
  y = (0:25%%5),
  shape_id = 0:25)

ggplot(shape_dat, aes(x, y)) +
  geom_point(aes(shape = shape_id), size = 5) +
  scale_shape_identity() + # 加上這行，強制要求 ggplot2 直接解讀數字，並且解除 6 種形狀的限制+
  ylim(-1, 5) + # 稍微拉高 y 軸範圍，避免最上面的文字被切掉
  theme_void() +# 隱藏背景格子，讓畫面變乾淨
  geom_text(aes(x = x + 0.35, label = shape_id), hjust = 0, size = 4, fontface = "bold") 在形狀右邊 (x + 0.35) #加上靠左對齊 (hjust = 0) 的數字標籤
  
#2
install.packages('ggpubr')
library(ggpubr)

ggpubr::show_point_shapes()

#9.2.1 Exercises
ggplot(mpg, aes(displ, hwy)) +
  geom_point(shape = 17, color = 'pink', size = 3) #"triangle"也行

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(stroke = class))

# -------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(size = 1.5) +
  geom_point(data = mpg |> filter(class == "2seater"),
             color = "red") +
  geom_point(data = mpg |> filter(class == "2seater"),
             color = "red",
             shape = "circle open",
             size = 4)
#當你在某個圖層要換資料時，必須用 data = ... 來指定

install.packages("ggridges")
library(ggridges)

ggplot(mpg, aes(x = hwy, y = drv)) +
  geom_density_ridges(aes(fill = drv, color = drv), alpha = 0.5) #全部都變一樣的話不用aes

ggplot(mpg, aes(x = hwy, y = drv)) +
  geom_density_ridges(scale = 1) + # 調整scale 避免圖形壅擠
  theme_ridges() #美觀

# facet -------------------------------------------------------------------
'grid是兩個變數'
'嚴格的棋盤格（矩陣），絕不換行'
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl, scales = "free")
#var1~var2 => 拆成n(var1)*n(var2)個
#"free_x"(或y) =>允許x,y軸不同，把plot散開來

'wrap是一個變數' 
'像寫字一樣，從左到右，滿了就自動換行'"(所以一定是~var,所以才能填滿col)"
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2) #ncol

#dir: 垂直水平

#舉例
head(diamonds)
glimpse(diamonds)

'不同切割品質 (cut)」下，克拉與價格的關係'
ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ cut, ncol = 2, scales = 'free_x')

'比對「切割 (cut)」與「顏色 (color)」的組合'
ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_hex() + # point 改用 hex 比較不會卡
  facet_grid(cut ~ color)

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_hex() +
  facet_grid(color ~ ., scales = "free")

#9.4.1 Exercises
'Recreate the following plot using facet_wrap()'
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap( ~ drv, nrow = 3) 
# Rows ~ Columns


# -------------------------------------------------------------------------

?geom_bar()

?stat_bin



