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
'像寫字一樣，從左到右，滿了就自動換行'  "(所以一定是~var,所以才能填滿col)"
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

geom_bar(stat = "identity") #stat = "identity" :不要用預設的count,用我給的資料去畫圖
geom_col() # 跟上面一樣，跟geom_bar差在後者是用於data是一筆一筆observation的狀態
#如果已經依照類別統計好數字了，用geom_col就好，geom_col(x=類別,y=數字)

#display a bar chart of proportions :y = after_stat(prop), group = 1
ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar()
#再多包含diamonds color,有顏色的版本
ggplot(diamonds, aes(x = cut, fill = color, y = after_stat(prop), group = color)) + 
  geom_bar()

#
diamonds_summary <- diamonds %>% 
  group_by(cut) %>% 
  summarise(y_median = median(depth),   # 算出中位數
            y_min    = min(depth),      # 算出最小值
            y_max    = max(depth))      # 算出最大值

ggplot(diamonds_summary, aes(x = cut)) +
  geom_pointrange(aes(y = y_median, 
                      ymin = y_min, 
                      ymax = y_max),
                  color = "royalblue", size = 0.8) +
  labs(title = "不同切割品質的深度（中位數與範圍）", y = "depth")
   #or
diamonds |>
  group_by(cut) |>
  summarize(
    lower = min(depth),
    upper = max(depth),
    midpoint = median(depth)
  ) |>
  ggplot(aes(x = cut, y = midpoint)) +
  geom_pointrange(aes(ymin = lower, ymax = upper))

# -------------------------------------------------------------------------
# position = "fill" : 比較成分多寡 (疊在一起))
ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "fill")

# position = "dodge" : (分散 都從底部開始畫)
ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "dodge")


# -避免overplotting，adds random noise to the location of the points-------------------------------------------------------------

geom_point(position = "jitter")
geom_jitter(height = 1, width = 1) #跟上面一樣

#或是+geom_count()，就能用點的大小看重疊的分佈狀況了


# 圓餅圖 ---------------------------------------------------------------------

ggplot(diamonds, aes(x = "", fill = cut)) +
  geom_bar() + 
  coord_polar(theta = "y") #coord_polar()

# -------------------------------------------------------------------------
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() + #畫y=x的直線。 預設: intercept = 0, slope = 1 (?)
  coord_fixed() #防止視窗拉長，把圖表形狀扭曲

# CH10, EDA ---------------------------------------------------------------------

+ coord_cartesian(ylim = c(0, 50)) #限制Y軸的上下限 顯示哪個範圍就好

unusual <- diamonds |>     #outlier
  filter(y < 3 | y > 20) |> 
  select(price, x, y, z) |>
  arrange(y)

unusual

#10.3.3 Exercises
'Explore the distribution of each of the x, y, and z variables in diamonds'
cleaned_diamonds <- diamonds |>
  filter(x > 0, y >0, y < 20, z >0, z <20)

cleaned_diamonds |>
  pivot_longer(cols = c(x, y, z),
               names_to = "dimension",
               values_to = "value") |>
  ggplot(aes(x = value)) +  # 我的X軸要是哪個欄位的值
  geom_histogram(binwidth = 0.1, fill = "seagreen", color = "white") +
  facet_wrap( ~ dimension, scales = "free", ncol = 1)

summary(diamonds)

# 2D
ggplot(diamonds, aes(x, y, color = z)) +
  geom_point(position = "jitter", alpha = 0.5, na.rm = TRUE)

# 3D圖 ---------------------------------------------------------------------
install.packages("plotly")
library(plotly)

plot_ly(diamonds, 
        x = ~x, y = ~y, z = ~z, 
        type = 'scatter3d', 
        mode = 'markers')

# Q2 ----------------------------------------------------------------------

'Explore the distribution of price'
diamonds |>
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 500, fill = "royalblue", color = "white") +
  labs(title ="粗略的價格分配(binwidth = 500)")

diamonds |>
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 10, fill = "coral") +
  labs(title ="精細的價格分配(binwidth = 10)") + 
  coord_cartesian(xlim = c(0, 2500))

#做 EDA 時，同一張圖一定要大大小小調整各種組距

# Q3 ----------------------------------------------------------------------

'How many diamonds are 0.99 carat? How many are 1 carat?'
carat_0.99 <- diamonds |>
  filter(carat == 0.99)
nrow(carat_0.99)

carat_1 <- diamonds |>
  filter(carat == 1)
nrow(carat_1)

# Q4 ----------------------------------------------------------------------
"Compare and contrast coord_cartesian() vs. xlim() or ylim() when zooming in on a histogram."
# coord_cartesian() 是不改變原圖形狀的放大鏡
# xlim() or ylim() 會先去除掉資料 再重新做一個圖形

# 想放大看細節，用這個!!!
# 確定要把特定範圍外的髒資料「從資料庫裡踢除」
# -------------------------------------------------------------------------

#recommend replacing the unusual values with missing values
diamonds2 <- diamonds |> 
  mutate(y = if_else(y < 3 | y > 20, NA, y))

# geom_bar柱子的頂端用線相連，形成折線圖
geom_freqpoly() #避免直方圖柱子互相檔到

#10.4.1 Exercises
#histogram 的NA(因為是處理連續型變數)，所以會直接剔除
#bar chart 的NA(因為是處理類別型)，所以會獨立形成一個bar，告訴你有多少遺失值

'frequency plot of scheduled_dep_time colored:cancelled flight. 
Also facet:cancelled. 
Experiment with different values of the scales variable in the faceting function to mitigate the effect of more non-cancelled flights than cancelled flights.'
# 1. from textbook
flights_dt <- flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60) )

# 2. 依照題目要求畫圖
ggplot(flights_dt, aes(x = sched_dep_time)) + 
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4) +  # 用顏色（color）區分是否取消，每 15 分鐘（1/4小時）畫一條折線
  facet_wrap(~ cancelled, scales = "free_y", ncol = 1)

# covariation -------------------------------------------------------------

'how the price of a diamond varies with its quality (measured by cut) '
# count的版本
diamonds |>
  ggplot(aes(x = price)) +
  geom_freqpoly((aes( color = cut)), binwidth = 500, linewidth = 0.75)
#frepoly跟bar是同一家人，要留意bar的規則，只能放一個我要的值(x=value's var name)，
#這樣它才會去count出Y軸!

'after_stat'
# density的版本 : y = after_stat(density)
diamonds |>
  ggplot(aes(x = price, y = after_stat(density))) +
  geom_freqpoly((aes(color = cut)), binwidth = 500, linewidth = 0.75)

# price & cut(是有order過的factor!) 
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

class(diamonds$cut) #只打cut會找到別的函數

'how highway mileage varies across classes'
mpg |>
  ggplot(aes(x = class, y = hwy)) +
  geom_boxplot()

# make the trend easier to see
#fct_reorder(原本的var, 要依照哪個var, 的什麼東西來排順序)
mpg |>
  ggplot(aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot()

# -------------------------------------------------------------------------

#變成平行 way1 : x y互換 
mpg |>
  ggplot(aes(x =  hwy, y = fct_reorder(class, hwy, median))) +
  geom_boxplot()

# way2 : coord_flip()
mpg |>
  ggplot(aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot() +
  coord_flip()

# -------------------------------------------------------------------------

#10.5.1.1 Exercises
'departure times of cancelled vs. non-cancelled flights.'
flights |>
  mutate(cancelled = is.na(dep_time),
         sched_hour = sched_dep_time %/% 100,
         sched_min = sched_dep_time %% 100,
         sched_dep_time = sched_hour + (sched_min / 60)) |>
  ggplot(aes(x = sched_dep_time, y = after_stat(density))) + #y軸不是count,是density的話，要加這行!
  geom_freqpoly(aes(color = cancelled), binwidth = 0.5, linewidth = 1) +
  #以下是多的
  scale_color_brewer(palette = "Set1", labels = c("Non-cancelled", "Cancelled"))+
  labs(title = "Departure Time Distribution: Cancelled vs. Non-cancelled Flights",
       x = "Scheduled Departure Time (Hours after midnight)",
       y = "Density",
       color = "Flight Status") +
  theme_minimal() #讓灰底網格消失

# Q2 ----------------------------------------------------------------------
'what variable in the diamonds dataset appears to be most important for predicting the price of a diamond'
#連續 類別 要分開處理!
#連續: (看point)
diamonds_conti <- diamonds |>
  select(price, carat, depth, table, x, y, z) |>
  pivot_longer(cols = -price,
               names_to = "all_var",
               values_to = "value")
ggplot(data = diamonds_conti, mapping = aes(x = value, y = price)) + #注意x是
  geom_point(alpha = 0.1) +
  facet_wrap( ~ all_var, scales = "free") # all continuos var

#類別: (看boxplot)
diamonds_cat <- diamonds |>
  select(price, cut, color, clarity) |>
  mutate(across(c(cut, color, clarity), as.character)) |>   #convert ordered factors  #across
  pivot_longer(cols = -price,
               names_to = "all_var_1",
               values_to = "value_1")

ggplot(data = diamonds_cat, mapping = aes(x = fct_reorder(value_1, price, median), y = price)) +
  geom_boxplot() +
  facet_wrap( ~ all_var_1, scales = "free_x") # all category var

' How is that variable correlated with cut?'
#根據上題 最影響的var是carat
diamonds |>
  select(carat, cut, price) |>
  ggplot(aes(x = fct_reorder(cut, price, median), y = carat)) +
  geom_boxplot()


# Q3 -------------------------------------------------------------------------
# geom_lv() : （Letter-Value Plot，字母值圖）是盒狀圖的延伸
install.packages("lvplot", dependencies = TRUE)
library(lvplot)
install.packages("ggplot2")
library(ggplot2)

'after_stat'
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv(aes(fill = after_stat(LV))) + # 自動根據不同的 letter value 填色
  scale_fill_brewer(palette = "Blues", direction = -1) + # 讓越寬的盒子顏色越深
  theme_minimal()

# Q4 ----------------------------------------------------------------------
'prices vs. a categorical variable. pros and cons of 看distribution of a numerical variable based on the levels of a categorical variable?'

# 1. geom_violin()
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_violin()
'對比各組的中位數 (pros)/(cons) 將左右對稱對稱化'

# 2. Faceted geom_histogram()
ggplot(diamonds, aes(x = price)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ cut, scales = "free_y")
'獨立分格，不會有線條或圖層重疊遮擋的問題 / 鋸齒狀外觀、難精準對比不同組別在「同一個 X 軸點」上的高低落差'

# 3. Colored geom_freqpoly()
ggplot(diamonds, aes(x = price, y = after_stat(density), color = cut)) +
  geom_freqpoly(bins = 30)
'對比局部波峰的 X 軸位置 / 顏色>3時 視覺上很亂'

# 4. Colored geom_density()
ggplot(diamonds, aes(x = price, fill = cut)) +
  geom_density(alpha = 0.2)
'平滑化外觀(且比折線圖更美觀)、易看出整體的微幅波動趨勢與形狀 / 在邊界會有「超出真實範圍」的平滑估算誤差、顏色>3時 半透明圖疊起來會變髒髒的'

# Q5 ----------------------------------------------------------------------
install.packages('ggbeeswarm')
library(ggbeeswarm) #蜂群 # geom_jitter() => 隨機打散

# 蜂群(一)
'geom_beeswarm()'  #點會向兩側對稱地規律排開，寬度直接反映了該點的密度
   # method = "swarm"(預設)/"compactswarm"(點的間距)/"hex"(點呈現六角蜂巢)/"square"(點呈現方正地水平/垂直對齊)

# 蜂群(二)
'geom_quasirandom()'  # jitter隨機 + beeswarm蜂群排序 :避免排得太寬超出圖表
   # method = "tukey"(預設)/"frowney"(點呈現小提琴圖)


# 10.5.2 Two categorical variables ----------------------------------------

'二維圖(用點的大小呈現)'
# + geom_count() 

'表格(tibble 數字)'                       
# count(類別var1, 類別var2) 
# 再+ geom_tile(aes(fill = n)) =>
'二維圖(用方塊顏色深淺呈現)' 
                                                  
#10.5.2.1 Exercises --------------------------------------------

'在各個 Color之內，Cut 的比例分佈 (Color 內加總 = 100%)'
diamonds |> 
  count(color, cut) |>   #count完就會有n
  group_by(color) |>     #你的組別是color
  mutate(prop = n / sum(n)) |>   #用n做出新的欄位(比例)
  ggplot(aes(x = color, y = cut, fill = prop)) +  #map
  geom_tile() +
  scale_fill_viridis_c()  #改顏色(?

# bar : 類別型 ---------------------------------------------------------------------
'#position = "stack"是預設值'
ggplot(diamonds, aes(x = color)) + 
  geom_bar(aes(fill = cut), position = "stack") 

'# position = "fill"' '#改成所有bar高度拉成1 => 變成是在看每個x的組別裡的各色佔比'
ggplot(diamonds, aes(x = color)) + 
  geom_bar(aes(fill = cut), position = "fill") 

'# 加 group = 1 => 每個 color 不用是獨立的 100%'
ggplot(diamonds, aes(x = color)) +
  geom_bar(group = 1)
'# Y從計數變比例 : 加上 y = after_stat(prop)'  
ggplot(diamonds, aes(x = color, y = after_stat(prop), group = 1)) + 
  geom_bar()

# 折線圖(geom_freqpoly) : 連續型 ------------------------------------------------
'y = after_stat(density)'
ggplot(flights, aes(x = sched_dep_time, y = after_stat(density), color = cancelled)) +
  geom_freqpoly()

# '航班延誤探索' ----------------------------------------------------------------

library(nycflights13)

# 1. 找出前 20 大目的地
top_dest <- flights |> 
  count(dest, sort = TRUE) |> 
  slice_max(n, n = 20) |> 
  pull(dest)

# 2. 篩選資料、計算平均延誤並繪圖
flights |> 
  filter(dest %in% top_dest) |> 
  group_by(dest, month) |> 
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE), .groups = "drop") |> 
  mutate(month = factor(month)) |> # 轉成因子，避免 X 軸連續漸層
  ggplot(aes(x = month, y = dest, fill = avg_delay)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Avg Delay (min)") +
  theme_minimal()


# 10.5.3 Two numerical variables ------------------------------------------

'舊方法'
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_point(alpha = 1 / 100)

'新*2'
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d()
#> `stat_bin2d()` using `bins = 30`. Pick better value `binwidth`.
#所以 bins 跟 binwidth不一樣，但他們各別是啥?

install.packages("hexbin")
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_hex()
# hex = 六角形

'bin 連續型變數，for each group, display a boxplot:'
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.1)))  # bin var"carat" =>
# group = cut_width(conti_var_name, 我想要的width)

# 10.5.3.1 Exercises ------------------------------------------------------











