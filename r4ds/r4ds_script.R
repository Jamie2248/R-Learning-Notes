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

'新*2#Binned plot（區間箱化圖）會把資料「格子化」並加總'
'1. 方塊...?二維直方圖?不會重疊?'
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d()  
#> `stat_bin2d()` using `bins = 30`. Pick better value `binwidth`.
#所以 bins 跟 binwidth不一樣。但他們各別是啥?

'2. hex = 六角形'
install.packages("hexbin")
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_hex()

'bin 連續型變數，for each group, display a boxplot:'
# group = cut_width(conti_var_name, 我想要的width)
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.1)))  # bin var = "carat" 
# group也可以改成color

# --棒棒糖圖 (Lollipop Chart)-----------------------------------------------------------------------

# 先計算次數
library(dplyr)
dia_count <- diamonds %>% dplyr::count(color)


#geom_segment()  (蛤 那跟geom_line()有什麼差嗎?)
ggplot(dia_count, aes(x = color, y = n)) +
  geom_segment(aes(x = color, xend = color, y = 0, yend = n), color = "grey", lwd = 2) + # 桿子
  geom_point(color = "orange", size = 3) + # 糖果
  coord_flip()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 關鍵：文字旋轉 45 度

#table()可以算任何形式的資料
#count()只能算一個df或tibble的某個指定變數 => 所以是count(df/tibble, var_name)
#n()只能用在summarise,mutate裡 括號裡不放變數名稱 空著就好(前面會先用group_by分好組 所以已經知道要算什麼了)

# 更多note ------------------------------------------------------------------

#(1) Density（密度曲線）就是把Histogram（直方圖）的柱子切得無限細，再用一條平滑的曲線把頂端連起來
#(2) 加group = 1 的時機: 
# 當我想要Y軸是比例，此時若 X 軸不是連續數值或日期物件時，R畫折線圖或長條圖時會不知道怎麼連。加上去之後R就知道要把所有 X 軸的類別看成「同一個大母體」，這樣計算百分比時，分母才會是「全部人的總和（100%）」，進而得到加總等於 1
#(類別型:geom_bar()、y = after_stat(prop)、group = 1(也可以是var name)；連續型:geom_histogram()、y = after_stat(density) 或 geom_density())
#(3) 類別型 但柱子高度一樣: geom_bar(position = "fill")  # y 軸會自動變成 0 到 1 的比例
#柱子高度不同: (position =  "stack") => 預設

# prop好麻煩。之後用count就好了:上面都不用看了
# 這樣寫最直覺，完全不用背 prop 和 group
ggplot(diamonds, aes(x = color, fill = cut)) + 
  geom_bar() # 預設就是 position = "stack"，柱子高低本來就不同！
#呃 好像還是會用到prop吧 有時候數量落差太大 一部份會太貼Y軸

# 10.5.3.1 Exercises ------------------------------------------------------
'cut_width(conti_var, width) vs cut_number(conti_var, m)'
#每個區間的寬度固定 / 每個區間的資料筆數固定

'以 Price 切分，視覺化 Carat 的分佈'
library(tidyverse)
#prop
ggplot(diamonds, aes(x = carat, y = after_stat(density), color = cut_width(price, 2500))) +
  geom_freqpoly(binwidth = 0.1) +
  labs(color = "Price Range")
#count
ggplot(diamonds, aes(x = carat, color = cut_width(price, 2500))) +
  geom_freqpoly(binwidth = 0.1) +
  labs(color = "Price Range")

'大鑽石與小鑽石的價格分佈對比'  '分佈對比!'
# 用 cut_number 將克拉分為 5 組，對比小鑽石（第 1 組）與大鑽石（第 5 組）的價格分佈
ggplot(diamonds, aes(x = price, y = after_stat(density), color = cut_number(carat, 5))) +
  geom_freqpoly(binwidth = 500) +
  labs(color = "Carat Quintiles")  +
  guides(color = guide_legend(reverse = TRUE)) # 這行就是你要的「顛倒」開關！

'克拉區間與價格之間的關係'  '之間的關係'
ggplot(diamonds, aes(x = cut_number(carat, 5), y = price)) +
  geom_boxplot() +
  labs(x = "Carat Quintiles") +
  # 在箱型圖上疊加各組的平均值點（設定為紅色、放大、形狀為鑽石形）
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") 
  
'同時使用連續變數區間化（Binning）與面版分組（Faceting）'
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d() + # 1. 使用二維直方圖避免點重疊
  scale_fill_viridis_c() +  #用顏色代表密度
  facet_wrap(~ cut) + # 2. 用面版分組（Facet）拆開不同的切工（Cut）
  labs(title = "Combined Distribution of Carat, Price, and Cut")

'Why is a scatterplot a better display than a binned plot for outliers?'
diamonds |> 
  filter(x >= 4) |> 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
'Scatterplot（散佈圖）會畫出「每一個獨立的原始點」'

# 10.6 Patterns and models ------------------------------------------------

#補充across用法: summarise/mutate(across(【怎麼選欄位】, 【要做什麼事】))
'下面暫時裝不好'
install.packages('tidymodels')
library(tidymodels)

# 課本 用tidymodels取對數 ------------------------------------------------------------------

diamonds <- diamonds |>
  mutate(
    log_price = log(price),
    log_carat = log(carat)  #克拉對價格的影響力太強了。取對數（Log transform，為了解決非線性或偏態問題）
  )

diamonds_fit <- linear_reg() |>
  fit(log_price ~ log_carat, data = diamonds)

diamonds_aug <- augment(diamonds_fit, new_data = diamonds) |> #把預測結果貼回原本的資料集。它會產生一個欄位叫 .resid（殘差）
  mutate(.resid = exp(.resid))  #指數化還原

ggplot(diamonds_aug, aes(x = carat, y = .resid)) + 
  geom_point()

'lm版本'
# 用內建 R 寫法 取對數 ------------------------------------------------------------

diamonds_base <- diamonds |> mutate(log_price = log(price), log_carat = log(carat))
fit <- lm(log_price ~ log_carat, data = diamonds_base)

# 直接把殘差取 exp 放回去
diamonds_base$.resid <- exp(residuals(fit))

# 用 ggplot 畫圖
ggplot(diamonds_base, aes(x = carat, y = .resid)) +
 geom_point(alpha = 0.2) +  # 畫出殘差的散點
  # 加上一條 Y = 1 的水平基準線（因為 exp 之後，完美預測的值會接近 1，而不是 0）
  # 如果是看未經 exp 的原始對數殘差，基準線就會是 geom_hline(yintercept = 0)
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", size = 1) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # 加上一條平滑線，幫助我們肉眼看出殘差是否還殘留「彎曲的趨勢」
  labs(
    title = "鑽石克拉 vs 價格模型之殘差圖",
    x = "克拉數 (Carat)",
    y = "還原後的殘差 (Exponentiated Residuals)",
    subtitle = "藍色平滑線若接近水平，代表非線性關係已被 Log 轉換成功吸收" ) +
  theme_minimal()

plot(fit)

# 如果不取對數(原本的 第一個fit)---------------------------------
#raw data EDA (看:整為何要取對數) (?)
'1'
diamonds_base_1 <- diamonds |> select(price, carat)
fit_1 <- lm(price ~ carat, data = diamonds_base_1)
plot(fit_1)

'2'
ggplot(diamonds, aes(carat, price)) +
  geom_point() +
  geom_smooth(method = "loess",se = F)

# 為什麼課本要提這一部份: --------------------------------------------------------------

# Fair（差切工）的鑽石，平均價格竟然比 Ideal（完美切工）還要貴
diamonds |>
  select(price, cut) |>
  group_by(cut) |>
  summarise(mean_price = mean(price)) |>
  ggplot(aes(x = cut, y = mean_price)) +
  geom_col() 

#洗完之後，得到一張正確的boxplot(Y軸:新模型殘差，X軸:cut)
  #(這題的殘差圖還是有問題，因為只洗掉大趨勢，克拉還是有殘留的心理學效應可以解釋(變數底下的非線性特徵)))

# my note -----------------------------------------------------------------

  #(修模型: 用使用加權最小平方法（WLS）或對 \(Y\) 進行 Box-Cox 轉換)(或是3個偷懶方法:1.不要改模型 用用 sandwich 套件去重新計算 \(p\)-value；2. 改用「無母數檢定（Non-parametric test）」: Kruskal-Wallis 等檢定。這些檢定完全不看殘差圖，只看數據的「排序（Rank）」。；3.隨機森林(不在乎那些假設 會直接跟你說哪個因子重要))
#新模型的殘差對 \(X\) 畫圖，理論上要是水平帶狀平均散布分佈
  #順序: 先建立model <- aov(), plot(model)，再shapiro.test(residuals(model)) 搭配 leveneTest(...) 確認P值顯著與否，最後summary(model) + TukeyHSD(model)：若剛剛的ANOVA有顯著，進行事後比較
  #想看組別差異（ANOVA表）:用 aov() 或 anova(lm())。想看預估公式（迴歸係數): 用 summary(lm())
    #以下補充怎麼寫不會有順序影響的anova寫法:     
#-----## 1. 建立 lm 模型，並強制指定對比編碼為 contr.sum (效果編碼)
model_lm <- lm(score ~ color * size * text, 
               data = sim_data, 
               contrasts = list(color = contr.sum, size = contr.sum, text = contr.sum))

      # 2. 使用 car 套件的 Anova 函數，並指定 type = 3
library(car)
Anova(model_lm, type = 3)

# 11  Communication -------------------------------------------------------

library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(    # !
    x = "Engine displacement (L)",   #軸
    y = "Highway fuel economy (mpg)", 
    color = "Car type",   # (legend?) 就是旁邊那個圖示的框框
    title = "Fuel efficiency generally decreases with engine size",  #大標題
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",  #副標題
    caption = "Data from fueleconomy.gov"  # 圖的最底部 右下，放資料來源
  )


df <- tibble(
  x = 1:10,
  y = cumsum(x^2))

ggplot(df, aes(x, y)) +
  geom_point() +
  labs(x = quote(x[i]),
       y = quote(sum(x[i] ^ 2, i == 1, n))) # 數學式 把"my labs"改成用quote(my equation)

# 11.2.1 Exercises --------------------------------------------------------
glimpse(mpg)
mpg |>
  ggplot(aes(x = cty, y = hwy, color = drv, shape = drv)) +
  geom_point()+
  labs(x = "City MPG", y = "Highway MPG", color = "type of drive train", shape = "type of drive train")

# -- geom_text()-----------------------------------------------------------------------
label_info <- mpg |>
  group_by(drv) |>
  arrange(desc(displ)) |>
  slice_head(n = 1) |>
  mutate(drive_type = case_when(  #new! case_when("原本的變數名稱"="對照到我要的新變數名稱")
      drv == "f" ~ "front-wheel drive",  
      drv == "r" ~ "rear-wheel drive",   #像map的概念
      drv == "4" ~ "4-wheel drive"
    )) |>
  select(displ, hwy, drv, drive_type)
label_info # class = tibble

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  geom_text(data = label_info,  #geom_text(data, aes(x=. y=, label=我要加在圖上面的字被收在前面那個data的哪個col裡))
            aes(x = displ, y = hwy, label = drive_type),
            fontface = "bold", size = 5, hjust = "right", vjust = "bottom") +
  theme(legend.position = "none")  # turns all the legends off 


#  geom_label_repel() -----------------------------------------------------

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  geom_label_repel(  #geom_label_repel(data, aes(x=, y=, label=同上geom_text))
    data = label_info, 
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold", size = 5, nudge_y = 2) +
  theme(legend.position = "none")
# 就是多一個打底框框 比較好看清楚字

# geom_text_repel() -------------------------------------------------------
potential_outliers <- mpg |>
  filter(hwy > 40 | (hwy > 20 & displ > 5))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() + #全部的點
  # ! geom_text_repel(data=我有要標的點是哪些, aes(label = model))
  geom_text_repel(data = potential_outliers, aes(label = model)) +
  geom_point(data = potential_outliers, color = "red") + #異常點 變成紅色
  geom_point( #異常點 加上紅色環
    data = potential_outliers,
    color = "red", size = 3, shape = "circle open")


# other geoms in ggplot2 to annotate plot ---------------------------------

'add reference lines'
geom_hline()
geom_vline() #(linewidth = 2) and white (color = white)

'draw a rectangle around points of interest'
geom_rect() # aes: xmin, xmax, ymin, ymax

'draw arrow'
geom_segment(arrow = arrow(type = "closed")) #aesthetics x and y: 起始點、xend and yend: 結束處

'annotate()'
# ex:
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  annotate(
    geom = "label", x = 3.5, y = 38,
    label = trend_text,
    hjust = "left", color = "red"
  ) +
  annotate(
    geom = "segment",
    x = 3, y = 35, xend = 5, yend = 25, color = "red",
    arrow = arrow(type = "closed"))

# stringr::str_wrap()  ----------------------------------------------------

trend_text <- "Larger engine sizes tend to have lower fuel economy." |>
  str_wrap(width = 30)
trend_text
#自動幫很長的text 加上\n來換行

# 11.4 Scales -------------------------------------------------------------
ggplot(mpg, aes(x = displ, y = hwy)) + geom_point(aes(color = class)) +
  scale_x_continuous() + # 以下3行是 Default scales
  scale_y_continuous() +   # 命名有邏輯! scale_對照的aes(裡面有提到過的)_它的資料型態
  scale_color_discrete()
# 可以加:
'labels = NULL' #: 數字坐標軸  
'labels = c("4" = "4-wheel", "f" = "front", "r" = "rear")' #替換 map

# 11.4.2 Axis ticks and legend keys ---------------------------------------
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))  #調整Y軸的上下限、及每格級距

# adds dollar sign, thousand separator comma
ggplot(diamonds, aes(x = price, y = cut)) +
  geom_boxplot(alpha = 0.05) +
  scale_x_continuous(labels = label_dollar()) #labels = label_dollar()
#也可以加K:
'labels = label_dollar(scale = 1/1000, suffix = "K")'

# 加%
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Percentage", labels = label_percent())
'label_percent()'

# breaks
presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(name = NULL, breaks = presidential$start, date_labels = "'%y") #只顯示西元年份的後兩碼
"name = NULL" # name是軸的名稱

# 11.4.3 Legend layout ----------------------------------------------------
+ theme(legend.position = "right") # the default
# 'top, bottom'
+ guides(color = guide_legend(nrow = 2, override.aes = list(size = 4)))

# 11.4.4 Replacing a scale ------------------------------------------------
# 直接改X、Y單位
ggplot(diamonds, aes(x = log10(carat), y = log10(price))) +
  geom_bin2d() #但這樣name(座標軸名稱)會被改的很難看

# 所以:
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d() + 
  scale_x_log10() +  # !
  scale_y_log10()    # !


# 用scale改顏色 ---------------------------------------------------------------

# 最多只到12顏色限制的 category data
'ColorBrewer scales'
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  scale_color_brewer(palette = "Set1") # 這個set紅綠色盲也看的清楚

# 注意!  color: 點線(包括文字)； fill: 面

# -------------------------------------------------------------------------

# 沒有顏色數量上限的 category data #是需要自己手動指定顏色?
'scale_color_manual()' 
presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id, color = party)) +  #aes裡放分類的變數
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(values = c(Republican = "#E81B23", Democratic = "#00AEF3")) #給定value=(顏色對照清單)

'scale_color_gradient()' #gradient: 漸層
#(low = "顏色1", high = "顏色2")
'scale_fill_gradient()'

'scale_color_gradient2()' #三色: 適用於資料有一個明確的中間值/零點
#(low, mid, high)

# -------------------------------------------------------------------------

# viridis color scales (viridis也沒有顏色上限、有內建色系 不用手動指定色號)----------------------------------------------------
df <- tibble(x = rnorm(10000),
             y = rnorm(10000)) #rnorm(要隨機抽幾個)
# 
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() + #產生的線為45 度/確保1個 X 單位和1個 Y 單位在螢幕上佔據相同的長度
  labs(title = "Default, continuous", x = NULL, y = NULL)

ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() +
  scale_fill_viridis_c() +  # c = continuos
  labs(title = "Viridis, continuous", x = NULL, y = NULL)

ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() +
  scale_fill_viridis_b() +  # b = binned
  labs(title = "Viridis, binned", x = NULL, y = NULL)
#補充: d = discrete

# 11.4.5 Zooming ----------------------------------------------------------

mpg |>
  filter(displ >= 5 & displ <= 6 & hwy >= 10 & hwy <= 25) |> #!
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth() +
  scale_x_continuous(limits = c(5, 6)) + #!
  scale_y_continuous(limits = c(10, 25)) #!
# 以上兩個圖的結果一樣

'多張圖表如何共用相同尺度（Scales）'
# 統一的X軸、Y軸、顏色範圍
x_scale <- scale_x_continuous(limits = range(mpg$displ)) #range(): 得到最大與最小值
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv)) #unique() 會挑選出資料中所有不重複的類別（結果為 "f", "r", "4"），這樣顏色才不會因為缺乏某個車種 系統自動給色 顏色跑掉 每張圖不一致

# 第一張
ggplot(suv, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

# 第二張
ggplot(compact, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

# 11.4.6 Exercises --------------------------------------------------------

'scale vs labs'
# 前者: 想改數據呈現(breaks, labels, limits, trans)
# 後者: 只想改文字


# Q3 -----------------------------------------------------------------------
# 4 年一個斷點的設定
year_breaks <- seq(1952, 2026, by = 4) |> paste0("-01-01") |> as.Date()

ggplot(presidential, aes(x = start, y = name, color = party)) +
  # 1. 這裡把 yend 也換成 name，用總統名字當作 Y 軸
  geom_segment(aes(xend = end, yend = name), linewidth = 2) +
  # 2. 標註名字（直接用現有的 name 欄位，調整一下文字位置）
  geom_text(aes(label = name), hjust = -0.1, vjust = -0.4, size = 3, show.legend = FALSE) +
  # 3. 自訂顏色與每 4 年的 X 軸斷點
  scale_color_manual(values = c("Democratic" = "#1405BD", "Republican" = "#DE0100")) +
  scale_x_date(
    breaks = year_breaks,
    date_labels = "%Y",
    expand = expansion(mult = c(0.05, 0.15)) # 擴展右側避免名字被切掉
  ) +
  # 4. 優化 Y 軸：依照時間先後順序排列（原本預設會按字母排，這會亂掉）
  scale_y_discrete(limits = presidential$name) +
  # 5. 加上圖表標籤
  labs(
    title = "US Presidential Terms & Party Affiliation",
    subtitle = "From Eisenhower to Trump",
    x = "Year",
    y = NULL,
    color = "Party"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Q4 -----------------------------------------------------------------------
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1/20) +
  # 使用 override.aes 強制讓圖例的點保持清晰不透明
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme_minimal()


# 11.5 Themes -------------------------------------------------------------
#ggthemes
+ theme_bw()
+ theme_minimal()

# example
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  labs(
    title = "Larger engine sizes tend to have lower fuel economy",
    caption = "Source: https://fueleconomy.gov."
  ) +
  theme(  # ! 以下
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal",
    legend.box.background = element_rect(color = "black"), #除了color,還有fill,linewidth,linetype
    plot.title = element_text(face = "bold"), #除了face, 還有size, color
    plot.title.position = "plot",    # title跟"plot"對齊 (預設是跟panel畫圖區對齊)
    plot.caption.position = "plot",  # .position : caption 以誰(plot or panel)為基準定位
    plot.caption = element_text(hjust = 0)) #水平對齊=0 => 左對齊
# element_text() = 在改字體樣式

ggplot(diamonds, aes(x = log10(carat), y = log10(price))) +
  geom_bin2d() +
  labs(x = "log10(carat)")+
  theme(axis.title.x = element_text(face = "bold", color = "blue"))
#補充: 
axis.title.x  #Y
axis.text.x   #Y   # text : 刻度
legend.title
legend.text

# 11.6 Layout -------------------------------------------------------------

#(1) 直接兩張圖相加 : plot1 + plot2
#(2) patchwork package: 
plot3 <- ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 3")
# !
(plot1 | plot3) / plot2   # | : places plots next to each other 
                          # / : moves plots to the next line

# (3) guide_area()
p1 <- ggplot(mpg, aes(x = drv, y = cty, color = drv)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy, color = drv)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 2")
p3 <- ggplot(mpg, aes(x = cty, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 3")
p4 <- ggplot(mpg, aes(x = hwy, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 4")
p5 <- ggplot(mpg, aes(x = cty, y = hwy, color = drv)) + 
  geom_point(show.legend = FALSE) + 
  facet_wrap(~drv) +
  labs(title = "Plot 5")

# !
(guide_area() / (p1 + p2) / (p3 + p4) / p5) +
  plot_annotation(title = "City and highway mileage for cars with different drive trains",
                  caption = "Source: https://fueleconomy.gov.") +
  plot_layout(guides = "collect",
              heights = c(1, 3, 2, 4)) &  #???不懂那個1是啥。 # height of 1, the box plots 3, density plots 2, and the faceted scatterplot 4
  theme(legend.position = "top")      #用& 因為we’re modifying the theme for the patchwork plot as opposed to the individual ggplots

# 11.6.1 Exercises --------------------------------------------------------
p1 <- ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy)) + 
  geom_boxplot() + 
  labs(title = "Plot 2")
p3 <- ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 3")

(p1 | p2) / p3
p1 | p2 / p3
p1 / (p2 | p3)

# 12  Logical vectors -----------------------------------------------------
library(tidyverse)
library(nycflights13)

# review:
flights |> 
  mutate(daytime = dep_time > 600 & dep_time < 2000,
         approx_ontime = abs(arr_delay) < 20,
        .keep = "used")  # 只留下上面有用到的

flights |> 
  mutate(daytime = dep_time > 600 & dep_time < 2000,
         approx_ontime = abs(arr_delay) < 20,) |> 
  filter(daytime & approx_ontime)

# -------------------------------------------------------------------------

x <- c(1 / 49 * 49, sqrt(2) ^ 2)

x == as.integer(c(1, 2))  # X ； 有看到/、sqrt、log => 一律不要用==，用near!
near(x, c(1, 2))          # O ； dplyr::near()會容忍誤差地去對照比較
# 不能用==比是因為Na, NaN都像是問號一樣的存在，丟到==裡只會得到一樣是問號(不知道)；
# 但Inf就不一樣了，無限是確定的，所以可以用==，也可以用is.infinite(x)

# -------------------------------------------------------------------------

#is.na(var_name) 產生T、F(0、1)
flights |> 
  filter(month == 1, day == 1) |> 
  arrange(desc(is.na(dep_time)), dep_time)  # arrange(多個var name) => 第一順位和第二順位(若第一順位同分，會用到)

# 12.2.4 Exercises --------------------------------------------------------

# Q1:
near(sqrt(2)^2, 2)

# Q2: how the missing values in dep_time, sched_dep_time and dep_delay are connected
flights %>%
  mutate(na_dep_time       = is.na(dep_time),  # 新增都是T或F的新欄位
         na_sched_dep_time = is.na(sched_dep_time),
         na_dep_delay      = is.na(dep_delay)) %>%
  count(na_dep_time, na_sched_dep_time, na_dep_delay) # count()會計算distinct組合的數量
  #我自己的
flights |>
  select(dep_time,  dep_delay) |>
  filter(is.na(dep_time) | is.na(dep_delay)) |>
  distinct()

# 12.3 Boolean algebra ----------------------------------------------------
xor(x, y)  # 不是x 也不是y
# 12.3.1 Missing values ---------------------------------------------------
df <- tibble(x = c(TRUE, FALSE, NA)) #x 本身就已經是「判斷完的結果」了/X已經用is.家族得到TF了

df |> 
  mutate(and = x & NA,
         or = x | NA)

# 12.3.3 %in%  ------------------------------------------------------------

#大 %in% 小，看大的有沒有小的的值(NA也可以看)，輸出T F
letters[1:10] %in% c("a", "e", "i", "o", "u")

# 12.3.4 Exercises --------------------------------------------------------

#Q1 -------------------------------------------------
'arr_delay is missing but dep_delay is not. '
flights |>
  filter(is.na(arr_delay) & !is.na(dep_delay)) |>
  relocate(arr_delay, dep_delay)

'neither arr_time nor sched_arr_time are missing, but arr_delay is.'
flights |>
  filter(!is.na(arr_time) & !is.na(sched_arr_time) & is.na(arr_delay)) |>
  relocate(arr_time, sched_arr_time, arr_delay)
#Q2 -------------------------------------------------

# mine ans
'How many flights have a missing dep_time? '
flights %>%
  select(dep_time) %>%
  mutate(na_dep_time = is.na(dep_time)) %>%
  filter(na_dep_time %in% TRUE) %>%
  summarise(n_mis = n())

# faster
flights %>%
  summarise(n_missing = sum(is.na(dep_time)))

'What other variables are missing in these rows?'
# mine
flights %>%
  mutate(na_dep_time = is.na(dep_time)) %>%
  filter(na_dep_time %in% TRUE) %>%
  relocate(dep_time) 

# 補充:
select(data, where(~ any(is.na(.x)) #任何有NA的col都選
select(data, where(~ mean(.x, na.rm = TRUE) > 10)) 
# .x: 對all col做迴圈，控制每次照順序坐下去 一次鎖定一個col. 
# 後面接的這些類型的函數 例如 mean()、sum()、length()、any()、all()...: 對這一整欄的所有數值進行計算
# ~: 匿名函數，代表後面是一個函數條件

# 正解:
# 篩選出出發時間遺失的整張表，並查看每個欄位的摘要
flights %>%
  filter(is.na(dep_time)) %>%
  summary()

# Q3 ---------------------------------------------

'number of missing dep_time per day?' 
# !只要題目出現「每」或「各」，就用 group_by

'pattern、connection between the proportion of cancelled flights and the average delay of non-cancelled flights?'

cancelled_delay_data <- flights %>%
  group_by(year, month, day) %>% 
  summarise(total_flights = n(), # 每天總航班數
            n_cancelled = sum(is.na(dep_time)), # 每天取消的航班數（dep_time 是 NA）
            prop_cancelled = mean(is.na(dep_time)), # 取消航班的比例。 !看到proportion, 想到mean()
            avg_delay_non_cancelled = mean(dep_delay, na.rm = TRUE),   # 未取消航班（dep_time 不是 NA）的平均出發延誤時間
            .groups = "drop") %>%
  relocate(avg_delay_non_cancelled,prop_cancelled)

ggplot(cancelled_delay_data, aes(x = avg_delay_non_cancelled, y = prop_cancelled)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    x = "未取消航班的平均延誤時間 (分鐘)",
    y = "航班取消比例",
    title = "航班取消比例與平均延誤時間的關係")

# ----------------------------------------------------------

flights |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay[arr_delay > 0], na.rm = TRUE), # 或是要在前面用 filter(arr_delay > 0)
    ahead = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    n = n(),
    .groups = "drop" )

# 12.4.4 Exercises --------------------------------------------------------

prod()
# 向量內所有元素的乘積 => 只要向量裡有F，prod就是0
# 在概念上等價於 all()，但all()是回傳邏輯值(F/T)
# 跟min()就是完全相等了

# 12.5 Conditional transformations ----------------------------------------
'if_else()'
# 自訂我要回傳的/執行的動作 #優點:可以自定義NA怎麼處理(用 missing = "沒有值")
if_else(my_condition, if T then do what, if F ..., if NA ...)

'case_when()'
# condition ~ output； condition must be a logical vector
x <- c(-3:3, NA)
case_when(
  x == 0   ~ "0",
  x < 0    ~ "-ve", 
  x > 0    ~ "+ve",
  is.na(x) ~ "???")
# .default = "???" (是不是在處理剩下其他的?)

flights |> 
  mutate(status = 
    case_when(is.na(arr_delay)      ~ "cancelled",
              arr_delay < -30       ~ "very early",
              arr_delay < -15       ~ "early",
              abs(arr_delay) <= 15  ~ "on time",
              arr_delay < 60        ~ "late",
              arr_delay < Inf       ~ "very late",), # < Inf (蛤?)
  .keep = "used") # 新增完欄位，選擇要留下來哪些

# 12.5.4 Exercises --------------------------------------------------------
#Q1
'whether each number between 0 and 20 is even or odd'
if_else(1:20 %% 2 == 0, "even", "odd")

#Q2
'Given a vector of days, use an if_else() label them as weekends or weekdays.'

s <- c("Monday", "Saturday", "Wednesday")
case_when(s == "Monday" ~ 1,  #嚴格規定 輸出的型態要一致!
          s == "Tuesday" ~ 2,
          s == "Wednesday" ~ 3,
          s == "Thursday" ~ 4,
          s == "Friday" ~ 5,
          s == "Saturday" ~ 6,
          s == "Sunday" ~ 7,
          TRUE ~ NA_real_) #如果.default因為版本太舊不能跑 就改成TRUE
s <- 1:7
if_else(s > 5, 'weekends', 'weekdays')

# 其他作法:
weekdays_list <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
match(s, weekdays_list) # match 會回傳 s 在 weekdays_list 裡面的第幾個位置（也就是 1 到 7）

#Q3
'compute the absolute value of a numeric vector called x'

input <- readline(prompt = "enter number（用空格隔開）:")

# strsplit(input, split): 用空格將字串切割成文字向量
input_split <- unlist(strsplit(input, split = " ")) # unlist(): 降維成標準向量

num_vector <- as.numeric(input_split)
class(num_vector) #numeric (vector)
#------
num_vector <- as.numeric(unlist(c(-5, 9, -13)))
ifelse(num_vector < 0, -1 * num_vector, num_vector)

#Q4
'Write a case_when() statement that uses the month and day columns from flights to label a selection of important US holidays (e.g., New Years Day, 4th of July, Thanksgiving, and Christmas). First create a logical column that is either TRUE or FALSE, and then create a character column that either gives the name of the holiday or is NA.'
#my sol----------------------------------
holiday_map <- c("11" = "New Year's Day",
                 "74" = "4th of July",
                 "1128" = "Thanksgiving",
                 "1225" = "Christmas")
names(holiday_map)

date <- flights |>
  #group_by(month, day) #用 paste0 組合時，可以不用 group_by，直接用欄位即可
  mutate(my_date = paste0(month, day)) |>
  mutate(is_it_holiday = my_date %in% names(holiday_map)) |>
  mutate('name of the holiday' = holiday_map[my_date]) |> #欄位名稱有空格的話要用引號包起來 不然就要注意用_連接好
  relocate('name of the holiday', is_it_holiday) 
# 因為我在map裡有定義好了 所以不用用map 用holiday_map[my_date]就可以了  
date

# 正解------------------------------------
# 必須連同 flights 和 mutate 一起執行
flights_with_holiday <- flights %>%
  mutate(
    holiday_name = case_when(
      month == 1  & day == 1  ~ "New Year's Day",
      month == 7  & day == 4  ~ "4th of July",
      month == 11 & day == 28 ~ "Thanksgiving",
      month == 12 & day == 25 ~ "Christmas",
      TRUE                    ~ NA_character_))  
# 篩選 12 月 25 日的資料，看看 holiday_name 欄位
flights_with_holiday %>%
  filter(month == 12, day == 25) %>%
  select(month, day, holiday_name) %>%
  head()

# 補充: 用 match ---------------------------

# 定義兩個分開的向量
holiday_dates <- c("11", "74", "1128", "1225")
holiday_names <- c("New Year's Day", "4th of July", "Thanksgiving", "Christmas")

date <- flights |>
  mutate(my_date = paste0(month, day)) |>
  # match 會回傳位置數字（例如 1, 2, 3, 4），找不到就回傳 NA
  mutate(pos = match(my_date, holiday_dates)) |>
  mutate(is_it_holiday = !is.na(pos)) |>
  mutate(`name of the holiday` = holiday_names[pos])

# 13  Numbers -------------------------------------------------------------

# parsing strings into numbers
parse_number() #只取裡面的數字，不管其他不是數字的字串跟符號
parse_double() #換算成數值

# 比較count()和n()-------------------------------------------------------------------------

flights |> 
  count(dest, sort = TRUE)   # 在外面；最頻繁的在上面
flights |>  
  group_by(dest) |> 
  summarize(n = n())  # 在裡面，屬於summary function

# n_distinct() ---------------------------------------
flights |> 
  group_by(dest) |>  #試著加#跑跑看 很好玩
  summarize(carriers = n_distinct(carrier)) |> 
  arrange(desc(carriers))  

# summarize(sum函數+is.na()) => number of_

# 13.3.1 Exercises --------------------------------------------------------
'number of rows with a missing value'
flights |>
  filter(is.na(dep_time)) |>
  summarize(n = n())

# pmin()、pmax() -----------------------------------------------------------
df <- tribble(
  ~x, ~y,
  1,  3,
  5,  2,
  7, NA,)

df |> 
  mutate(min = pmin(x, y, na.rm = TRUE), #pmin(all var name, na.rm = T)
         max = pmax(x, y, na.rm = TRUE) )  

# round() ----------------------------------------------------------
round(123.456, -1)  #去零頭
#default = 0, 意思是取到整數
round(x / 4) * 4  #四捨五入 以4為最小單位

#floor/ceiling: round down/up
# cut ---------------------------------------------------------------------
x <- c(1, 2, 5, 10, 15, 20)
cut(x, breaks = c(0, 5, 10, 100)) # x是一個數值向量，我給定breaks作為標準，cut會看X對照到我標準的哪個區間裡

cut(x, breaks = c(0, 5, 10, 15, 20), 
       labels = c("sm", "md", "lg", "xl"))  # 讓輸出值不是我給定的breaks,而是用labels來表示
# 超出我給的範圍的話就顯示NA

# cum_()系列 ----------------------------------------------------------------
x <- 1:10
cumsum(x) # 1,1+2,1+2+3,1+2+3+4,......
# 還有cummean, cummax, cumprod

# Ranks -------------------------------------------------------------------
x <- c(1, 5, 5, 17, 22, NA)
min_rank(x) # 排名大小順序(數字越小 名次越前面) #加上desc(x)就是順序反過來

# 更多:
row_number() #第幾列
dense_rank() #跟min_rank差在哪??
percent_rank() #百分位數的排名(all=1) 
cume_dist() #累計的百分位數排名

# 範例:
df <- tibble(id = 1:10)

df |> 
  mutate(
    row0 = row_number() - 1,
    three_groups = row0 %% 3,  #分三組 給予組別
    three_in_each_group = row0 %/% 3)  #每組拿到相同值

# 13.5.2 Offsets ----------------------------------------------------------
x <- c(2, 5, 11, 11, 19, 35)
lag(x) #每次讀的時候 讓X lag => 第一次讀到沒東西 => NA
lead(x) #X lead => 提前一個位置 => 讀到第二個位置的

#範例:
events |> mutate(
  group = cumsum(has_gap))

# creating grouping variables ---------------------------------------------
'group_by(x)' #只要內容物一樣 就分成同組 
'consecutive_id(x)' #不管這個內容物之前有沒有出現過 只要前後內容物不一樣 就幫新的內容物開一個新的組別
                    #用於:有時間先後順序、且狀態會反覆跳轉」的資料
#舉例
df <- tibble(
  x = c("a", "a", "a", "b", "c", "c", "d", "e", "a", "a", "b", "b"),
  y = c(1, 2, 3, 2, 4, 1, 3, 9, 4, 8, 10, 199)
)
df |> 
  group_by(id = consecutive_id(x)) |> 
  slice_head(n = 1) #抓取每個群組的第一列 / 抓取正常跟異常那個改變的時間點

# 13.5.4 Exercises --------------------------------------------------------
#Q1
'Find the 10 most delayed flights using a ranking function.'
library(nycflights13)
glimpse(flights)
# 不用min_rank的話
flights |>
  arrange(desc(dep_delay)) |>
  relocate(dep_delay, flight) |> 
  slice_head(n = 10)
#用
flights |>
  mutate(most_delayed = min_rank(-dep_delay)) |> #如果要反序，就是在變數名稱前面加上負號
  arrange(most_delayed) |>
  relocate(flight, dep_delay, most_delayed) |> 
  filter(most_delayed <= 10)

#Q2  
'Which plane (tailnum) has the worst on-time record?'
'這架飛機平均每次起飛，延誤多久?'
flights |>
  filter(!is.na(tailnum)) |> #加上去
  group_by(tailnum) |>
  summarize(mean_dep_delay = mean(dep_delay, na.rm = TRUE), #加上去
            n = n()) |> #看看是不是極少狀況
  arrange(desc(mean_dep_delay)) |>
  relocate(mean_dep_delay, tailnum)

#Q3
'What time of day should you fly if you want to avoid delays as much as possible?'
#用時間(每天的 24小時分鐘)當X軸；delay時間當Y軸，畫圖

#Q4
flights |> 
  group_by(dest) |> 
  filter(row_number(dep_delay) < 4) #把每個dest裡 dep_delay前三名抓出來

#Q5
'For each destination, compute the total minutes of delay.'
flights |>
  filter(!is.na(dep_delay) | !is.na(arr_delay)) |>
  group_by(dest) |>
  summarize(total_minutes_of_delay = sum(dep_delay - arr_delay, na.rm = T),
            n = n()) |>
  relocate(dest, total_minutes_of_delay, n) |>
  arrange(total_minutes_of_delay)

' For each flight, compute the proportion of the total delay for its destination'
# mine
flights |>
  filter(!is.na(dep_delay), !is.na(arr_delay)) |>
  mutate(delay_diff = dep_delay - arr_delay) |>
  group_by(dest) |>
  mutate(n = n(),
         total_diff = sum(delay_diff, na.rm = TRUE),
         prop_delay_for_its_dest = delay_diff/ total_diff) |>
  ungroup() |>
  relocate(dest, prop_delay_for_its_dest, n) |>
  arrange(prop_delay_for_its_dest)

# 怎麼不一樣阿
flights |>
  filter(!is.na(dep_delay) & !is.na(arr_delay)) |>
  group_by(dest) |>
  # 因為接的是 mutate，所以它會為「每一列」保留原本的 dep_delay 和 arr_delay，
  # 同時，裡面的 n() 會非常聰明地自動代表「該目的地的總航班數」！
  mutate(prop_delay_for_its_dest = (dep_delay - arr_delay) / n() ) |>
  ungroup() |>
  relocate(dest, prop_delay_for_its_dest) |>
  arrange(prop_delay_for_its_dest)

#Q6
"Using lag(), explore how the average flight delay for an hour is related to the average delay for the previous hour."
flights |> 
  mutate(hour = dep_time %/% 100) |> 
  group_by(year, month, day, hour) |> 
  summarize(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop") |> 
  filter(n > 5) |> #以下
  filter(hour - lag(hour) == 1) |> #lag() 抓到的「前一列」不一定真的是「前一小時」,可能中間跳過去了
  group_by(year, month, day) |>          # 按「天」重新分組
  mutate(lag_dep_delay = lag(dep_delay)) |> 
  ungroup() |> 
  filter(!is.na(lag_dep_delay)) |> 
  ggplot(aes(x = lag_dep_delay, y = dep_delay)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE)

#Q7
'find flights that are suspiciously fast (i.e. flights that represent a potential data entry error)/Compute the air time of a flight relative to the shortest flight to that destination' 
flights |>
  filter(!is.na(air_time)) |>
  group_by(dest) |>
  mutate(
    min_air_time = min(air_time),
    med_air_time = median(air_time), #真正該懷疑的是「跟這個目的地的一般水準(中位數)差很多」的航班
    rel_to_min = air_time / min_air_time,
    rel_to_med = air_time / med_air_time
  ) |>
  ungroup() -> air_time_check

air_time_check |>
  arrange(rel_to_med) |>
  select(dest, carrier, flight, air_time, med_air_time, rel_to_med) |>
  head(20)

'Which flights were most delayed in the air'
air_time_check |>
  arrange(desc(rel_to_min)) |> #反過來,看相對最短時間「多花了多少」
  select(dest, carrier, flight, air_time, min_air_time, rel_to_min) |>
  head(20)

#Q8
'Find all destinations that are flown by at least two carriers. Use those destinations to '
'relative ranking of the carriers based on their performance for the same destination'
# 第一步:哪些目的地有 >= 2 家航空
dest_multi <- flights |>
  distinct(dest, carrier) |>
  count(dest) |>
  filter(n >= 2)
# 剛剛的第一步看不懂的話:
dest_multi <- flights |>
  group_by(dest) |>
  # n_distinct() 會自動在幕後幫你算：這組裡面有幾種不重複的 carrier
  summarize(n_carriers = n_distinct(carrier)) |>
  filter(n_carriers >= 2)

# 第二步:只看這些目的地,算每家航空在每個目的地的平均延誤,
# 再依目的地分別排名(名次 1 = 該目的地最準時)
flights |>
  filter(!is.na(arr_delay), dest %in% dest_multi$dest) |> #我只要那些在dest_multi裡(符合>=2條件)的dest
  group_by(dest, carrier) |> #1.算每家航空(carrier)在不同dest的平均
  summarize(avg_delay = mean(arr_delay), n = n(), .groups = "drop") |> #計算組別值 => 用summarize
  group_by(dest) |> #2.依照不同dest 算各家航空的排名
  mutate(rank = rank(avg_delay)) |> #計算個別值 => mutate
  ungroup() |>
  group_by(carrier) |> #3.綜合起來 算每家航空的排名
  summarize(avg_rank = mean(rank), n_dest = n_distinct(dest)) |> #每家航空排名平均值

# -------------------------------------------------------------------------
# 更多summarize()裡的argument:

"quantile(var_name, 第幾分位數, na.rm)"
summarize(
  max = max(dep_delay, na.rm = TRUE),
  q95 = quantile(dep_delay, 0.95, na.rm = TRUE)

#如果不是要分位數的數值，是要Spread的話:
# Two commonly used summaries are sd(x), and IQR()
'IQR()'
summarize(
  distance_iqr = IQR(distance),  #算一個變數的四分位距
  n = n()

# -------------------------------------------------------------------------
# interaction(var_name1, var_name2) :把合併後產物當成factor版本的paste0(是factor的話就會自動排序好大小順序，畫圖方便)
# 這題這樣就是想看dep_delay的每日freqpoly(頻率折線圖)分布，所以只指定X，Y就會自動是count
flights |>
  filter(dep_delay < 120) |> 
  ggplot(aes(x = dep_delay, group = interaction(day, month))) + 
  geom_freqpoly(binwidth = 5, alpha = 1/5)

# 13.6.5 Positions --------------------------------------------------------

# 想要每天的第1、最後、第5筆資料:
flights |> 
  group_by(year, month, day) |> 
  summarize(
    first_dep = first(dep_time, na_rm = TRUE), 
    fifth_dep = nth(dep_time, 5, na_rm = TRUE), # nth 自己指定第n個
    last_dep = last(dep_time, na_rm = TRUE))  # 注意!dplyr 用的是na_rm；算簡單數學的是na.rm

# 舉例:
flights |> 
  group_by(year, month, day) |> 
  mutate(r = min_rank(sched_dep_time)) |> 
  filter(r %in% c(1, max(r)))

# 如果是想抓最大最小值 忽略時間順序的話:
slice_max(sched_dep_time, n = 1) #=> 但這不能在同一串裡面先後接著，如果想同時取最大最小，要用
filter(變數 == min(變數) | 變數 == max(變數))

# 13.6.6 With mutate() ----------------------------------------------------

x / sum(x) # proportion of a total.
(x - mean(x)) / sd(x) # Z-score (standardized to mean 0 and sd 1).
(x - min(x)) / (max(x) - min(x)) # standardizes to range [0, 1].
x / first(x) # index based on the first observation.

# 13.6.7 Exercises --------------------------------------------------------

# Q1
library(nycflights13)
library(dplyr)

# 追蹤同一架飛機的連鎖延誤
airplane_chains <- flights %>%
  filter(!is.na(dep_delay), !is.na(tailnum)) %>%
  arrange(tailnum, year, month, day, dep_time) %>%  # 按照飛機和時間排序
  group_by(tailnum) %>%
  mutate(
    prev_dep_delay = lag(dep_delay) # 抓出這架飛機「上一班」的延誤時間
  ) %>%
  filter(!is.na(prev_dep_delay)) # 刪除當天第一班（沒有前一班資料）

# 看一下結果：如果上一班延誤超過30分鐘，這一班的平均延誤
airplane_chains %>%
  group_by(prev_delay_status = prev_dep_delay > 30) %>%
  summarise(
    avg_current_delay = mean(dep_delay),
    flight_count = n())

# Q2
'Which destinations show the greatest variation in air speed'
flights |>
  select(dest, distance, air_time) |>
  group_by(dest) |> #!題目要的是各個dest
  summarize(var_air_speed = var(distance/air_time, na.rm = T),
            n = n()) |>
  filter(n() > 1) |>
  arrange(desc(var_air_speed)) |>
  relocate(var_air_speed) 

# Q3
'EGE機場是否搬家'

library(nycflights13)
library(ggplot2)
library(dplyr)
# 1. 撈出所有飛往 EGE 的航班
ege_flights <- flights %>% 
  filter(dest == "EGE")

# 2. 畫圖：看看不同年份/月份，不同航空公司的飛行距離變化
ggplot(ege_flights, aes(x = factor(month), y = distance, fill = carrier)) +
  geom_boxplot() +
  labs(title = "Exploring the Adventures of EGE Airport",
       x = "Month",
       y = "Flight Distance (miles)",
       fill = "Airline (Carrier)") +
  theme_minimal()

# -------------------------------------------------------------------------

# 先把資料濃縮成「每個月、每家航空公司的中位數距離」
ege_summary <- flights %>% 
  filter(dest == "EGE") %>% 
  group_by(month, carrier) %>% 
  summarise(median_distance = median(distance, na.rm = TRUE), .groups = "drop")

# 畫折線圖：乾淨俐落，完全沒有 Overplot 的問題
ggplot(ege_summary, aes(x = factor(month), y = median_distance, color = carrier, group = carrier)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "EGE Flight Distance Trend (No Overplotting)", x = "Month", y = "Median Distance") +
  theme_minimal()

# 14  Strings -------------------------------------------------------------
# 如果要打以下三個:單雙引號、反斜線，在他們之前要先加上跳脫字元(反斜線\)
# 前提是內外引號一樣啦 不一樣的話就沒關係 不會被誤認
my_str <- 'this is example of typing \', \", and \\ '
my_str

# 補充: 算字數 用'nchar()'

# 14.2.4 Exercises --------------------------------------------------------
#He said "That's amazing!"
cat('He said \"That\'s amazing!\"') #注意英文縮寫(還是那叫簡寫?)也要
#\a\b\c\d
cat('\\a\\b\\c\\d')
#\\\\\\
cat('\\\\\\\\\\\\')  

# 14.3 Creating many strings from data ------------------------------------
'str_c()' # 把多個vectors變成一個str vector
library(stringr)

whtsthediff <- c('x', 'y', 'z') # length(whtsthediff) = 3
textbookex <- str_c('x', 'y', 'z') # length(textbookex) = 1
class(whtsthediff) #都是character；都是向量，只是長度不同
class(textbookex)  

#paste跟str_c只有在處理NA時、以及 要黏貼的東西長度不同時是否會警告你 不一樣 
paste('Hello', c("John", "Susan"), sep = '_')
str_c("Hello ", c("John", "Susan"))  #單字跟單字中間會自動空格
   
#補充:paste0跟paste的sep=指定符號，符號的位置不同
# paste0 的 sep 是放在不同結果中間，paste 的是放在同一個結果裡字跟字之間

paste(c('x', 'y', 'z'), collapse = ",") #有加collapse=某個間隔符號的話就會偵測內容 最後存成一個而已(長度=1))
paste(c('x', 'y', 'z')) 
paste(..., sep = "") #就可以百分之百取代 paste0()

# 有NA時，指定回應內容，用: -----------------------------------------------------------
'coalesce()'
df |> 
  mutate(
    greeting1 = str_c("Hi ", coalesce(name, "you"), "!"), #如果不是NA就用name,如果是NA就用you
    greeting2 = coalesce(str_c("Hi ", name, "!"), "Hi!")) #如果不是NA就用Hi name!,如果是NA就用Hi!

# 一個句子裡，被很多個變數隔成很多個字串 -----------------------------------------------------
'str_glue()' # 也就是用一組引號就好，裡面的變數名稱用大括號括起來

# 原本的痛苦寫法：
paste("哈囉，我是", name, "，我今年", age, "歲了。")
# 用 str_glue：
str_glue("哈囉，我是 {name}，我今年 {age} 歲了。")


'str_flatten()'
# str_flatten(要壓平的東西，'中間要填充的(default = 沒東西 所以才會說是壓平)')
str_flatten(c("x", "y", "z"), ", ")

# str_flatten(要壓平的東西，'中間要填充的東西'，last = "最後一個中間要填充的東西")
str_flatten(c("x", "y", "z"), ", ", last = ", and ")

#示範:
df <- tribble(
  ~ name, ~ fruit,
  "Carmen", "banana",
  "Carmen", "apple",
  "Marvin", "nectarine",
  "Terence", "cantaloupe",
  "Terence", "papaya",
  "Terence", "mandarin")
df |>
  group_by(name) |>  #row的名稱要是什麼
  summarize(fruits = str_flatten(fruit, ", "))

# 14.3.4 Exercises --------------------------------------------------------
#str_c("The price of ", food, " is ", price)
str_glue("The price of {food} is {price}")
#str_glue("I'm {age} years old and live in {country}")
str_c("I'm", age ,"years old and live in",country, sep = " ") #注意!str_c預設變數跟字串之間沒空格
#str_c("\\section{", title, "}")
str_glue("\\section{ {title} }")

# 14.4 Extracting data from strings ---------------------------------------

#separate_longer_delim
df1 <- tibble(x = c("a,b,c", "d,e", "f"))
df1 |> 
  separate_longer_delim(x, delim = ",") # 依照間隔的元素
#separate_longer_position
df2 <- tibble(x = c("1211", "131", "21"))
df2 |> 
  separate_longer_position(x, width = 1) #依照固定的字元長度
#separate_wider_delim
df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))
df3 |> 
  separate_wider_delim(
    x,
    delim = ".",  #指定間隔元素
    names = c("code", "edition", "year"))  #指定新增的col_name(因為是wider)
df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", NA, "year"))  #! 如果我不要某一組資料 就讓col_name是NA就好
#separate_wider_position
df4 <- tibble(x = c("202215TX", "202122LA", "202325CA")) 
df4 |> 
  separate_wider_position(
    x,
    widths = c(year = 4, age = 2, state = 2)) #指定固定佔了幾格去劃分

# 14.4.3 Diagnosing widening problems -------------------------------------
# 用arguments to help: too_few and too_many

debug <- df |>  
  separate_wider_delim(
    a,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "debug")  #會得到新的3個col(分別是:a_ok  a_pieces a_remainder)
debug |> filter(!a_ok)  #取出不ok的

'上面感覺是在存一個裝著規格不對的資料大集合，存起來看；下面是直接處理，決定之後的資料要長怎樣'

# fill in the missing pieces with NAs and move on.
# => 用:too_few = "align_start"， too_few = "align_end"
df |> 
  separate_wider_delim(
    a,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "align_start")  # 少的就填入NA

# 決定多出來的資料怎麼處理
#用: too_many = "drop"，too_many = "merge"
df |> 
  separate_wider_delim(
    a,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "drop")  # 照格式填完 把多的丟掉

df |> 
  separate_wider_delim(
    a,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "merge")  # 多的都保留 寫在最後一項

# 14.5 Letters ------------------------------------------------------------

str_length() #看字母有幾個 #count()的話是算出現次數

#示範:
library(babynames)  
glimpse(babynames)

#如果我想看每個名字長度的出現次數。結果只會有名字常度跟次數兩行
babynames |>
  count(length = str_length(name), wt = n) 

#如果我想看名字長度是15 的人裡 有哪些名字 各個名字的出現次數
babynames |> 
  filter(str_length(name) == 15) |> 
  count(name, wt = n, sort = TRUE)

# mine
babynames |>
  group_by(name) |> #1
  summarize(n = n()) |> #2
  mutate(length = str_length(name)) |>
  relocate(name, length, n) |>
  arrange(desc(length), desc(n))

# count()的用法: # 自動幫你 group_by + summarize(n=n()) 變成不重複名單
# 這是你程式碼的「人類閱讀精簡版」，效能跟你原本的一樣好！
babynames |>
  count(name) |> #1+2
  mutate(length = str_length(name)) |>
  relocate(name, length, n) |>
  arrange(desc(length), desc(n))

# 14.5.2 Subsetting -------------------------------------------------------
'str_sub(string, start, end)'

babynames |> 
  mutate(
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1))

# 14.5.3 Exercises --------------------------------------------------------

# Q3
'trends in length of babynames over time?'
range(babynames$year)

babynames |>
  mutate(length = factor(str_length(name)))  |> # 轉成 factor，讓每種長度自成一條線
  # 加上 weight = n（R 會把實際出生人數加總，用於叫R用我算好了的數字，不要再自己去count時）
  ggplot(aes(x = year, weight = n, color = length)) + #n可以改成prop。
  geom_freqpoly(lwd = 0.01) +
  coord_cartesian(xlim = c(1880, 2017))

'popularity of first and last letters?'
library(babynames)
library(tidyverse)

babynames |>
  # 1. 抓出首字與尾字，並統一轉大寫
  mutate(
    `首字 (First Letter)` = toupper(str_sub(name, start = 1, end = 1)),
    `尾字 (Last Letter)`  = toupper(str_sub(name, start = -1, end = -1))) |>
  # 2. 轉成「長資料」以便同時畫兩張圖
  pivot_longer(
    cols = c(`首字 (First Letter)`, `尾字 (Last Letter)`),
    names_to = "position",
    values_to = "letter") |>
  # 3. 開始畫圖
  ggplot(aes(x = year, weight = n, color = letter)) +
  geom_freqpoly(binwidth = 1, show.legend = FALSE) + # 字母太多，不顯示圖例畫面比較乾淨
  facet_wrap(~ position, scales = "free_y") +
  coord_cartesian(xlim = c(1880, 2017)) +
  labs(
    title = "美國嬰兒名字首字與尾字之歷史流行趨勢",
    x = "年份",
    y = "出生總人數"
  ) +
  theme_minimal()

# 15  Regular expressions -------------------------------------------------
# 15.2 Pattern basics

str_view(fruit, "apple")
str_view(fruit, "apple|melon|nut") #alternation

#. 跟空白框框一樣
str_view(words, "q...e")
str_view(words, "q....e")
str_view(words, "q.e")

#找出a + 任意一格
str_view(c("a", "ab", "ae", "bd", "ea", "eab"), "a.")

# Quantifiers (數量詞) -------------------------------------------------------
# 控制緊接著左邊的那一個字母!
'?': 0到1次(可有可無)
'+': 至少一次，越多越好
'*': 隨便(0~無限多次)

# Character classes -------------------------------------------------------
"[]": 只要有出現在括號裡，就算符合
#例如:[aeiou]
"[^]": 加上^，變成反義
  
"[]x[]":給定左右兩邊指定要的set
#例如:[aeiou]x[aeiou]
  
"[^]y[^]":當前後是^時 要把X改成Y

# 15.3 Key functions ------------------------------------------------------

# 15.3.1 Detect matches ---------------------------------------------------

str_detect(c("a", "b", "c"), "[aeiou]") #(我要判斷的東西們，有沒有在我指定的set裡)；set用"[]"包起來
#看到產出邏輯值 => 想到跟filter連用

# 示範: 我想找名字裡有X的人的數字 以及他們的名字
babynames |>
  filter(str_detect(name, "[x]")) |>
  count(name,wt = n, sort = T) # 只有wt=n, 沒有weight=n這個東西
# 沒加wt=n的話 算出來會是名字在每年是否有出現。加了才是真正的人數
  
babynames |> 
  summarize(含有x的比例 = mean(str_detect(name, "[x]")))
# 示範: (翻譯程式碼想做的事:每年名字有X的比例 的圖)
babynames |> 
  group_by(year) |>  #想看每年 依照年來分 => group_by(year)
  summarize(prop_x = mean(str_detect(name, "x"))) |> # 在確定好X軸變數的組別之後 以既定組別為前提 計算Y軸
  ggplot(aes(x = year, y = prop_x)) + 
  geom_line()
  
#補充:還有
'str_subset()' #跟str_view不同在這個是得到字串向量，可存入變數，進行後續資料清理。view是用來檢查與測試的，得到 HTML/文字顯示物件（僅供閱讀）
# more arguments:
#1. negate = TRUE，抓出「不包含」指定條件的元素。
#2. regex("A", ignore_case = TRUE)，取代"A"，忽略大小寫

'str_which()' # which是return數字index,告訴你符合的東西在哪些位置

# 15.3.2 Count matches ----------------------------------------------------
' str_count()' # 看我的X裡的每個元素 各自出現幾次我好奇的東西

x <- c("apple", "banana", "pear")
str_count(x, "p") # X有n個元素，則得到n個數字

str_count("abababa", "aba")
str_view("abababa", "aba") # this shows that :regex matches never overlap(前後不重疊)

# str_count()與mutate連用，示範:
babynames |> 
  count(name) |>  # 先數每個名字的出現次數
  mutate(
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]"))
#解決上面的大小寫問題: 
#1. 土法煉鋼 把所有大小寫寫進[]裡
#2. regex("[aeiou]", ignore_case = TRUE) 
#3. 換成小寫: str_to_lower(name) ； 可以寫在str_count哩，也可以在mutate時新增一個新的小寫變數

# 15.3.3 Replace values ---------------------------------------------------
'str_replace()' #(my_data, set(我要替換掉哪些元素)，換成什麼東西)
# 示範
x <- c("apple", "pear", "banana")
str_replace_all(x, "[aeiou]", "-")

'str_replace_all()'  #(my_data, set(哪些元素我要讓他們消失))
# 只給兩個argument,沒指定新的要換成什麼東西時，default = ""，等同於str_replace(x, pattern, "")
# ! 結論: 它們兩個差在 all是換掉全部的 否則只會換第一個!

# str_replace() 常與mutate連用，資料清理

# 15.3.4 Extract variables ------------------------------------------------
'separate_wider_regex(my_data, patterns(也就是格式))'
df <- tribble(
  ~str,
  "<Sheryl>-F_34",
  "<Kisha>-F_45", 
  "<Brandon>-N_33",
  "<Sharon>-F_38", 
  "<Penny>-F_58",
  "<Justin>-M_41", 
  "<Patricia>-F_84",) #最後要逗號喔? 好奇怪

df |> 
  separate_wider_regex(str,
                       patterns = c("<",  name = "[A-Za-z]+", ">-", gender = ".", "_", age = "[0-9]+"))
# regex可以讓我指定格式，格式裡的變數是新的，執行完會得到以新變數為col_name的新表格
# 新變數也需要指定格式，用之前學/練習過的那些符號
 
#如果對照失敗，跟前面的separate_wider_delim() and separate_wider_position()一樣，用too_few = "debug"來看哪些錯了

# 15.3.5 Exercises --------------------------------------------------------
# Q1
'What baby name has the most vowels? What name has the highest proportion of vowels? (Hint: what is the denominator?)'
babynames |> 
  distinct(name) |>  
  mutate(prop_vowels = str_count(name, "[aeiouAEIOU]")/str_length(name), #str_length() 用來算字長
         n_vowels = sum(str_count(name, "[aeiouAEIOU]"))) |>
  arrange(desc(prop_vowels), desc(n_vowels))|> 
#可以再加上: 
# 找出母音最多、與比例最高的人
  summarize(
  max_vowels_name = name[which.max(n_vowels)],
  max_prop_name   = name[which.max(prop_vowels)])

# Q2  
x <- "a/b/c/d/e"
y <- str_replace_all(x, "/", "\\\\")
print(y)

# Q3
'用 str_replace_all() 做 str_to_lower() '
input <- readline(prompt = "請輸入英文字母，大小寫不限:")
#input <- "aiJF"
str_replace_all(input, "[A-Z]", tolower) #第三個是字串或是函數，寫"[a-z]"的話是錯的

# Q4
'Create a regular expression that will match telephone numbers as commonly written in your country.'
#(+886)0911-235-653
"\\(\\+[0-9]{3}\\)[0-9]{4}-?[0-9]{3}-?[0-9]{3}"
#括號、加號+、點. 這三個前面要加上解鎖符號(escape) \\

# 15.4.1 Escaping ---------------------------------------------------------
# to match a literal value like: ., $, |, *, +, ?, {, }, (, ),
# 用中括號包起來，就不用用反斜線了。 !用在pattern裡(要match時)

# 15.4.2 Anchors ----------------------------------------------------------

str_view(fruit, "^a") #words that start with 字母a
str_view(fruit, "a$") #end

x <- c("summary(x)", "summarize(df)", "rowsum(x)", "sum(x)")
str_view(x, "\\bsum\\b") # 指定the start or end of a word (match the boundary between words)

# 綜合示範:
str_replace_all("abc", c("$", "^", "\\b"), "--")
#引號裡的跳脫字元要兩個

# -------------------------------------------------------------------------
# 找我給定的X裡 哪些字元符合某種格式
\d matches any digit
\s matches any whitespace (e.g., space, tab, newline)
\w matches any “word” character, i.e. letters and numbers
# 大寫(D S W)就是相反

#舉例:
x <- "abcd ABCD 12345 -!@#%."
str_view(x, "\\S+")
str_view(x, "\\w+")

# 15.4.4 Quantifiers ------------------------------------------------------

# control how many times a pattern matches
{n} matches exactly n times.
{n,} matches at least n times.
{n,m} matches between n and m times.

str_view("ababab", "ab+")
str_view("ababab", "(ab)+")

# ?:
x <- c("a gray cat", "a grey dog")
str_match(x, "gr(e|a)y")
str_match(x, "gr(?:e|a)y")

#選項 (Alternation)： 優先級最低
# \1 回頭引用 (Backreference): 重複前面抓到的東西
# Lookahead / Lookbehind (前後預查)

# 15.5.1 Regex flags ------------------------------------------------------
regex()
# ignore_case = TRUE
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))

# dotall = TRUE
x <- "Line 1\nLine 2\nLine 3"
str_view(x, ".Line")
str_view(x, regex(".Line", dotall = TRUE))

# multiline = TRUE
x <- "Line 1\nLine 2\nLine 3"
str_view(x, "^Line")
str_view(x, regex("^Line", multiline = TRUE))

# comments = TRUE
phone <- regex(
  r"(\(?     # optional opening parens
    (\d{3}) # area code
    [)\-]?  # optional closing parens or dash
    \ ?     # optional space
    (\d{3}) # another three numbers
    [\ -]?  # optional space or dash
    (\d{4}) # four more numbers)", 
  comments = TRUE)

str_extract(c("514-791-8141", "(123) 456 7890", "123456"), phone)

# 15.5.2 Fixed matches ----------------------------------------------------
' fixed()' #先跳過








