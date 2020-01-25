library(tidyverse)

Col_scores = c(0.46146209852804976, 0.4660142987294707, 0.46332375803309916,
              0.48535528831653807, 0.5033691901037788, 0.5113087788565597,
              0.5024807248550341, 0.5217688330905572, 0.522906106011823,
              0.5362983851935087)
Col_classes = c(0,0,0,0,0,1,1,1,1,1)

LFS_scores = c(0.143248980250318, 0.24897467608935317, 0.44782411190511706,
               0.6626369593025436, 0.6624945737277211, 0.7663180280849983)
LFS_classes = c(0,0,1,1,1,1)

df_Col = data.frame(Col_scores, Col_classes)
df_LFS = data.frame(LFS_scores, LFS_classes)

ggplot(df_Col, aes(x = Col_scores, y = Col_classes, colour = Col_scores > mean(Col_scores))) + geom_point()
