library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(glm2)
library(car)
library(tinytable)
library(stringr)
library(stargazer)
L2 = read.csv("L2_cleaned.csv")

Naive = read.csv("naive_cleaned.csv")

Native = read.csv("native_cleaned.csv")

final_df = read.csv("Final.csv")

L2_cleaned_2 <- L2 %>%
  filter(!is.na(RatingType))

L2_cleaned_3 <- L2_cleaned_2[-(621:640), ]

Native_cleaned <- Native %>%
  filter(!is.na(RatingType))

Naive_cleaned <- Naive %>%
  filter(!is.na(RatingType))

Native_cleaned$Group <- "Native"
L2_cleaned_3$Group <- "L2"
Naive_cleaned$Group <- "Naive"

combined_df <- bind_rows(Native_cleaned, L2_cleaned_3, Naive_cleaned)

lmer = lmer(Rating ~ proficiency + (1|Prototype) + Emphatic_Sign*Confident_Sign, data = combined_df)

lmer.model = lmer(Rating ~ proficiency + (1|Prototype) + Emphatic_Sign*Confident_Sign + (1|speaker), data = combined_df)

lmer.model_2 = lmer(Rating ~ proficiency + (1|Prototype) + (1|Emphatic_Sign)+ (1|Confident_Sign) + (1|speaker), data = combined_df)

lmer.model_3 = lmer(Rating ~ proficiency + (1|Prototype) + Emphatic_Sign*Confident_Sign + (1|RatingType) + (1|speaker), data = combined_df)

anova(lmer, lmer.model)

AIC(lmer, lmer.model, lmer.model_2, lmer.model_3)
BIC(lmer, lmer.model, lmer.model_2, lmer.model_3)

hist(residuals(lmer.model))
qqnorm(residuals(lmer.model))

combined_df_1 <- combined_df %>%
  mutate(
    Emphatic_rating = if_else(RatingType == "Emphatic", Rating, NA_real_),
    Confident_rating = if_else(RatingType == "Confident", Rating, NA_real_)
  )

combined_ <- data %>% filter(!is.na(Emphatic_rating))

combined_df_3 <- combined_df_2 %>% filter(!is.na(Emphatic_rating))

combined_df_4 = select(combined_df_3, -c(X.1, LBQ_3, Original_Column, RatingType, Rating))



combined_df_4 <- combined_df_4 %>%
  mutate(across(X, Prototype, EmotionCode, Emphatic_Sign, Confident_Sign, speaker, as.factor))


combined_df_5 <- combined_df_4 %>%
  rename(
    Subject = X,
    Sentence_Prototype = Prototype,
    Speaker = speaker,
    Proficiency = proficiency
  )

combined_df_6 <- combined_df_5  %>%
  mutate(participant = factor((row_number() - 1) %/% 40 + 1))


combined_df_7 <- combined_df_6 %>%
  mutate(speaker_group = case_when(
    Proficiency == 1 ~ "Native",
    Proficiency == 0 ~ "Naive",
    Proficiency > 0 & Proficiency < 1 ~ "L2"
  ))

combined_df_8 <- combined_df_8 %>%
  mutate(
    Sentence_Prototype = as.factor(Sentence_Prototype), 
    Emphatic_Sign = as.factor(Emphatic_Sign),
    Confident_Sign = as.factor(Confident_Sign),
    Speaker = as.factor(Speaker),
    speaker_group = as.factor(speaker_group),
    participant = as.factor(participant),
  )

write.csv(combined_df_8, file = "Final.csv")

final_df = read.csv("Final2.csv", stringsAsFactors = TRUE)

final_df$EmotionCode = factor(final_df$EmotionCode)
final_df$speaker_group = factor(final_df$speaker_group)
final_df$speaker_group = relevel(final_df$speaker_group, "Naive")
final_df$Speaker = factor(final_df$Speaker)

lmer.Emphatic = lmer(Emphatic_rating ~ Proficiency + (1|participant) + (1|Speaker) + (1|Sentence_Prototype) + Emphatic_Sign*Confident_Sign, data = final_df)
lmer.Confident = lmer(Confident_rating ~ Proficiency + (1|participant) + (1|Speaker) + (1|Sentence_Prototype) + Emphatic_Sign*Confident_Sign, data = final_df)

lmer.Emphatic.1 = lmer(Emphatic_rating ~ Proficiency + (1|speaker_group) + (1|participant) + (1|Speaker) + (1|Sentence_Prototype) + Emphatic_Sign*Confident_Sign, data = final_df)
lmer.Emphatic.2 = lmer(Emphatic_rating ~ Proficiency + (1|speaker_group) + (1|participant) + (1|Speaker) + Emphatic_Sign*Confident_Sign, data = final_df)
lmer.Emphatic.3 = lmer(Emphatic_rating ~ Proficiency + (1|participant) + (1|Speaker) + Emphatic_Sign*Confident_Sign, data = final_df)

lmer.Emphatic.4 = lmer(Emphatic_rating ~ Proficiency + (1|participant) + (1|Speaker) +(1|EmotionCode), data = final_df)

lmer.Emphatic.5 = lmer(Emphatic_rating ~ EmotionCode + speaker_group + (1|participant) + (1|Sentence_Prototype) + (1|Speaker), data = final_df)

lmer.Emphatic.6 = lmer(Emphatic_rating ~ EmotionCode * speaker_group + (1|participant) + (1|Sentence_Prototype) + (1|Speaker), data = final_df)

lmer.Emphatic.7 = lmer(Emphatic_rating ~ EmotionCode * speaker_group + Speaker + (1|participant) + (1|Sentence_Prototype), data = final_df)


hist(residuals(lmer.Confident))

qqnorm(residuals(lmer.Confident))

hist(residuals(lmer))
qqnorm(residuals(lmer))
anova(lmer)

summary(lmer.Emphatic.3)
summary(lmer.Emphatic)

anova(lmer.Emphatic.1, lmer.Emphatic.2, lmer.Emphatic.3)


hist(residuals(lmer.Emphatic.1))

qqnorm(residuals(lmer.Emphatic.1))

n_rows <- nrow(final_df) - 630

new_participants <- rep(17:(16 + ceiling(n_rows / 40)), each = 40)[1:n_rows]

final_df$participant[631:nrow(final_df)] <- factor(new_participants, levels = unique(new_participants))


AIC(lmer.Emphatic, lmer.Emphatic.1)
BIC(lmer.Emphatic, lmer.Emphatic.1)

anova(lmer.Emphatic, lmer.Emphatic.1)


summary(lmer.Emphatic)
summary(lmer.Confident)


lmer.model_4 = lmer(proficiency ~ Emphatic_rating + Confident_rating + (1|Prototype) + Emphatic_Sign*Confident_Sign + (1|speaker), data = combined_df_1)


final_df$speaker_group

ggplot(data = final_df, aes(Emphatic_rating, fill = speaker_group)) + geom_bar(stat = "count", pos = "dodge") +
  facet_wrap(~EmotionCode)

ggplot(data = final_df, aes(Confident_rating, fill = speaker_group)) + geom_bar(stat = "count", pos = "dodge") +
  facet_wrap(~EmotionCode)


lmer.Confident.5 = lmer(Confident_rating ~ EmotionCode + speaker_group + (1|participant) + (1|Sentence_Prototype), data = final_df)

lmer.Confident.6 = lmer(Confident_rating ~ EmotionCode * speaker_group + (1|participant) + (1|Sentence_Prototype), data = final_df)

lmer.Confident.6 = lmer(Confident_rating ~ EmotionCode * speaker_group + (1|participant) + (1|Sentence_Prototype) + (1|Speaker), data = final_df)


anova(lmer.Confident.5, lmer.Confident.6)

lmer.Confident.7 = lmer(Confident_rating ~ EmotionCode * speaker_group + narrator + (1|participant) + (1|Sentence_Prototype), data = final_df)

lmer.Emphatic.7 = lmer(Emphatic_rating ~ EmotionCode * speaker_group + narrator + (1|participant) + (1|Sentence_Prototype), data = final_df)

em.Emphatic = emmeans(lmer.Emphatic.7, ~ EmotionCode*speaker_group)
em.Confident = emmeans(lmer.Confident.7, ~EmotionCode*speaker_group)
suname = c('participant')
final_df = final_df %>%
  mutate(across(all_of(name), as.factor))

sapply(final_df, class)

summary(lmer.Emphatic.7)


final_df <- final_df %>%
  mutate(EmotionCode = str_replace_all(EmotionCode, "1", "Plain_Statement"))

final_df <- final_df %>%
  mutate(EmotionCode = str_replace_all(EmotionCode, "2", "Plain_Question"))

final_df <- final_df %>%
  mutate(EmotionCode = str_replace_all(EmotionCode, "Surprise_Question", "Emphatic_Question"))

final_df <- final_df %>%
  mutate(EmotionCode = str_replace_all(EmotionCode, "4", "Command"))

final_df = final_df %>%
  rename(narrator = Speaker)

final_df <- final_df %>%
  mutate(narrator = str_replace_all(narrator, "S", "F"))

final_df = final_df %>%
  mutate(narrator = str_replace_all(narrator, "L", "M"))


em_df_Emphatic = as.data.frame(em.Emphatic)

em_df_Confident = as.data.frame(em.Confident)


ggplot(em_df_Emphatic, aes(x = speaker_group, y = emmean, fill = speaker_group)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  facet_wrap(~ EmotionCode) +
  labs(title = "Estimated Means by Speaker Group and Sentence Type",
       y = "Estimated Score",
       x = "Speaker Group") +
  theme_minimal()

ggpairs(em_df_Emphatic, columns = 1:4, aes(color = speaker_group, alpha = 0.5),
        lower = list(categorical = "smooth"))


ggplot(em_df_Emphatic, aes(x = speaker_group, y = emmean, color = speaker_group)) +
  geom_line(aes(group = EmotionCode)) +
  geom_point() +
  theme_light()



ggplot(em_df_Emphatic, aes(x = factor(speaker_group, levels = c("Naive", "L2", "Native")), y = emmean, fill = speaker_group)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  facet_wrap(~ EmotionCode, labeller = labeller(EmotionCode = custom_labels_Emphatic)) +
  theme_bw() +
  labs(title = "Emphatic Rating Emmeans")

ggplot(em_df_Confident, aes(x = factor(speaker_group,levels = c("Naive", "L2", "Native")), y = emmean, fill = speaker_group)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  facet_wrap(~ EmotionCode, labeller = labeller(EmotionCode = custom_labels_Confident)) +
  theme_bw() +
  labs(title = "Confidence Rating Emmeans")


custom_labels_Emphatic <- c(
  "Plain_Statement" = "Plain_Statement(-)",
  "Command" = "Command(+)",
  "Emphatic_Question" = "Emphatic_Question(+)",
  "Plain_Question" = "Plain_Question(-)"
)


custom_labels_Confident <- c(
  "Plain_Statement" = "Plain_Statement(+)",
  "Command" = "Command(+)",
  "Emphatic_Question" = "Emphatic_Question(-)",
  "Plain_Question" = "Plain_Question(-)"
)


ggplot(em_df_Emphatic, aes(x = EmotionCode, y = emmean, fill = EmotionCode)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  facet_wrap(~ speaker_group) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),     # removes x-axis text
    axis.ticks.x = element_blank()     # removes x-axis tick marks
  ) +
  labs(title = "Emphatic Rating Emmeans")
