###################
### Download data
##################
spaceship <-  read.table(
"https://raw.githubusercontent.com/Emmanuel-Dupuis-GitHub/SpaceShip/main/spaceship.csv"
, sep = ",", dec = ".", header = TRUE)

### Visualization of the first lines
head(spaceship)


########################
## Changing the columns
########################

### We will create a column containing the group of each passenger
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
temp = spaceship$PassengerId %>% str_split(pattern="_", simplify=TRUE)
spaceship$Group = temp[, 1] %>% as.factor()

### We will create 3 columns containing the deck, the number, and the side of each cabin.
temp = spaceship$Cabin %>% str_split(pattern="/", simplify=TRUE)
spaceship$CabinDeck = temp[, 1] %>% as.factor()
spaceship$CabinNum = temp[, 2] %>% as.integer()
spaceship$CabinSide = temp[, 3] %>% as.factor()
spaceship$Cabin <- NULL

### We will create 2 columns containing first nane and the last name of each passenger.
temp = spaceship$Name %>% str_split(pattern=" ", simplify=TRUE)
spaceship$FirsName = temp[, 1] %>% as.character()
spaceship$LastName = temp[, 2] %>% as.character()
spaceship$Name <- NULL

### Visualization of the first new lines
head(spaceship)


########################
## Verification of the data type
########################

spaceship$PassengerId = as.factor(spaceship$PassengerId)
spaceship$HomePlanet = as.factor(spaceship$HomePlanet)
spaceship$Destination = as.factor(spaceship$Destination)
spaceship$CryoSleep = as.logical(spaceship$CryoSleep)
spaceship$VIP = as.logical(spaceship$VIP)
spaceship$Age = as.integer(spaceship$Age)
spaceship$RoomService = as.numeric(spaceship$RoomService)
spaceship$FoodCourt = as.numeric(spaceship$FoodCourt)
spaceship$ShoppingMall = as.numeric(spaceship$ShoppingMall) 
spaceship$Spa = as.numeric(spaceship$Spa) 
spaceship$VRDeck = as.numeric(spaceship$VRDeck) 
spaceship$FirsName = as.character(spaceship$FirsName)
spaceship$LastName = as.character(spaceship$LastName)
spaceship$CabinDeck = as.factor(spaceship$CabinDeck) 
spaceship$CabinSide = as.factor(spaceship$CabinSide) 
spaceship$CabinNum = as.integer(spaceship$CabinNum)
spaceship$Group = as.factor(spaceship$Group) 
spaceship$Transported=as.logical(spaceship$Transported) 


########################
## Management of empty cells
########################

### If some cells are empty, we place the value NA in them.
spaceship[spaceship==""] <- NA

### We can now examine the distribution of values in each of the columns.
summary(spaceship)

### We replace the NA values of the factor or character columns by the mode  and  the NA values of the numerical columns by the median.
spaceship[is.na(spaceship$HomePlanet),"HomePlanet"]<- "Earth"
spaceship[is.na(spaceship$CryoSleep),"CryoSleep"]<- FALSE
spaceship[is.na(spaceship$Destination),"Destination"]<- "TRAPPIST-1e"
spaceship[is.na(spaceship$Age),"Age"]<- 27
spaceship[is.na(spaceship$VIP),"VIP"]<- FALSE
spaceship[is.na(spaceship$RoomService),"RoomService"]<- 0
spaceship[is.na(spaceship$FoodCourt),"FoodCourt"]<- 0
spaceship[is.na(spaceship$ShoppingMall),"ShoppingMall"]<- 0
spaceship[is.na(spaceship$Spa),"Spa"]<- 0
spaceship[is.na(spaceship$VRDeck),"VRDeck"]<- 0
spaceship[is.na(spaceship$CabinDeck),"CabinDeck"]<- "F"
spaceship[is.na(spaceship$CabinNum),"CabinNum"]<- 427
spaceship[is.na(spaceship$CabinSide),"CabinSide"]<- "S"


########################
## An additional column
########################

### We add a last column containing the travellers total expenses. 
spaceship$total=
spaceship$RoomService+spaceship$FoodCourt+spaceship$ShoppingMall+spaceship$Spa+spaceship$VRDeck

### Visualization of the first new lines
head(spaceship)


########################
## train set and test set
########################

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
set.seed(2, sample.kind = "Rounding") #if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(spaceship$Transported, times = 1, p = 0.2, list = FALSE)
test_set <- spaceship[test_index, ]
train_set <- spaceship[-test_index, ]

########################
### A first  graphical study of the variables distribution of the population according to variables
########################

train_set %>% ggplot(aes(x=HomePlanet,fill=HomePlanet)) + geom_bar() + theme(
  axis.text.x = element_blank())  + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(x=CryoSleep,fill=CryoSleep)) + geom_bar() + theme(
  axis.text.x = element_blank())  + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(x=VIP,fill=VIP)) + geom_bar() + theme(
  axis.text.x = element_blank())  + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(x=CabinDeck,fill=CabinDeck)) + geom_bar() + theme(
  axis.text.x = element_blank())  + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(x=CabinSide,fill=CabinSide)) + geom_bar()+ theme(
  axis.text.x = element_blank())  + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(x=Destination,fill=Destination)) + geom_bar() + theme(
  axis.text.x = element_blank())  + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(Age)) + geom_histogram(bins = 35)  + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(CabinNum)) + geom_histogram(bins = 35) + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(RoomService)) + geom_histogram(bins = 35)  + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(FoodCourt)) + geom_histogram(bins = 35)  + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(ShoppingMall)) + geom_histogram(bins = 35)  + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(Spa)) + geom_histogram(bins = 35)  + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(VRDeck)) + geom_histogram(bins = 35)  + scale_y_continuous(name="Population size (count)")

train_set %>% ggplot(aes(total)) + geom_histogram(bins = 35)  + scale_y_continuous(name="Population size (count)")


########################
### A graphical study of the relationship between the variable Transported to explane and the explanatory variables
########################
 
train_set %>% group_by(HomePlanet) %>%
  summarize(avg = mean(Transported == "TRUE")) %>% 
  ggplot(aes(HomePlanet, avg, fill = HomePlanet)) + geom_col() + labs(y = "% Transported by Category ") + 
  scale_y_continuous(labels = scales::percent) + theme( axis.text.x = element_blank()) 

train_set %>% group_by(CryoSleep) %>%
  summarize(avg = mean(Transported == TRUE)) %>% 
  ggplot(aes(CryoSleep, avg, fill = CryoSleep)) + geom_col() + labs(y = "% Transported by Category ") +
  scale_y_continuous(labels = scales::percent) + theme( axis.text.x = element_blank()) 

train_set %>% group_by(Destination) %>%
  summarize(avg = mean(Transported == TRUE)) %>% 
  ggplot(aes(Destination, avg, fill = Destination)) + geom_col() + labs(y = "% Transported by Category ") +
  scale_y_continuous(labels = scales::percent) + theme( axis.text.x = element_blank())

train_set %>% group_by(VIP) %>%
  summarize(avg = mean(Transported == TRUE)) %>% 
  ggplot(aes(VIP, avg, fill = VIP)) + geom_col() + labs(y = "% Transported by Category ") +
  scale_y_continuous(labels = scales::percent) + theme( axis.text.x = element_blank())

train_set %>% group_by(CabinDeck) %>%
  summarize(avg = mean(Transported == TRUE)) %>% 
  ggplot(aes(CabinDeck, avg, fill = CabinDeck)) + geom_col() + labs(y = "% Transported by Category ") +
  scale_y_continuous(labels = scales::percent)  + theme( axis.text.x = element_blank())

train_set %>% group_by(CabinSide) %>%
  summarize(avg = mean(Transported == TRUE)) %>% 
  ggplot(aes(CabinSide, avg, fill = CabinSide)) + geom_col() + labs(y = "% Transported by Category ") + scale_y_continuous(labels = scales::percent)  + theme( axis.text.x = element_blank())

train_set %>% mutate(RoomService = RoomService +1) %>%
  ggplot(aes(RoomService,fill=Transported)) +
  geom_density(alpha=0.5,bw=1) + scale_x_log10() 

train_set %>% mutate(FoodCourt = FoodCourt +1) %>%
  ggplot(aes(FoodCourt,fill=Transported)) +
  geom_density(alpha=0.5,bw=1) + scale_x_log10() 

train_set %>% mutate(ShoppingMall = ShoppingMall +1) %>%
  ggplot(aes(ShoppingMall,fill=Transported)) +
  geom_density(alpha=0.5,bw=1) + scale_x_log10() 

train_set %>% mutate(Spa = Spa +1) %>%
  ggplot(aes(Spa,fill=Transported)) +
  geom_density(alpha=0.5,bw=1) + scale_x_log10() 

train_set %>% mutate(VRDeck = VRDeck +1) %>%
  ggplot(aes(VRDeck,fill=Transported)) +
  geom_density(alpha=0.5,bw=1) + scale_x_log10() 

train_set %>% mutate(total = total +1) %>%
  ggplot(aes(total,fill=Transported)) +
  geom_density(alpha=0.5,bw=1) + scale_x_log10() 

train_set %>%
  ggplot(aes(Age,fill=Transported)) +
  geom_density(alpha=0.5,bw=1)


########################
### Evaluation metrics used
########################

accuracy <- function(true_vector, predicted_vector)
{ mean(true_vector == predicted_vector)
}


########################
## First basic learning machine based only on the  variable CryoSleep
########################

prediction <- ifelse(test_set$CryoSleep == TRUE, TRUE, FALSE)    
model_accuracy = accuracy(test_set$Transported , prediction)
model_accuracy


########################
## Second basic learning machine based only on the  variable total
########################

prediction <- ifelse(test_set$total > 50, FALSE,TRUE)   
model_accuracy = accuracy(test_set$Transported , prediction)
model_accuracy


########################
## A logistic regression model with these three explanatory variables : total, CryoSleep and Age
########################

### The variable Transported must be considered as a qualitative variable of the type factor.
train_set$Transported=as.factor(train_set$Transported) 
test_set$Transported=as.factor(test_set$Transported) 

### train a GLM model on the training set, we will use the caret train function:
set.seed(1, sample.kind = "Rounding") 
train_glm <- train(Transported ~ total + CryoSleep + Age, method = "glm", data = train_set)
prediction <- predict(train_glm, test_set)
model_accuracy = accuracy(test_set$Transported , prediction)
model_accuracy

########################
## The kNN model with these three explanatory variables : total, CryoSleep and Age
########################

set.seed(6, sample.kind = "Rounding") 
train_knn <- train(Transported ~ Age + total + CryoSleep,
                   method = "knn",
                   data = train_set)
prediction <- predict(train_knn, test_set)
model_accuracy = accuracy(test_set$Transported , prediction)
model_accuracy


########################
## The kNN model with more explicative variables
########################

set.seed(6, sample.kind = "Rounding") 
train_knn <- train(Transported ~ Age + CryoSleep + RoomService + FoodCourt + ShoppingMall 
                   + Spa + VRDeck,
                   method = "knn",
                   data = train_set)
prediction <- predict(train_knn, test_set)
model_accuracy = accuracy(test_set$Transported , prediction)
model_accuracy

########################
## The classification tree model
########################

if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
library(rpart)
repart_fit <- rpart(Transported ~ Age + CryoSleep + RoomService + FoodCourt + ShoppingMall 
                    + Spa + VRDeck, data = train_set)
prediction <- predict(repart_fit, test_set, type = "class")
model_accuracy = accuracy(test_set$Transported , prediction)
model_accuracy


########################
## The random forest model
########################

if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
library(randomForest)
Forest_fit <- randomForest(Transported ~ Age + CryoSleep + RoomService + FoodCourt 
                           + ShoppingMall + Spa + VRDeck , data = train_set)
prediction <- predict(Forest_fit, test_set, type = "class")
model_accuracy = accuracy(test_set$Transported , prediction)
model_accuracy


########################
## The random forest model with more explicative variables
########################

Forest_fit <- randomForest(Transported ~ HomePlanet + Destination + Age + VIP 
                           + CryoSleep + RoomService + FoodCourt + ShoppingMall 
                           + Spa + VRDeck + CabinDeck + CabinSide + CabinNum, data = train_set)
Forest_model <- predict(Forest_fit, test_set, type = "class")
prediction <- predict(Forest_fit, test_set, type = "class")
model_accuracy = accuracy(test_set$Transported , prediction)
model_accuracy


