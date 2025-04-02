library(readxl)
library(ggplot2)
library(dplyr)
library(cluster)
library(FNN)
library(caret)
library(shiny)

## ----------------------------
## 1. Enhanced Data Loading & Preprocessing
## ----------------------------

load_recipe_data <- function(file_path, n_max = 500) {
  data <- read_excel(file_path, sheet = 1, n_max = n_max)
  
  data <- data %>%
    select(
      id = RecipeId,
      name = Name,
      cook_time = CookTime,
      prep_time = PrepTime,
      calories = Calories,
      fat = FatContent,
      saturated_fat = SaturatedFatContent,
      cholesterol = CholesterolContent,
      sodium = SodiumContent,
      carbs = CarbohydrateContent,
      fiber = FiberContent,
      sugar = SugarContent,
      protein = ProteinContent,
      ingredients = RecipeIngredientParts,
      instructions = RecipeInstructions
    ) %>%
    mutate(cook_time = as.numeric(cook_time),
           prep_time = as.numeric(prep_time),
           total_time = cook_time + prep_time,
           is_vegetarian = !grepl("chicken|beef|pork|fish|meat|bacon|lamb|gelatin|shrimp|salmon|halibut steaks|tuna", ingredients, ignore.case = TRUE),
           is_vegan = !grepl("chicken|beef|pork|fish|meat|bacon|lamb|gelatin|salmon|milk|cheese|egg|butter|cream|honey|shrimp|halibut steaks|tuna", 
                             ingredients, ignore.case = TRUE))
  
  return(data)
}

recipe_data <- load_recipe_data("C:/01_College/SEM 6/Datasets/recipes.xlsx")
#View(recipe_data %>% select(name, ingredients, is_vegetarian, is_vegan))

## ----------------------------
## 2. Nutritional Analysis Module
## ----------------------------

daily_values <- list(
  calories = 2000,
  fat = 65,
  saturated_fat = 20,
  cholesterol = 300,
  sodium = 2300,
  carbs = 300,
  fiber = 28,
  sugar = 50,
  protein = 50
)

analyze_nutritional_gaps <- function(intake_data, user_profile) {
  adjusted_values <- adjust_recommendations(daily_values, user_profile)
  
  gap_analysis <- intake_data %>%
    mutate(
      across(c(calories:sugar), 
             ~ round(.x / adjusted_values[[cur_column()]] * 100, 1),
             .names = "{.col}_percent")
    )
  
  return(gap_analysis)
}

adjust_recommendations <- function(values, profile) {
  if (profile$activity_level == "high") {
    values$calories <- values$calories * 1.2
    values$protein <- values$protein * 1.3
  } else if (profile$activity_level == "low") {
    values$calories <- values$calories * 0.8
  }
  
  if (profile$dietary_restriction == "vegetarian") {
    values$protein <- values$protein * 1.2
  }
  
  return(values)
}

## ----------------------------
## 3. Clustering Module (K-means)
## ----------------------------

cluster_recipes <- function(data, n_clusters = 4) {
  nutritional_features <- data %>%
    select(calories:protein) %>%
    scale()
  
  set.seed(123)
  clusters <- kmeans(nutritional_features, centers = n_clusters)
  
  data$cluster <- as.factor(clusters$cluster)
  return(list(data = data, model = clusters))
}

clustering_result <- cluster_recipes(recipe_data)
recipe_data <- clustering_result$data

plot_clusters <- function(data) {
  ggplot(data, aes(x = calories, y = protein, color = cluster)) +
    geom_point(alpha = 0.7) +
    labs(title = "Recipe Clusters by Nutritional Content",
         x = "Calories", y = "Protein (g)") +
    theme_minimal()
}

## ----------------------------
## 4. User Clustering Module (FIXED)
## ----------------------------

# Create more comprehensive sample user data
sample_users <- data.frame(
  preferred_fat = runif(100, min = 20, max = 100),
  preferred_carbs = runif(100, min = 100, max = 300),
  preferred_protein = runif(100, min = 40, max = 150)
)

# Scale features properly
prepare_user_features <- function(user_profiles) {
  user_profiles %>%
    select(preferred_fat, preferred_carbs, preferred_protein) %>%
    scale()
}

# Train user clustering model with proper validation
set.seed(123)
scaled_users <- prepare_user_features(sample_users)

# Determine optimal number of clusters (elbow method)
wss <- sapply(1:10, function(k){kmeans(scaled_users, k, nstart=10)$tot.withinss})
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Based on elbow plot, select appropriate k (e.g., 4)
user_clustering_model <- kmeans(scaled_users, centers = 4, nstart = 20)

# Enhanced user cluster prediction
predict_user_cluster <- function(user_profile) {
  user_features <- data.frame(
    preferred_fat = user_profile$preferred_fat,
    preferred_carbs = user_profile$preferred_carbs,
    preferred_protein = user_profile$preferred_protein
  )
  
  # Scale the features based on the training data's scaling parameters
  scaled_features <- scale(user_features, 
                           center = attr(scaled_users, "scaled:center"),
                           scale = attr(scaled_users, "scaled:scale"))
  
  # Find the nearest cluster based on Euclidean distance
  distances <- apply(user_clustering_model$centers, 1, function(center) {
    sum((scaled_features - center)^2)
  })
  
  # Return the cluster with the minimum distance
  return(which.min(distances))
}


## ----------------------------
## 5. Predictive Modeling Module
## ----------------------------

train_cluster_models <- function(data) {
  models <- list()
  
  for (cl in unique(data$cluster)) {
    cluster_data <- data %>% filter(cluster == cl)
    model <- lm(calories ~ fat + carbs + protein + fiber, data = cluster_data)
    models[[as.character(cl)]] <- model
  }
  
  return(models)
}

nutrition_models <- train_cluster_models(recipe_data)

predict_nutritional_needs <- function(user_profile, models) {
  user_cluster <- predict_user_cluster(user_profile)
  model <- models[[as.character(user_cluster)]]
  
  pred_data <- data.frame(
    fat = user_profile$preferred_fat,
    carbs = user_profile$preferred_carbs,
    protein = user_profile$preferred_protein,
    fiber = user_profile$preferred_fiber
  )
  
  predict(model, newdata = pred_data)
}

## ----------------------------
## 6. Recommendation Engine
## ----------------------------

generate_recommendations <- function(user_profile, recipe_data, n_recommendations = 5) {
  target_calories <- predict_nutritional_needs(user_profile, nutrition_models)
  filtered_recipes <- recipe_data
  
  if (user_profile$dietary_restriction == "vegetarian") {
    filtered_recipes <- filtered_recipes %>% filter(is_vegetarian)
  } else if (user_profile$dietary_restriction == "vegan") {
    filtered_recipes <- filtered_recipes %>% filter(is_vegan)
  }
  
  nutritional_features <- filtered_recipes %>%
    select(calories, fat, carbs, protein) %>%
    as.matrix() %>%
    scale()
  
  target_df <- data.frame(
    calories = target_calories,
    fat = user_profile$preferred_fat,
    carbs = user_profile$preferred_carbs,
    protein = user_profile$preferred_protein
  ) %>% as.matrix()
  
  target_scaled <- scale(
    target_df,
    center = attr(nutritional_features, "scaled:center"),
    scale = attr(nutritional_features, "scaled:scale")
  )
  
  nn_result <- get.knnx(nutritional_features, target_scaled, k = n_recommendations)
  filtered_recipes[as.vector(nn_result$nn.index), ]
}


## ----------------------------
## 7. Shiny Dashboard
## ----------------------------

run_interactive_dashboard <- function() {
  ui <- fluidPage(
    titlePanel("Personalized Nutrition Recommender"),
    sidebarLayout(
      sidebarPanel(
        h3("User Profile"),
        numericInput("age", "Age:", 30, min = 10, max = 100),
        selectInput("gender", "Gender:", c("Male", "Female", "Other")),
        numericInput("weight", "Weight (kg):", 70, min = 30, max = 200),
        numericInput("height", "Height (cm):", 170, min = 100, max = 250),
        selectInput("activity", "Activity Level:", 
                    c("Sedentary", "Lightly Active", "Moderately Active", "Very Active")),
        
        h3("Dietary Preferences"),
        selectInput("diet_type", "Diet Type:", 
                    c("No Restrictions", "Vegetarian", "Vegan")),
        sliderInput("fat_pref", "Preferred Fat (g):", min = 20, max = 100, value = 50),
        sliderInput("carbs_pref", "Preferred Carbs (g):", min = 100, max = 300, value = 200),
        sliderInput("protein_pref", "Preferred Protein (g):", min = 40, max = 150, value = 80),
        
        actionButton("recommend", "Get Recommendations", class = "btn-primary")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Recommendations",
                   h3("Recommended Recipes"),
                   tableOutput("recommendations"),
                   h3("Nutritional Summary"),
                   plotOutput("nutrition_plot")),
          tabPanel("Analysis",
                   h3("Recipe Clusters"),
                   plotOutput("cluster_plot"),
                   h3("User Clusters"),
                   plotOutput("user_cluster_plot"))
        )
      )
    )
  )
  
  server <- function(input, output) {
    recommendations <- eventReactive(input$recommend, {
      user_profile <- list(
        age = input$age,
        gender = input$gender,
        weight = input$weight,
        height = input$height,
        activity_level = tolower(gsub(" ", "_", input$activity)),
        dietary_restriction = tolower(gsub(" ", "_", input$diet_type)),
        preferred_fat = input$fat_pref,
        preferred_carbs = input$carbs_pref,
        preferred_protein = input$protein_pref,
        preferred_fiber = 25
      )
      
      generate_recommendations(user_profile, recipe_data)
    })
    
    output$recommendations <- renderTable({
      req(recommendations())
      recommendations() %>%
        select(name, calories, fat, carbs, protein, cluster) %>%
        mutate(across(calories:protein, round, 1))
    })
    
    output$nutrition_plot <- renderPlot({
      req(recommendations())
      recs <- recommendations()
      
      ggplot(recs, aes(x = name)) +
        geom_col(aes(y = protein, fill = "Protein"), position = "dodge") +
        geom_col(aes(y = carbs, fill = "Carbs"), position = "dodge") +
        geom_col(aes(y = fat, fill = "Fat"), position = "dodge") +
        labs(title = "Nutritional Content of Recommended Recipes",
             x = "Recipe", y = "Grams", fill = "Nutrient") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$cluster_plot <- renderPlot({
      plot_clusters(recipe_data)
    })
    
    output$user_cluster_plot <- renderPlot({
      plot(user_clustering_model$centers, 
           main = "User Cluster Centers",
           xlab = "Preferred Fat (scaled)",
           ylab = "Preferred Carbs (scaled)")
      text(user_clustering_model$centers, labels = 1:4, pos = 3)
    })
  }
  
  shinyApp(ui, server)
}

## ----------------------------
## 8. Run Application
## ----------------------------

run_interactive_dashboard()