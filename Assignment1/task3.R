#task 3 
library(sna)
library(network)
library(ggplot2)

seed = 161
set.seed(seed)

# load 10_W2
net_w2 <- read.csv("./Lintner/10_W2.csv", header = TRUE, row.names = 1, sep = ";")
net_w2_mat <- as.matrix(net_w2)

# load attr for classroom 10
attributes <- read.csv("./Lintner/attr.csv", header = TRUE, sep = ";") 
attr_10 <- attributes[attributes$classroomID == 10, ]

node_ids <- gsub("[^0-9]", "", rownames(net_w2_mat))
attr_ids <- gsub("[^0-9]", "", as.character(attr_10$studentID))

# match the cleaned IDs
attr_10 <- attr_10[match(node_ids, attr_ids), ]

# convert gender to a binary for regression
attr_10$gender_female <- ifelse(attr_10$gender == "female", 1, 0)
cat("1. Starting Network dimensions:", nrow(net_w2_mat), "rows by", ncol(net_w2_mat), "columns\n")
valid_nodes_attr <- complete.cases(attr_10[, c("literacy_end", "gender_female", "HISEI")])
cat("2. Nodes with valid attributes:", sum(valid_nodes_attr, na.rm = TRUE), "\n")
diag(net_w2_mat) <- 0
valid_nodes_net <- rowSums(is.na(net_w2_mat)) < (ncol(net_w2_mat) - 1)
cat("3. Nodes with valid network data:", sum(valid_nodes_net, na.rm = TRUE), "\n")

valid_nodes <- valid_nodes_attr & valid_nodes_net

net_w2_clean <- net_w2_mat[valid_nodes, valid_nodes]
attr_10_clean <- attr_10[valid_nodes, ]

cat("\nStudents remaining after cleaning:", nrow(net_w2_clean), "\n\n")

# 3.1
y <- attr_10_clean$literacy_end
W_raw <- net_w2_clean

# row normalization trick
row_sums <- rowSums(W_raw)
row_sums[row_sums == 0] <- 1 
W_norm <- W_raw / row_sums

nam_model_1 <- lnam(y, W1 = W_norm)
print("task 3.1")
summary(nam_model_1)

# 3.2
X <- cbind(attr_10_clean$gender_female, attr_10_clean$HISEI)
colnames(X) <- c("Gender_Female", "HISEI")

# 3.3
# interpretation: See task3.tex.
nam_model_2 <- lnam(y, x = X, W1 = W_norm)
print("task 3.3")
summary(nam_model_2)

# 3.4 is dependent on task 2. See task3.tex

# 3.5
run_nam_for_classroom <- function(class_id, attr_data) {
  file_name <- sprintf("./Lintner/%02d_W2.csv", class_id) 
  
  if(!file.exists(file_name)) {
    return(paste("File", file_name, "not found."))
  }
  
  net_mat <- as.matrix(read.csv(file_name, header = TRUE, row.names = 1, sep = ";"))
  attr_sub <- attr_data[attr_data$classroomID == class_id, ]
  
  node_ids <- gsub("[^0-9]", "", rownames(net_mat))
  attr_ids <- gsub("[^0-9]", "", as.character(attr_sub$studentID))
  attr_sub <- attr_sub[match(node_ids, attr_ids), ]
  
  attr_sub$gender_female <- ifelse(attr_sub$gender == "female", 1, 0)
  
  v_attr <- complete.cases(attr_sub[, c("literacy_end", "gender_female", "HISEI")])
  diag(net_mat) <- 0
  v_net <- rowSums(is.na(net_mat)) < (ncol(net_mat) - 1)
  
  valid <- v_attr & v_net
  
  net_clean <- net_mat[valid, valid]
  attr_clean <- attr_sub[valid, ]
  
  # model
  y <- attr_clean$literacy_end
  W_raw <- net_clean
  
  #row normalization trick
  row_sums <- rowSums(W_raw)
  row_sums[row_sums == 0] <- 1 
  W_norm <- W_raw / row_sums
  
  X <- cbind(attr_clean$gender_female, attr_clean$HISEI)
  colnames(X) <- c("Gender_Female", "HISEI")
  
  model <- lnam(y, x = X, W1 = W_norm)
  return(summary(model))
}

# run on all unique classrooms
classrooms <- unique(attributes$classroomID)

# get rid of classroom 10 since we already analysed it
remaining_classrooms <- classrooms[!(classrooms %in% c(10,6,7,8,11))]

for(cid in remaining_classrooms) {
  cat("\n================================================\n")
  cat(" Task 3.5: NAM Results for Classroom:", cid, "\n")
  cat("================================================\n")
  
  result <- run_nam_for_classroom(cid, attributes)
  print(result)
}

#3.6
# analysis: See task3.tex
plot_classrooms <- classrooms[!(classrooms %in% c(10,6,7,8,11))]

class_ids <- c()
rho_ests <- c()
rho_ses <- c()

for (cid in plot_classrooms) {
  file_name <- sprintf("./Lintner/%02d_W2.csv", cid)
  
  if (file.exists(file_name)) {
    net_mat <- as.matrix(read.csv(file_name, header = TRUE, row.names = 1, sep = ";"))
    attr_sub <- attributes[attributes$classroomID == cid, ]
    
    node_ids <- gsub("[^0-9]", "", rownames(net_mat))
    attr_ids <- gsub("[^0-9]", "", as.character(attr_sub$studentID))
    attr_sub <- attr_sub[match(node_ids, attr_ids), ]
    attr_sub$gender_female <- ifelse(attr_sub$gender == "female", 1, 0)
    
    v_attr <- complete.cases(attr_sub[, c("literacy_end", "gender_female", "HISEI")])
    diag(net_mat) <- 0
    v_net <- rowSums(is.na(net_mat)) < (ncol(net_mat) - 1)
    
    valid <- v_attr & v_net
    net_clean <- net_mat[valid, valid]
    attr_clean <- attr_sub[valid, ]
    
    if (nrow(net_clean) > 0) {
      y <- attr_clean$literacy_end
      W_raw <- net_clean
      
      row_sums <- rowSums(W_raw)
      row_sums[row_sums == 0] <- 1 
      W_norm <- W_raw / row_sums
      
      X <- cbind(attr_clean$gender_female, attr_clean$HISEI)
      colnames(X) <- c("Gender_Female", "HISEI")
      
      model <- lnam(y, x = X, W1 = W_norm)
      
      class_ids <- c(class_ids, cid)
      rho_ests <- c(rho_ests, model$rho1)
      rho_ses <- c(rho_ses, model$rho1.se)
    }
  }
}

plot_data <- data.frame(
  Classroom = as.factor(class_ids),
  Rho = rho_ests,
  SE = rho_ses
)

# 0.95 conf interval
plot_data$Lower <- plot_data$Rho - 1.96 * plot_data$SE
plot_data$Upper <- plot_data$Rho + 1.96 * plot_data$SE

# plot
ggplot(plot_data, aes(x = Classroom, y = Rho)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  coord_flip() +
  labs(title = "Network Autocorrelation (\u03c11) Across Classrooms",
       subtitle = "With 95% Confidence Intervals",
       x = "Classroom ID",
       y = "Autocorrelation Estimate (\u03c11)") +
  theme_minimal()