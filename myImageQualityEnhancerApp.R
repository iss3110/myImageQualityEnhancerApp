# Installer le package "magick" si ce n'est pas déjà fait
if (!require(magick)) {
  install.packages("magick")
}

library(magick)

segmentation_image <- function(image_path, threshold) {
  # Charger l'image
  image <- image_read(image_path)
  
  # Convertir l'image en une matrice 3D
  image_array <- image_data(image)
  
  # Dimensions de l'image
  height <- dim(image_array)[1]
  width <- dim(image_array)[2]
  
  # Convertir la matrice 3D en une matrice 2D (pixels x canaux)
  flattened_matrix <- matrix(image_array, nrow = height * width, ncol = 3, byrow = TRUE)
  
  # Initialiser les groupes de pixels
  groups <- rep(0, height * width)
  group_id <- 1
  
  # Parcourir chaque pixel
  for (i in 1:(height * width)) {
    # Si le pixel n'a pas encore été attribué à un groupe
    if (groups[i] == 0) {
      # Attribuer le pixel au groupe courant
      groups[i] <- group_id
      
      # Extraire les valeurs des canaux RGB du pixel courant
      r <- flattened_matrix[i, 1]
      g <- flattened_matrix[i, 2]
      b <- flattened_matrix[i, 3]
      
      # Parcourir les pixels voisins
      for (j in 1:(height * width)) {
        # Si le pixel voisin n'a pas encore été attribué à un groupe
        if (groups[j] == 0) {
          # Extraire les valeurs des canaux RGB du pixel voisin
          r_neighbor <- flattened_matrix[j, 1]
          g_neighbor <- flattened_matrix[j, 2]
          b_neighbor <- flattened_matrix[j, 3]
          
          # Calculer la distance euclidienne entre les deux pixels
          distance <- sqrt((r - r_neighbor)^2 + (g - g_neighbor)^2 + (b - b_neighbor)^2)
          
          # Si la distance est inférieure au seuil, attribuer le pixel voisin au groupe courant
          if (distance < threshold) {
            groups[j] <- group_id
          }
        }
      }
      
      # Augmenter l'identifiant du groupe
      group_id <- group_id + 1
    }
  }
  
  # Calculer la moyenne des valeurs des pixels pour chaque groupe
  group_means <- tapply(flattened_matrix, groups, colMeans)
  
  # Remplacer les valeurs des pixels de chaque groupe avec la moyenne des valeurs du groupe
  for (i in 1:(height * width)) {
    flattened_matrix[i, ] <- group_means[groups[i]]
  }
  
  # Réorganiser la matrice 2D en une matrice 3D
  segmented_matrix <- array(flattened_matrix, dim = c(height, width, 3))
  
  # Créer une nouvelle image à partir de la matrice segmentée
  segmented_image <- image_copy(image)
  image_data(segmented_image) <- segmented_matrix
  
  # Afficher l'image segmentée
  print(segmented_image)
}

# Exemple d'utilisation
image_path <- "Les Déménageurs Express.jpg"
seuil <- 50  # Modifier le seuil selon vos besoins
segmentation_image(image_path, seuil)
