#'Description of is_matrix_in_list function
#'@description A function to check if a matrix is in a matrix list.
#'@param matrix is the matrix we want to check
#'@param list_of_matrices is the list of matrices
#'@return  A bool value. Returning TRUE if matrix is in the list_of_matrices, False if it is not



is_matrix_in_list <- function(matrix, list_of_matrices) {
  for (mat in list_of_matrices) {
    if (identical(matrix, mat)) {
      return(TRUE)
    }
  }
  return(FALSE)
}
