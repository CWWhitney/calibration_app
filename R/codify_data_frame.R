# # Function to codify, compress, and encrypt the data
# encrypt_question_index <- function(data, key, nonce) {
#   # Codify the data into a single string
#   codified_string <- paste(
#     data$Group, data$QuestionTypeCode, data$NumberGS, sep = "x", collapse = "y"
#   )
#   
#   # Compress the codified string
#   compressed_string <- memCompress(charToRaw(codified_string), type = "gzip")
#   
#   # Encrypt the compressed string
#   encrypted_string <- sodium::data_encrypt(compressed_string, key, nonce)
#   
#   # Encode the encrypted string using base64
#   encoded_string <- base64enc::base64encode(encrypted_string)
#   
#   return(encoded_string)
# }
# 
# # Function to decode, decompress, and decrypt the data
# decrypt_question_index <- function(encoded_string, key, nonce) {
#   # Decode the base64 string
#   decoded_encrypted_string <- base64enc::base64decode(encoded_string)
#   
#   # Decrypt the string back to the compressed string
#   decrypted_compressed <- sodium::data_decrypt(decoded_encrypted_string, key, nonce)
#   
#   # Decompress the string back to the original codified string
#   decompressed_string <- rawToChar(memDecompress(decrypted_compressed, type = "gzip"))
#   
#   # Split the string by 'y' to get each row
#   rows <- strsplit(decompressed_string, "y")[[1]]
#   
#   # Split each row by 'x' to get the column values and convert to a data frame
#   decompressed_question_index <- map_dfr(rows, ~ {
#     values <- strsplit(.x, "x")[[1]]
#     tibble(Group = as.numeric(values[1]), QuestionTypeCode = as.numeric(values[2]), NumberGS = as.numeric(values[3]))
#   })
#   
#   return(decompressed_question_index)
# }


# Function to codify, compress, encrypt, and URL encode the data
base_encrypt <- function(codified_string, key, nonce) {
  compressed_string <- memCompress(charToRaw(codified_string), type = "gzip")
  encrypted_string <- sodium::data_encrypt(compressed_string, key, nonce)
  encoded_string <- base64enc::base64encode(encrypted_string)
  url_encoded_string <- URLencode(encoded_string, reserved = TRUE)
  return(url_encoded_string)
}

# Function to decode, decompress, decrypt, and URL decode the data
base_decrypt <- function(url_encoded_string, key, nonce) {
  encoded_string <- URLdecode(url_encoded_string)
  decoded_encrypted_string <- base64enc::base64decode(encoded_string)
  decrypted_compressed <- sodium::data_decrypt(decoded_encrypted_string, key, nonce)
  decompressed_string <- rawToChar(memDecompress(decrypted_compressed, type = "gzip"))
  return(decompressed_string)
}

encrypt_question_index <- function(data, key, nonce) {
  codified_string <- paste(
    data$Group, data$QuestionTypeCode, data$NumberGS, sep = "x", collapse = "y"
  )
  encoded_string <- base_encrypt(codified_string, key, nonce)
  return(encoded_string)
}

decrypt_question_index <- function(encoded_string, key, nonce) {
  decompressed_string <- base_decrypt(encoded_string, key, nonce)
  rows <- strsplit(decompressed_string, "y")[[1]]
  decompressed_question_index <- map_dfr(rows, ~ {
    values <- strsplit(.x, "x")[[1]]
    tibble(Group = as.numeric(values[1]), QuestionTypeCode = as.numeric(values[2]), NumberGS = as.numeric(values[3]))
  })
  return(decompressed_question_index)
}

encrypt_binary_table <- function(data, key, nonce) {
  codified_string <- paste(
    data$Group,
    data$Question,
    data$QuestionText,
    data$Index,
    data$Response,
    data$Confidence,
    data$Truth,
    data$Brier,
    data$Source,
    sep = "x", 
    collapse = "y"
  )
  
  encoded_string <- base_encrypt(codified_string, key, nonce)
  return(encoded_string)
}


decrypt_binary_table <- function(encoded_string, key, nonce) {
  decompressed_string <- base_decrypt(encoded_string, key, nonce)
  rows <- strsplit(decompressed_string, "y")[[1]]
  
  decompressed_binary_table <- if (length(rows) == 0 || (length(rows) == 1 && rows == "")) {
    data.frame(
      Group = as.integer(), 
      Question = as.integer(), 
      QuestionText = as.character(), 
      Index = as.integer(), 
      Response = as.character(), 
      Confidence = as.character(), 
      Truth = as.character(), 
      Brier = as.numeric(), 
      Source = as.character(), 
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, lapply(rows, function(row) {
      values <- strsplit(row, "x")[[1]]
      data.frame(
        Group = as.integer(values[1]), 
        Question = as.integer(values[2]), 
        QuestionText = values[3], 
        Index = as.integer(values[4]), 
        Response = values[5], 
        Confidence = values[6], 
        Truth = values[7], 
        Brier = as.numeric(values[8]), 
        Source = values[9], 
        stringsAsFactors = FALSE
      )
    }))
  }
  
  return(decompressed_binary_table)
}


encrypt_range_table <- function(data, key, nonce) {
  codified_string <- paste(
    data$Group, data$Question, data$QuestionText, data$Index, data$Lower90, data$Upper90, data$Truth, data$RelativeError, sep = "x", collapse = "y"
  )
  encoded_string <- base_encrypt(codified_string, key, nonce)
  return(encoded_string)
}


decrypt_range_table <- function(encoded_string, key, nonce) {
  decompressed_string <- base_decrypt(encoded_string, key, nonce)
  rows <- strsplit(decompressed_string, "y")[[1]]
  
  decompressed_range_table <- if (length(rows) == 0 || (length(rows) == 1 && rows == "")) {
    data.frame(
      Group = as.integer(), 
      Question = as.integer(), 
      QuestionText = as.character(), 
      Index = as.integer(), 
      Lower90 = as.numeric(), 
      Upper90 = as.numeric(), 
      Truth = as.numeric(), 
      RelativeError = as.numeric(), 
      Source = as.character(), 
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, lapply(rows, function(row) {
      values <- strsplit(row, "x")[[1]]
      data.frame(
        Group = as.integer(values[1]), 
        Question = as.integer(values[2]), 
        QuestionText = values[3], 
        Index = as.integer(values[4]), 
        Lower90 = as.numeric(values[5]), 
        Upper90 = as.numeric(values[6]), 
        Truth = as.numeric(values[7]), 
        RelativeError = as.numeric(values[8]), 
        Source = as.character(), 
        stringsAsFactors = FALSE
      )
    }))
  }
  
  return(decompressed_range_table)
}
