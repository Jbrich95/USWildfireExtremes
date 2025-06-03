# Check if tensorflow is available

reticulate::use_condaenv("USWildfiresExtremes", required = T)
print("Is Keras available?")
print(keras::is_keras_available()) #Check if keras is available
sess = keras::k_get_session()
print(sess$list_devices())