
library(libresense)


#### 4 copas rep 1 ########

run_panel(
  products_file = "grupo_f1/vinos_f1.csv",
  attributes_file = "atributos.csv",
  answers_dir = "grupo_f1/answers",
  product_name = "CodigoProducto",
  design_file = "grupo_f1/grupo_f1.csv",
  dest_url ="192.168.1.132:4000",
  numeric_range = c(0,3),
  numeric_step=1)

run_board2("grupo_f1/answers/")


