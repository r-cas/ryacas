# https://github.com/stan-dev/rstan/issues/569#issuecomment-473407840
get_stage("before_install") %>%
  add_step(step_write_text_file("CXX17FLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -Wno-macro-redefined\nCXX17=g++ -std=c++17 -fPIC", path = "~/.R/Makevars"))

