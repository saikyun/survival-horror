(declare-project
  :name "Suho"
  :author "Jona Ekenberg <saikyun@gmail.com>"
  :dependencies ["https://github.com/saikyun/janet-bounded-queue"
                 "https://github.com/saikyun/freja"])

(declare-executable
  :name "Suho"
  :entry "suho.janet")
