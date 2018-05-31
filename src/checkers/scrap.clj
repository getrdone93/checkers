    (println "called" (if (nil? left)
                        "nil"
                        (count left)) 
                      (if (nil? right)
                         "nil"
                         (count right)) ) 