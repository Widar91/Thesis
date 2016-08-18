let obs = observable $ \obr -> 
    do obr 1
       obr 2
       obr 3

subscribe obs print

output> 
1
2
3