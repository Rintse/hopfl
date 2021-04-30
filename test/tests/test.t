( ( fix f . λ x . 
    if (x = k_max) 
        then x 
        else prev (f (*) next (x+1.0) )
) 0 )
