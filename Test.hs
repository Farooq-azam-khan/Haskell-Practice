module Test where
    sign x = 
        if x < 0 then -1
        else if x > 0 then 1
        else 0

    add1 x = do
        let y = 1
        x+y

    -- pattern matching
    isNum01 x = 
        case x of
            0 -> True
            1 -> True
            _ -> False

    -- factorial with pattern matching
    fac 0 = 1
    fac x = x*fac(x-1)

    -- fib pattern
    fib 0 = 1
    fib 1 = 1
    fib n = fib(n-1) + fib(n-2)

    -- mlen
    mlength [] = 0
    -- mlength l = 1 + mlength(tail(l))
    mlength (h:t) = 1 + mlength(t)


    -- mapping one positive function to list
    positiveF x = x >= 0

    checkNum p [] = [] 
    checkNum p (h:t) = 
        if p h then h : checkNum p t
        else checkNum p t

    -- quad roots
    discriminant a b c = b*b - 4*a*c 
    isComplex a b c = (discriminant a b c) < 0
    doubleMe x = x * 2
    qroot a b c = ( (-b + sqrt (discriminant a b c)) / (doubleMe a), 
                    (-b - sqrt(discriminant a b c)) / (doubleMe a))
    qrootLetIn a b c = 
        let disc = sqrt (discriminant a b c)
        in ((-b + disc) / (doubleMe a), 
            (-b - disc) / (doubleMe a))


    -- how to add
    addMine a b = (+) a b 

    -- lambda function 
    plane = \x y -> 2*x-y
    quadF = \x -> x*x
    lineF = \x -> 2*x+1

    mapLambda l = map (\x -> x+1) l