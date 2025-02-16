let factorial x =
    if x < 0 then
        invalidArg "x" "Parameter x is less than zero"

    let rec support x acc =
        if x = 1 then acc else support (x - 1) (acc * x)

    support x 1

let fibonacci x =
    if x < 0 then
        invalidArg "x" "Parameter x is less than zero"

    let rec support x acc y =
        if y = 0 then acc else support acc (acc + x) (y - 1)

    support 1 0 x

let reverseList ls =
    let rec support ls out =
        if List.length ls = 0 then
            out
        else
            support (List.tail ls) (List.head ls :: out)

    support ls []

let arrayOfDegrees n m =
    let rec support deg out =
        if deg = n then
            out
        else
            support (deg - 1) (List.head out / 2 :: out)

    support (n + m) [ pown 2 (n + m) ]

let findFirst ls n =
    let rec support ls n i =
        if List.head ls = n then i
        else if List.length ls = 1 then -1
        else support (List.tail ls) (n) (i + 1)

    support ls n 0
