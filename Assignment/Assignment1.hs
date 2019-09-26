module Assignment1 where
-- Ryan Fernandes
-----------------------
-- Question 1
convertFromBinary::Int->Int
convertFromBinary n
    | n<0 = error "Number can't be negative"
    | n==0 = 0
    | n==1 = 1
    | lastDigit==1 = 1+2*(convertFromBinary otherDigit)
    | lastDigit==0 = 2*(convertFromBinary otherDigit)
    | otherwise = error"All numbers should be either 1s or 0s"
    where
        lastDigit=mod n 10
        otherDigit=div n 10
        
        
-- Question 2
countStepsinBinarySearchhelper::Int->Int->Int->Int->Int
countStepsinBinarySearchhelper low target upper steps
    | target==mid = steps + 1 -- base case, when target equals mid, returns counter
    | target<mid = countStepsinBinarySearchhelper low target (mid-1) steps+1
    | otherwise = countStepsinBinarySearchhelper (mid+1) target upper steps+1
    where
        mid=div (upper+low) 2

countStepsinBinarySearch::Int->Int->Int
countStepsinBinarySearch x upper     
    | upper<x = error "Incorrect values, can't be searched"
    | x<1 || upper<1 = error "x or limit can't be less than one"
    | otherwise= countStepsinBinarySearchhelper low x upper steps
    where
        low=1
        steps=0
    
 ---Question 3
helper::Int->Int->Int->Int->Int
helper i j x ctr
    | i==x = ctr -- Base case when outer loop reaches bound
    | j==x = helper (i+1) (i+1) x (ctr) --Base case when inner loop reaches bound
    | j<x = helper i (j+1) x (ctr+1)
    | otherwise = 0
    
countNestedForward :: Int->Int
countNestedForward x
    | x<0 = error "Limit cannot be less than one"
    | otherwise = helper i j x ctr
    where
        i=0
        j=i
        ctr=0
        
-- ---------------------
-- END OF ASSIGNMENT 1
-- ---------------------        
        
        
        
        
