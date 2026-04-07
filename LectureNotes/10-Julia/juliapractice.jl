using LinearAlgebra

function maxtrix_multiplication(X,Y)
    product = X * Y
    return product
end 

function execution ()
    x = rand(15,500);
    y = rand(500,1000);
    z = maxtrix_multiplication(x,y)
    println(z)
    return nothing 
end 

@show execution()