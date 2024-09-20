soft_max<-function(z)
{
    return (exp(z)/sum(exp(z)))
}
sigmoid<-function(z)
{
    return (1/(1+exp(-z)))
    
}
