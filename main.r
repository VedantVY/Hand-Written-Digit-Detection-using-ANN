read<-function()
{
    inputLayer<-784
    hiddenLayer<-128
    outputLayer<-10
    rate<-0.1    

W1 <- matrix(rnorm(inputLayer*hiddenLayer,sd=0.1),nrow=inputLayer,ncol=hiddenLayer)
W2 <- matrix(rnorm(hiddenLayer*outputLayer,sd=0.1),nrow=hiddenLayer,ncol=outputLayer)
B1 <- matrix(0,nrow=1,ncol=128)
B2 <- matrix(0,nrow=1,ncol=10)
epoch<-9

for(j in 1:epoch)
{
    for(i in 1:60000)
    {
        X<-matrix(as.numeric(mnist$train$images[i,]/255),nrow=1)
        train_label<-as.numeric(mnist$train$labels[i])
        Z1<-X %*% W1 +B1
        Y1<-sigmoid(Z1)
        Z2<-Y1 %*% W2 +B2
        Y2 <-soft_max(Z2)
        loss <- -log(Y2[train_label+1])
        dz2 <- Y2
        dz2[train_label+1]<- Y2[train_label+1]-1

        dW2<-t(Y1) %*% dz2
        db2 <-dz2
        dz1 <- (dz2 %*% t(W2))*Y1*(1-Y1)
        dW1 <- t(X) %*% dz1
        db1 <- dz1
        W1 <-W1 -rate*dW1
        B1 <- B1 - rate*db1
        W2 <- W2 - rate*dW2
        B2 <- B2 - rate*db2

    }
    correct<-0
    for(i in 1:10000)
    {
        Xtest <- matrix(as.numeric(mnist$test$images[i,]/255),nrow=1)
        T1 <- sigmoid(Xtest %*% W1 + B1)
        T2 <- soft_max(T1 %*% W2+B2)
        if(mnist$test$labels[i]== (which.max(T2)-1))
            correct <-correct+1
    }
    accuracy <- correct*100/10000
    print(accuracy)
}
}