b=c(10,20,30,40,50)
i=c(1,2,3,4,5)     
a=c(100,200,300,400,500)

dat <- data.frame(i,a,b)

ac=a[which(250 < a & a < 450)]

bc=b[which(250 < a & a < 450)]

ic=i[which(250 < a & a < 450)]

a[ic]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
a=c(100,200,300,400,500,600,700,800,900)
b=c(10,20,30,40,50,60,70,80,90)
ii=c(7,4,9,6,3,5,2,8,1)
datt <- data.frame(ii,a,b)

dattordenado <- datt[order(datt[,1]),]

# esto ya me da los id 
id <- which(20 < dattordenado[3] & dattordenado[3] < 80 & 200 < dattordenado[2] & dattordenado[2] < 800) 
# dattordenado[id,]

dataind <- data.frame(id)
idsel <- sample(nrow(dataind), 3, replace=FALSE)



# id[idsel] me da los valores de id seleccionados
# dattordenado[id[idsel],] esto me da los datos con los id seleccionados

aa=c(3000,9000,2000,8000,6000,5000,1000,7000,4000)
jj=c(9,1,4,8,5,3,7,2,6)
datts <- data.frame(jj,aa,aa)

datoordenados <- datts[order(datts[,1]),]
datoordenados[id,]
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 







indc = which(20 < b & b < 70 & 200 < a & a < 700)
datoc <- datt[indc,]dattordenado[1]
datoordenado <- datoc[order(datoc[,1]),]



ac = a[which(20 < b & b < 70 & 200 < a & a < 700)]
bc = b[which(20 < b & b < 70 & 200 < a & a < 700)]


datoordenado <- datt[order(datt[,1]),]


iic=ii[which(250 < a & a < 450)]
