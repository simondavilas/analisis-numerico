from matplotlib import pyplot as pl

def factorial(n):
    if n==0:
        return 1
    else:
        return n*factorial(n-1)
    
def C(n,i):
    return factorial(n)/(factorial(n-i)*factorial(i))

def Spline(n,puntos):
    coeficientesx = []
    coeficientesy = []
    for i in range(n+1):
        Coef = C(n,i)
        coeficientesx.append(Coef*puntos[i][0])
        coeficientesy.append(Coef*puntos[i][1])
    return [coeficientesx,coeficientesy]

def B(n,t,coef):
    ans = 0
    for i in range(n+1):
        ans += coef[i]*((1-t)**(n-i))*(t**i)
    return ans
       
def graficar(n,T,coeficientes):
    x = []
    y = []
    for t in T:
        x.append(B(n,t,coeficientes[0]))
        y.append(B(n,t,coeficientes[1]))
    pl.plot(x,y)
    pl.show()
    return None

T = []
for i in range(100):
    T.append(i/100.0)

puntos = [[1.67,4.33][0.96,4.33][0.38,4.23][-0.23,4.22][-0.69,3.88][-0.99, 3.54][-1,3][-0.84, 2.66][-0.48,2.43][-0.04,2.30][0.49,2.56][1.09,2.31][1.67,2.25][2.14,1.97][2.41,1.56][2.43,1.06][2.14,0.72][1.63,0.62][1.07,0.60][0.52,0.58][0.07,0.54][-0.32,0.54][-0.79,0.55]]
n = len(puntos)-1
coeficientes = Spline(n,puntos)
graficar(n,T,coeficientes)


