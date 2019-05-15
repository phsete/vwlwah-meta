import numpy as np
import matplotlib.pyplot as plt
import math

# evenly sampled time at 200ms intervals
d = np.array([1/2**i for i in np.arange(1, 20, 0.01)])
N = 100000000

def P1F(w,d):
    return d ** (w-1)

def P0F(w,d):
    return (1-d)**(w-1)

def PN1F(w,d):
    return 1 - P1F(w,d)

def PN0F(w,d):
    return 1 - P0F(w,d)

def PL(w,d):
    return 1 - P0F(w,d) - P1F(w,d)


w = 32

lessList = [k for k in range(1,math.ceil((math.log((math.floor(N/(w-1))-1),2))/(w-2)) - 1)]
lessEqualList = [k for k in range(1,math.floor((math.log((math.floor(N/(w-1))-1),2))/(w-2)))]
plt.plot(d, (
    3
    + (math.floor(N / (w-1) - 1)) * (PL(w,d) + PN0F(w,d) * P0F(w,d) + PN1F(w,d) * P1F(w,d))
    + sum([(P0F(w,d)**(1+2**(k*(w-2))) + P1F(w,d)**(1+2**(k*(w-2)))) for k in lessEqualList])
    + sum([
        (math.floor(N/(w-1)) - 1 - 2**(k*(w-2))) * (PN0F(w,d) * P0F(w,d)**(1+2**(k*(w-2))) + PN1F(w,d) * P1F(w,d)**(1+2**(k*(w-2))))
        for k in lessList])
    ) * w / N, 'g--')


w=3

lessList = [k for k in range(1,math.ceil((math.log((math.floor(N/(w-1))-1),2))/(w-2)) - 1)]
lessEqualList = [k for k in range(1,math.floor((math.log((math.floor(N/(w-1))-1),2))/(w-2)))]
print(4, sep="\n")
print((math.floor(N / (w-1) - 1)) * (PL(w,d) + PN0F(w,d) * P0F(w,d) + PN1F(w,d) * P1F(w,d)) * w / N)
print(w / N * sum([(P0F(w,d)**(1+2**(k*(w-2))) + P1F(w,d)**(1+2**(k*(w-2)))) for k in lessEqualList]), sep="\n")
print(w / N * sum([(math.floor(N/(w-1)) - 1 - 2**(k*(w-2))) * (PN0F(w,d) * P0F(w,d)**(1+2**(k*(w-2))) + PN1F(w,d) * P1F(w,d)**(1+2**(k*(w-2)))) for k in lessList]), sep="\n")

plt.plot(d, (
    3
    + (math.floor(N / (w-1) - 1)) * (PL(w,d) + PN0F(w,d) * P0F(w,d) + PN1F(w,d) * P1F(w,d))
    + sum([(P0F(w,d)**(1+2**(k*(w-2))) + P1F(w,d)**(1+2**(k*(w-2)))) for k in lessEqualList])
    + sum([
        (math.floor(N/(w-1)) - 1 - 2**(k*(w-2))) * (PN0F(w,d) * P0F(w,d)**(1+2**(k*(w-2))) + PN1F(w,d) * P1F(w,d)**(1+2**(k*(w-2))))
        for k in lessList])
    ) * w / N, 'b--')

plt.plot(d, 1+d-d, 'r--')
plt.xscale('log')
# plt.xscale('logit')
plt.show()
