import numpy as np
import matplotlib.pyplot as plt
import math

# evenly sampled time at 200ms intervals
# d = np.array([1/(1+math.exp(-i)) for i in np.arange(-25, 25, 0.001)])
d = np.array([1/(1+math.exp(-i)) for i in np.arange(-25, 0, 0.001)])
f = 8
N = 100000000
w = 32
v = 14

P1F = d**(w-1)
P0F = (1-d)**(w-1)
PN1F = 1 - P1F
PN0F = 1 - P0F
PL  = 1 - P0F - P1F

#
# Random
# 

# Reference
w32 = 32
#plt.plot(d, (math.floor(N / (w-1)) + 2 - (math.floor(N / (w-1)) - 1) *  ((1-d)**(2*w - 2) + d**(2*w - 2))) * w / N, 'b--')

#plt.plot(d, (math.floor(N / (w32-1)) + 2 - (math.floor(N / (w32-1)) - 1) *  ((1-d)**(2*w32 - 2) + d**(2*w32 - 2))) * w32 / N, 'b--')

# Simple Append
'''
lessList = [k for k in range(math.ceil(2-w+math.log(math.floor(N/(w-1))-1,2)) - 1)]
lessEqualList = [k for k in range(math.floor(2-w+math.log(math.floor(N/(w-1))-1,2)))]

plt.plot(d, (
    3
    + (math.floor(N / (w-1) - 1)) * (PL + PN0F * P0F + PN1F * P1F)
    + sum([(P0F**(1+2**(w+k-2)) + P1F**(1+2**(w+k-2))) for k in lessEqualList])
    + sum([
        (math.floor(N/(w-1)) - 1 - 2**(w+k-2)) * (PN0F * P0F**(1+2**(w+k-2)) + PN1F * P1F**(1+2**(w+k-2)))
        for k in lessList])
    ) * w / N, 'g--')
'''

# Log Append
lessList = [k for k in range(1,math.ceil((math.log((math.floor(N/(w-1))-1),2))/(w-2)) - 1)]
lessEqualList = [k for k in range(1,math.floor((math.log((math.floor(N/(w-1))-1),2))/(w-2)))]
# print(3, sep="\n")
# print(w / N * (math.floor(N / (w-1) - 1)) * (PL + PN0F * P0F + PN1F * P1F))
# print(w / N * sum([(P0F**(1+2**(k*(w-2))) + P1F**(1+2**(k*(w-2)))) for k in lessEqualList]), sep="\n")
# print(w / N * sum([(math.floor(N/(w-1)) - 1 - 2**(k*(w-2))) * (PN0F * P0F**(1+2**(k*(w-2))) + PN1F * P1F**(1+2**(k*(w-2)))) for k in lessList]), sep="\n")

plt.plot(d, (
    3
    + (math.floor(N / (w-1) - 1)) * (PL + PN0F * P0F + PN1F * P1F)
    + sum([(P0F**(1+2**(k*(w-2))) + P1F**(1+2**(k*(w-2)))) for k in lessEqualList])
    + sum([
        (math.floor(N/(w-1)) - 1 - 2**(k*(w-2))) * (PN0F * P0F**(1+2**(k*(w-2))) + PN1F * P1F**(1+2**(k*(w-2))))
        for k in lessList])
    ) * w / N, 'g--')

w=22
P1F = d**(w-1)
P0F = (1-d)**(w-1)
PN1F = 1 - P1F
PN0F = 1 - P0F
PL  = 1 - P0F - P1F
# Log Append
lessList = [k for k in range(1,math.ceil((math.log((math.floor(N/(w-1))-1),2))/(w-2)) - 1)]
lessEqualList = [k for k in range(1,math.floor((math.log((math.floor(N/(w-1))-1),2))/(w-2)))]
# print(4, sep="\n")
# print((math.floor(N / (w-1) - 1)) * (PL + PN0F * P0F + PN1F * P1F) * w / N)
# print(w / N * sum([(P0F**(1+2**(k*(w-2))) + P1F**(1+2**(k*(w-2)))) for k in lessEqualList]), sep="\n")
# print(w / N * sum([(math.floor(N/(w-1)) - 1 - 2**(k*(w-2))) * (PN0F * P0F**(1+2**(k*(w-2))) + PN1F * P1F**(1+2**(k*(w-2)))) for k in lessList]), sep="\n")

plt.plot(d, (
    3
    + (math.floor(N / (w-1) - 1)) * (PL + PN0F * P0F + PN1F * P1F)
    + sum([(P0F**(1+2**(k*(w-2))) + P1F**(1+2**(k*(w-2)))) for k in lessEqualList])
    + sum([
        (math.floor(N/(w-1)) - 1 - 2**(k*(w-2))) * (PN0F * P0F**(1+2**(k*(w-2))) + PN1F * P1F**(1+2**(k*(w-2))))
        for k in lessList])
    ) * w / N, 'b--')

# Markov
# plt.plot(d, w * d / (w-1) * (1 + (2*w - 3) / f), 'b--')
# plt.plot(d, v * d / (v-1) * (1 + (2*v - 3) / f), 'g--')

plt.plot(d, 1+d-d, 'r--')
plt.xscale('log')
# plt.xscale('logit')
plt.show()
