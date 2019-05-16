import numpy as np
import matplotlib.pyplot as plt

t = np.array([10**i for i in np.arange(4, 7, 0.05)])

plt.plot(t, t * 0.5**20 * (1 - 0.5**20)**(t-1), 'k')
plt.plot(t, 1 - (1 - 0.5**20)**(t), 'r')

datapoints = zip(t, t * 0.5**20 * (1 - 0.5**20)**(t-1))
print('\\addplot coordinates { %s };\n\\addlegendentry{\\(P(X=x)\\)};\n' % ' '.join(map(str, datapoints)))

datapoints = zip(t, 1 - (1 - 0.5**20)**(t))
print('\\addplot coordinates { %s };\n\\addlegendentry{\\(F(X)\\)};\n' % ' '.join(map(str, datapoints)))

plt.axvline(x=2**(19-2)*(19-1))
plt.axvline(x=2**(18-2)*(18-1))
plt.axvline(x=2**(17-2)*(17-1))
plt.axvline(x=2**(16-2)*(16-1))
plt.axvline(x=2**(15-2)*(15-1))
plt.axvline(x=2**(14-2)*(14-1))

plt.xscale('log')
plt.show()
