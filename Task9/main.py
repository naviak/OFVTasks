import numpy as np
import matplotlib.pyplot as plt


def shuttle_method(x: np.array, a: np.array, b: np.array, c: np.array, d: np.array, init_cond: np.array) -> np.array:
    alpha = np.zeros_like(x)
    beta = np.zeros_like(x)
    y = np.zeros_like(x)

    alpha[0] = 0.0
    beta[0] = init_cond[0]

    for i in range(1, alpha.shape[0] - 1):
        alpha[i] = - a[i] / (b[i] + c[i] * alpha[i - 1])
        beta[i] = (d[i] - c[i] * beta[i - 1]) / (b[i] + c[i] * alpha[i - 1])

    y[-1] = init_cond[1]

    for j in range(y.shape[0] - 2, -1, -1):
        y[j] = alpha[j] * y[j + 1] + beta[j]

    return y


def dif_solver(x: np.array, init_cond: np.array, p: np.array = None, q: np.array = None, r: np.array = None) -> np.array:
    # Приводит К каноническому виду
    if p is None:
        p = np.zeros_like(x)
    else:
        p = p
    if q is None:
        q = np.zeros_like(x)
    else:
        q = q
    if r is None:
        r = np.zeros_like(x)
    else:
        r = r

    h = x[1] - x[0]
    a = 1 - p * h / 2
    b = -2 + q * h ** 2
    c = 1 + p * h / 2
    d = r * h ** 2
    return shuttle_method(x, a, b, c, d, init_cond)


x = np.linspace(0, np.pi, 5)
r = np.sin(x)

fig, axes = plt.subplots()

y = dif_solver(x, np.array([0., 0.3]), r=r)
axes.scatter(x, y, label="shuttle method")
axes.legend()
axes.grid()
plt.show()
