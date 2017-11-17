# Copied from https://stackoverflow.com/questions/34669082/open-quantum-system-modelling
# Use as template for modifications playing with qutip

from qutip import Qobj, Options, mesolve
import numpy as np
import scipy
from math import *
import matplotlib.pyplot as plt

hamiltonian = np.array([
    [215, -104.1, 5.1, -4.3, 4.7, -15.1, -7.8],
    [-104.1, 220.0, 32.6, 7.1, 5.4, 8.3, 0.8],
    [5.1, 32.6, 0.0, -46.8, 1.0, -8.1, 5.1],
    [-4.3, 7.1, -46.8, 125.0, -70.7, -14.7, -61.5],
    [4.7, 5.4, 1.0, -70.7, 450.0, 89.7, -2.5],
    [-15.1, 8.3, -8.1, -14.7, 89.7, 330.0, 32.7],
    [-7.8, 0.8, 5.1, -61.5, -2.5, 32.7, 280.0]
])

recomb = np.zeros((7, 7), dtype=complex)
np.fill_diagonal(recomb, 33.33333333)
recomb = recomb * -1j
trap = np.zeros((7, 7), complex)
trap[2][2] = -0.033333333333j
hamiltonian = recomb + trap + hamiltonian
H = Qobj(hamiltonian)

gammaL = (2 * pi) * (296 * 0.695) * (35.0 / 150)

L1 = np.array([
    [1, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0]
])

L2 = np.array([
    [0, 0, 0, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0]
])

L3 = np.array([
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0]
])      

L4 = np.array([
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0]
])

L5 = np.array([
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0]
])

L6 = np.array([
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0]
])

L7 = np.array([
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 1]
])

# Since our gamma variable cannot be directly applied onto
# the Lindblad operator, we must multiply it with
# the collapse operators:  

rho0=Qobj(L1)

L1 = Qobj(gammaL * L1)
L2 = Qobj(gammaL * L2)
L3 = Qobj(gammaL * L3)
L4 = Qobj(gammaL * L4)
L5 = Qobj(gammaL * L5)
L6 = Qobj(gammaL * L6)
L7 = Qobj(gammaL * L7)

options = Options(nsteps=1000000, atol=1e-5)

bra3 = [[0, 0, 1, 0, 0, 0, 0]]
bra3q = Qobj(bra3)

ket3 = [[0], [0], [1], [0], [0], [0], [0]]
ket3q = Qobj(ket3)

starttime = 0
# this is effectively just a label - `mesolve` alwasys starts from `rho0` -
# it's just saying what we're going to call the time at t0
endtime = 100
# Arbitrary - this solves with the options above
# (max 1 million iterations to converge - tolerance 1e-10)
num_intermediate_state = 100

state_evaluation_times = np.linspace(
    starttime,
    endtime,
    num_intermediate_state
)

result = mesolve(
    H,
    rho0,
    state_evaluation_times,
    [L1, L2, L3, L4, L5, L6, L7],
    [],
    options=options
)

number_of_interest = bra3q * (result.states * ket3q)

points_to_plot = []
for number in number_of_interest:
    if number == number_of_interest[0]:
        points_to_plot.append(0)
    else:
        points_to_plot.append(number.data.data.real[0])

plt.plot(state_evaluation_times, points_to_plot)
plt.show()
exit()