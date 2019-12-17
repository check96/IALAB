import numpy as np
import matplotlib.pyplot as plt
from numpy import pi, cos, sin, sqrt, diag
from numpy.linalg import inv
from numpy.random import randn


t = np.linspace(0, 2*pi, 100)
dt = t[1] - t[0]
# position
x = 2*cos(t)
y = sin(2*t)

# velocity
dxdt = -2*sin(t)
dydt = 2*cos(2*t)

# accel
d2xdt2 = -2*cos(t)
d2ydt2 = -4*sin(2*t)

# jerk
d3xdt3 = 2*sin(t)
d3ydt3 = -8*cos(2*t)

# generazione dei segnali GPS, TACHIMETRO E GIROSCOPIO
# angular speed (scalar)
omega = (dxdt*d2ydt2 - dydt*d2xdt2) / (dxdt**2 + dydt**2)

# speed (scalar)
speed = sqrt(dxdt**2 + dydt**2)

# measurement error
gps_error = 0.1
omega_sig = 0.3
speed_sig = 0.1


# noisy measurements
x_gps = x + gps_error * randn(*x.shape)
y_gps = y + gps_error * randn(*y.shape)
omega_sens = omega + omega_sig * randn(*omega.shape)
speed_sens = speed + speed_sig * randn(*speed.shape)

# posX velX accX posY velY accY
# matrice di transizione
A = np.array([
    [1, dt, (dt**2)/2, 0, 0, 0],
    [0, 1, dt, 0, 0, 0],
    [0, 0, 1, 0, 0, 0],
    [0, 0, 0, 1, dt, (dt**2)/2],
    [0, 0, 0, 0, 1, dt],
    [0, 0, 0, 0, 0, 1],
    ])

#Matrice dell'errore di transizione
Qx = np.array([(dt**3)/6, (dt**2)/2, dt, 0, 0, 0])
Qx = np.expand_dims(Qx, 1)
Qy = np.array([0, 0, 0, (dt**3)/6, (dt**2)/2, dt])
Qy = np.expand_dims(Qy, 1)

j_var = max(np.var(d3xdt3), np.var(d3ydt3))
Q = j_var * (Qx @ Qx.T + Qy @ Qy.T)

#modello di sensore
H = np.array([
    [1, 0, 0, 0, 0, 0],
    [0, 0, 0, 1, 0, 0],
    ])

x_init = np.array([x[0], dxdt[0], d2xdt2[0], y[0], dydt[0], d2ydt2[0]])
P_init = 0 * np.eye(len(x_init))  # small initial prediction error

##################################
### inizio della parte lineare ###
##################################

# erroe di osservazione
R = diag(np.array([gps_error**2, gps_error**2]))

# create an observation vector of noisy GPS signals
observations = np.array([x_gps, y_gps]).T


# matrix dimensions
nx = Q.shape[0]
ny = R.shape[0]
nt = observations.shape[0]

# allocate identity matrix for re-use
Inx = np.eye(nx)

# allocate result matrices
x_pred = np.zeros((nt, nx))      # prediction of state vector
P_pred = np.zeros((nt, nx, nx))  # prediction error covariance matrix
x_est = np.zeros((nt, nx))       # estimation of state vector
P_est = np.zeros((nt, nx, nx))   # estimation error covariance matrix
K = np.zeros((nt, nx, ny))       # Kalman Gain

# set initial prediction
x_pred[0] = x_init
P_pred[0] = P_init

# for each time-step...
for i in range(nt):

    # prediction stage
    if i > 0:
        x_pred[i] = A @ x_est[i-1]
        P_pred[i] = A @ P_est[i-1] @ A.T + Q

    # estimation stage
    y_obs = observations[i]
    # kalman gain calculation
    K[i] = P_pred[i] @ H.T @ inv((H @ P_pred[i] @ H.T) + R)
    # nuova media e quindi nuova stima della posizione
    x_est[i] = x_pred[i] + K[i] @ (y_obs - H @ x_pred[i])
    # nuova varianza del rumore
    P_est[i] = (Inx - K[i] @ H) @ P_pred[i]

'''
######################################
### inizio della parte non lineare ###
######################################

def eval_h(x_pred):

    # expand prediction of state vector
    px, vx, ax, py, vy, ay = x_pred

    # compute angular vel
    w = (vx*ay - vy*ax) / (vx**2 + vy**2)

    # compute speed
    s = sqrt(vx**2 + vy**2)

    return np.array([px, py, w, s])

def eval_H(x_pred):

    # expand prediction of state vector
    px, vx, ax, py, vy, ay = x_pred
    V2 = vx**2 + vy**2

    # angular vel partial derivs
    dwdvx = (V2*ay - 2*vx*(vx*ay-vy*ax)) / (V2**2)
    dwdax = -vy / V2
    dwdvy = (-V2*ax - 2*vy*(vx*ay-vy*ax)) / (V2**2)
    dwday = vx / V2

    # speed partial derivs
    dsdvx = vx / sqrt(V2)
    dsdvy = vy / sqrt(V2)

    return np.array([
        [1, 0, 0, 0, 0, 0],
        [0, 0, 0, 1, 0, 0],
        [0, dwdvx, dwdax, 0, dwdvy, dwday],
        [0, dsdvx, 0, 0, dsdvy, 0],
        ])

# redefine R to include speedometer and gyro variances
R = diag(np.array([gps_error**2, gps_error**2, omega_sig**2, speed_sig**2]))

# create an observation vector of all noisy signals
observations = np.array([x_gps, y_gps, omega_sens, speed_sens]).T

# matrix dimensions
nx = Q.shape[0]
ny = R.shape[0]
nt = observations.shape[0]

# allocate identity matrix for re-use
Inx = np.eye(nx)

# allocate result matrices
x_pred = np.zeros((nt, nx))      # prediction of state vector
P_pred = np.zeros((nt, nx, nx))  # prediction error covariance matrix
x_est = np.zeros((nt, nx))       # estimation of state vector
P_est = np.zeros((nt, nx, nx))   # estimation error covariance matrix
K = np.zeros((nt, nx, ny))       # Kalman Gain

# set initial prediction
x_pred[0] = x_init
P_pred[0] = P_init

# for each time-step...
for i in range(nt):

    # prediction stage
    if i > 0:
        x_pred[i] = A @ x_est[i-1]
        P_pred[i] = A @ P_est[i-1] @ A.T + Q

    y_pred = eval_h(x_pred[i])
    H_pred = eval_H(x_pred[i])

    # estimation stage
    y_obs = observations[i]
    # kalman gain calculation
    K[i] = P_pred[i] @ H_pred.T @ inv((H_pred @ P_pred[i] @ H_pred.T) + R)
    # nuova media e quindi nuova stima della posizione
    x_est[i] = x_pred[i] + K[i] @ (y_obs - y_pred)
    # nuova varianza del rumore
    P_est[i] = (Inx - K[i] @ H_pred) @ P_pred[i]
'''

fig = plt.figure()
ax = fig.add_subplot(111)
# percorso reale
ax.plot(x, y, color='orange', marker='+', label="reale")
# stima
ax.plot(x_est[:, 0], x_est[:, 3], color='blue', linewidth=3, label="stima")
# GPS plot
ax.scatter(x_gps, y_gps, color='black', marker='^', label="misurazioni")
# posizione iniziale
ax.scatter(2, 0, color='red', marker='o', label="posizione iniziale")

plt.legend()
plt.show()
