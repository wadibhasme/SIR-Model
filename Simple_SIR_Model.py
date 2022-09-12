####################### Simple S-I-R Model Using Derivative
import matplotlib.pyplot as plt
# First we will define the initial values of alpha,beta and gamma respectively.
bt = 0.02             ### Infected rate
gm = 0.5               #### Recovery rate
nb_rate = 0.4              #### new born rate
dt = 0.7               #### fraction of recovery


def Susceptible_Agent(t, S, I):
    global bt                  ##### TO use the Global variable inside the function,we use global keyword
    return -bt*S*I + nb_rate


def infected_Agent(t, S, I):
    global bt, gm
    return bt*S*I-dt*gm*I - (1-dt)*gm*I

def recovered_Agent(t, I):
    global gm, dt
    return dt*gm*I


# Initial value for suspected,recovered and infected
S0 = 1500       ####### No of Susceptible Agent
I0 = 5           ####### No of Infected Agent
R0 = 0          ###### No of Recovered Agent(Initialy)
D0 = 0
t0 = 0
n = 3000       ######## No of Iteration
tn = 1000      ### Result after 1000 Iteration
# defining the function for RK4


def RK4_Order_Method(t0, S0, I0, R0, D0, tn, n):

    # calculate step size
    h = 0.002
    # storing the values in the list
    Susceptible_list = []
    Infected_list = []
    Recovered_list = []

    while(n):               ### n is non zero, so loop is terminated when n is zero
        a1 = h * (Susceptible_Agent(t0, S0, I0))
        b1 = h * (infected_Agent(t0, S0, I0))
        c1 = h * (recovered_Agent(t0, I0))

        a2 = h * (Susceptible_Agent((t0+h/2), (S0+h*a1/2), (I0+h*b1/2)))
        b2 = h * (infected_Agent((t0+h/2), (S0+h*a1/2), (I0+h*b1/2)))
        c2 = h * (recovered_Agent((t0+h/2), (I0+h*b1/2)))

        a3 = h * (Susceptible_Agent((t0+h/2), (S0+h*a2/2), (I0+h*b2/2)))
        b3 = h * (infected_Agent((t0+h/2), (S0+h*a2/2), (I0+h*b2/2)))
        c3 = h * (recovered_Agent((t0+h/2), (I0+h*b2/2)))

        a4 = h * (Susceptible_Agent((t0+h), (S0+h*a3), (I0+h*b3)))
        b4 = h * (infected_Agent((t0+h), (S0+h*a3), (I0+h*b3)))
        c4 = h * (recovered_Agent((t0+h), (I0+h*b3)))

        k = (a1 + 2*a2 + 2*a3 + a4)/6
        Sn = S0 + k
        S0 = Sn

        l = (b1 + 2*b2 + 2*b3 + b4)/6
        In = I0 + l
        I0 = In

        m = (c1 + 2*c2 + 2*c3 + c4)/6
        Rn = R0 + m
        R0 = Rn

        t0 = t0+h
        Susceptible_list.append(S0)
        Infected_list.append(I0)
        Recovered_list.append(R0)
        n=n-1                    ######### Decrese the value of n by 1

    print('\nAt t=%.2f, S=%.4f' % (tn, Sn))
    print('\nAt t=%.2f, R=%.4f' % (tn, Rn))
    print('\nAt t=%.2f, I=%.4f' % (tn, In))
    return (Susceptible_list,Infected_list,Recovered_list)
S_list,I_list,R_list=RK4_Order_Method(t0,S0,I0,R0,D0,tn,n)
print(S0+I0+R0+D0)
plt.plot(list(range(len(S_list))), S_list, color = 'green')
plt.plot(list(range(len(I_list))), I_list, color = 'red')
plt.plot(list(range(len(R_list))), R_list, color = 'blue')
plt.gca().legend(['Susceptible','Infected','Recovered','Death','Birth'])
plt.xlabel("Number of Days")
plt.ylabel("Population")
plt.title("SIR Model")
plt.show()
##################################################################
