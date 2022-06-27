library(flowdiagramr)

####################################
# acute virus and IR model
####################################
# original version fails
# several alternatives I tried also fail
# seems the division component is not processed right?

variables = c("U","I","V","F","T")
flows = list(U_flows = c("-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             V_flows = c("p*I", "-dV*V","-g*b*U*V"), # works
             #V_flows = c("I/F", "-dV*V","-g*b*U*V"), # also fails
             #V_flows = c("p*I/F", "-dV*V","-g*b*U*V"), #also fails
             #V_flows = c("p*I/(kF*F)", "-dV*V","-g*b*U*V"), #also fails
             #V_flows = c("p*I/(1+kF*F)","-dV*V","-g*b*U*V"), #original, fails
             F_flows = c("rF*I","-dF*F"),
             T_flows = c("rT*T*F","-dT*T")
             )
model <- list(variables = variables, flows = flows)

layout = list(varlocations = matrix(c("U","I","V",
                                      "F","T",""),
                            nrow = 2, byrow = TRUE)
              )

dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)


####################################
# chronic virus and IR model
####################################

variables = c("U","I","V","F","T")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             V_flows = c("p*I", "-dV*V","-g*b*U*V"), # works
             #V_flows = c("p*I/(1+kF*F)","-dV*V","-g*b*U*V"), #original, fails
             F_flows = c("rF*I","-dF*F"),
             T_flows = c("rT*T*V","-dT*T")
)
model <- list(variables = variables, flows = flows)

layout = list(varlocations = matrix(c("U","I","V",
                                      "","F","T"),
                                    nrow = 2, byrow = TRUE)
)

dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)



####################################
# extended bacteria model
####################################

variables = c("B","I","A")
flows = list(B_flows = c("g*B*(1-B/bmax)","-dB*B","-kI*B*I", "-kA*B*A"),
             I_flows = c("rI*B*(1-I/imax)", "-dI*I"),
             A_flows = c("rA*A*I/(h+I)","-dA*A")
             #A_flows = c("rA*A*log(I)/(h+log(I))","-dA*A") #original
)
model <- list(variables = variables, flows = flows)

layout = list(varlocations = matrix(c("","B","",
                                      "I","","A"),
                                    nrow = 2, byrow = TRUE)
)

dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)

