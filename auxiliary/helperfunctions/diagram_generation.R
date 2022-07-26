library(flowdiagramr)

####################################
# basic virus model
# NOT OK
####################################
variables = c("U","I","V")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I"),
             V_flows = c("p*I", "-dV*V","-g*b*U*V")
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","I","V"),
                                    nrow = 1, byrow = TRUE),
              varspace_x_size = 0.3
)

dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)
ggplot2::ggsave("basicvirus.png",diag)

####################################
# acute virus and IR model
# NOT OK
####################################
variables = c("U","I","V","F","T")
flows = list(U_flows = c("-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             #V_flows = c("p*I", "-dV*V"), # works
             #V_flows = c("I/F", "-dV*V","-g*b*U*V"), # also fails
             #V_flows = c("p*I/F", "-dV*V","-g*b*U*V"), #also fails
             #V_flows = c("p*I/(kF*F)", "-dV*V","-g*b*U*V"), #also fails
             V_flows = c("p*I/(1+kF*F)","-dV*V","-g*b*U*V"), #original, fails
             F_flows = c("rF*I","-dF*F"),
             T_flows = c("rT*T*F","-dT*T")
             )
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","","I","","V",
                                      "","F","","T",""),
                            nrow = 2, byrow = TRUE),
              varspace_x_size = 0.3
              )
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)


####################################
# chronic virus and IR model
# not OK
####################################

variables = c("U","I","V","F","T")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             V_flows = c("p*I/(1+kF*F)","-dV*V","-g*b*U*V"), #original, fails
             F_flows = c("rF*I","-dF*F"),
             T_flows = c("rT*T*V","-dT*T")
)
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","","I","","V",
                                      "","F","","T",""),
                                    nrow = 2, byrow = TRUE),
              varspace_x_size = 0.3
)

dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)


####################################
# extended virus and IR model
# not OK
####################################
variables = c("U","I","V","F","T","B","A")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             V_flows = c("p*I/(1+sF*F)","-dV*V","-b*U*V","-kA*A*V"),
             F_flows = c("pF","-dF*F","V*gF*(fmax-F)/(V+hV)"),
             T_flows = c("gT*F*V","rT*T"),
             B_flows = c("gB*B*F*V/(F*V+hF)"),
             A_flows = c("rA*B","-dA*A","-kA*A*V"))
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","I","V",
                                      "F","T","A",
                                      "","B",""),
                                    nrow = 3, byrow = TRUE),
              varspace_x_size = 0.5
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)



####################################
# basic bacteria model
# OK
####################################
variables = c("B","I")
flows = list(B_flows = c("g*B*(1-B/bmax)","-dB*B","-kI*B*I"),
             I_flows = c("rI*B*(1-I/imax)", "-dI*I")
          )
model <- list(variables = variables, flows = flows)

layout = list(varlocations = matrix(c("B","I"),
                                    nrow = 1, byrow = TRUE)
)

dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)


####################################
# extended bacteria model
# not OK
####################################
variables = c("B","I","A")
flows = list(B_flows = c("g*B*(1-B/bmax)","-dB*B","-kI*B*I", "-kA*B*A"),
             I_flows = c("rI*B*(1-I/imax)", "-dI*I"),
             A_flows = c("rA*A*log(I)/(h+log(I))","-dA*A")
)
model <- list(variables = variables, flows = flows)

layout = list(varlocations = matrix(c("","B","",
                                      "I","","A"),
                                    nrow = 2, byrow = TRUE)
)

dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)

####################################
# extended virus and IR model
# not OK
####################################
variables = c("U","I","V","F","T","B","A")
flows = list(U_flows = c("n","-dU*U","-b*U*V"),
             I_flows = c("b*U*V","-dI*I","-kT*T*I"),
             V_flows = c("p*I/(1+sF*F)","-dV*V","-b*U*V","-kA*A*V"),
             F_flows = c("pF","-dF*F","V*gF*(fmax-F)/(V+hV)"),
             T_flows = c("gT*F*V","rT*T"),
             B_flows = c("gB*B*F*V/(F*V+hF)"),
             A_flows = c("rA*B","-dA*A","-kA*A*V"))
model <- list(variables = variables, flows = flows)
layout = list(varlocations = matrix(c("U","I","V",
                                      "F","T","A",
                                      "","B",""),
                                    nrow = 3, byrow = TRUE),
              varspace_x_size = 0.5
)
dlist <- prepare_diagram(model,layout)
diag <- make_diagram(dlist)
plot(diag)
