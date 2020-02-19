library(mars)

# Don't display mars library warnings.
Sys.setenv(NET_FLUX_FUNCTION_SHOW_WARNINGS = FALSE)

# This R script is being executed from within a Ruby script.
# Fetch the arguments passed along from the Ruby script.
args = commandArgs(trailingOnly=TRUE)

# Check that expected number of arguments have been given.
if (length(args) != 7) {
    write("Expected 7 arguments: Ls, phi, longitude, tau, Ts, beta (tilt or roll), and gamma_c (yaw)", stderr())
}

# Cast arguments to numeric.
args = as.numeric(args)

Ls = args[1]
phi = args[2]
longitude = args[3]
tau = args[4]
Ts = args[5]
beta = args[6]
gamma_c = args[7]

# Calculate global irradiance for given arguments.
Gi = G_i(Ls=Ls, phi=phi, longitude=longitude, Ts=Ts, tau=tau, beta=beta, gamma_c=gamma_c)

# Output the result.
write(Gi, stdout())

