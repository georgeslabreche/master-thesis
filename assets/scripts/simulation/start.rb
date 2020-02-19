#!/usr/bin/env ruby
require "orocos"
require "open3"
require "csv"

# Areocentric longitude.
ls = 81

# Planetary latitude.
phi = 34

# Planetary longigude
longitude = 17

# Optical opacity, tau factor.
tau = 0.5

# Solar time on Mars [h].
t_mars_start = 12.0

# Step time [s].
# i.e. sleep time between data fetching iterations.
t_step = 0.1

# Step counter.
step = 1

# Simulation start time [s]
t_sim_start = nil

# Elapsed time since start of simulation [s]
t_elapsed = 0

# Ismenus Cavus: 2.4 m2.
# Iani Chaos: 1.5 m2.
sc_area = 2.4

# Solar cell efficiency.
sc_efficiency = 0.22

# Performance Ratio.
pr = 0.62

# CSV file data header.
headers = ["step", "t_elapsed", "Ls", "phi", "longitude", "tau", "Ts", "beta", "gamma_c", "irradiance", "power"]

# Execute an R script to calculate global irradiance with the given values.
# TODO: implement error handling.
def get_global_irradiance(ls, phi, longitude, tau, t, beta, gamma_c)

   # The R command to execute the R script.
   # Not the secured way of passing arguments but for some reason capture3('Rscript', args) does not work
   r_command = "Rscript --vanilla irradiance.R #{ls} #{phi} #{longitude} #{tau} #{t} #{beta} #{gamma_c}"

   # Execute the irradiance calculation R script.
   #puts r_command
   out, err, st = Open3.capture3(r_command)

   # Return calculated irradiance.
   return out.gsub("\n", "").to_f.round(2)
end

# Init Orocos
Orocos.initialize

# Run Orocos
Orocos.run do
    # Fetch the IMU task.
    imu = Orocos.name_service.get "sherpa_tt_imu"

    # Write collected simmulation data into CSV file.
    CSV.open("data.csv", "w") do |csv_row|
        csv_row << headers
    end

    # The program loop. Every iteration fetches up to date data.
    loop do

        # Listen for 'q' character input to exit the program.
        system("stty raw -echo")
        char = STDIN.read_nonblock(1) rescue nil
        system("stty -raw echo")
        break if /q/i =~ char

        # Reader for the IMU output port.
        reader = imu.orientation_samples.reader

        # Fetch IMU data from the output port.
        # Watch out for potential infinite loop.
        data = nil
        while data == nil
            data = reader.read
        end

        # Get elapsed time based on simulation timestamp
        timestamp = data.time.to_time.to_i

        if t_sim_start == nil
            t_sim_start = timestamp
        else
            t_elapsed = timestamp - t_sim_start
        end

        # Update Mars solar time for the irradiance calculation.
        t_mars = (t_mars_start + t_elapsed/3600.0).round(2)

        # Get roll, pitch, and yaw values
        euler = Eigen::Quaternion.new(data.orientation.re, *data.orientation.im.to_a)
        roll = euler.roll
        pitch = euler.pitch
        yaw = euler.yaw

        # Inclination angle beta (pitch or roll or resulting combination of both).
        beta = (pitch * 180 / Math::PI).round(2)

        # Orientation angle gamma_c.
        gamma_c = (yaw * 180 / Math::PI).round(2)

        # Calculate irradiance.
        irradiance = get_global_irradiance(ls, phi, longitude, tau, t_mars, beta, gamma_c)

        # Calculate power.
        power = sc_area * sc_efficiency * irradiance * pr
        p power

        # Append new data to CSV file.
        CSV.open("data.csv", "a+") do |csv_row|
            data_row = [step, t_elapsed, ls, phi, longitude, tau, t_mars, beta, gamma_c, irradiance, power]
            csv_row << data_row
        end

        # Update step counter.
        step = step + 1

        # Wait for a bit before starting the next iteration.
        sleep(t_step)

    end
end

