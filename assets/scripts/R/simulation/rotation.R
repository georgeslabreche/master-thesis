
# Roll rotation.
Rx = function(theta){
  theta_rad = theta * pi/180
  
  rot = c(
    1, 0, 0,
    0, cos(theta_rad), sin(theta_rad),
    0, -sin(theta_rad), cos(theta_rad))
  
  rot_mat = matrix(rot, ncol = 3, nrow = 3)
  
  return(rot_mat)
}

# Pitch rotation
Ry = function(theta){
  theta_rad = theta * pi/180
  
  rot = c(
    cos(theta_rad), 0, -sin(theta_rad),
    0, 1, 0,
    sin(theta_rad), 0, cos(theta_rad))
  
  rot_mat = matrix(rot, ncol = 3, nrow = 3)
  
  return(rot_mat)
}

# Yaw rotation.
Rz = function(theta){
  theta_rad = theta * pi/180
  
  rot = c(
    cos(theta_rad), sin(theta_rad), 0,
    -sin(theta_rad), cos(theta_rad), 0,
    0, 0, 1)
  
  rot_mat = matrix(rot, ncol = 3, nrow = 3)
  
  return(rot_mat)
}


Pitch = function(theta){
  return(Ry(theta))
}

Roll = function(theta){
  return(Rx(theta))
}

Yaw = function(theta){
  return(Rz(theta))
}


vect = matrix(c(1,0,0), ncol = 1, nrow = 3)

# Pitch the vector by 20 deg.
cat("\nPitch 20 deg.\n")
print(Pitch(20)%*%vect)

# Pitch 20deg, Roll 12 deg.
cat("\n\nPitch 20 deg, Roll 12 deg.\n")
print(Pitch(20)%*%Roll(12)%*%vect)

# Pitch 20deg, Roll 12 deg.
cat("\n\nRoll 12 deg, Pitch 20 deg.\n")
print(Roll(12)%*%Pitch(20)%*%vect)