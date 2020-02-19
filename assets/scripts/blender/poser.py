import bpy
from math import radians

pose = 'sa_beta'

if pose == 'sa_beta':
    # Leg joints rotations.
    
    # Bag legs.
    gamma_limits_upper_rad = 1.3089969158 # Approx. 75 degrees.
    gamma2_limits_upper_rad = gamma_limits_upper_rad
    beta_limits_upper_rad = 0.296705961227417 # Approx. 17 degrees.
    
    # Front legs.
    beta_front_rad = radians(50)
    
    # Arm joints rotations.
    arm_joint_2_rad = radians(-20)
    arm_joint_3_rad = radians(165)
    arm_joint_5_rad = radians(4)
else:
    # Leg joints rotations.
    gamma_limits_upper_rad = 0
    gamma2_limits_upper_rad = radians(7.47996)
    beta_limits_upper_rad = 0
    beta_front_rad = 0
    
    # Arm joints rotations.
    arm_joint_2_rad = radians(180)
    arm_joint_3_rad = 0
    arm_joint_5_rad = 0

pose_rotations = {
    'beta_rear_right': beta_limits_upper_rad,
    'beta1_fake_rear_right': -beta_limits_upper_rad,
    'beta2_fake_rear_right': beta_limits_upper_rad,
    'gamma_rear_right': gamma_limits_upper_rad,
    'gamma1_fake_rear_right': -gamma_limits_upper_rad,
    'gamma2_fake_rear_right': gamma2_limits_upper_rad,
    'beta_rear_left': beta_limits_upper_rad,
    'beta1_fake_rear_left': -beta_limits_upper_rad,
    'beta2_fake_rear_left': beta_limits_upper_rad,
    'gamma_rear_left': gamma_limits_upper_rad,
    'gamma1_fake_rear_left': -gamma_limits_upper_rad,
    'gamma2_fake_rear_left': gamma2_limits_upper_rad,
    'beta_front_right': -beta_front_rad,
    'beta1_fake_front_right': beta_front_rad,
    'beta2_fake_front_right': -beta_front_rad,
    'beta_front_left': -beta_front_rad,
    'beta1_fake_front_left': beta_front_rad,
    'beta2_fake_front_left': -beta_front_rad,
    'arm_joint_2': arm_joint_2_rad,
    'arm_joint_3': arm_joint_3_rad,
    'arm_joint_5': arm_joint_5_rad
}


for obj in bpy.context.scene.objects:
    if obj.phobostype == 'link':
        if obj.name in pose_rotations:
            obj.rotation_euler = (obj.rotation_euler.x, pose_rotations[obj.name], obj.rotation_euler.z)
