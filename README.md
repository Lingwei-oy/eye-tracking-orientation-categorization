# eye-tracking-orientation-categorization
figure out fixation/saccades and moving orientations

# The experiment used head-free method, thus the visual angle can't be calculated
# Initially the velocity-based method is used to differentiate saccade/fixation, the threshold is selected based on exploratory analysis
# The method I am currently developing is based on Dispersion-Threshold Identification (I-DT) method (Salvucci & Goldberg, 2000)
# It needs two parameters, a minimum duration threshold between 100-200 ms and a dispersion threshold which is estimated based on exploratory analysis

# Pseudocode for the I-DT algorithm from the paper (Salvucci & Goldberg, 2000)

# While there are still points
# Initialize window over first points to cover the duration threshold
# If dispersion of window points <= threshold
#    Add additional points to the window until dispersion > threshold
#    Note a fixation at the centeroid of the window points
#    Remove window points from points
# Else 
#    Remove first point from points
# Return Fixations
