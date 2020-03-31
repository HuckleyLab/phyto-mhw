import pandas as pd

def load_plankton(datafile, minqual='good', maxqual='good', curvequal='good'):

    plankton = pd.read_csv(datafile, engine='python')
    plankton = plankton[(plankton.minqual == "good") &
                    (plankton.maxqual == "good") &
                    (plankton.curvequal == "good")]
    plankton = plankton[plankton.habitat == 'marine']
    return(plankton)
    