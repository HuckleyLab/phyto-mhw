import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

def tpc(T, a, b, z, w):
    '''
    https://science.sciencemag.org/content/sci/suppl/2012/10/25/science.1224836.DC1/Thomas.SM.pdf
    '''
    return a * np.exp(b*T) * (1 - ((T - z)/(w / 2))**2)

def physiological_scaler(physio_data):
    max_performance = physio_data['mu.g.opt.val.list'].item() # max growth rate
    
    def _scaler(data):
        oldmin = -max_performance
        oldmax = max_performance
        newmin = -1
        newmax = 1
        
        return (((data - oldmin) * (newmax - newmin)) / (oldmax - oldmin)) + newmin
        return(newdata)
    return(_scaler)
    
    

def plot_tpc(sample, ax=None, color='k', alpha=1):
    T = np.arange(sample['mu.c.opt.list'] - (sample['mu.wlist'] / 2), sample['mu.c.opt.list'] + (sample['mu.wlist'] / 2), 0.1)
    perf = tpc(T, sample['mu.alist'], sample['mu.blist'], sample['mu.c.opt.list'], sample['mu.wlist'])
    try:
        plotTitle = "{} {} [{}]".format(sample.genus, sample.species, sample['isolate.code'])
    except AttributeError as e: 
        plotTitle = ""
    if ax:
        ax.plot(T, perf, alpha=alpha, color=color)
        ax.set_title(plotTitle)
        ax.set_xlabel("T")
        sns.despine(ax=ax)
    else:
        plt.plot(T, perf, alpha=alpha, color=color)
        plt.title(plotTitle)
        plt.xlabel("$T$ [${^\circ}{C}$]")
        sns.despine()
        
def perf_det(T, T_opt, tpc, axis=1):
    return tpc(T_opt) - tpc(T)

def plot_det(s, ax, t=None):
    this_tpc = partial(tpc, a=s['mu.alist'], b=s['mu.blist'], z=s['mu.c.opt.list'], w=s['mu.wlist'])
    T = np.arange(s['mu.c.opt.list'] - (s['mu.wlist'] / 2), s['mu.c.opt.list'] + (s['mu.wlist'] / 2), 0.1)
    
    try:
        plotTitle = "{} {} [{}]".format(sample.genus, sample.species, sample['isolate.code'])
    except AttributeError as e: 
        plotTitle = ""
    
    plt.title(plotTitle)
    
    perf = this_tpc(T)
    
    max_perf = this_tpc(s['mu.g.opt.list'])
    plt.scatter(s['mu.g.opt.list'], max_perf, color='red', zorder=5, label='$T_{opt} = ' + '{:.2f}$'.format(s['mu.g.opt.list']))
    
    if not t:
        randomT = np.random.choice(T, size=1)[0]
    else: 
        randomT = t
    perf_T = this_tpc(randomT)
    det_T = perf_det(randomT, s['mu.g.opt.list'], this_tpc)

    plt.plot(T, perf, label='Performance Curve')
    plt.vlines(randomT,0, perf_T, linewidth=2.5, color='green', label='Performance at $T = {:.2f}$'.format(randomT))
    plt.vlines(randomT,max_perf, max_perf - det_T, linewidth=2.5, color='red', label='Performance Detriment')
    plt.axhline(max_perf, linestyle='--', color='orange', label="Maximum Performance")
    plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
    
    plt.ylabel("Growth Rate")
    plt.xlabel("Temperature [C]")

    sns.despine()
    plt.tight_layout()
    plt.savefig("{}_{}_{}.png".format(sample.genus, sample.species, sample['isolate.code']))
    
    
    
def tpc_general(T, Topt, CTmin, CTmax):
    """
    R Code: 
    
    TPC<- function(T,Topt,CTmin, CTmax){
  F=T
  F[]=NA
  sigma= (Topt-CTmin)/4
  F[T<=Topt & !is.na(T)]= exp(-((T[T<=Topt & !is.na(T)]-Topt)/(2*sigma))^2)
  F[T>Topt & !is.na(T)]= 1- ((T[T>Topt & !is.na(T)]-Topt)/(Topt-CTmax))^2
  #set negetative to zero
  F[F<0]<-0
  return(F)
}
    
    Reference: Deutsch CA, Tewksbury JJ, Huey RB, Sheldon KS, Ghalambor CK, Haak DC, Martin PR (2008) Impacts of climate warming on terrestrial ectotherms across latitude. Proceedings of the National Academy of Science of the United States of America, 105, 6668– 6672.
    
    """
    
    F = np.zeros_like(T)
    sigma = (Topt-CTmin) / 4
    
    F[(T <= Topt) & (~np.isnan(T))] = np.exp(
        -((T[(T<=Topt) & (~np.isnan(T))] - Topt)/(2*sigma))**2
    )
    
    F[(T > Topt) & (~np.isnan(T))] = 1 - ((T[(T>Topt) & (~np.isnan(T))]-Topt)/(Topt-CTmax))**2
    F[F<0] = 0
    return(F)
    