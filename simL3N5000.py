import pandas as pd
import numpy as np
import pystan
from sklearn.metrics import f1_score, cohen_kappa_score 
from sklearn.metrics import roc_auc_score, confusion_matrix

## FUNCTIONS
# CDF function
def cauchy_cdf(x, lamb):
    return ((0.5 + (1.0 / np.pi) * np.arctan(x)))**lamb

def logis_cdf(x):
    return (1.0 / (1.0 + np.exp(-x)))
primes = []
for num in range(1000, 1800):
    if all(num % i != 0 for i in range(2, num)):
            primes.append(num)
seeds = np.array(primes)

# metrics for classification
def to_labels(pos_probs, threshold):
	return (pos_probs >= threshold).astype('int')

def mat_con(y_obs, y_score, thr):
    y_pred = [1 if x > thr else 0 for x in y_score]
    cmx = confusion_matrix(y_obs, y_pred)
    pred_scor = dict(y_true = y_obs, y_score = y_score)
    AUC = roc_auc_score(**pred_scor)
    TN = cmx[0][0]
    FN = cmx[1][0]
    TP = cmx[1][1]
    FP = cmx[0][1]
    ACC = (TP+TN)/(TP+TN+FP+FN)
    TPR = (TP)/(TP+FN)
    TNR = (TN)/(TN+FP)
    CSI = (TP)/(TP+FP+FN)
    GS = (TP*TN - FP*FN)/((FN+FN)*(TP+FP+FN+TN)+(TP*TN-FP*FN))
    SSI = (TP)/(TP+2*FP+2*FN)
    FAITH = (TP+0.5*TN)/(TP+TN+FP+FN)
    PDIF = (4*FP*FN)/((TP+TN+FP+FN)**2)
    MCC = (TP * TN - FP * FN) / np.sqrt((TP + FP)*(TP + FN)*(TN + FP)*(TN + FN))
    G_M = np.sqrt(TPR*TNR)
    F1 = f1_score(y_obs, y_pred)
    KAPPA = cohen_kappa_score(y_obs, y_pred)
    metrics = np.hstack([AUC, ACC, TPR, TNR, CSI, GS, SSI, FAITH, PDIF, MCC, G_M, F1, KAPPA])
    return metrics

def mettric_class(y_obs, y_score, y_score2, metricOpt="f1"):
    thresholds = np.arange(0.001, 0.999, 0.001)
    if metricOpt == 'f1':
        metric = [f1_score(y_obs, to_labels(y_score, t)) for t in thresholds]
        ix = np.argmax(metric)
    elif metricOpt == 'kappa':
        metric = [cohen_kappa_score(y_obs, to_labels(y_score, t)) for t in thresholds]
        ix = np.argmax(metric)
    thr = thresholds[ix]
    metPC = mat_con(y_obs, y_score, thr)
    metL = mat_con(y_obs, y_score2, thr)
    s = np.hstack([metPC, metL])
    return s
              
# Simulation
def genpower(N, betavec, potencia):
    ## DATA SET
    x2 = np.random.normal(0, 1, size=N)
    eta = betavec[0]+betavec[1]*x2
    prob = cauchy_cdf(eta,potencia)
    y = np.random.binomial(1, prob, size = N) 
    # data for Stan
    np.mean(y)
    dataS = {}
    dataS['y_train']   = y
    dataS['X_train']  = x2
    dataS['p']   = 2
    dataS['N_train']  = N    
    ## PREDICT IN STAN
    model_pc = '''
    data {
    	int<lower = 0> p;
    	int<lower = 1> N_train;
        real X_train[N_train];
    	int<lower = 0, upper = 1> y_train[N_train];
    }
    parameters{
    	vector[p] beta;
    	real loglambda;
    }
    transformed parameters{
    	real prob[N_train];
    	real<lower = 0> lambda;
    	lambda <- exp(loglambda);
    	for(i in 1:N_train){
    	prob[i] <- pow(cauchy_cdf(beta[1]+beta[2]*X_train[i], 0, 1),lambda);
    	}
    }
    model {
    	beta ~ normal(0.0,100);
    	loglambda ~ uniform(-2,2);
    	y_train ~ bernoulli(prob);
    }
    generated quantities {
        vector[N_train] log_lik;
        for (i in 1:N_train) {
                log_lik[i] = bernoulli_lpmf(y_train[i] | prob[i]);
                }
        }
    '''
    sm_pc = pystan.StanModel(model_code=model_pc)
    fit_pc = sm_pc.sampling(data=dataS, 
                        chains=chains, iter=iters,
                        warmup=warmup,thin=thin,seed=seed)
    mpc_summary = pd.DataFrame(fit_pc.summary()['summary'],
                             columns=fit_pc.summary()['summary_colnames'],
                             index=fit_pc.summary()['summary_rownames'])
    mpc = mpc_summary.loc[['beta[1]','beta[2]','lambda']]
    
    model_l = '''
    data {
    	int<lower = 0> p;
    	int<lower = 1> N_train;
        real X_train[N_train];
    	int<lower = 0, upper = 1> y_train[N_train];
    }
    parameters{
    	vector[p] beta;
    }
    transformed parameters{
    	real prob[N_train];
    	for(i in 1:N_train){
    	prob[i] <-logistic_cdf(beta[1]+beta[2]*X_train[i], 0, 1);
    	}
    }
    model {
    	beta ~ normal(0.0,100);
    	y_train ~ bernoulli(prob);
    }
    generated quantities {
        vector[N_train] log_lik;
        for (i in 1:N_train) {
                log_lik[i] = bernoulli_lpmf(y_train[i] | prob[i]);
                }
        }
    '''
    sm_l = pystan.StanModel(model_code=model_l)
    fit_l = sm_l.sampling(data=dataS, 
                        chains=chains, iter=iters,
                        warmup=warmup,thin=thin,seed=seed)
    ml_summary = pd.DataFrame(fit_l.summary()['summary'],
                             columns=fit_l.summary()['summary_colnames'],
                             index=fit_l.summary()['summary_rownames'])
    ml = ml_summary.loc[['beta[1]','beta[2]']]
    # metrics
    y_score = np.mean(fit_pc["prob"], axis = 0)
    y_score2 = np.mean(fit_l["prob"], axis = 0)
    base_f1 = mettric_class(y, y_score, y_score2,  metricOpt="f1")
    base_k = mettric_class(y, y_score, y_score2,  metricOpt="kappa")
    # ouput 
    s_metric = np.hstack([base_f1, base_k])
    summar = {
        "metric": s_metric
        }
    return summar

def rpower(M, N, betavec, potencia):
    sim_parm = np.zeros((M, 6))
    sim_metric = np.zeros((M, 52))
    sim_crit = np.zeros((M, 6))    
    for i in range(M):
        np.random.seed(seeds[i])
        gen = genpower(N, betavec, potencia)
        sim_parm[i] = gen['param']
        sim_metric[i] = gen['metric']
        sim_crit[i] = gen['citer']
    summ_sim = {
        "sim_parm": sim_parm,
        "sim_metric": sim_metric,
        "sim_crit": sim_crit
        }
    
    return summ_sim


# mcmc values
chains = 2
iters = 2000
warmup = 1000
thin = 1
seed = 10000003

M = 100

# fiex values
N = 5000
betavec = np.array([-0.5, 1.5])
potencia = 3

sim1 = rpower(M, N, betavec, potencia)
with pd.ExcelWriter('output_simulation_M100L3N5000.xlsx') as writer:
    pd.DataFrame(sim1["sim_metric"]).to_excel(writer, sheet_name='sim_metric')














