import numpy as np
import pandas as pd
import random
from sklearn.datasets import make_classification
from sklearn.neighbors import NearestNeighbors

def get_tail_label(df):

    columns = df.columns
    n = len(columns)
    irpl = np.zeros(n)
    for column in range(n):
        irpl[column] = df[columns[column]].value_counts()[1]
    irpl = max(irpl) / irpl
    mir = np.average(irpl)
    tail_label = []
    for i in range(n):
        if irpl[i] > mir:
            tail_label.append(columns[i])
    return tail_label



    tail_labels = get_tail_label(df)
    index = set()
    for tail_label in tail_labels:
        sub_index = set(df[df[tail_label] == 1].index)
        index = index.union(sub_index)
    return list(index)


def get_minority_instace(X, y):

    index = get_index(y)
    X_sub = X[X.index.isin(index)].reset_index(drop=True)
    y_sub = y[y.index.isin(index)].reset_index(drop=True)
    return X_sub, y_sub


def nearest_neighbour(X):

    nbs = NearestNeighbors(n_neighbors=5, metric='euclidean', algorithm='kd_tree').fit(X)
    euclidean, indices = nbs.kneighbors(X)
    return indices


def agfSMOTE(X, y, n_sample):

    indices2 = nearest_neighbour(X)
    n = len(indices2)
    new_X = np.zeros((n_sample, X.shape[1]))
    target = np.zeros((n_sample, y.shape[1]))
    for i in range(n_sample):
        reference = random.randint(0, n - 1)
        neighbour = random.choice(indices2[reference, 1:])
        all_point = indices2[reference]
        nn_df = y[y.index.isin(all_point)]
        ser = nn_df.sum(axis=0, skipna=True)
        target[i] = np.array([1 if val >= 1 else 0 for val in ser])
        for val in range(7):
            if val == 0:
                target[i][val]=1 if ser[val] >= 1 else 0
            else:
                target[i][val]=1 if ser[val] >= 2 else 0
        ratio = random.random()
        gap = X.loc[reference, :] - X.loc[neighbour, :]
        new_X[i] = np.array(X.loc[reference, :] + ratio * gap)
    new_X = pd.DataFrame(new_X, columns=X.columns)
    target = pd.DataFrame(target, columns=y.columns)
    return new_X, target

def load_dataset(filename,n):
	X = list()
	y = list()
	with open(filename, 'r') as f:
		for l in f:
			l = l.strip().replace("'",'').split('\t')
			l1 = [float(i) for i in l[0:len(l)-n]]
			l2 = [int(i) for i in l[len(l)-n:len(l)+1]]
			X.append(l1)
			y.append(l2)
	return pd.DataFrame(np.array(X)), pd.DataFrame(np.array(y))


def main():

    filename = ''
    X, y = load_dataset(filename,)
    X_sub, y_sub = get_minority_instace(X, y)  # Getting minority instance of that datframe
    X_res, y_res = MLSMOTE(X_sub, y_sub, n_samples )  # Applying SMOTE to augment the dataframe
    new_X = pd.concat([X,X_res], axis=0)
    new_y = pd.concat([y,y_res], axis=0)
    new_data = pd.concat([new_X, new_y], axis=1)
    return(new_data)
if __name__ == '__main__':
    new_data = main(filename, n_labels)
    filepath = ''
    output_file =
    new_data.to_csv(output_file,index=False,sep='\t')



