# =====================================================
# Problem 1:
# =====================================================
import numpy as np
import numpy.random as npr
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import itertools

# helper function to compute indicator variables
def indicator(k,j):
  return int(k==j)

# helper function to calculate the squared norm of a vector
def normsq(vec):
  return np.dot(vec,vec)

# function to handle one iteration of the k-means algorithm.
def kmeans_iter(data, means):
  
  bestmeans = []
  newmeanslist = list(means)
  
  # create a list of the indices of the means with minimum distance to each point.
  for i in range(len(data)):
    for k in range(len(means)):
      if k == 0:
	lowestdist = 0
      else:
	if (np.dot(data[i]-means[k],data[i]-means[k]) < np.dot(data[i]-means[lowestdist],data[i]-means[lowestdist])):
	  lowestdist = k	  
    bestmeans.append(lowestdist)
  
  # produce x and y axis vectors for plotting
  xvec = [data[i][0] for i in range(len(data))]
  yvec = [data[i][1] for i in range(len(data))]
  
  xgroups = []
  ygroups = []
  colors = iter(cm.rainbow(np.linspace(0, 1, len(means))))
  
  # produce and colorize the plot of the points in each group
  for L in range(len(means)):
    xgroups.append([xvec[i] for i in range(len(data)) if bestmeans[i]==L])
    ygroups.append([yvec[i] for i in range(len(data)) if bestmeans[i]==L])
    plt.scatter(xgroups[L],ygroups[L],color=next(colors))
  
  # show the plot grouping for the iteration
  plt.show()
  
  # use this list to compute new centers
  for k in range(len(means)):
    numersummands = [indicator(k,bestmeans[i])*data[i] for i in range(len(data))]
    denomsummands = [indicator(k,bestmeans[i]) for i in range(len(data))]
    newmeanslist[k] = sum(numersummands)/sum(denomsummands)
  
  # return the updated centers
  return newmeanslist

def kmeans_alg(k=2,filename="old_faithful.txt",thresh = 0.00001):
  oldfaithful = open(filename)
  data = []

  # read in data from file
  for line in oldfaithful:
    stringdata = line.split()
    datapoint = np.array([float(stringdata[0]),float(stringdata[1])])
    data.append(datapoint)
  
  # generate k random starting means
  means = [data[npr.randint(0,len(data))] for i in range(k)]
  previter = means
  means = kmeans_iter(data,means)
  
  # use the kmeans_iter function to cycle through updates until convergence.
  while (normsq(previter[0]-means[0]) > thresh) & (normsq(previter[1]-means[1]) > thresh):
    previter = means
    means = kmeans_iter(data,means)
  
  return means