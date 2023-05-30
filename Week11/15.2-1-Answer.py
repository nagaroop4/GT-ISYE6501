####### 15.2.2 Answer using PuLP with constraints######

# ---------- Import modules using PIP -----------

#import PuLP and pandas modules


import pandas as pd
from pulp import *


# ------------ Reading the Data from the given Diet.xls ---------------
# Create Food data list
# Column Header
# Min and Max Values

data = pd.read_excel("/Users/nagarajanmurugan/Documents/MS/data/diet.xls", header = 0)

foodTable = data[0:64]
foodTable = foodTable.values.tolist()
foodNamelists = list(data.columns.values) 
minValue = data[65:66].values.tolist() 
maxValue = data[66:67].values.tolist()
    

# ------------ Using  python "dict" structure to extract Values and amount of food --------#

foodnames = [j[0] for j in foodTable]
foodcost = dict([(j[0], float(j[1])) for j in foodTable]) # foodcost for each food
foodlist = []
for i in range(0,11): 
    foodlist.append(dict([(j[0], float(j[i+3])) for j in foodTable])) 


# ------------ Find the lowest foodcost using LpMinimize ------------ 

prob = LpProblem('Food optimization', LpMinimize) 


# ------------ Define the variables with '0' as lower limit --------------


foodVars = LpVariable.dicts("Foodnames", foodnames, 0)



# ------------ Create objective function to find total foodcost ------------ 


prob += lpSum([foodcost[f] * foodVars[f] for f in foodnames]), 'Total foodcost'


# ------------ Add constraints for each nutrient ------------ 

for i in range(0,11): 
    prob += lpSum([foodlist[i][j] * foodVars[j] for j in foodnames]) >= minValue[0][i+3], 'min nutrient ' + foodNamelists[i]
    prob += lpSum([foodlist[i][j] * foodVars[j] for j in foodnames]) <= maxValue[0][i+3], 'max nutrient ' + foodNamelists[i]


# ------------ Solve the optimization problem ------------ 

prob.solve()


# ------------ Print the output in a readable format ----------- 

print()
print("---------The solution to the diet problem with out constraint is----------")
for var in prob.variables():
    if var.varValue > 0:
        print(str(var.varValue)+" units of "+str(var).replace('Foods_','') )
print()
print("Total cost of food = $%.2f" % value(prob.objective))        
