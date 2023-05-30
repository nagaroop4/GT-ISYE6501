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
# find the binary value if the food is eaten

foodVars = LpVariable.dicts("foodnames", foodnames, 0)
foodVars_selected = LpVariable.dicts("food_select",foodnames,0,1,LpBinary)


# ------------ Create objective function ------------ 
#
# Note that the first function we add is taken to be the objective function

prob += lpSum([foodcost[f] * foodVars[f] for f in foodnames]), 'Total foodcost'


# ------------ Add constraints for each nutrient ------------ 

for i in range(0,11): 
    prob += lpSum([foodlist[i][j] * foodVars[j] for j in foodnames]) >= minValue[0][i+3], 'min nutrient ' + foodNamelists[i]
    prob += lpSum([foodlist[i][j] * foodVars[j] for j in foodnames]) <= maxValue[0][i+3], 'max nutrient ' + foodNamelists[i]


# ------------ Adding additional constraints ------------   

# CONSTRAINT A

# If a food is eaten, must eat at least 0.1 serving

for food in foodnames:
    prob += foodVars[food] >= 0.1 * foodVars_selected[food]

# If any of a food is eaten, its binary variable must be 1

for food in foodnames:
    prob += foodVars_selected[food] >= foodVars[food]*0.0000001 

# CONSTRAINT B

# Include at most 1 of celery and frozen brocolli

prob += foodVars_selected['Frozen Broccoli'] + foodVars_selected['Celery, Raw'] <= 1 

# CONSTRAINT C

# At least 3 kinds of meat/poultry/fish/eggs

prob += foodVars_selected['Roasted Chicken'] + foodVars_selected['Poached Eggs'] \
        + foodVars_selected['Scrambled Eggs'] + foodVars_selected['Bologna,Turkey'] \
        + foodVars_selected['Frankfurter, Beef'] + foodVars_selected['Ham,Sliced,Extralean'] \
        + foodVars_selected['Kielbasa,Prk'] + foodVars_selected['Pizza W/Pepperoni'] \
        + foodVars_selected['Hamburger W/Toppings'] \
        + foodVars_selected['Hotdog, Plain'] + foodVars_selected['Pork'] \
        + foodVars_selected['Sardines in Oil'] + foodVars_selected['White Tuna in Water'] \
        + foodVars_selected['Chicknoodl Soup'] + foodVars_selected['Splt Pea&Hamsoup'] \
        + foodVars_selected['Vegetbeef Soup'] + foodVars_selected['Neweng Clamchwd'] \
        + foodVars_selected['New E Clamchwd,W/Mlk'] + foodVars_selected['Beanbacn Soup,W/Watr'] >= 3


# ------------ Solve the optimization problem ------------ 

prob.solve()


# ------------ Print the output to find the cost ----------- 

print()
print("---------The solution to the diet problem with Constraint is----------")
for var in prob.variables():
    if var.varValue > 0 and "food_select" not in var.name: 
        print(str(var.varValue)+" units of "+str(var).replace('foodnames_','') )
print()
print("Total foodcost = $%.2f" % value(prob.objective))        
