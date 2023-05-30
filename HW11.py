import numpy as np 
from pulp import * 
import pandas as pd 
import os

# 15.2 
#Part 1

path = '/Users/meghajoshi/Library/Mobile Documents/com~apple~CloudDocs/Documents/Georgia Tech OMS/OMSA_ISYE6501/'
diet = pd.read_excel(path + 'diet.xls', header=0)
print(diet.head(5)) 

data = diet[0:64] 
data = data.values.tolist() 

min_intake = diet[65:66].values.tolist() 
max_intake = diet[66:67].values.tolist()

print(min_intake)
print(max_intake) 

unique_foods = [i[0] for i in data] 
cost_per_food = dict([(i[0], float(i[1])) for i in data])

nutrient_constraints = []
for i in range (3, 14): 
    nutrient_constraints.append(dict([(w[0], float(w[i])) for w in data]))

# Creating the optimization problem
opt_problem = LpProblem('FoodSelection', LpMinimize) 
# continuous variables
food_variables = LpVariable.dicts("foods", unique_foods, 0)
# binary variables
binary_variables = LpVariable.dicts("Chosen", unique_foods, 0, 1, "Binary")
# dictionary of lp variables 
x = LpVariable.dicts("x", unique_foods, 0)
# obj func
opt_problem += lpSum([cost_per_food[f] * food_variables[f] for f in unique_foods]) 

#adding constraints to objective function 
for i in range(0,11):
    opt_problem += min_intake[i] <= + lpSum([nutrient_constraints[i][j] * food_variables[j] for j in unique_foods]) 

for i in range(0,11):
    opt_problem += max_intake[i] >= + lpSum([nutrient_constraints[i][j] * food_variables[j] for j in unique_foods]) 


#Part 2

#Adding additional Constraints
#1) Must eat 1/10 of a serving 
for food in unique_foods:    
    opt_problem += unique_foods[food] >= 0.1 * binary_variables[food]
#2) At most 1 celery and frozen broccli 
opt_problem += binary_variables['Frozen Broccoli'] + binary_variables['Celery, Raw'] <= 1 
#3) Three kinds of meat 
opt_problem += binary_variables['Roasted Chicken'] + binary_variables['Poached Eggs'] + \
  binary_variables['Scrambled Eggs'] + binary_variables['Frankfurter, Beef'] + \
  binary_variables['Kielbasa,Prk'] + binary_variables['Hamburger W/Toppings'] + \
  binary_variables['Hotdog, Plain'] + binary_variables['Pork'] + \
  binary_variables['Bologna,Turkey'] + binary_variables['Ham,Sliced,Extralean'] + \
  binary_variables['White Tuna in Water'] >= 3 

opt_problem.solve()

#The solution
# 0.1 units of Bologna,Turkey
# 42.423026 units of Celery,_Raw
# 82.673927 units of Lettuce,Iceberg,Raw
# 3.0856009 units of Oranges
# 1.9590978 units of Peanut_Butter
# 0.1 units of Poached_Eggs
# 13.214473 units of Popcorn,Air_Popped
# 0.1 units of Scrambled_Eggs

# cost = $4.51