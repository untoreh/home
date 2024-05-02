# 12 characthers is the max length for allowed compact names
ismissingcat()
ismissingcatfood() # too long!
ismissingcat_food() # 12 _ 4 OK!
# The idea is that words are split with underscores in chunks of max length 12
ismissingcatandmouse() # too long!
ismissingcat_andmouse() # 12 _ 8 OK!
# always fill the chunks as much as possible but respect words boundaries
ismissingcatandmousewithcheese() # too long!
ismissingcat_andmousewithcheese() # too long!
ismissingcat_andmousewith_cheese() # 12 _ 12 _ 6 OK!
