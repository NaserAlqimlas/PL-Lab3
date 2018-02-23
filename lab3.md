# Write Up Questions
### Question 2
####
(a)

    i = 1
    func(i) {i} (2) 
    print i

the following would print 1 under static scoping and 2 under dynamic scoping. They have different results since the environment changes when you call the function 
while evaluating using big steps instead of being subsituted when evaluating with small steps.


####

### Question 4

#### 
The evaluation order for e1+e2 is left to right. To change said order, the search rule in the small step to evaluate to e1 and not e2
####

### Question 5

####
(a) an example where short circuit evaluation is useful would be 

    if (x != NULL && x.getFoo() ! = 10) {
      //do something
    }
  
  x.getFoo() would normally throw an exception if x was nuill but since the expression uses short circuit evaluation if x != NULL then x.getFoo() will never happen,
  thus we do not get said exception. 
  
  (b) Yes, e1 && e2 short circuits, since there exists a search rule that deals with the case of e1 being false. 
