Implements the formal grammar in "Learning Language Games through Interaction" by Sida I. Wang, Percy Liang, and Chris Manning.

Includes hardcoded translation to natural language.

```
Add (With Cyan) Orange
Add orange to cyan blocks.
OOO
CCCBR
XXXXX

Remove (With Brown)
Remove brown blocks.
OOO
CCC R
XXXXX

Add (Leftmost (With Orange)) Red
Add red to leftmost orange blocks.
R
OOO
CCC R
XXXXX

Add All Brown
Add brown to all blocks.
B
RBB
OOO B
CCCBR
XXXXX

Remove (Leftmost All)
Remove leftmost all blocks.
RBB
OOO B
CCCBR
XXXXX

Add (Diff (With Brown) (Rightmost All)) Orange
Add orange to brown blocks that are not rightmost all blocks.
 OO
RBB
OOOOB
CCCBR
XXXXX
```

