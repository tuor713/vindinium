Vindinium Clojure bot (powering malvert)

## Strategies

The agent uses a heuristic approach combining a couple of weighted behavioural traits:
* _Breadth first search movement_ A 7-ply search tree to determine suitable near term options
* _Tavern finder_ Kicking in at life <= 35 and adjacent to a tavern, the trait weighs the path to the nearest tavern
* _Mine finder_ Kicking in at life >= 35, the trait weighs the path to the nearest conquerable mine
* _Combat mode_ When a hero is less than three moves away a strongly weighted combat mode takes over to avoid being caught in the pos+1 space and also take advantage of opponents that can be killed easily. Rules get a bit more complex to take into account adjacency to taverns, which can completely change combat math over multiple turns.
