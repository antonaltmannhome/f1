Thanks for your data Noriaki. I had thought about doing this sort of analysis before, the problem of course is getting the data. Still, working with what you've gathered I thought I'd check something. Obviously some drivers get involved in accidents more than others throughout a season, but does that mean they are genuinely more accident prone? Could it just a case that they are all actually equally accident prone but the way luck plays out, sometimes you have a clean weekend, sometimes you don't. For example, Vettel didn't have an accident in France 2019 but did have an accident in Great Britain 2019. Does that mean he wasn't an accident prone driver in France but was in Great Britain? Obviously not, he was the same driver in both races but he had worse luck in Great Britain than in France. On the other hand, if a driver has 20 accidents in one season but zero the next season, it seems a bit unlikely that that is solely due to luck. So, how many accidents does a driver have to have before we can't dismiss it as bad luck?

So, taking your data, I've counted a total of 103 incidents on your graph. Doing a quick totting up of the numebr of races each season, I get a total of 560 driver/races in total. Therefore, the probability of a crash on any given race is about 103/560 = 0.184. So the probability of have 0 accidents throughout a 20 race season is (1 - 0.184)^20 = 0.017. It's a little more complicated to get the calculations for all the other numbers but using the binomial formula, this is the probability of having from 0 to 10 accidents throughout a 20 race season:

 0 0.017131807
 1 0.077261091
 2 0.165505376
 3 0.223919038
 4 0.214589078
 5 0.154840746
 6 0.087287676
 7 0.039365030
 8 0.014424196
 9 0.004336686
10 0.001075668

So, what this is showing is, if you crash at the average race i.e 0.184 crashes every race, then the chance of being as accident prone as Vettel was in 2018 ie 7 or more accidents is 0.059 (I added the probabilities for 7 to 10 accidents)