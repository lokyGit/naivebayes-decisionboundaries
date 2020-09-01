# naivebayes-decisionboundaries
Naïve Bayes is a popular classification algorithm which is preferably used while dealing with datasets with categorical features. It can also be used with continuous variables but preferred when the inputs are categorical. Naïve Bayes works on an assumption that the existence of a feature in a particular class is independent of the existence of other features in all the other classes. With such assumptions made, this algorithm is referred to as Naïve. For the continuous datapoints it makes an assumption that the data is normally distributed. Naïve Bayes algorithm can outperform many other classification algorithms when working with huge datasets because of its simple functioning.  The below equation is a simple Bayesian inference for finding out the posterior probabilities for winning the blackjack when getting an Ace card.  P(A) = the probability of getting an Ace card P(BJ) = the probability of winning blackjack  P(A | BJ) = P(BJ | A)*P(A)/P(BJ)  Where, P(A | BJ) is the posterior probability P(BJ | A) is the likelihood P(A) is class prior probability P(BJ) is predictor prior probability.