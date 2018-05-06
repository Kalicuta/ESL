SAheart = read.csv("Data/SAheart.data", header = TRUE)

m = glm( chd ~ sbp + tobacco + ldl + famhist + obesity + alcohol + age, family=binomial(), data=SAheart )
print( summary(m), digits=2 )


# The largest and smallest model we would consider (all possible variables):
# 
m_Largest = glm( chd ~ sbp + tobacco + ldl + famhist + obesity + alcohol + age, family=binomial(), data=SAheart )
m_Smallest = glm( chd ~ 1.0, family=binomial(), data=SAheart )

stepped_model = step( m_Largest, scope=list(lower=~+1, upper=~sbp + tobacco + ldl + famhist + obesity + alcohol + age), 
                      direction="backward", data=SAheart )

print( stepped_model )