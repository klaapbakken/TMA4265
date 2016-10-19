clear
clc

P = [0.5,0.35,0.15;0.1,0.75,0.15;0.05,0.6,0.35]

N = 100;
X = 2
states = [0,1,2];
lrd = [0,0,0];

for i = 1:N
    p = rand;
    if p < P(X + 1,1) 
        X = 0;
    elseif p < (P(X+1,1) + P(X+1,2))
        X = 1;
    else
        X = 2;
    end
    lrd(X+1) = lrd(X+1) + 1;
end
lrd = lrd/N*1997;
bar(states, lrd);
title('Long - run distribution')
xlabel('State')
ylabel('Time spent in state')



            
          
           
            