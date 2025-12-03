function k=decayfun(t,p)
% decayfun The decay function
% Syntax k=decayfun(t,p)
% cheng-ta yang 2008.12.12.
a0=p(1);
a1=p(2);
tau=p(3);
k=a0 +a1.*exp(-t./tau);
k(k==Inf)=zeros(length(k(k==Inf)),1);
%if tau<0 | sigma<0,
%   k=zeros(length(k),1);
%end   
