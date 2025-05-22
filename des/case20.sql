create or replace table t(a int, b int);

select a from t where a = all (select b from t) group by a;  
select a from t where a = (select b from t) group by a;      
select a from t where a = 3 group by a;                      
select a from t where a = (select min(b) from t) group by a; 

select a, count(*) from t group by a;          
select a, sum(b) from t group by a;            
select a, avg(b) from t group by a;            
select a, max(b) from t group by a;            
select a, min(b) from t group by a;            
select a from t where a in (select b from t) group by a;  

