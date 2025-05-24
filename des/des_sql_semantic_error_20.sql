create or replace table t(a int, b int);

select count(a) from t where a = 3 group by a;
select 1 from t where a = 3 group by a;

select a from t where a = all (select b from t) group by a;  
select a from t where a = (select b from t) group by a;      
 


