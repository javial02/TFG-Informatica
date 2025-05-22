create or replace table t(a int, b int);
create or replace table s(a int, b int);

select a, b from t group by a, b;
select a, b from s group by a, b;
select a, b from t where a > 10 group by a, b;
select a from t group by a;


select a, b from t where a > 10;
select a, b from s where b < 20;
select distinct a, b from t;
select distinct a, b from s where a > 5;
select a from t group by a having count(*) > 1;

