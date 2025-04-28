create or replace table t(a int, b int);
create or replace table s(a int primary key, b string determined by a);

select a from t where a = b order by a, b;
select a from s order by a, b;