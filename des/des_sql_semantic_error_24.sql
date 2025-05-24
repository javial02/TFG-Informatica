create or replace table t(a int, b int);
select a from t where a = b order by a, b;

create or replace table t(a int primary key, b int determined by a, c int determined by b);
select a from t order by a, b;

create or replace table t(a int primary key, b int);
select a from t order by a, b;

create or replace table t(a int candidate key, b int);
select a from t order by a, b;
