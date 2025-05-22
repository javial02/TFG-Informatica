create table j(a int, b int);
select a from j where a = b group by a, b;

select a from j where a = b group by a, b having b = 1;

create or replace table t(a int primary key, b string determined by a);
select a from t group by a, b;

select a, b from t group by a, b;

create or replace table t(a int primary key, b int determined by a, c int determined by b);
select a from t group by c, a, b;
select a from t where a = c group by c, a, b;

select a from t group by a, b having b = 1;
