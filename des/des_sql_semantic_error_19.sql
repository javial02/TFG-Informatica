create or replace table s(a int, b int, primary key (a, b));
create or replace table t(a int primary key, b string determined by a);

select a from t group by a;
select a, b from s group by a, b;
select a, b from t group by a, b;
select a, count(*) from t group by a;  
select a, max(b) from t group by a;     

create or replace table t(a int primary key, b string determined by a, c int determined by b);
select a, b from t group by a, b, c;

create or replace table t(a int, b int determined by a);
select a, b from t group by a, b;

create or replace table t(a int primary key, b int)
select a, b from t where a = b group by a, b;

