create or replace table t(a int primary key, b int);
create or replace table s(c int primary key, d int);

select distinct a from t;
select distinct * from t,s where t.a=s.c;
select distinct t.a,s.d from t,s where t.a=s.c;

create or replace table t(a int primary key);
select distinct a from t group by a;

create or replace table t(a int, b int);
select distinct a,b from t group by a,b;
    
create or replace table t(a int candidate key, b int, c int);
select distinct c from t where a = b and b = c;