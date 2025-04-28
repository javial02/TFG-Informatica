create or replace table t(a int primary key, b int);
create or replace table s(c int primary key, d int);

select distinct a from t;
select distinct * from t,s where t.a=s.c;
select distinct t.a,s.d from t,s where t.a=s.c;