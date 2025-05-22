create table t(a int primary key, b int);
create table s(a int, b int, primary key (a, b));
create or replace table t(a int candidate key, b int);
create or replace table s(a int, b int, candidate key (a, b));
create or replace table t(a int primary key, b string determined by a);

select a from t group by a;
select a, b from s group by a, b;
select a from t group by a;
select a, b from s group by a, b;
select a, b from t group by a, b;
select a, count(*) from t group by a;  
select a, max(b) from t group by a;   

select a, count(*) from u group by a;  -- sin restricciones, puede haber múltiples por grupo
select a, max(b) from u group by a;    -- no hay garantía de unicidad
select b, count(*) from t group by b;  -- b no es único, se justifica agrupar
select c, sum(b) from u group by c;    -- agrupación válida

create or replace table t(a int primary key, b string determined by a, c int determined by b);
select a, b from t group by a, b, c;

create or replace table t(a int, b int determined by a);
select a, b from t group by a, b;

create or replace table t(a int primary key, b int)
select a, b from t where a = b group by a, b;

