create or replace table t (a int, b int, c int); 
create or replace table s (a int, b int, c int); 
create or replace table u (a int, b int); 

select * from t where exists (select 1 from s group by b);
select * from t where exists (select 1 from s where s.a = t.a group by s.b);
select * from t where exists (select 1 from s where s.a = t.a group by s.c);
select * from t where exists (select 1 from s where s.b = t.b group by s.a);
select * from t where exists (select 1 from s join u on u.a = s.a where t.b = s.b group by s.a);

select * from t where exists (select 1 from s where s.a = t.a group by s.a having count(*) > 1);
select a, count(*) from t group by a;