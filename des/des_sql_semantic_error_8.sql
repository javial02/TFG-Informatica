create table department (deptno char(3) primary key, deptname varchar(36) not null); 
create table employee (empno char(6) primary key, firstname varchar(12), lastname varchar(15) not null, workdept char(3), foreign key (workdept) references department(deptno));

select deptno, deptname from department left join employee on workdept = deptno where lastname is null order by deptno;
select deptno, deptname from department right join employee on workdept = deptno where deptname is null order by deptno;
select deptno, deptname from department full join employee on workdept = deptno where deptname is null order by deptno;