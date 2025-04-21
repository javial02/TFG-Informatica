CREATE OR REPLACE TABLE T(A INT, B INT);

SELECT A FROM T WHERE A = ALL (SELECT B FROM T) GROUP BY A;  -- ALL devuelve solo un valor, el GROUP BY es innecesario
SELECT A FROM T WHERE A = (SELECT B FROM T) GROUP BY A;      -- Subconsulta devuelve un solo valor, el GROUP BY es innecesario
SELECT A FROM T WHERE A = 3 GROUP BY A;                        -- Comparación constante, el GROUP BY es innecesario
SELECT A FROM T WHERE A = (SELECT MIN(B) FROM T) GROUP BY A;  -- MIN(B) devuelve un solo valor, el GROUP BY es innecesario

SELECT A, COUNT(*) FROM T GROUP BY A;          -- Agrupación por A, válido
SELECT A, SUM(B) FROM T GROUP BY A;            -- Agrupación por A, válido
SELECT A, AVG(B) FROM T GROUP BY A;            -- Agrupación por A, válido
SELECT A, MAX(B) FROM T GROUP BY A;            -- Agrupación por A, válido
SELECT A, MIN(B) FROM T GROUP BY A;            -- Agrupación por A, válido
SELECT A FROM T WHERE A IN (SELECT B FROM T) GROUP BY A;  -- Aquí se puede justificar si estamos agrupando por A
