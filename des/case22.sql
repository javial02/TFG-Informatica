CREATE TABLE T(A INT, B INT);
CREATE TABLE S(A INT, B INT);

SELECT A, B FROM T GROUP BY A, B;    -- GROUP BY innecesario
SELECT A, B FROM S GROUP BY A, B;    -- GROUP BY innecesario
SELECT A, B FROM T WHERE A > 10 GROUP BY A, B;  -- GROUP BY innecesario
SELECT A FROM T GROUP BY A;          -- GROUP BY innecesario
SELECT DISTINCT A, B FROM T GROUP BY A, B;  -- Reemplazable por SELECT DISTINCT

SELECT A, B FROM T WHERE A > 10;          -- No hay necesidad de GROUP BY, ya que no es una agregación
SELECT A, B FROM S WHERE B < 20;          -- No hay necesidad de GROUP BY, ya que no es una agregación
SELECT DISTINCT A, B FROM T;              -- Usar DISTINCT es correcto aquí en lugar de GROUP BY
SELECT DISTINCT A, B FROM S WHERE A > 5;  -- Usar DISTINCT es correcto
SELECT A FROM T GROUP BY A HAVING COUNT(*) > 1;  -- El GROUP BY es necesario porque hay una función de agregación
