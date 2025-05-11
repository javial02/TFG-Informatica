-- Crear tabla j
CREATE TABLE j(a INT, b INT);

-- Ejemplo inválido
SELECT a FROM j WHERE a = b GROUP BY a, b;

-- Ejemplo positivo
SELECT a FROM j WHERE a = b GROUP BY a, b HAVING b = 1;

-- Crear tabla t con dependencia funcional b determinado por a
CREATE OR REPLACE TABLE t(a INT PRIMARY KEY, b STRING DETERMINED BY a);

-- Ejemplo inválido
SELECT a FROM t GROUP BY a, b;

-- Ejemplo positivo
SELECT a, b FROM t GROUP BY a, b;

-- Crear tabla t con dependencias funcionales encadenadas
CREATE OR REPLACE TABLE t(a INT PRIMARY KEY, b STRING DETERMINED BY a, c INT DETERMINED BY b);

-- Caso 1
SELECT a FROM t GROUP BY c, a, b;

-- Caso 2
SELECT a FROM t WHERE a = c GROUP BY c, a, b;

-- Caso 3
SELECT a FROM t GROUP BY a, b HAVING b = 1;

-- Ejemplo positivo
SELECT a FROM t GROUP BY a, b HAVING a = 1;
