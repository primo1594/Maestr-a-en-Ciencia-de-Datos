/*ANDRÉS PAREDES 
J. SEBASTIÁN M. BENÍTEZ*/

/*Dividir la tabla PAYMENT en dos tablas. Una con los pagos del año 2005 y otra con los pagos de año 2006. Cada tabla tendrá los siguientes campos: customer_id, amount, payment_date.
-	La tabla PAYMENT2005 debe llenarse con los pagos de rentas cuyas películas tienen tiempos de renta > a 3 días
La tabla PAYMENT2006 debe llenarse con los pagos de rentas cuyas películas tienen tiempos de renta <= a 3 días
-	Una vez tengamos las dos tablas con sus correspondientes registros se debe crear una tabla temporal llamada PAYMENTS 2005_2006 con la unión entre ellas.*/
drop table PAYMENT2005;
create table PAYMENT2005 as
select p.customer_id, amount, payment_date
from payment p
join rental r on(r.rental_id=p.rental_id)
join inventory i on(i.inventory_id=r.inventory_id)
join film f on(f.film_id=i.film_id and f.rental_duration>3)
where year(p.payment_date)='2005';

drop table PAYMENT2006;
create table PAYMENT2006 as
select p.customer_id, amount, payment_date
from payment p
join rental r on(r.rental_id=p.rental_id)
join inventory i on(i.inventory_id=r.inventory_id)
join film f on(f.film_id=i.film_id and f.rental_duration<=3)
where year(p.payment_date)='2006';

drop table PAYMENTS2005_2006;
create table PAYMENTS2005_2006 as
select *
from PAYMENT2005
union
select *
from PAYMENT2006;

select count(*)
from PAYMENTS2005_2006;#12542
select count(*)
from PAYMENT2005;#12496
select count(*)
from PAYMENT2006;#46