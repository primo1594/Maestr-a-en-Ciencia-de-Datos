/*1. Muestre los diferentes actores (first_name, last_name) que han participado en diferentes películas.*/
select distinct first_name, last_name
from actor a
where exists(select 'x'
from film_actor fa
where a.actor_id=fa.actor_id)
order by first_name, last_name;#199

/*2. Muestre los diferentes actores (first_name, last_name) que han participado en películas cuyo título tiene la palabra "fantasy" o "fantasia"*/
select distinct first_name, last_name
from actor a
where exists(select 'x'
from film_actor fa
join film f on(f.film_id=fa.film_id and f.title like '%fantas%')
where a.actor_id=fa.actor_id)
order by first_name, last_name;#16

/*3. Muestre cuántos actores están registrados en la BD Sakila*/
select count(*)
from actor;#200

/*4. Muestre por película (title), cuántos actores participaron por película*/
select title 'Titulo Película',count(fa.actor_id) 'Total de Actores' 
from film f
join film_actor fa on(f.film_id=fa.film_id)
group by title;

/*5. Muestre la lista de actores (first_name, last_name) que participaron por cada película (title)*/
select title 'Titulo Película',group_concat(concat(a.first_name,' ',a.last_name) order by a.first_name, a.last_name separator ', ') 'Actores'
from film f
join film_actor fa on(f.film_id=fa.film_id)
join actor a on(a.actor_id=fa.actor_id)
group by title;

/*6. Muestre cuántos pagos ha realizado un cliente (first_name, last_name) por año, entre el año 2005 y 2006*/
select c.first_name,c.last_name,tabla.contador 'Total Pagos', tabla.año
from(
select count(p.payment_id) contador,p.customer_id,year(p.payment_date) año
from payment p
group by p.customer_id,año) tabla
join customer c on(c.customer_id=tabla.customer_id)
where tabla.año between '2005' and '2006'
order by año, first_name, last_name;

select first_name, last_name, count(p.payment_id) "Total Pagos", YEAR(payment_date) año
from customer c 
join payment p on (c.customer_id = p.customer_id and year(p.payment_date) between '2005' and '2006')
group by first_name, last_name, año
order by año;
