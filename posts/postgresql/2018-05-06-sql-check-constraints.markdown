---
title: SQL CHECK constraints
tags: databases, postgres, snippet
---

`CHECK` constraints are a powerful way to control data stored in
a SQL database. Combined with choosing the appropriate type for we can enforce
that the data written adheres to our business rules.

<!--more-->

Taking PostgreSQL as our SQL database of choice, the [docs](https://www.postgresql.org/docs/9.6/static/ddl-constraints.html)
show some basic examples, i.e.

``` sql
CREATE TABLE products (
    product_no integer,
    name text,
    price numeric CHECK (price > 0),
    discounted_price numeric CHECK (discounted_price > 0),
    CHECK (price > discounted_price)
);
```

Here the schema imposes three different but related constraints, namely
that the price and discounted price of a product must not be negative
amounts if they exist and that the discounted price must be less than
the normal price. The docs stop here but recently I found myself wanting
to express something like: if the price is _less than an amount n_, there cannot
be a discount price. Essentially I wanted to express a _conditional_ constraint
on a column that depends on the value of another one.

There's no examples of `CHECK` constraints using [conditional expressions](https://www.postgresql.org/docs/9.6/static/functions-conditional.html#FUNCTIONS-CASE)
and I wasn't totally sure if the resulting expression would actually be a valid
SQL expression; it turns out it does and with hindsight it makes total sense.
Knowing that, we can express that business rule in a straightforward way:

``` sql
CREATE TABLE products (
    product_no integer,
    name text,
    price numeric CHECK (price > 0),
    discounted_price numeric CHECK (discounted_price > 0),
    CHECK (price > discounted_price),
    CONSTRAINT products_bargain_no_discount
    CHECK (CASE WHEN price < 1 THEN discounted_price IS NULL END)
);
```

Now if we try to insert a product that with a bargain and a discounted price,
the `INSERT` statement will be rejected:

```
postgres=# INSERT INTO products VALUES (1,'test',0.99,0.55);
ERROR:  new row for relation "products" violates check constraint "products_bargain_no_discount"
DETAIL:  Failing row contains (1, test, 0.99, 0.55).
```
