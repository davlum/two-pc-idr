all : pq-idr.o pq-idr.so

pq-idr.o : pq-idr.c pq-idr.h
	cc -c pq-idr.c

pq-idr.so : pq-idr.c pq-idr.h
	cc -lpq -fPIC -o pq-idr.so -shared pq-idr.c
