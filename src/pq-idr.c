#include "pq-idr.h"

PGconn *connect(const char *connstr) {
  return PQconnectdb(connstr);
}

char *dbErrorMessage(PGconn *ptr) {
  return PQerrorMessage(ptr);
}

char *resultErrorMessage(const PGresult *res) {
  return PQresultErrorMessage(res);
}

void dbClose(PGconn *ptr) {
  return PQfinish(ptr);
}

PGresult *exec(PGconn *ptr, char *q) {
  return PQexec(ptr, q);
}

void clearResult(PGresult *res) {
  return PQclear(res);
}
