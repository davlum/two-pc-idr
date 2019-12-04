#include <libpq-fe.h>

PGconn *connect(const char *connstr);
char *dbErrorMessage(PGconn *ptr);
char *resultErrorMessage(const PGresult *res);
void dbClose(PGconn *ptr);
PGresult *exec(PGconn *ptr, char *q);
void clearResult(PGresult *res);

