/* Copyright (c) 2004, 2008, Oracle. All rights reserved.  */

/*

   NAME
     strmmon.c - STReams MONitor : used for Streams diagnostics

   DESCRIPTION
     used for Streams diagnostics

   NOTES
     Serves as an example for simple queries to monitor Streams performance.

   MODIFIED   (MM/DD/YY)
   snalla      06/06/08 - turn off domain controller authentication
   rmao        09/26/07 - bug 6367892, add event message of knlslmw
   snalla      09/07/07 - lrglrf1 3095832 do not include unistd.h on win32
                          platform - adding changes reverted from bpwang_lrg-1938240
   tianli      02/27/07 - lrg 2858006: variable "status" fix
   yizhang     08/16/06 - Fix bug 5382689
   yizhang     04/07/06 - Fix bug 5118340. 
   yizhang     09/29/05 - Collect the time used in wait events for all stream 
                          related processes. 
   narora      09/13/05 - code hygiene
   narora      08/12/05 - add propagation bandwidth 
   narora      04/27/05 - bug 4334913: fix LOG stats 
   narora      03/31/05 - print memsize with M acronym 
   narora      03/18/05 - print streams pool size repeatedly 
   narora      03/14/05 - bug 4204079: add destination to check for old prop 
   narora      02/04/05 - bug 4164092: -sysdba option shouldn't coredump 
   sbalaram    10/06/04 - indicate backlogs, flow control
   sbalaram    08/17/04 - sbalaram_streams_diagnostics
   sbalaram    07/26/04 - enhancements
   narora      11/18/03 - fix flushing to file
   narora      09/12/03 - handle errors if the anydata queue doesn't have
                          any buffered enqueues
   narora      09/12/03 -

*/

/*****************************************************************************

               STRMMON - Streams Monitoring Program
               ------------------------------------

Overview:
--------
STRMMON is a monitoring tool focused on Oracle Streams.  Using this
tool, database administrators get a quick overview of the Streams
activity occurring within a database. In a single line display, strmmon
reports information. The reporting interval and number of iterations to
display are configurable.

Usage:
-----
There are 7 command line input parameters for STRMMON:  interval, count,
user, passw, dbname, sysdba, long. The first 2 parameters (interval and count)
control the sampling rate and the amount of output.  The next 4
parameters specify the connect information to the particular Streams
database. Specifying the last parameter displays cumulative information
rather than rate information, which is printed by default.

When the command "strmmon" is issued without any parameters, a usage
message is displayed:

% strmmon

Usage: strmmon -interval <seconds> -count <number> [-user <user name>]
               [-passw <password>] [-dbname <database name>] [-sysdba]
               [-long]

Parameters:
----------
-interval     The interval in seconds at which STRMMON will monitor the
              database. To specify that the sampling rate to be every 3
              seconds:
              -interval 3
              This is a required parameter for strmmon.

-count        The number of iterations to monitor the Streams environment.
              To specify 5 iterations, use the following:
              -count 5
              This is a required parameter for strmmon.

-user         The schema name for logging into the database. Any schema name
              can be specified. If the SYS schema is specified, additional
              information is displayed. To specify the SYSTEM schema, use
              -user SYSTEM
              This parameter should not be specified if logging in as
              "/ as sysdba" is desired.
              -user  is an optional parameter for strmmon.

-passw        The login password for the schema identified with the -user
              clause. To specify the password for the SYSTEM schema, use
              -passw oracle
              This parameter should not be specified if logging in as
              "/ as sysdba" is desired.
              -passw is an optional parameter for strmmon.

-dbname       The connection information or service name from tnsnames.ora
              for the specific database to be monitored. To specify the connect
              information for the monitored database, use
              -dbname ORCL.WORLD
              This is an optional parameter for strmmon.

-sysdba       This flag indicates that the login role is SYSDBA. This optional
              parameter is typically used with the SYS schema. To specify the
              login role SYSDBA, use
              -sysdba
              When logging in as "/ as sysdba",  the -user and -passw
              parameters are not required.

-long         Indication to print a detailed report for capture, apply
              and propagation.
              -long is an optional parameter.
              By default, only the capture, apply and propagation rates are
              displayed.


Output:
------
The strmmon output begins with a banner line identifying the program parameters
and database. This information is followed with a brief description of the
major components of the output display. An example of this identifying output
is shown below:

% strmmon -interval 5 -count 5 -sysdba

STREAMS Monitor, v 2.5  Copyright Oracle Corp. 2002, 2005.
Interval = 5, Count=5
 
Logon= @ ORACLE 11.1.0.2.0
 
Streams Pool Size = 152M
 
LOG : <redo generated per sec>
NET: <client bytes per sec> <dblink bytes per sec>
Cxxx: <lcrs captured per sec> <lcrs enqueued per sec> <capture latency>
MEM : <percent of memory used> % <streams pool size>
PRxx: <messages received per sec>
Qx  : <msgs enqueued per sec> <msgs spilled per sec>
PSxx: <lcrs propagated per sec> <bytes propaged per sec>
Axxx: <lcrs applied per sec> <txns applied per sec> <dequeue latency>
<F>: flow control in effect
<B>: potential bottleneck
AR: apply reader
AS(n): n number of apply server
<x%I x%F x%xx>: <idle wait events percentage> <flow control wait events percentage> <other wait event percentage and name>
xx->: database instance name
 
The following example shows the identifying output for "-long" option.

% strmmon -long -interval 5 -count 5 -sysdba

STREAMS Monitor, v 2.5  Copyright Oracle Corp. 2002, 2005.
Interval = 5, Count=5
 
Logon= @ ORACLE 11.1.0.2.0
 
Streams Pool Size = 152M
 
LOG : <last write scn> <redo blocks written>
NET: <client bytes> <dblink bytes>
Cxxx: <read scn> <msgs captured> <capture scn> <msgs enqueued> <enqueue scn> <capture latency>
MEM : <percent of memory used> % <streams pool size>
PRxx: <number received>/<total time>
Qx  : <outstanding mesgs>/<cumulative mesgs> <cumulative spilled>
PSxx: <number propagated> <total bytes>/<total time>
Axxx: <msgs deq'd> <dequeue scn> <dequeue latency> <txns recv'd> <txns assigned> <txns applied> <hwm scn> <hwm latency>
<F>: flow control in effect
<B>: potential bottleneck
AR: apply reader
AS(n): n number of apply servers
<x%I x%F x%xx>: <idle wait events percentage> <flow control wait events percentage> <other wait event percentage and name>
xx->: database instance name


Note: The information about the Streams Pool Size is displayed only for
database versions greater than or equal to 10gR1. The wait event percentages
for propagation receiver are displayed only for database version greater than
or equal to 10gR2. The wait event percentages for the rest components are
displayed only for database version greater than or equal to 10gR1.  


After this initial information about the program, Strmmon produces a single
line of output representing the current status of Oracle Streams after the
requested interval for each iteration . For example, if strmmon is invoked with
"-interval 5 -count 5", a line of output will be displayed every 5 seconds.
After 5 lines have been displayed (25 seconds), the monitoring will end.

Each line is composed of multiple blocks of information dependent on the
Streams processes configured within the database. These blocks are displayed
by a keyword to identify the component followed by the statistics for that
particular component. The separator between the components is the "|" symbol. 

There are 6 components for Streams:  LOG, Cxxx, Qx, PSxx, PRxx, Axxx. 
Except for the LOG component, multiple occurrences of each component are 
possible dependent on the streams processes configured at database.


LOG

Information about the redo log activity is written in this block. The statistic
following the LOG: keyword gives the redo generated per second. If the "-long"
option is specified, then the first statistic following the LOG: keyword is the
current SCN that has been written to the redo log. This number represents the
current activity within the database. If this number does not increase, no
activity is occurring on the database. The second statistic is the last block
number written in the redo log. Redo blocks are always 512 bytes, so this
statistic can be used to calculate the amount of redo generated between
intervals. The output for the LOG component is always the first entry after the
timestamp on the display and appears as follows:

2006-10-27 11:48:55 | LOG 440569 325388
2006-10-27 11:49:00 | LOG 440609 325389
2006-10-27 11:49:05 | LOG 440650 325391
2006-10-27 11:49:10 | LOG 440691 325393
2006-10-27 11:49:15 | LOG 440732 325395

In the above example, the current scn written to the redo log is 440569 and
the last block number is 325388. Since the strmmon command was issued with
-interval 5 -count 5, 5 lines of output are displayed with a 5 second interval
between them. 


Cxxx

For each capture process configured in the database, a separate block will be
displayed. Each block displays the number of lcrs captured per sec, number of
lcrs enqueued per sec and the capture latency. If "-long" option is specified,
then in each block, the Logminer read scn, the total number of messages
captured from the redo log and the most recent scn captured from the redo log
are shown. In addition, the number of messages that match the rules specified
for the capture process including the most recent message scn enqueued are
shown along with the capture latency. 

One can also use the difference between successive capture "messages captured"
statistics to determine the rate at which capture is mining the redo log. The
enqueue scn of capture is an indicator of where the capture process will
restart, if capture is stopped and restarted while the database is running.
This statistic can also be used for comparison with the appropriate database
Apply process high-water mark scn.  If these statistics match, the capture and
apply are caught up and the data is synchronized.

It lists the percentage of idle events, flow control events, and one of the 
most significant wait event for the capture process if the database version 
is 10gR1 or higher.

MEM

If strmmon is run from the SYS schema connected as SYSDBA, and the
database version is 10gR1 or higher, then this displays the percentage
of Streams Pool memory currently in use and the total size of the
streams pool.


Qx

For each streams queue in the database, a seperate block displays the
queue identifier, the cummulative message rate and the spill rate. If
"-long" option is specified, then for each streams queue in the
database, a separate block will display the queue identifier as well
as the number of outstanding messages in the buffered queue, the
cumulative number of messages that have been in the queue and the
number of messages spilled from memory to disk. In version 9iR2, the
number of messages currently spilled is displayed. In 10gR1, the
cumulative number of messages spilled is displayed. For 9iR2, the
default and the "-long" option display the same data.

The queue identifier (QID) can be used to identify the name of the
queue. Use the QID in queries against the DBA_QUEUES view to identify
the particular queue in the database. In 9i, the number of outstanding
messages in the buffered queue is only displayed if strmmon is run
from the SYS schema as SYSDBA. In 9iR2, if the number of spilled
messages becomes non-zero, consider stopping capture temporarily to
slow down the flow of data.

PSxx

For each propagation sender, the number of lcrs propagated per sec is 
displayed. If "-long" option is specified, then for each propagation, 
the total number of messages and the total number of bytes propagated to 
the destination site and the total time needed to propagate those messages 
is displayed.

It lists the percentage of idle events, flow control events, and one of the 
most significant wait event for the propagation sender process if the 
database version is 10gR2 or higher.

PRxx

For each propagation receiver, the number of messages received per sec is 
displayed. If "-long" option is specified, then for each propagation, 
the total number of messages received and the total time needed to receive 
those messages is displayed.

It lists the percentage of idle events, flow control events, and one of the 
most significant wait event for the propagation receiver process if the 
database version is 10gR1 or higher.

Axxx

For each apply process, the number of lcrs dequeued per sec and the dequeue
latency are displayed. If the "-long" option is specified, then for each
apply process in the database, in addition to the total messages dequeued
and the most recent scn dequeued by the apply reader, the dequeue latency, the
total number of transactions received, assigned and applied by the coordinator
are displayed. The apply high water mark scn along with the apply high water
mark latency is also shown. This statistic records the most recent scn from
the source site that has been applied at the destination site.

It lists the percentage of idle events, flow control events, and one of the 
most significant wait event for the apply reader process and the apply server
processes if the database version is 10gR1 or higher. 

<F>

This indicates that the capture process is blocked due to flow control.

<B>

This indicates that the capture or apply process is currently a bottleneck.

xx->
 
This indicates the name of the database instance for which the data follows.


Installation:
------------
Before compiling and linking the strmmon program, make sure that the
ORACLE_HOME and LD_LIBRARY_PATH environmental variables are set up
appropriately. First delete any old executable, and then compile and link:

    rm -f strmmon strmmon.o; make -f demo_rdbms.mk strmmon

After the program has been compiled and linked, strmmon can be used for
monitoring a Streams environment.

******************************************************************************/

/* for WINDOWS compatibility of 'sleep' call */
#if defined(WIN32COMMON) || defined(WIN32) || defined(_WIN32) 
#include <windows.h>
#define sleep(x) Sleep(1000*(x))
#endif

/* for WINDOWS compatibility of 'strncasecmp' call */
#if defined(WIN32COMMON) || defined(WIN32) || defined(_WIN32) 
#define strncasecmp strnicmp 
#endif

#ifndef ORATYPES
#include <oratypes.h>
#endif

#ifndef _STDIO_H
#include <stdio.h>
#endif

#ifndef _STDLIB_H
#include <stdlib.h>
#endif

#ifndef _STRING_H
#include <string.h>
#endif

#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif

#ifndef _TIME_H
#include <time.h>
#endif

#if !(defined(WIN32COMMON) || defined(WIN32) || defined(_WIN32)) 
#ifndef _UNISTD_H
#include <unistd.h>
#endif
#endif 

#ifndef _SIGNAL_H
#include <signal.h>
#endif

#ifndef OCI_ORACLE
#include <oci.h>
#endif

#define VERSION_9iR2    92020
#define VERSION_10g    101010
#define VERSION_10gR2  102000

typedef enum process_type 
  {CAPTURE_PROCESS=1, PR_SENDER_PROCESS=2, 
   PR_RECEIVER_PROCESS=3, APPLY_READER=4, APPLY_SERVER=5}
  process_type_t;

/*STREAMS capture process waiting for archive log 
 *and wait for transaction are the idle events for 10.1 version
 */
static const char* capture_idle_events = 
  {"|LogMiner: client waiting for transaction||events in waitclass Other||Streams capture: waiting for archive log||wait for transaction||STREAMS capture process waiting for archive log|"};
   /*"LogMiner: wakeup event for builder",
     "LogMiner: wakeup event for preparer" */

/* waiting for subscribers to catch up is the flowcontrol event for 10.1 
 * version. Rest events are for 10.2 and above. */
static const char* capture_flowcontrol_events =
  {"|Streams capture: waiting for subscribers to catch up||Streams capture: resolve low memory condition||Streams: resolve low memory condition||waiting for subscribers to catch up|"};
  
static const char* propagation_sender_idle_events = 
  {"|wait for unread message on broadcast channel||jobq slave wait|"};

static const char* propagation_sender_flowcontrol_events =
  {"|SQL*Net more data to dblink||SQL*Net message from dblink|"};

static const char* propagation_receiver_idle_events =
  {"|SQL*Net more data from client||SQL*Net message from client||Streams AQ: qmn slave idle wait|"};

static const char* propagation_receiver_flowcontrol_events =
  { "|Streams AQ: enqueue blocked on low memory||Streams AQ: enqueue blocked due to flow control|"};

/*queue messages & knlqdeq is the idle wait events for 10.1 version*/
static const char* apply_reader_idle_events = 
  {"|Streams AQ: waiting for messages in the queue||queue messages||knlqdeq|"};

static const char* apply_reader_flowcontrol_events = 
  {"|rdbms ipc message|"};

static const char* apply_server_idle_events = 
  {"|rdbms ipc message|"};

static const char* apply_server_flowcontrol_events = 
  {"||"};

typedef struct options
{
  ub4       interval;
  ub4       elapstim; /* The elape time in one-hundredth of second */
  ub4       count;
  ub4       iters;
  ub1       short_opt;
} options_t;

typedef struct oci
{
  OCIEnv    * envp;
  OCIError  * errp;
  OCIServer * srvp;
  OCISvcCtx * svcp;
  OCISession * authp;
  OCIStmt   * stmtp;
  OCIStmt   * stmt2p;
  OCIStmt   * stmt3p;
  struct      oci *nxt_oci;
} oci_t;

typedef struct sid_pval
{
  ub4             sid;
  void          * pval;
  process_type_t  type;
  struct          sid_pval *next;
} sid_pval_t;

typedef struct event_pval
{
  /* For apply server, it contains the number of apply server process. 
     For other process, it is the sid of that process. */ 
  ub4        sidnum; 
  oratext    event_name[128];
  ub2        event_namelen;
  ub4        iters;
  ub8        time_in_micro_second;
  ub8        time_in_micro_second_old;
  struct     event_pval *next_event;
} event_pval_t;

typedef struct cap_pval
{
  ub4       capturenum;
  oratext   capturename[128];
  ub2       capturenamelen;
  OCINumber read_scn;
  ub4       total_captured;
  ub4       total_enqueued;
  ub4       elapsed_pause_time;
  boolean   flow_ctrl;
  struct    event_pval *pevent;
  struct    cap_pval *next_cap;
} cap_pval_t;

typedef struct app_pval
{
  ub4       applynum;
  oratext   applyname[128];
  ub2       applynamelen;
  ub4       total_applied;
  ub4       txns_applied;
  struct    event_pval *pevent_reader;
  struct    event_pval *pevent_server;
  struct    app_pval *next_app;
  ub4       server_num;
} app_pval_t;

typedef struct prop_pval
{
  ub4       src_queueid;
  ub4       dst_queueid;
  oratext   src_queuename[30];
  ub2       src_queuenamelen;
  oratext   src_schemaname[30];
  ub2       src_schemanamelen;
  oratext   src_dbname[128];
  ub2       src_dbnamelen;
  oratext   dst_queuename[30];
  ub2       dst_queuenamelen;
  oratext   dst_schemaname[30];
  ub2       dst_schemanamelen;
  oratext   dst_dbname[128];
  ub2       dst_dbnamelen;
  char     *destination[128];
  ub2       destination_len;
  ub4       total_number;
  ub8       total_bytes;
  struct    event_pval *pevent_sender;
  struct    event_pval *pevent_receiver;
  struct    prop_pval *next_prop;
} prop_pval_t;

typedef struct que_pval
{
  ub4       queueid;
  ub4       cnummsgs;
  ub4       cspillmsgs;
  struct    que_pval *next_que;
} que_pval_t;

typedef struct connection
{
  oratext * user;
  ub4       userlen;
  oratext * passw;
  ub4       passwlen;
  oratext * dbname;
  ub4       dbnamelen;
  oratext   instname[128];
  ub2       instnamelen;
  oratext   versionstr[20];
  ub2       versionstrlen;
  ub4       version;
  ub1       sysdba;
  ub1       sysuser;

  struct    connection * next_connection;

  oci_t        *ocip;
  cap_pval_t   *cap_pval;
  app_pval_t   *app_pval;
  que_pval_t   *que_pval;
  prop_pval_t  *prop_pval;
  sid_pval_t   *sid_pval;
  ub4           num_sid;
  ub4           org_sid;
  ub4           lgwr_pval;
  ub8           client_bytes;
  ub8           dblink_bytes;
  char          sidstr[2048];
} connection_t;

typedef struct queue
{
  ub4 queueid;
  oratext queuename[128];
  ub2 queuenamelen;
  oratext schemaname[128];
  ub2 schemanamelen;
  ub4 cnummsgs;
  ub4 cspillmsgs;
  ub4 nummsgs;
} queue_t;

#define OCICALL(ocip, function) do {\
sword status=function;\
if (OCI_SUCCESS==status) break;\
else if (OCI_SUCCESS_WITH_INFO==status) \
{puts((char *)"Error: OCI_SUCCESS_WITH_INFO");\
exit(1);}\
else if (OCI_NEED_DATA==status) \
{puts((char *)"Error: OCI_NEED_DATA");\
exit(1);}\
else if (OCI_NO_DATA==status) \
{puts((char *)"Error: OCI_NO_DATA");\
exit(1);}\
else if (OCI_ERROR==status) \
{ocierror(ocip, (char *)"OCI_ERROR", TRUE);\
exit(1);}\
else if (OCI_INVALID_HANDLE==status) \
{puts((char *)"Error: OCI_INVALID_HANDLE");\
exit(1);}\
else if (OCI_STILL_EXECUTING==status) \
{puts((char *)"Error: OCI_STILL_EXECUTING");\
exit(1);}\
else if (OCI_CONTINUE==status) \
{puts((char *)"Error: OCI_CONTINUE");\
exit(1);}\
else {printf("Error: unknown status %d\n", status);\
exit(1);}\
} while(0)


static void get_options(options_t * opts, connection_t ** first_conn, int argc,
            char ** argv);
static void print_usage(int exitcode);
static void oraconnect(options_t * opts, connection_t * connp);
static void oradisconnect(oci_t * ocip);
static void ocierror(oci_t * ocip, char * msg, boolean stop_on_error);
static void print_header(options_t * opts, connection_t * connp);
static void print_stats(options_t * opts, connection_t * connp);
static void print_capture_stats(options_t * opts, connection_t * connp,
            oci_t * ocip, cap_pval_t ** cap_pval);
static void print_short_capture_stats(options_t * opts,
            connection_t * connp, oci_t * ocip, cap_pval_t ** cap_pval);
static void print_queue9i_stats(options_t * opts, connection_t * connp,
            oci_t * ocip, prop_pval_t ** prop_pval);
static void print_queue_stats(options_t * opts, connection_t * connp,
            oci_t * ocip, que_pval_t ** que_pval, prop_pval_t ** prop_pval);
static void print_prop_receiver_stats(options_t * opts, connection_t * connp,
                                      oci_t * ocip, queue_t* que,
                                      prop_pval_t ** prop_pval,
                                      unsigned *pnum_ptr);
static void print_prop_sender_stats(options_t * opts, connection_t * connp,
                                    oci_t * ocip, ub4 queueid, 
                                    prop_pval_t ** prop_pval,
                                    unsigned *pnum_ptr);
static void print_apply_stats(options_t * opts,  connection_t * connp,
            oci_t * ocip, app_pval_t ** app_pval);
static void print_short_apply_stats(options_t * opts, connection_t * connp,
            oci_t * ocip, app_pval_t ** app_pval);
static void get_version(connection_t * connp);
static void print_log_stats(options_t * opts, oci_t * ocip, ub4 *lgwr_pval);
static void print_net_stats(options_t * opts, connection_t * connp);
static void print_pool_size(oci_t * ocip);
static void print_mem_stats(options_t * opts, oci_t * ocip);
static void print_latency(sb4 latency);
static void print_bytes(ub8 bytes);
static void collect_event_data (options_t * opts, connection_t * connp);
static void print_event_stats(options_t * opts, void* pval, process_type_t type);

int main(int argc, char ** argv)
{
  options_t     opts;
  connection_t *first_conn = (connection_t *)NULL;
  connection_t *cur_conn;
  time_t        ptim = 0;
  time_t        tim;
  struct tm *   ltime;
  
  /* store the command line arguments */
  get_options(&opts, &first_conn, argc, argv);

  /* initialize each connection */
  for (cur_conn = first_conn;
       cur_conn != (connection_t *)NULL;
       cur_conn = cur_conn->next_connection)
  {
    /* connect to the database */
    oraconnect(&opts, cur_conn);

    /* get the version of ORACLE for current database */
    get_version(cur_conn);

    /* print header for current database */
    print_header(&opts, cur_conn);

    /* Set the number of sids to 0 */
    cur_conn->num_sid = 0;
    cur_conn->org_sid = 0;
  }

  opts.elapstim = opts.interval * 100;
    
  for (opts.iters = 0; opts.iters < opts.count; opts.iters++)
  {
    if (opts.iters) {      
      sleep(opts.interval); 
    }
    
    tim   = time(NULL);
    ltime = localtime(&tim);

    /* first print the current time */
    printf("%d-%02d-%d %02d:%02d:%02d",
            (ltime->tm_year)+1900, (ltime->tm_mon)+1, ltime->tm_mday,
            ltime->tm_hour, ltime->tm_min, ltime->tm_sec);

    /* don't sleep in the first iteration */
    if (opts.iters) {      
      opts.elapstim = (ub4)(difftime(tim, ptim) * 100);
    }    
    
    ptim = tim;
    
    for (cur_conn = first_conn;
         cur_conn != (connection_t *)NULL;
         cur_conn = cur_conn->next_connection)
    {
      /* print stats for current database */
      printf(" || %.*s->", cur_conn->instnamelen, cur_conn->instname);
      print_stats(&opts, cur_conn);
    }

    /* flush the current line of output */
    fprintf(stdout, "\n");
    fflush(stdout);
  }

  for (cur_conn = first_conn;
       cur_conn != (connection_t *)NULL;
       cur_conn = cur_conn->next_connection)
  {
    oradisconnect(cur_conn->ocip);
  }

  return 0;
}

static void ocierror(oci_t * ocip, char * msg, boolean stop_on_error)
{
  sb4 errcode=0;
  text bufp[4096];

  if (ocip->errp)
  {
    OCIErrorGet((void *) ocip->errp, (ub4) 1, (text *) NULL, &errcode,
                bufp, (ub4) 4096, (ub4) OCI_HTYPE_ERROR);
    printf("%s: %s", msg, bufp);
  }
  else
    puts(msg);

  if (stop_on_error)
    exit(1);
}

static void get_options(options_t * opts, connection_t ** first_conn, int argc,
                        char ** argv)
{
  char * option;
  char * value;
  connection_t *cur_connection = NULL;
  connection_t *new_connection = NULL;
  connection_t *prev_connection = NULL;
  
  /* clear the options */
  memset(opts, 0, sizeof(*opts));

  /* The default option is the short option */
  opts->short_opt = 1;

  if (argc == 1)
  {
    print_usage(0);
  }

  while(--argc)
  {
    /* get the option name */
    argv++;
    option = *argv;

    /* check that the option begins with a "-" */
    if (!strncmp(option, (char *)"-", 1))
    {
      option ++;
    }
    else
    {
      printf("Error: bad argument %s\n", option);
      exit(1);
    }

    /* check if its a boolean option */
    if (!strncmp(option, (char *)"sysdba", 6))
    {
      if (!cur_connection)
      {
        /* allocate and clear a new connection options parameter */
        cur_connection = (connection_t *)malloc(sizeof(connection_t));
        memset(cur_connection, 0, sizeof(connection_t));
        /* for a blank username password a single byte user password still
         * needs to be given */
        cur_connection->user = (oratext *)" ";
        cur_connection->userlen = 1;
        cur_connection->passw = (oratext *)" ";
        cur_connection->passwlen = 1;
        
        if (*first_conn == (connection_t *) NULL)
        {
          *first_conn = cur_connection;
        }
        else
        {
          prev_connection->next_connection = cur_connection;
        }
      }
      cur_connection->sysdba = 1;
      /* sysdba is the last option for a connection */
      prev_connection = cur_connection;
      cur_connection = (connection_t *)0;
      
      continue;
    }

    if (!strncmp(option, (char *)"long", 4))
    {
      opts->short_opt = 0;
      continue;
    }

    /* get the value of the option */
    --argc;
    argv++;
    if (!argc)
    {
      printf("Error: option '%s' needs a value\n", option);
      exit(1);
    }
    value = *argv;    

    if (!strncmp(option, (char *)"int", 3))
    {
      opts->interval = (ub4) atol(value);
      if (!opts->interval)
      {
        printf("Error: interval must be +ve\n");
      }
    }
    else if (!strncmp(option, (char *)"count", 5))
    {
      opts->count = (ub4) atol(value);
      if (!opts->count)
      {
        printf("Error: count must be +ve\n");
      }
    }
    else if (!strncmp(option, (char *)"user", 4))
    {
      for (cur_connection = *first_conn;
           cur_connection != (connection_t *)NULL;
           cur_connection = cur_connection->next_connection)
        prev_connection = cur_connection;
      ;
      new_connection = (connection_t *)malloc(sizeof(connection_t));
      memset(new_connection, 0, sizeof(connection_t));
      new_connection->user = (oratext *)value;
      new_connection->userlen = strlen(value);
      new_connection->next_connection = (connection_t *)NULL;
      cur_connection = new_connection;

      if (*first_conn == (connection_t *) NULL)
      {
        *first_conn = new_connection;
      }
      else
      {
        prev_connection->next_connection = new_connection;
      }
    }
    else if (!strncmp(option, (char *)"passw", 5))
    {
      if (!cur_connection)
      {
        print_usage(1);
      }
      else
      {
        cur_connection->passw = (oratext *)value;
        cur_connection->passwlen = strlen(value);
      }
    }
    else if (!strncmp(option, (char *)"dbname", 6))
    {
      if (!cur_connection)
      {
        print_usage(1);
      }
      else
      {
        cur_connection->dbname = (oratext *)value;
        cur_connection->dbnamelen = strlen(value);
      }
    }
    else
    {
      printf("Error: unknown option %s\n", option);
      exit(1);
    }
  }

  if (!opts->interval || !opts->count)
  {
    print_usage(1);
  }

  /* for each connection set the "sysuser" flag if either
   * the username is SYS
   * or
   * the connection is made AS SYSDBA
   */
  for (cur_connection = *first_conn;
       cur_connection != (connection_t *)NULL;
       cur_connection = cur_connection->next_connection)
  {
    if (cur_connection->sysdba ||
        (!strncasecmp((char *)cur_connection->user,(char *)"SYS",3) &&
         (cur_connection->userlen==3)))
    {
      cur_connection->sysuser = 1;
    }
  }

  /*
  if (!opts->userlen)
  {
    opts->userlen = 1;
    opts->user=(oratext *)" ";
  }
  if (!opts->passwlen)
  {
    opts->passwlen = 1;
    opts->passw= (oratext *)" ";
  }
  */
}

static void print_usage(int exitcode)
{
  puts((char *)"Usage: strmmon -interval <seconds> -count <number>"
       " [-user <user name>]"
       "\n"
       "               [-passw <password>] [-dbname <database name>] [-sysdba]"
       "\n"
       "               [-long]"
       "\n");

  exit(exitcode);
}

static void oraconnect(options_t * opts, connection_t * connp)
{
  oci_t * ocip = connp->ocip = (oci_t *)malloc(sizeof(oci_t));
 
  if (OCIEnvCreate(&ocip->envp, OCI_OBJECT, (void *)0,
                   (void * (*)(void *, size_t)) 0,
                   (void * (*)(void *, void *, size_t))0,
                   (void (*)(void *, void *)) 0,
                   (size_t) 0, (void **) 0 ))
  {
    ocierror(ocip, (char *)"OCIEnvCreate() failed", TRUE);
  }

  if (OCIHandleAlloc((void *) ocip->envp, (void **) &ocip->errp,
                     (ub4) OCI_HTYPE_ERROR, (size_t) 0, (void **) 0))
  {
    ocierror(ocip, (char *)"OCIHandleAlloc(OCI_HTYPE_ERROR) failed", TRUE);
  }

  /* allocate the server handle */
  OCICALL(ocip,
          OCIHandleAlloc((void *) ocip->envp, (void **) &ocip->srvp,
                         OCI_HTYPE_SERVER, (size_t) 0, (void **) 0));

  /* create a server context */
  OCICALL(ocip,
          OCIServerAttach (ocip->srvp, ocip->errp, connp->dbname, 
                           (sb4)connp->dbnamelen, OCI_DEFAULT));

  /* allocate the service handle */
  OCICALL(ocip,
          OCIHandleAlloc((void *) ocip->envp, (void **) &ocip->svcp,
                         OCI_HTYPE_SVCCTX, (size_t) 0, (void **) 0));

  /* set attribute server context in the service context */
  OCICALL(ocip,
          OCIAttrSet((void *) ocip->svcp, OCI_HTYPE_SVCCTX,
                     (void *) ocip->srvp, (ub4) 0, OCI_ATTR_SERVER,
                     (OCIError *) ocip->errp));

  /* allocate a session handle */
  OCICALL(ocip,
          OCIHandleAlloc((void *) ocip->envp, (void **)&ocip->authp,
                         (ub4) OCI_HTYPE_SESSION, (size_t) 0, (void **) 0));

  /* set the username in the session */
  OCICALL(ocip,
          OCIAttrSet((void *) ocip->authp, (ub4) OCI_HTYPE_SESSION,
                     (void *) connp->user,
                     (ub4) connp->userlen,
                     (ub4) OCI_ATTR_USERNAME, ocip->errp));

  /* set the password in the session */
  OCICALL(ocip,
          OCIAttrSet((void *) ocip->authp, (ub4) OCI_HTYPE_SESSION,
                     (void *) connp->passw,
                     (ub4) connp->passwlen,
                     (ub4) OCI_ATTR_PASSWORD, ocip->errp));

  OCICALL(ocip,
          OCISessionBegin(ocip->svcp,  ocip->errp, ocip->authp,
                          OCI_CRED_RDBMS,
                          (ub4) connp->sysdba ?
                          OCI_SYSDBA : OCI_DEFAULT));

  OCICALL(ocip,
          OCIAttrSet((void *) ocip->svcp, (ub4) OCI_HTYPE_SVCCTX,
                     (void *) ocip->authp, (ub4) 0,
                     (ub4) OCI_ATTR_SESSION, ocip->errp));
  
  if (OCIHandleAlloc((void *) ocip->envp, (void **) &ocip->stmtp,
                     (ub4) OCI_HTYPE_STMT, (size_t) 0, (void **) 0))
  {
    ocierror(ocip, (char *)"OCIHandleAlloc(OCI_HTYPE_STMT) failed", TRUE);
  }

  if (OCIHandleAlloc((void *) ocip->envp, (void **) &ocip->stmt2p,
                     (ub4) OCI_HTYPE_STMT, (size_t) 0, (void **) 0))
  {
    ocierror(ocip, (char *)"OCIHandleAlloc(OCI_HTYPE_STMT-2) failed", TRUE);
  }

  if (OCIHandleAlloc((void *) ocip->envp, (void **) &ocip->stmt3p,
                     (ub4) OCI_HTYPE_STMT, (size_t) 0, (void **) 0))
  {
    ocierror(ocip, (char *)"OCIHandleAlloc(OCI_HTYPE_STMT-3) failed", TRUE);
  }

  /* restore the interrupt signal handler */
  signal(SIGINT, SIG_DFL);
}

static void oradisconnect(oci_t * ocip)
{
  if (OCILogoff(ocip->svcp, ocip->errp))
  {
    ocierror(ocip, (char *)"OCILogoff() failed", TRUE);
  }

  if (ocip->stmtp)
    OCIHandleFree((void *) ocip->stmtp, (ub4) OCI_HTYPE_STMT);

  if (ocip->stmt2p)
    OCIHandleFree((void *) ocip->stmt2p, (ub4) OCI_HTYPE_STMT);

  if (ocip->stmt2p)
    OCIHandleFree((void *) ocip->stmt3p, (ub4) OCI_HTYPE_STMT);
  
  if (ocip->errp)
    OCIHandleFree((void *) ocip->errp, (ub4) OCI_HTYPE_ERROR);

  if (ocip->envp)
    OCIHandleFree((void *) ocip->envp, (ub4) OCI_HTYPE_ENV);
}

static void print_header(options_t * opts, connection_t * connp)
{
  oci_t * ocip = connp->ocip;

  puts((char *)"\nSTREAMS Monitor, v 2.5  Copyright Oracle Corp. 2002, 2005.");
  printf("Interval = %u, Count=%u \n\n",
         opts->interval, opts->count);

  printf("Logon=%.*s@%.*s ORACLE %.*s\n\n",
         connp->userlen,
         connp->userlen ? (char *)connp->user : "",
         connp->dbnamelen,
         connp->dbnamelen ? (char *)connp->dbname : "",
         connp->versionstrlen, connp->versionstr);

  if (connp->version >= VERSION_10g)
  {
    printf("Streams Pool Size = ");
    print_pool_size(ocip);
    printf("\n\n");
  }

  if (connp->sysuser)
  {
    if (opts->short_opt)
    {
      printf("LOG : <redo generated per sec>\n");
    }
    else
    {
      printf("LOG : <last write scn> <redo blocks written>\n");
    }
  }
  
  printf("NET: ");
  if (opts->short_opt)
  {
    printf("<client bytes per sec> <dblink bytes per sec>\n");
  }
  else
  {
    printf("<client bytes> <dblink bytes>\n");    
  }
  
  printf("Cxxx: ");
  if(opts->short_opt)
  {
    printf("<lcrs captured per sec> <lcrs enqueued per sec> "
           "<capture latency>\n");
  }
  else
  {
    if (connp->version >= VERSION_10g)
    {
      printf("<read scn> ");
    }
    printf("<msgs captured> <capture scn> <msgs enqueued> <enqueue scn> "
           "<capture latency>\n");
  }
  
  if ((connp->sysuser) && (connp->version >= VERSION_10g))
  {
    printf("MEM : <percent of memory used> %% <streams pool size>\n");
  }

  if (opts->short_opt)
  {
    puts((char *)"PRxx: <messages received per sec>");
  }
  else {
    puts((char *)"PRxx: <number received>/<total time>");    
  }
  
  if (!opts->short_opt)
  {
    printf("Qx  : ");
    if (connp->version >= VERSION_10g)
    {
      printf("<outstanding mesgs>/<cumulative mesgs> ");
    }
    else if (connp->sysuser)
    {
      /* in 9i only SYS can query the outstanding messages */
      printf("<outstanding mesgs> ");
    }
    if (connp->version >= VERSION_10g)
    {
      printf("<cumulative spilled>\n");
    }
    else
    {
      printf("<currently spilled>\n");
    }
  }
  else
  {
    if (connp->version >= VERSION_10g)
    {
      printf("Qx  : <msgs enqueued per sec> <msgs spilled per sec>\n");
    }
    /* for 9i short queue stats are not printed */
  }

  if (opts->short_opt)
  {
    puts((char *)"PSxx: <lcrs propagated per sec> <bytes propaged per sec>");
  }
  else
  {
    puts((char *)"PSxx: <number propagated> <total bytes>/<total time>");
  }

  if (opts->short_opt)
  {
    puts((char *)"Axxx: <lcrs applied per sec> <txns applied per sec> "
         "<dequeue latency>");
  }
  else
  {
    puts((char *)"Axxx: <msgs deq'd> <dequeue scn> <dequeue latency> "
         "<txns recv'd> "
         "<txns assigned> <txns applied> <hwm scn> <hwm latency>");
  }

  puts((char *)"<F>: flow control in effect");
  puts((char *)"<B>: potential bottleneck");

  puts((char *)"AR: apply reader");
  puts((char *)"AS(n): n number of apply server");
  
  if (connp->version >= VERSION_10g)
    puts((char *)"<x%I x%F x%xx>: <idle wait events percentage> <flow control wait events percentage> <other wait event percentage and name>");

  puts((char *)"xx->: database instance name");

  printf("\n");
}

static void print_stats(options_t * opts, connection_t * connp)
{
  if (opts->iters > 0) 
  {
    collect_event_data(opts, connp);
  }
  
  if (connp->sysuser)
  {
    print_log_stats(opts, connp->ocip, &connp->lgwr_pval);
  }

  /* print the network usage stats */
  print_net_stats(opts, connp);

  /* now print the capture statistics */
  if (opts->short_opt)
  {
    print_short_capture_stats(opts, connp, connp->ocip, &connp->cap_pval);
  }
  else
  {
    print_capture_stats(opts, connp, connp->ocip, &connp->cap_pval);
  }

  if (connp->version >= VERSION_10g)
  {
    print_queue_stats(opts, connp, connp->ocip, &connp->que_pval,
                      &connp->prop_pval);
  }
  else
  {
    /* in 9iR2, we only get the current spilled messages.
     * Also, there is no way to get the cummulative messages.
     * So, we cannot compute the rates in 9iR2.
     */
    print_queue9i_stats(opts, connp, connp->ocip, &connp->prop_pval);
  }
  
  if (opts->short_opt)
  {
    print_short_apply_stats(opts, connp, connp->ocip, &connp->app_pval);
  }
  else
  {
    print_apply_stats(opts, connp, connp->ocip, &connp->app_pval);
  }

  if ((connp->sysuser) && (connp->version >= VERSION_10g))
  {
    print_mem_stats(opts, connp->ocip);
  }

  if (opts->iters == 0) 
  {
    collect_event_data(opts, connp);
  }
  
}

#define POOL_SIZE_10g "select current_size from v$sga_dynamic_components \
where component='streams pool'"

static void print_pool_size(oci_t * ocip)
{
  OCIDefine  *defnp;
  OCINumber   pool_size;
  OCINumber   megabyte;
  OCINumber   pool_mb;
  sword       temp;
  uword       tempsize;

  OCICALL(ocip,
          OCIStmtPrepare(ocip->stmtp, ocip->errp, (oratext *) POOL_SIZE_10g,
                         sizeof(POOL_SIZE_10g), OCI_NTV_SYNTAX, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1,
                         (void *)&pool_size,
                         sizeof(pool_size), SQLT_VNU, NULL,
                         NULL, NULL, OCI_DEFAULT));

  OCICALL(ocip,
          OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 1, 0,
                         (OCISnapshot *)0, (OCISnapshot *)0, OCI_EXACT_FETCH));

  /* create an OCI number with 1M in it */
  temp = (sword) 1 << 20;
  tempsize = sizeof(temp);
  OCICALL(ocip, OCINumberFromInt(ocip->errp, &temp, tempsize,
                                 OCI_NUMBER_SIGNED, &megabyte));

  /* divide the pool size by 1M */
  OCICALL(ocip, OCINumberDiv(ocip->errp, &pool_size, &megabyte, &pool_mb));
  
  /* covert the pool size to an integer and print it */
  OCICALL(ocip, OCINumberToInt(ocip->errp, &pool_mb, tempsize,
                               OCI_NUMBER_SIGNED, &temp));

  printf("%dM", temp);
}


#define CAPTURE_SHORT_STATS_9iR2 "select c.sid, c.CAPTURE# CAPTURENUM, \
c.CAPTURE_NAME, nvl(c.TOTAL_MESSAGES_CAPTURED,0) TOTAL_MESSAGES_CAPTURED, \
nvl(c.TOTAL_MESSAGES_ENQUEUED,0) TOTAL_MESSAGES_ENQUEUED, \
nvl((c.CAPTURE_TIME - c.CAPTURE_MESSAGE_CREATE_TIME)*86400, -1) \
CAPTURE_LATENCY from v$streams_capture c order by 1"

#define CAPTURE_SHORT_STATS "select c.sid, c.CAPTURE# CAPTURENUM,  \
c.CAPTURE_NAME, nvl(c.TOTAL_MESSAGES_CAPTURED,0) TOTAL_MESSAGES_CAPTURED, \
nvl(c.TOTAL_MESSAGES_ENQUEUED,0) TOTAL_MESSAGES_ENQUEUED, \
nvl((c.CAPTURE_TIME - \
     c.CAPTURE_MESSAGE_CREATE_TIME)*86400, -1) CAPTURE_LATENCY, \
nvl(c.CAPTURE_MESSAGE_NUMBER,0) CAPTURE_MESSAGE_NUMBER, \
l.read_scn, c.ELAPSED_PAUSE_TIME \
from v$streams_capture c, v$logmnr_session l where \
c.capture_name=l.session_name order by 1"

#define CAPTURE_STATS_9iR2 "select c.sid, c.CAPTURE# CAPTURENUM, \
c.CAPTURE_NAME, nvl(c.TOTAL_MESSAGES_CAPTURED,0) TOTAL_MESSAGES_CAPTURED, \
nvl(c.CAPTURE_MESSAGE_NUMBER,0) CAPTURE_MESSAGE_NUMBER, \
nvl(c.TOTAL_MESSAGES_ENQUEUED,0) TOTAL_MESSAGES_ENQUEUED, \
nvl(c.ENQUEUE_MESSAGE_NUMBER,0) ENQUEUE_MESSAGE_NUMBER, \
nvl((c.CAPTURE_TIME - \
     c.CAPTURE_MESSAGE_CREATE_TIME)*86400, -1) CAPTURE_LATENCY \
from v$streams_capture c order by 1"

#define CAPTURE_STATS "select c.sid, c.CAPTURE# CAPTURENUM, c.CAPTURE_NAME, \
nvl(c.TOTAL_MESSAGES_CAPTURED,0) TOTAL_MESSAGES_CAPTURED, \
nvl(c.CAPTURE_MESSAGE_NUMBER,0) CAPTURE_MESSAGE_NUMBER, \
nvl(c.TOTAL_MESSAGES_ENQUEUED,0) TOTAL_MESSAGES_ENQUEUED, \
nvl(c.ENQUEUE_MESSAGE_NUMBER,0) ENQUEUE_MESSAGE_NUMBER, \
nvl((c.CAPTURE_TIME - \
     c.CAPTURE_MESSAGE_CREATE_TIME)*86400, -1) CAPTURE_LATENCY, \
l.read_scn, c.ELAPSED_PAUSE_TIME \
from v$streams_capture c, v$logmnr_session l where \
c.capture_name=l.session_name order by 1"

typedef struct capture
{
  ub4        capturenum;
  oratext    capturename[128];
  ub2        capturenamelen;
  ub4        total_captured;
  OCINumber  captured_scn;
  oratext    captured_scn_str[128];
  ub4        captured_scn_strlen;
  ub4        total_enqueued;
  OCINumber  enqueued_scn;
  oratext    enqueued_scn_str[128];
  ub4        enqueued_scn_strlen;
  OCINumber  read_scn;
  oratext    read_scn_str[128];
  ub4        read_scn_strlen;
  sb4        capture_latency;
  ub4        elapsed_pause_time;
  ub4        sid;
} capture_t;


static cap_pval_t* print_capture_rates(oci_t * ocip, options_t * opts,
                                connection_t * connp,
                                cap_pval_t **cap_pval,
                                capture_t cap,
                                boolean short_opt, boolean *flow_ctrl,
                                boolean *bottleneck)
{
  boolean     cur_cap_exists = FALSE;
  cap_pval_t *cap_elem;
  cap_pval_t *new_elem;
  sword       result = 0;
  cap_pval_t *cur_cap = 0;
  sid_pval_t  sidval;
  sid_pval_t *new_sid = 0;
  
  for (cap_elem = *cap_pval;
       cap_elem != (cap_pval_t *)NULL;
       cap_elem = cap_elem->next_cap)
  {
    cur_cap = cap_elem;

    if ((cap_elem->capturenamelen == cap.capturenamelen) &&
        !memcmp((void *)cap_elem->capturename, (void *)cap.capturename,
                (size_t)cap_elem->capturenamelen))
    {
      cur_cap_exists = TRUE;

      if (connp->version >= VERSION_10g)
      {
        if ((*flow_ctrl) = (cap.elapsed_pause_time >
                            cap_elem->elapsed_pause_time))
        {
          printf("<F> "); 
        }
        else
        {
          OCICALL(ocip, OCINumberCmp(ocip->errp, &(cap.captured_scn),
                                   &(cap_elem->read_scn), &result));

          if (result < 0)
          {
            /* We do not want to report that capture is a bottleneck in the
             * current interval if it was under flow control in the previous
             * interval, and if the interval is less than 60 seconds.
             */
            if ((!cap_elem->flow_ctrl) ||
                ((cap_elem->flow_ctrl) && (opts->interval > 60)))
            {
              *bottleneck = TRUE;
              printf("<B> ");  /* indicate capture as potential bottleneck */
            }
            else
            {
              printf(" -  ");  /* for alignment */
            }
          }
          else
          {
            printf(" -  ");  /* for alignment */
          }
        }
      }

      if (short_opt)
      {
        /* In case the capture had been brought down and then restarted */
        if (cap.total_captured >= cap_elem->total_captured)
        {
          printf("C%03u %u %u", cap.capturenum,
          (cap.total_captured-cap_elem->total_captured)*100/(opts->elapstim),
          (cap.total_enqueued-cap_elem->total_enqueued)*100/(opts->elapstim));
          print_latency(cap.capture_latency);
        }
        else
        {
          printf("                     "); /* for alignment */
        }
      }
      
      /* update the data for the current capture in the linked list */
      memcpy((void *)cap_elem->capturename, (void *)cap.capturename,
             (size_t)cap.capturenamelen);
      cap_elem->capturenamelen = cap.capturenamelen;
      cap_elem->capturenum     = cap.capturenum;
      cap_elem->total_captured = cap.total_captured;
      cap_elem->total_enqueued = cap.total_enqueued;

      if (*flow_ctrl)
      {
        cap_elem->flow_ctrl = TRUE;
      }
      else
      {
        cap_elem->flow_ctrl = FALSE;
      }

      if (connp->version >= VERSION_10g)
      {
        cap_elem->read_scn = cap.read_scn;
        cap_elem->elapsed_pause_time = cap.elapsed_pause_time;
      }

      break;
    }
  }

  /* add the data for this new capture process into the linked list */
  if (!cur_cap_exists)
  {
    printf("    ");  /* for alignment */
    /* add the new capture stats to the linked list */
    new_elem = (cap_pval_t *)malloc (sizeof(cap_pval_t)); 
    memset(new_elem, 0, sizeof(cap_pval_t));
    if (connp->version >= VERSION_10g)
    {
      new_elem->read_scn = cap.read_scn;
      new_elem->elapsed_pause_time = cap.elapsed_pause_time;
    }
    memcpy((void *)new_elem->capturename, (void *)cap.capturename,
           (size_t)cap.capturenamelen);
    new_elem->capturenamelen = cap.capturenamelen;
    new_elem->capturenum     = cap.capturenum;
    new_elem->total_captured = cap.total_captured;
    new_elem->total_enqueued = cap.total_enqueued;
    new_elem->flow_ctrl = FALSE;
    if (*cap_pval == (cap_pval_t *)NULL)
    {
      *cap_pval = new_elem;
    }
    else
    {
      cur_cap->next_cap = new_elem;
    }
    cur_cap = new_elem;

    /* add the new sid info to sid_pval list */
    new_sid = (sid_pval_t *)malloc(sizeof(sid_pval_t));
    new_sid->sid = cap.sid;
    new_sid->pval = new_elem;
    new_sid->type = CAPTURE_PROCESS;
    new_sid->next = connp->sid_pval;
    
    connp->sid_pval = new_sid;
    connp->num_sid ++;
  }
  return cur_cap;
  
}

static void print_short_capture_stats(options_t * opts, connection_t * connp,
                               oci_t * ocip, cap_pval_t ** cap_pval)
{
  capture_t   cap;
  OCIDefine  *defnp;
  sword       result = 0;
  boolean     flow_ctrl = FALSE;
  boolean     bottleneck = FALSE;
  sword       status = OCI_SUCCESS;
  boolean     cur_cap_exists = FALSE;
  cap_pval_t  *cur_cap;
  
  if (connp->version >= VERSION_10g)
  {
    OCICALL(ocip,
            OCIStmtPrepare(ocip->stmtp, ocip->errp,
                           (oratext *) CAPTURE_SHORT_STATS,
                           sizeof(CAPTURE_SHORT_STATS), OCI_NTV_SYNTAX,
                           OCI_DEFAULT));
  }
  else
  {
    OCICALL(ocip,
            OCIStmtPrepare(ocip->stmtp, ocip->errp,
                           (oratext *) CAPTURE_SHORT_STATS_9iR2,
                           sizeof(CAPTURE_SHORT_STATS_9iR2), OCI_NTV_SYNTAX,
                           OCI_DEFAULT));
  }

  OCICALL(ocip,
    OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 0, 0,
                   (OCISnapshot *)NULL, (OCISnapshot *)NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1, &cap.sid,
                         sizeof(cap.sid), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 2, &cap.capturenum,
                         sizeof(cap.capturenum), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 3, &cap.capturename,
                         sizeof(cap.capturename), SQLT_CHR, NULL,
                         &cap.capturenamelen, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 4,
                         &cap.total_captured, sizeof(cap.total_captured),
                         SQLT_UIN, NULL, NULL, NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 5,
                         &cap.total_enqueued, sizeof(cap.total_enqueued),
                         SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 6,
                         &cap.capture_latency, sizeof(cap.capture_latency),
                         SQLT_INT, NULL, NULL, NULL, OCI_DEFAULT));

  if (connp->version >= VERSION_10g)
  {
    defnp = (OCIDefine *) 0;
    OCICALL(ocip,
            OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp,7,&cap.captured_scn,
                           sizeof(cap.captured_scn), SQLT_VNU, NULL, NULL,
                           NULL, OCI_DEFAULT));

    defnp = (OCIDefine *) 0;
    OCICALL(ocip,
            OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 8, &cap.read_scn,
                           sizeof(cap.read_scn), SQLT_VNU, NULL, NULL, NULL,
                           OCI_DEFAULT));

    defnp = (OCIDefine *) 0;
    OCICALL(ocip,
            OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 9,
                           &cap.elapsed_pause_time,
                           sizeof(cap.elapsed_pause_time),
                           SQLT_UIN, NULL, NULL, NULL, OCI_DEFAULT));
  }

  while(OCI_SUCCESS ==
        (status = OCIStmtFetch2(ocip->stmtp, ocip->errp, 1, OCI_DEFAULT,
                                0, OCI_DEFAULT)))
  {
    flow_ctrl = FALSE;
    bottleneck = FALSE;
    result = 0;

    printf(" | ");
    
    cur_cap = print_capture_rates(ocip, opts, connp, cap_pval, cap, TRUE,
                                  &flow_ctrl, &bottleneck);

    if (connp->version >= VERSION_10g)
    {
      print_event_stats(opts, (void *)cur_cap, CAPTURE_PROCESS);
    }
  }

  if (OCI_ERROR ==  status)
  {
    ocierror(ocip, (char *)" | C ", FALSE);
  }
  
  /* finish the cursor */
  OCICALL(ocip,
    OCIStmtFetch2(ocip->stmtp, ocip->errp, 0, OCI_DEFAULT, 0, OCI_DEFAULT));
}
 
static void print_capture_stats(options_t * opts, connection_t * connp,
                                oci_t * ocip, cap_pval_t ** cap_pval)
{
  capture_t   cap;
  OCIDefine  *defnp;
  sword       result = 0;
  boolean     flow_ctrl = FALSE;
  boolean     bottleneck = FALSE;
  sword       status = OCI_ERROR;
  boolean     cur_cap_exists = FALSE;
  cap_pval_t  *cur_cap;

  if (connp->version >= VERSION_10g)
  {
    OCICALL(ocip,
            OCIStmtPrepare(ocip->stmtp, ocip->errp, (oratext *) CAPTURE_STATS,
                           sizeof(CAPTURE_STATS), OCI_NTV_SYNTAX,
                           OCI_DEFAULT));
  }
  else
  {
    OCICALL(ocip,
            OCIStmtPrepare(ocip->stmtp, ocip->errp,
                           (oratext *) CAPTURE_STATS_9iR2,
                           sizeof(CAPTURE_STATS_9iR2), OCI_NTV_SYNTAX,
                           OCI_DEFAULT));
  }
  
  OCICALL(ocip,
    OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 0, 0,
                   (OCISnapshot *)NULL, (OCISnapshot *)NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1, &cap.sid,
                         sizeof(cap.sid), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 2, &cap.capturenum,
                         sizeof(cap.capturenum), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 3, &cap.capturename,
                         sizeof(cap.capturename), SQLT_CHR, NULL,
                         &cap.capturenamelen, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 4,
                         &cap.total_captured, sizeof(cap.total_captured),
                         SQLT_UIN, NULL, NULL, NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 5, &cap.captured_scn,
                         sizeof(cap.captured_scn), SQLT_VNU, NULL, NULL, NULL, 
                         OCI_DEFAULT));


  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 6,
                         &cap.total_enqueued, sizeof(cap.total_enqueued),
                         SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 7, &cap.enqueued_scn,
                         sizeof(cap.enqueued_scn), SQLT_VNU, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 8,
                         &cap.capture_latency, sizeof(cap.capture_latency),
                         SQLT_INT, NULL, NULL, NULL, OCI_DEFAULT));

  if (connp->version >= VERSION_10g)
  {
    defnp = (OCIDefine *) 0;
    OCICALL(ocip,
            OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 9, &cap.read_scn,
                           sizeof(cap.read_scn), SQLT_VNU, NULL, NULL, NULL, 
                           OCI_DEFAULT));

    defnp = (OCIDefine *) 0;
    OCICALL(ocip,
            OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 10,
                           &cap.elapsed_pause_time,
                           sizeof(cap.elapsed_pause_time),
                           SQLT_UIN, NULL, NULL, NULL, OCI_DEFAULT));
  }

  while(OCI_SUCCESS ==
        (status = OCIStmtFetch2(ocip->stmtp, ocip->errp, 1, OCI_DEFAULT,
                                0, OCI_DEFAULT)))
  {
    flow_ctrl = FALSE;
    bottleneck = FALSE;
    result = 0;
    cur_cap_exists = FALSE;


    if (connp->version >= VERSION_10g)
    {
      printf(" | ");
      cur_cap = print_capture_rates(ocip, opts, connp, cap_pval, cap, FALSE,
                          &flow_ctrl, &bottleneck);

      cap.read_scn_strlen = 128;
      OCICALL(ocip,
              OCINumberToText(ocip->errp, &cap.read_scn, 
                              (const oratext *)"TM9", 3, (oratext *)0, 0,
                              &cap.read_scn_strlen, cap.read_scn_str));
      printf(" C%03u %.*s ", cap.capturenum, cap.read_scn_strlen,
               cap.read_scn_str);
    }
    else
    {
       printf(" | C%03u ", cap.capturenum);
    }

    cap.captured_scn_strlen = 128;
    OCICALL(ocip,
            OCINumberToText(ocip->errp, &cap.captured_scn, 
                            (const oratext *)"TM9", 3, (oratext *)0, 0,
                            &cap.captured_scn_strlen, cap.captured_scn_str));

    cap.enqueued_scn_strlen = 128;
    OCICALL(ocip,
            OCINumberToText(ocip->errp, &cap.enqueued_scn, 
                            (const oratext *)"TM9", 3, (oratext *)0, 0,
                            &cap.enqueued_scn_strlen, cap.enqueued_scn_str));

    printf("%u %.*s %u %.*s",
           cap.total_captured, cap.captured_scn_strlen, cap.captured_scn_str,
           cap.total_enqueued, cap.enqueued_scn_strlen, cap.enqueued_scn_str
           );

    print_latency(cap.capture_latency);

    if (connp->version >= VERSION_10g)
    {
      print_event_stats(opts, (void *)cur_cap, CAPTURE_PROCESS);
    }
  }

  if (OCI_ERROR ==  status)
  {
    ocierror(ocip, (char *)" | C ", FALSE);
  }

  /* finish the cursor */
  OCICALL(ocip,
    OCIStmtFetch2(ocip->stmtp, ocip->errp, 0, OCI_DEFAULT, 0, OCI_DEFAULT));
}


#define QUEUE9i_STATS "select dq.qid, dq.owner, dq.queue_table from dba_queues\
  dq, dba_queue_tables dqt where dq.owner not in ('SYS','SYSTEM') and \
dq.QUEUE_TYPE='NORMAL_QUEUE'  and dq.owner=dqt.owner and \
dq.queue_table=dqt.queue_table and dqt.object_type='SYS.ANYDATA'"

#define QUEUE9i_OUTMESG_STATS "select bufqm_nmsg from x$bufqm where \
bufqm_qid=:1"

#define QUEUE9i_SPILL_STATS "select count(*) from %.*s.AQ$_%.*s_P"

typedef struct queue9i
{
  ub4     queueid;
  oratext owner[128];
  ub2     ownerlen;
  oratext qtable[128];
  ub2     qtablelen;
  ub4     spilled;
  ub4     outstanding;
} queue9i_t;

static void print_queue9i_stats(options_t * opts, connection_t * connp,
                                oci_t * ocip, prop_pval_t ** prop_pval)
{
  queue9i_t   que9i;
  OCIDefine  *defnp;
  OCIBind    *bndp;
  unsigned    pnum = 0;   
  char        spillquery[1024];
  ub4         spillquerylen;
  sword       status;
  sword       status2 = OCI_SUCCESS;

  que_pval_t *que_elem;
  que_pval_t *new_elem;
  que_pval_t *cur_que;
  boolean     cur_que_exists = FALSE;

  OCICALL(ocip,
          OCIStmtPrepare(ocip->stmtp, ocip->errp, (oratext *) QUEUE9i_STATS,
                         sizeof(QUEUE9i_STATS), OCI_NTV_SYNTAX,
                         OCI_DEFAULT));

  OCICALL(ocip,
    OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 0, 0,
                   (OCISnapshot *)NULL, (OCISnapshot *)NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1, &que9i.queueid,
                         sizeof(que9i.queueid), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 2, &que9i.owner,
                         sizeof(que9i.owner), SQLT_CHR, NULL,
                         &que9i.ownerlen, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 3, &que9i.qtable,
                         sizeof(que9i.qtable), SQLT_CHR, NULL,
                         &que9i.qtablelen, NULL, 
                         OCI_DEFAULT));

  while(OCI_SUCCESS ==
        (status2 = OCIStmtFetch2(ocip->stmtp, ocip->errp, 1, OCI_DEFAULT,
                                 0, OCI_DEFAULT)))
  {
    if (!opts->short_opt)
    {
      printf(" | Q%u", que9i.queueid);

      if (connp->sysuser)
      {
        OCICALL(ocip,
                OCIStmtPrepare(ocip->stmt2p, ocip->errp,
                               (oratext *)QUEUE9i_OUTMESG_STATS,
                               sizeof(QUEUE9i_OUTMESG_STATS), OCI_NTV_SYNTAX,
                               OCI_DEFAULT));

        bndp = (OCIBind *)0;
        OCICALL(ocip,
                OCIBindByPos(ocip->stmt2p, &bndp, ocip->errp, 1,
                             (void *) &que9i.queueid, sizeof(que9i.queueid),
                             SQLT_UIN, (void *) 0, (ub2 *) 0, (ub2 *) 0,
                             (ub4) 0, (ub4 *) 0, OCI_DEFAULT));

        defnp = (OCIDefine *) 0;
        OCICALL(ocip,
                OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 1,
                               &que9i.outstanding, sizeof(que9i.outstanding),
                               SQLT_UIN, NULL, NULL, NULL, OCI_DEFAULT));
      
        status = OCIStmtExecute(ocip->svcp, ocip->stmt2p, ocip->errp, 1, 0,
                                (OCISnapshot *)NULL, (OCISnapshot *)NULL,
                                OCI_EXACT_FETCH);

        if (OCI_NO_DATA == status)
        {
          /* if there has been no buffered enqueues then x$bufqm will not show
           * a row for this table */
          printf(" -");
        }
        else if (OCI_SUCCESS == status)
        {
          printf(" %u", que9i.outstanding);
        }
        else
        {
          /* lrg 2858006: compiler issue, temporary fix to initialize status */
          sword status_tmp = status;
          OCICALL(ocip, status_tmp);
        }
      }

      /* contruct the query for the spilled messages */
      spillquerylen = 
        (ub4)sprintf(spillquery, QUEUE9i_SPILL_STATS, que9i.ownerlen,
                que9i.owner, que9i.qtablelen, que9i.qtable);

      OCICALL(ocip,
              OCIStmtPrepare(ocip->stmt2p, ocip->errp, (oratext *) spillquery,
                             spillquerylen, OCI_NTV_SYNTAX, OCI_DEFAULT));

      defnp = (OCIDefine *) 0;
      OCICALL(ocip,
              OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 1,
                             &que9i.spilled,
                             sizeof(que9i.spilled), SQLT_UIN, NULL, NULL, NULL,
                             OCI_DEFAULT));

      status = OCIStmtExecute(ocip->svcp, ocip->stmt2p, ocip->errp, 1, 0,
                              (OCISnapshot *)NULL, (OCISnapshot *)NULL,
                              OCI_EXACT_FETCH);

      if (OCI_SUCCESS == status)
      {
        printf(" %u", que9i.spilled);
      }
      else
      {
        printf(" -");
      }
    }
    print_prop_sender_stats(opts, connp, ocip, que9i.queueid, prop_pval, &pnum);
  }

  if (OCI_ERROR ==  status2)
  {
    ocierror(ocip, (char *)" | Q ", FALSE);
  }
}

#define QUEUE_STATS "select queue_id, QUEUE_NAME, QUEUE_SCHEMA, nvl(cnum_msgs,0) CNUM_MSGS, \
nvl(cspill_msgs,0) CSPILL_MSGS, nvl(num_msgs,0) num_msgs from \
v$buffered_queues"


static void print_queue_stats(options_t * opts, connection_t * connp,
                              oci_t * ocip, que_pval_t ** que_pval,
                              prop_pval_t ** prop_pval)
{
  queue_t     que;
  OCIDefine  *defnp;
  sword       status = OCI_SUCCESS;
  que_pval_t *que_elem;
  que_pval_t *new_elem;
  que_pval_t *cur_que;
  unsigned    pnum = 0;
  boolean     cur_que_exists = FALSE;
  
  /* now print the queue statistics and the propagation stats for each
   * buffered queue */
  OCICALL(ocip,
          OCIStmtPrepare(ocip->stmtp, ocip->errp, (oratext *) QUEUE_STATS,
                         sizeof(QUEUE_STATS), OCI_NTV_SYNTAX, OCI_DEFAULT));

  OCICALL(ocip,
          OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 0, 0,
                         (OCISnapshot *)NULL, (OCISnapshot *)NULL,
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1, &que.queueid,
                         sizeof(que.queueid), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 2, &que.queuename,
                         sizeof(que.queuename), SQLT_CHR, NULL, 
                         &que.queuenamelen, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 3, &que.schemaname,
                         sizeof(que.schemaname), SQLT_CHR, NULL, 
                         &que.schemanamelen, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 4, &que.cnummsgs,
                         sizeof(que.cnummsgs), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 5, &que.cspillmsgs,
                         sizeof(que.cspillmsgs), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 6, &que.nummsgs,
                         sizeof(que.nummsgs), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  while(OCI_SUCCESS ==
        (status = OCIStmtFetch2(ocip->stmtp, ocip->errp, 1, OCI_DEFAULT, 0,
                      OCI_DEFAULT)))
  {
    print_prop_receiver_stats(opts, connp, ocip, &que, prop_pval, &pnum);

    if (!opts->short_opt)
    {
      printf(" | Q%u %5u/%u %5u", que.queueid, que.nummsgs, que.cnummsgs,
             que.cspillmsgs);
    }
    else
    {
      for (que_elem = *que_pval;
           que_elem != (que_pval_t *)NULL;
           que_elem = que_elem->next_que)
      {
        cur_que = que_elem;
        if (que_elem->queueid == que.queueid)
        {
          cur_que_exists = TRUE;
          printf (" | Q%u %u %u", que.queueid,
                  (que.cnummsgs-que_elem->cnummsgs)*100/(opts->elapstim),
                  (que.cspillmsgs-que_elem->cspillmsgs)*100/(opts->elapstim));

          /* update the values for the queue in the list */
          que_elem->cnummsgs = que.cnummsgs;
          que_elem->cspillmsgs = que.cspillmsgs;
          break;
        }
      }
      if (!cur_que_exists)
      {
        /* add the values for the new queue to the linked list */
        new_elem = (que_pval_t *)malloc(sizeof(que_pval_t));
        new_elem->queueid = que.queueid;
        new_elem->cnummsgs = que.cnummsgs;
        new_elem->cspillmsgs = que.cspillmsgs;
        new_elem->next_que = (que_pval_t *)NULL;
        if (*que_pval == (que_pval_t *)NULL)
        {
          *que_pval = new_elem;
        }
        else
        {
          cur_que->next_que = new_elem;
        }
      }
    }
    print_prop_sender_stats(opts, connp, ocip, que.queueid, prop_pval, &pnum);

  }

  if (OCI_ERROR ==  status)
  {
    ocierror(ocip, (char *)" | Q ", FALSE);
  }
  
  /* finish fetching from the buffered queue cursor */
  OCICALL(ocip,
          OCIStmtFetch2(ocip->stmtp, ocip->errp, 0, OCI_DEFAULT, 0,
                        OCI_DEFAULT));
}

typedef struct prop
{
  ub4       total_number;
  ub4       total_time;
  ub8       total_bytes;
  oratext   src_queuename[30];
  ub2       src_queuenamelen;
  oratext   src_schemaname[30];
  ub2       src_schemanamelen;
  oratext   src_dbname[128];
  ub2       src_dbnamelen;
  oratext   dst_queuename[30];
  ub2       dst_queuenamelen;
  oratext   dst_schemaname[30];
  ub2       dst_schemanamelen;
  oratext   dst_dbname[128];
  ub2       dst_dbnamelen;
  oratext   destination[128];
  ub2       destination_len;
} prop_t;

#define PROP_RECEIVER_STATS_10gR2_FIRST \
"select NVL(SRC_QUEUE_NAME, '\0'), \
NVL(SRC_QUEUE_SCHEMA, '\0'), TOTAL_MSGS, ELAPSED_ENQUEUE_TIME, \
NVL(SRC_DBNAME, '\0') \
from v$propagation_receiver \
where (DST_QUEUE_NAME=:1 or DST_QUEUE_NAME is NULL) and \
(DST_QUEUE_SCHEMA=:2 or DST_QUEUE_SCHEMA is NULL)"

#define PROP_RECEIVER_STATS_10gR2 "select NVL(SRC_QUEUE_NAME, '\0'), \
NVL(SRC_QUEUE_SCHEMA, '\0'), TOTAL_MSGS, ELAPSED_ENQUEUE_TIME, \
NVL(SRC_DBNAME, '\0') \
from v$propagation_receiver \
where DST_QUEUE_NAME=:1 and DST_QUEUE_SCHEMA=:2"

#define PROPAGATION_RECEIVER_SIDS_10gR2 \
"select k.KWQPDSID from  x$kwqpd k where k.KWQPDDQN=:1 and k.KWQPDDQS=:2 \
and (KWQPDSQN IS NULL or KWQPDSQN=:3) \
and (KWQPDSQS IS NULL or KWQPDSQS=:4) \
and (KWQPDDBN IS NULL or KWQPDDBN=:5)"

static void print_prop_receiver_stats(options_t * opts, connection_t * connp,
                                      oci_t * ocip, queue_t *que,
                                      prop_pval_t ** prop_pval,
                                      unsigned * pnum_ptr)
{
  prop_t       prp;
  OCIBind     *bndp;
  OCIDefine   *defnp;
  sword        status = OCI_SUCCESS;
  prop_pval_t *prop_elem;
  prop_pval_t *new_elem;
  prop_pval_t *cur_prop;
  boolean      cur_prop_exists = FALSE;
  sid_pval_t   sidval;
  sid_pval_t   *new_sid = 0;
  
  if (connp->version >= VERSION_10gR2)
  {
    if (*pnum_ptr == 0)
      OCICALL(ocip,
              OCIStmtPrepare(ocip->stmt2p, ocip->errp,
                             (oratext *) PROP_RECEIVER_STATS_10gR2_FIRST,
                             sizeof(PROP_RECEIVER_STATS_10gR2_FIRST), 
                             OCI_NTV_SYNTAX,
                             OCI_DEFAULT));
    else 
      OCICALL(ocip,
              OCIStmtPrepare(ocip->stmt2p, ocip->errp,
                             (oratext *) PROP_RECEIVER_STATS_10gR2,
                             sizeof(PROP_RECEIVER_STATS_10gR2), 
                             OCI_NTV_SYNTAX,
                             OCI_DEFAULT));
  }
  else
  {
    return;
  }

  bndp = (OCIBind *)0;
  OCIBindByPos(ocip->stmt2p, &bndp, ocip->errp, 1,
               (void *) &(que->queuename), 
               (sword) sizeof(que->queuename),
               SQLT_CHR, (void *) 0, 
               (ub2 *)&(que->queuenamelen), (ub2 *) 0, (ub4) 0, (ub4 *) 0,
               OCI_DEFAULT);

  bndp = (OCIBind *)0;
  OCIBindByPos(ocip->stmt2p, &bndp, ocip->errp, 2,
               (void *) &(que->schemaname), 
               (sword) sizeof(que->schemaname),
               SQLT_CHR,
               (void *) 0, (ub2 *)&(que->schemanamelen), (ub2 *) 0, (ub4) 0, (ub4 *) 0,
               OCI_DEFAULT);

  OCICALL(ocip,
          OCIStmtExecute(ocip->svcp, ocip->stmt2p, ocip->errp, 0, 0,
                         (OCISnapshot *)NULL, (OCISnapshot *)NULL,
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 1,
                         (void *)&prp.src_queuename, 
                         sizeof(prp.src_queuename),
                         SQLT_CHR, NULL, &prp.src_queuenamelen, NULL,
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 2,
                         (void *)&prp.src_schemaname, 
                         sizeof(prp.src_schemaname),
                         SQLT_CHR, NULL, &prp.src_schemanamelen, NULL,
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 3,
                         &prp.total_number, sizeof(prp.total_number),
                         SQLT_UIN, NULL, NULL, NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 4,
                         &prp.total_time, sizeof(prp.total_time),
                         SQLT_UIN, NULL, NULL, NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 5,
                         (void *)&prp.src_dbname, sizeof(prp.src_dbname),
                         SQLT_CHR, NULL, &prp.src_dbnamelen, NULL,
                         OCI_DEFAULT));

  while(OCI_SUCCESS ==
        (status = OCIStmtFetch2(ocip->stmt2p, ocip->errp, 1, OCI_DEFAULT, 0,
                      OCI_DEFAULT)))
  {
    (*pnum_ptr)++;
    cur_prop_exists = FALSE;

    for (prop_elem = *prop_pval;
         prop_elem != (prop_pval_t *)NULL;
         prop_elem = prop_elem->next_prop) 
    {
      cur_prop = prop_elem;
      if ((prop_elem->src_dbnamelen == prp.src_dbnamelen) 
          && (prop_elem->src_queuenamelen == prp.src_queuenamelen)
          && !memcmp((void *)prop_elem->src_dbname, (void *)prp.src_dbname,
                     prp.src_dbnamelen)
          && !memcmp((void *)prop_elem->src_queuename, 
                     (void *)prp.src_queuename,
                     prp.src_queuenamelen))
      {
        cur_prop_exists = TRUE;
          
        if (opts->short_opt)
        {
          /* In case the propagation process was brought down and restarted */
          if (prp.total_number >= prop_elem->total_number)
          {
            printf(" | PR%02u ", *pnum_ptr);
            printf("%u", (prp.total_number - prop_elem->total_number)*100
                        /opts->elapstim);
          }
          else if (prp.total_time > 0) 
          {
            printf(" | PR%02u ", *pnum_ptr);
            printf("%u", prp.total_number/prp.total_time);
          }
        }
          
        /* update the data for the current propagation process */
        prop_elem->total_number = prp.total_number;
        
        break;
      }
    }
    if (!cur_prop_exists)
    {
      /* add the data for the new propagation process into the linked list*/
      new_elem = (prop_pval_t *)malloc (sizeof(prop_pval_t));
      new_elem->dst_queueid = que->queueid;
      memcpy((void *)new_elem->src_queuename, (void *)(prp.src_queuename), 
             (prp.src_queuenamelen));
      new_elem->src_queuenamelen = prp.src_queuenamelen;
      memcpy((void *)new_elem->src_schemaname, (void *)(prp.src_schemaname), 
             (prp.src_schemanamelen));
      new_elem->src_schemanamelen = prp.src_schemanamelen;
      new_elem->src_dbnamelen = prp.src_dbnamelen;
      memcpy((void *)new_elem->src_dbname, (void *)prp.src_dbname,
             prp.src_dbnamelen);

      new_elem->total_number = prp.total_number;
      new_elem->total_bytes = prp.total_bytes;
      new_elem->pevent_sender = NULL;
      new_elem->pevent_receiver = NULL;
      new_elem->next_prop = (prop_pval_t *)NULL;

      
      if (*prop_pval == (prop_pval_t *)NULL)
      {
        *prop_pval = new_elem;
      }
      else 
      {
        cur_prop->next_prop = new_elem;
      }
      cur_prop = new_elem;      

      if ((connp->version >= VERSION_10gR2) && connp->sysdba)
      {
        OCICALL(ocip,
                OCIStmtPrepare(ocip->stmt3p, ocip->errp,
                               (oratext *) PROPAGATION_RECEIVER_SIDS_10gR2,
                               sizeof(PROPAGATION_RECEIVER_SIDS_10gR2), 
                               OCI_NTV_SYNTAX,
                               OCI_DEFAULT));
        
        bndp = (OCIBind *)0;
        OCIBindByPos(ocip->stmt3p, &bndp, ocip->errp, 1,
                     (void *) &(que->queuename), 
                     (sword) sizeof(que->queuename),
                     SQLT_CHR,
                     (void *) 0, (ub2 *)&(que->queuenamelen), 
                     (ub2 *) 0, (ub4) 0, (ub4 *) 0,
                     OCI_DEFAULT);
        
        bndp = (OCIBind *)0;
        OCIBindByPos(ocip->stmt3p, &bndp, ocip->errp, 2,
                     (void *) &(que->schemaname), 
                     (sword) sizeof(que->schemaname),
                     SQLT_CHR,
                     (void *) 0, (ub2 *)&(que->schemanamelen), 
                     (ub2 *) 0, (ub4) 0, (ub4 *) 0,
                     OCI_DEFAULT);

        bndp = (OCIBind *)0;
        OCIBindByPos(ocip->stmt3p, &bndp, ocip->errp, 3,
                     (void *) &(cur_prop->src_queuename), 
                     (sword) sizeof(cur_prop->src_queuename),
                     SQLT_CHR,
                     (void *) 0, (ub2 *)&(cur_prop->src_queuenamelen), 
                     (ub2 *) 0, (ub4) 0, (ub4 *) 0,
                     OCI_DEFAULT);

        bndp = (OCIBind *)0;
        OCIBindByPos(ocip->stmt3p, &bndp, ocip->errp, 4,
                     (void *) &(cur_prop->src_schemaname), 
                     (sword) sizeof(cur_prop->src_schemaname),
                     SQLT_CHR,
                     (void *) 0, (ub2 *)&(cur_prop->src_schemanamelen), 
                     (ub2 *) 0, (ub4) 0, (ub4 *) 0,
                     OCI_DEFAULT);

        bndp = (OCIBind *)0;
        OCIBindByPos(ocip->stmt3p, &bndp, ocip->errp, 5,
                     (void *) &(cur_prop->src_dbname), 
                     (sword) sizeof(cur_prop->src_dbname),
                     SQLT_CHR,
                     (void *) 0, (ub2 *)&(cur_prop->src_dbnamelen), 
                     (ub2 *) 0, (ub4) 0, (ub4 *) 0,
                     OCI_DEFAULT);


        
        OCICALL(ocip,
                OCIStmtExecute(ocip->svcp, ocip->stmt3p, ocip->errp, 0, 0,
                               (OCISnapshot *)NULL, (OCISnapshot *)NULL, 
                               OCI_DEFAULT));
        
        defnp = (OCIDefine *) 0;
        OCICALL(ocip,
                OCIDefineByPos(ocip->stmt3p, &defnp, ocip->errp, 1, 
                               &sidval.sid,
                               sizeof(sidval.sid), SQLT_UIN, NULL,
                               NULL, NULL, 
                               OCI_DEFAULT));
        
        while (OCI_SUCCESS ==
               (status = OCIStmtFetch2(ocip->stmt3p, ocip->errp, 1, 
                                       OCI_DEFAULT,
                                       0, OCI_DEFAULT))) 
        {
          new_sid = (sid_pval_t *)malloc(sizeof(sid_pval_t));
          new_sid->sid = sidval.sid;
          new_sid->pval = new_elem;
          new_sid->type = PR_RECEIVER_PROCESS;
          new_sid->next = connp->sid_pval;
          
          connp->sid_pval = new_sid;
          connp->num_sid ++;
        }
      }
    }
    
    if (!opts->short_opt)
    {
      printf(" | PR%02u %u/%u", *pnum_ptr, prp.total_number,
             prp.total_time);
    }

    if (cur_prop != NULL && (connp->version >= VERSION_10gR2) && connp->sysdba)
    {
      print_event_stats(opts, (void *)cur_prop, PR_RECEIVER_PROCESS);
    }
  }

  if (OCI_ERROR ==  status)
  {
    ocierror(ocip, (char *)" | PR ", FALSE);
  }
  
  /* finish fetching from the propagation cursor */
  OCICALL(ocip,
          OCIStmtFetch2(ocip->stmt2p, ocip->errp, 0, OCI_DEFAULT, 0,
                        OCI_DEFAULT));
}


#define PROP_SENDER_STATS "select dq.name, dq.owner, dqs.TOTAL_NUMBER, dqs.TOTAL_TIME, \
dqs.TOTAL_BYTES, dqs.destination \
from dba_queue_schedules dqs, dba_queues dq where dqs.schema=dq.owner and \
dqs.qname=dq.name and dq.qid=:1 order by dqs.destination"

#define PROP_SENDER_STATS_10gR2 "select dq.name, dq.owner, dqs.TOTAL_NUMBER, dqs.TOTAL_TIME, \
dqs.TOTAL_BYTES, dqs.destination \
from dba_queue_schedules dqs, dba_queues dq where dqs.schema=dq.owner and \
dqs.qname=dq.name and dq.qid=:1 and dqs.message_delivery_mode = 'BUFFERED' \
order by dqs.destination"

#define PROPAGATION_SENDER_SIDS \
"select KWQPSSID from x$kwqps where KWQPSQID=:1 and (KWQPSDBN=:2 or KWQPSDQN=:3)"

static void print_prop_sender_stats(options_t * opts, connection_t * connp,
                                    oci_t * ocip, ub4 queueid,
                                    prop_pval_t ** prop_pval,
                                    unsigned *pnum_ptr)
{
  prop_t       prp;
  OCIBind     *bndp;
  OCIDefine   *defnp;
  OCINumber    total_bytes_num;
  sword        status = OCI_SUCCESS;
  prop_pval_t *prop_elem;
  prop_pval_t *new_elem;
  prop_pval_t *cur_prop;
  boolean      cur_prop_exists = FALSE;
  sid_pval_t  sidval;
  sid_pval_t *new_sid = 0;
      
  if (connp->version >= VERSION_10gR2)
  {
    OCICALL(ocip,
            OCIStmtPrepare(ocip->stmt2p, ocip->errp,
                           (oratext *) PROP_SENDER_STATS_10gR2,
                           sizeof(PROP_SENDER_STATS_10gR2), OCI_NTV_SYNTAX,
                           OCI_DEFAULT));
  }
  else
  {
    OCICALL(ocip,
            OCIStmtPrepare(ocip->stmt2p, ocip->errp,
                           (oratext *) PROP_SENDER_STATS,
                           sizeof(PROP_SENDER_STATS), OCI_NTV_SYNTAX,
                           OCI_DEFAULT));
  }

  bndp = (OCIBind *)0;
  OCIBindByPos(ocip->stmt2p, &bndp, ocip->errp, 1,
               (void *) &queueid, 
               (sword) sizeof(queueid),
               SQLT_UIN,
               (void *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
               OCI_DEFAULT);

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 1,
                         (void *)&prp.src_queuename, 
                         sizeof(prp.src_queuename),
                         SQLT_CHR, NULL, &prp.src_queuenamelen, NULL,
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 2,
                         (void *)&prp.src_schemaname, 
                         sizeof(prp.src_schemaname),
                         SQLT_CHR, NULL, &prp.src_schemanamelen, NULL,
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 3,
                         &prp.total_number, sizeof(prp.total_number),
                         SQLT_UIN, NULL, NULL, NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 4,
                         &prp.total_time, sizeof(prp.total_time),
                         SQLT_UIN, NULL, NULL, NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 5,
                         &total_bytes_num, sizeof(total_bytes_num),
                         SQLT_VNU, NULL, NULL, NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 6,
                         (void *)&prp.destination, sizeof(prp.destination),
                         SQLT_CHR, NULL, &prp.destination_len, NULL,
                         OCI_DEFAULT));

  OCICALL(ocip,
          OCIStmtExecute(ocip->svcp, ocip->stmt2p, ocip->errp, 0, 0,
                         (OCISnapshot *)NULL, (OCISnapshot *)NULL,
                         OCI_DEFAULT));

  while(OCI_SUCCESS ==
        (status = OCIStmtFetch2(ocip->stmt2p, ocip->errp, 1, OCI_DEFAULT, 0,
                      OCI_DEFAULT)))
  {
    (*pnum_ptr)++;
    cur_prop_exists = FALSE;

    /* convert total bytes to ub8 */
    OCICALL(ocip,
            OCINumberToInt(ocip->errp, &total_bytes_num,
                           sizeof(prp.total_bytes), OCI_NUMBER_UNSIGNED,
                           &prp.total_bytes));

    for (prop_elem = *prop_pval;
         prop_elem != (prop_pval_t *)NULL;
         prop_elem = prop_elem->next_prop) 
    {
      cur_prop = prop_elem;
      if ((prop_elem->src_queueid == queueid)
          && (prop_elem->destination_len == prp.destination_len)
          && !memcmp((void *)prop_elem->destination, (void *)prp.destination,
                     prp.destination_len))
      {
        cur_prop_exists = TRUE;
          
        if (opts->short_opt)
        {
          /* In case the propagation process was brought down and restarted */
          if (prp.total_number >= prop_elem->total_number)
          {
            printf(" | PS%02u ", *pnum_ptr);
            printf("%u", (prp.total_number - prop_elem->total_number)*100
                         /opts->elapstim);
            printf(" ");
            printf("%d ",(prp.total_bytes - prop_elem->total_bytes)*100
                         /opts->elapstim);

            print_bytes((prp.total_bytes - prop_elem->total_bytes)*100
                         /opts->elapstim);
          }
        }
          
        /* update the data for the current propagation process */
        prop_elem->total_number = prp.total_number;
        prop_elem->total_bytes = prp.total_bytes;
          
        break;
      }
    }
    if (!cur_prop_exists)
    {
      /* add the data for the new propagation process into the linked list*/
      new_elem = (prop_pval_t *)malloc (sizeof(prop_pval_t));
      new_elem->src_queueid = queueid;
      memcpy((void *)new_elem->src_queuename, (void *)(prp.src_queuename), 
             (prp.src_queuenamelen));
      new_elem->src_queuenamelen = prp.src_queuenamelen;
      memcpy((void *)new_elem->src_schemaname, (void *)(prp.src_schemaname), 
             (prp.src_schemanamelen));
      new_elem->src_schemanamelen = prp.src_schemanamelen;
      new_elem->destination_len = prp.destination_len;
      memcpy((void *)new_elem->destination, (void *)prp.destination,
             prp.destination_len);
      new_elem->total_number = prp.total_number;
      new_elem->total_bytes = prp.total_bytes;
      new_elem->pevent_sender = NULL;
      new_elem->pevent_receiver = NULL;
      new_elem->next_prop = (prop_pval_t *)NULL;
      if (*prop_pval == (prop_pval_t *)NULL)
      {
        *prop_pval = new_elem;
      }
      else 
      {
        cur_prop->next_prop = new_elem;
      }
      cur_prop = new_elem;        


      if (connp->version >= VERSION_10gR2 && connp->sysdba) {
        
        OCICALL(ocip,
                OCIStmtPrepare(ocip->stmt3p, ocip->errp,
                               (oratext *) PROPAGATION_SENDER_SIDS,
                               sizeof(PROPAGATION_SENDER_SIDS), 
                               OCI_NTV_SYNTAX,
                               OCI_DEFAULT));
        
        bndp = (OCIBind *)0;
        OCIBindByPos(ocip->stmt3p, &bndp, ocip->errp, 1,
                     (void *) &(cur_prop->src_queueid), 
                     (sword) sizeof(cur_prop->src_queueid),
                     SQLT_UIN,
                     (void *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
                     OCI_DEFAULT);
        
        
        bndp = (OCIBind *)0;
        OCIBindByPos(ocip->stmt3p, &bndp, ocip->errp, 2,
                     (void *) &(cur_prop->destination), 
                     (sword) sizeof(cur_prop->destination),
                     SQLT_CHR,
                     (void *) 0, (ub2 *)&(cur_prop->destination_len), 
                     (ub2 *) 0, (ub4) 0, (ub4 *) 0,
                     OCI_DEFAULT);
        
        bndp = (OCIBind *)0;
        OCIBindByPos(ocip->stmt3p, &bndp, ocip->errp, 3,
                     (void *) &(cur_prop->destination), 
                     (sword) sizeof(cur_prop->destination),
                     SQLT_CHR,
                     (void *) 0, (ub2 *)&(cur_prop->destination_len), 
                     (ub2 *) 0, (ub4) 0, (ub4 *) 0,
                     OCI_DEFAULT);
        
        OCICALL(ocip,
                OCIStmtExecute(ocip->svcp, ocip->stmt3p, ocip->errp, 0, 0,
                               (OCISnapshot *)NULL, (OCISnapshot *)NULL, 
                               OCI_DEFAULT));
        
        defnp = (OCIDefine *) 0;
        OCICALL(ocip,
                OCIDefineByPos(ocip->stmt3p, &defnp, ocip->errp, 1, 
                               &sidval.sid,
                               sizeof(sidval.sid), SQLT_UIN, NULL,
                               NULL, NULL, 
                               OCI_DEFAULT));
        
        while (OCI_SUCCESS ==
               (status = OCIStmtFetch2(ocip->stmt3p, ocip->errp, 1, OCI_DEFAULT,
                                       0, OCI_DEFAULT))) 
        {
          new_sid = (sid_pval_t *)malloc(sizeof(sid_pval_t));
          new_sid->sid = sidval.sid;
          new_sid->pval = new_elem;
          new_sid->type = PR_SENDER_PROCESS;
          new_sid->next = connp->sid_pval;
          
          connp->sid_pval = new_sid;
          connp->num_sid ++;
        }
      }

    }

    if (!opts->short_opt)
    {
      printf(" | PS%02u %u %llu/%u", *pnum_ptr, prp.total_number, prp.total_bytes,
             prp.total_time);
    }

    if (cur_prop != NULL &&(connp->version >= VERSION_10gR2) && connp->sysdba)
    {
      print_event_stats(opts, (void *)cur_prop, PR_SENDER_PROCESS);
    }
  }

  if (OCI_ERROR ==  status)
  {
    ocierror(ocip, (char *)" | PS ", FALSE);
  }
  
  /* finish fetching from the propagation cursor */
  OCICALL(ocip,
          OCIStmtFetch2(ocip->stmt2p, ocip->errp, 0, OCI_DEFAULT, 0,
                        OCI_DEFAULT));
}

typedef struct apply
{
  ub4       sid;
  ub4       applynum;
  oratext   applyname[128];
  ub2       applynamelen;
  ub4       msgs_deqd;
  OCINumber dequeue_scn;
  oratext   dequeue_scn_str[128];
  ub4       dequeue_scn_strlen;
  ub4       txns_recvd;
  ub4       txns_assigned;
  ub4       txns_applied;
  OCINumber hwm_scn;
  oratext   hwm_scn_str[128];
  ub4       hwm_scn_strlen;
  sb4       hwm_latency;
  sb4       dequeue_latency;
  ub4       total_applied;
} apply_t;

#define APPLY_SHORT_STATS "select ac.apply# applynum, \
ac.apply_name, \
nvl((ac.hwm_time - ac.hwm_message_create_time)*86400, -1) HWM_LATENCY, \
sum(aps.TOTAL_MESSAGES_APPLIED), ac.total_received, \
ac.total_assigned, ac.total_applied \
from v$streams_apply_coordinator ac, \
v$streams_apply_server aps where ac.apply#=aps.apply# \
group by ac.apply#, ac.apply_name, ac.hwm_time, ac.hwm_message_create_time, \
ac.total_assigned, ac.total_received, ac.total_applied order by 1"

#define APPLY_READER_SERVER_SIDS \
"(select sid, 4 process_type from v$streams_apply_reader where APPLY#=:1) UNION (select sid, 5 from v$streams_apply_server where APPLY#=:1) order by 1"

static void print_short_apply_stats(options_t * opts, connection_t * connp,
                                    oci_t * ocip,
                                    app_pval_t ** app_pval)
{
  OCIBind    *bndp;
  OCIDefine  *defnp;
  apply_t     app;
  sb4         diff = 0;
  sword       status = OCI_SUCCESS;
  app_pval_t *app_elem;
  app_pval_t *new_elem;
  app_pval_t *cur_app;
  boolean     cur_app_exists = FALSE;
  sid_pval_t  sidval;
  sid_pval_t *new_sid = 0;

  OCICALL(ocip,
          OCIStmtPrepare(ocip->stmtp, ocip->errp,
                         (oratext *) APPLY_SHORT_STATS,
                         sizeof(APPLY_SHORT_STATS), OCI_NTV_SYNTAX,
                         OCI_DEFAULT));

  OCICALL(ocip,
    OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 0, 0,
                   (OCISnapshot *)NULL, (OCISnapshot *)NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1, &app.applynum,
                         sizeof(app.applynum), SQLT_UIN, NULL, NULL, NULL,
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 2, &app.applyname,
                         sizeof(app.applyname), SQLT_CHR, NULL,
                         &app.applynamelen, NULL,
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 3, &app.hwm_latency,
                         sizeof(app.hwm_latency), SQLT_INT, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 4,
                         &app.total_applied,
                         sizeof(app.total_applied), SQLT_UIN, NULL, NULL, NULL,
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 5, &app.txns_recvd,
                         sizeof(app.txns_recvd), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 6, &app.txns_assigned,
                         sizeof(app.txns_assigned), SQLT_UIN, NULL, NULL,NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 7, &app.txns_applied,
                         sizeof(app.txns_applied), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  while(OCI_SUCCESS ==
        (status = OCIStmtFetch2(ocip->stmtp, ocip->errp, 1, OCI_DEFAULT,
                                0, OCI_DEFAULT)))
  {
    cur_app_exists = FALSE;

    printf(" | ");

    for (app_elem = *app_pval;
         app_elem != (app_pval_t *)NULL;
         app_elem = app_elem->next_app)
    {
      cur_app = app_elem;
      if ((app_elem->applynamelen == app.applynamelen) &&
          !memcmp((void *)app_elem->applyname, (void *)app.applyname,
                  (size_t)app_elem->applynamelen))
      {
        cur_app_exists = TRUE;

        /* In case the apply had been brought down and then restarted */
        if (app.total_applied >= app_elem->total_applied)
        {
          if ((diff=(sb4)(app.txns_recvd - app.txns_assigned)) > 10)
          {
            printf("<B>");     /* indicate apply as the potential bottleneck */
          }
          else
          {
            printf(" - ");                                  /* for alignment */
          }

          printf(" A%03u %u %u", app.applynum,
            (app.total_applied - app_elem->total_applied)*100/opts->elapstim,
            (app.txns_applied - app_elem->txns_applied)*100/opts->elapstim);
          print_latency(app.hwm_latency);

          if (connp->version >= VERSION_10g)
          {
            print_event_stats(opts, (void *)cur_app, APPLY_READER);
            print_event_stats(opts, (void *)cur_app, APPLY_SERVER);
          }
          
        }
        else
        {
          printf("                           ");            /* for alignment */
        }

        /* update the data for the current apply process */
        app_elem->total_applied = app.total_applied;
        app_elem->txns_applied = app.txns_applied;

        break;
      }
    }

    if (!cur_app_exists)
    {
      printf("                           ");                /* for alignment */

                /* add the data for the new apply process to the linked list */
      new_elem = (app_pval_t *)malloc(sizeof(app_pval_t));
      new_elem->applynum = app.applynum;
      memcpy((void *)new_elem->applyname, (void *)app.applyname,
             (size_t)app.applynamelen);
      new_elem->applynamelen = app.applynamelen;
      new_elem->total_applied = app.total_applied;
      new_elem->txns_applied = app.txns_applied;
      new_elem->server_num = 0;
      
      new_elem->pevent_reader = NULL;
      new_elem->pevent_server = NULL;
      new_elem->next_app = (app_pval_t *)NULL;
      if (*app_pval == (app_pval_t *)NULL)
      {
        *app_pval = new_elem;
      }
      else
      {
        cur_app->next_app = new_elem;
      }
      cur_app = new_elem;

      if (connp->version >= VERSION_10g)
      {

        /* Add the new sid info to sid_pval list */
        new_sid = (sid_pval_t *)malloc(sizeof(sid_pval_t));
        new_sid->sid = app.sid;
        new_sid->pval = new_elem;
        new_sid->type = APPLY_READER;
        new_sid->next = connp->sid_pval;
        
        connp->sid_pval = new_sid;
        connp->num_sid ++;
        
        /* Query to find the sids of the apply servers */
        OCICALL(ocip,
              OCIStmtPrepare(ocip->stmt2p, ocip->errp,
                             (oratext *) APPLY_READER_SERVER_SIDS,
                             sizeof(APPLY_READER_SERVER_SIDS), 
                             OCI_NTV_SYNTAX,
                             OCI_DEFAULT));
        
        bndp = (OCIBind *)0;
        OCIBindByPos(ocip->stmt2p, &bndp, ocip->errp, 1,
                     (void *) &(app.applynum), 
                     (sword) sizeof(app.applynum),
                     SQLT_UIN,
                     (void *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
                     OCI_DEFAULT);
      
        OCICALL(ocip,
                OCIStmtExecute(ocip->svcp, ocip->stmt2p, ocip->errp, 0, 0,
                               (OCISnapshot *)NULL, (OCISnapshot *)NULL, 
                               OCI_DEFAULT));
        
        defnp = (OCIDefine *) 0;
        OCICALL(ocip,
                OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 1, 
                               &sidval.sid,
                               sizeof(sidval.sid), SQLT_UIN, NULL,
                               NULL, NULL, 
                               OCI_DEFAULT));

        defnp = (OCIDefine *) 0;
        OCICALL(ocip,
                OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 2, 
                               &sidval.type,
                               sizeof(sidval.type), SQLT_UIN, NULL,
                               NULL, NULL, 
                               OCI_DEFAULT));
       
        while (OCI_SUCCESS ==
               (status = OCIStmtFetch2(ocip->stmt2p, ocip->errp, 1, OCI_DEFAULT,
                                       0, OCI_DEFAULT))) 
        {
          new_sid = (sid_pval_t *)malloc(sizeof(sid_pval_t));
          new_sid->sid = sidval.sid;
          new_sid->pval = new_elem;
          new_sid->type = sidval.type;
          new_sid->next = connp->sid_pval;
          
          connp->sid_pval = new_sid;
          connp->num_sid ++;

          if (new_sid->type == APPLY_SERVER)
          {
            new_elem->server_num++;
          }
        }
      }
    }
              
  }
  
  if (OCI_ERROR ==  status)
  {
    ocierror(ocip, (char *)" | A ", FALSE);
  }

  /* finish the cursor */
  OCICALL(ocip,
    OCIStmtFetch2(ocip->stmtp, ocip->errp, 0, OCI_DEFAULT, 0, OCI_DEFAULT));
}

#define APPLY_STATS "select ar.sid, ac.apply# applynum, ac.apply_name, ar.TOTAL_MESSAGES_DEQUEUED, \
nvl(ar.DEQUEUED_MESSAGE_NUMBER,0) DEQUEUED_MESSAGE_NUMBER, ac.total_received, \
ac.total_assigned, ac.total_applied, nvl(ac.hwm_message_number,0) \
HWM_MESSAGE_NUMBER, \
nvl((ac.hwm_time - hwm_message_create_time)*86400, -1) HWM_LATENCY, \
nvl((ar.dequeue_time - \
     ar.dequeued_message_create_time)*86400, -1) DEQUEUE_LATENCY \
from v$streams_apply_coordinator ac, \
v$streams_apply_reader ar where ac.apply#=ar.apply# order by 1"

#define APPLY_SERVER_SIDS \
"select sid from v$streams_apply_server where APPLY#=:1 order by 1"

static void print_apply_stats(options_t * opts, connection_t * connp, 
                              oci_t * ocip,
                              app_pval_t ** app_pval)
{
  OCIBind    *bndp;
  OCIDefine  *defnp;
  apply_t     app;
  sb4         diff = 0;
  sword       status = OCI_SUCCESS;
  app_pval_t *app_elem;
  app_pval_t *new_elem;
  app_pval_t *cur_app;
  boolean     cur_app_exists = FALSE;
  sid_pval_t  sidval;
  sid_pval_t *new_sid = 0;

  /* prepare the apply query */
  OCICALL(ocip,
          OCIStmtPrepare(ocip->stmtp, ocip->errp, (oratext *) APPLY_STATS,
                         sizeof(APPLY_STATS), OCI_NTV_SYNTAX, OCI_DEFAULT));
  
  OCICALL(ocip,
    OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 0, 0,
                   (OCISnapshot *)NULL, (OCISnapshot *)NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1, &app.sid,
                         sizeof(app.sid), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 2, &app.applynum,
                         sizeof(app.applynum), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 3, &app.applyname,
                         sizeof(app.applyname), SQLT_CHR, NULL,
                         &app.applynamelen, NULL,
                         OCI_DEFAULT));


  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 4, &app.msgs_deqd,
                         sizeof(app.msgs_deqd), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 5, &app.dequeue_scn,
                         sizeof(app.dequeue_scn), SQLT_VNU, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 6, &app.txns_recvd,
                         sizeof(app.txns_recvd), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 7,&app.txns_assigned,
                         sizeof(app.txns_assigned), SQLT_UIN, NULL, NULL,NULL, 
                         OCI_DEFAULT));


  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 8, &app.txns_applied,
                         sizeof(app.txns_applied), SQLT_UIN, NULL, NULL, NULL, 
                         OCI_DEFAULT));


  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 9, &app.hwm_scn,
                         sizeof(app.hwm_scn), SQLT_VNU, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 10, 
                         &app.hwm_latency,
                         sizeof(app.hwm_latency), SQLT_INT, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 11,
                         &app.dequeue_latency,
                         sizeof(app.txns_recvd), SQLT_INT, NULL, NULL, NULL, 
                         OCI_DEFAULT));

  while(OCI_SUCCESS ==
        (status = OCIStmtFetch2(ocip->stmtp, ocip->errp, 1, OCI_DEFAULT,
                                0, OCI_DEFAULT)))
  {
    cur_app_exists = FALSE;

    for (app_elem = *app_pval;
         app_elem != (app_pval_t *)NULL;
         app_elem = app_elem->next_app)
    {
      cur_app = app_elem;
      if ((app_elem->applynamelen == app.applynamelen) &&
          !memcmp((void *)app_elem->applyname, (void *)app.applyname,
                  (size_t)app_elem->applynamelen))
      {
        cur_app_exists = TRUE;

        /* update the data for the current apply process */
        app_elem->total_applied = app.total_applied;
        app_elem->txns_applied = app.txns_applied;

        break;
      }
    }

    if (!cur_app_exists)
    {
      new_elem = (app_pval_t *)malloc(sizeof(app_pval_t));
      new_elem->applynum = app.applynum;
      memcpy((void *)new_elem->applyname, (void *)app.applyname,
             (size_t)app.applynamelen);
      new_elem->applynamelen = app.applynamelen;
      new_elem->total_applied = app.total_applied;
      new_elem->txns_applied = app.txns_applied;
      new_elem->pevent_reader = NULL;
      new_elem->pevent_server = NULL;
      new_elem->server_num = 0;
      new_elem->next_app = (app_pval_t *)NULL;
      if (*app_pval == (app_pval_t *)NULL)
      {
        *app_pval = new_elem;
      }
      else
      {
        cur_app->next_app = new_elem;
      }
      cur_app = new_elem;

      if (connp->version >= VERSION_10g)
      {

        /* Add the new sid info to sid_pval list */
        new_sid = (sid_pval_t *)malloc(sizeof(sid_pval_t));
        new_sid->sid = app.sid;
        new_sid->pval = new_elem;
        new_sid->type = APPLY_READER;
        new_sid->next = connp->sid_pval;
        
        connp->sid_pval = new_sid;
        connp->num_sid ++;
        
        /* Query to find the sids of the apply servers */
        OCICALL(ocip,
              OCIStmtPrepare(ocip->stmt2p, ocip->errp,
                             (oratext *) APPLY_SERVER_SIDS,
                             sizeof(APPLY_SERVER_SIDS), 
                             OCI_NTV_SYNTAX,
                             OCI_DEFAULT));      
        
        bndp = (OCIBind *)0;
        OCIBindByPos(ocip->stmt2p, &bndp, ocip->errp, 1,
                     (void *) &(app.applynum), 
                     (sword) sizeof(app.applynum),
                     SQLT_UIN,
                     (void *) 0, (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
                     OCI_DEFAULT);
      
        OCICALL(ocip,
                OCIStmtExecute(ocip->svcp, ocip->stmt2p, ocip->errp, 0, 0,
                               (OCISnapshot *)NULL, (OCISnapshot *)NULL, 
                               OCI_DEFAULT));
        
        defnp = (OCIDefine *) 0;
        OCICALL(ocip,
                OCIDefineByPos(ocip->stmt2p, &defnp, ocip->errp, 1, 
                               &sidval.sid,
                               sizeof(sidval.sid), SQLT_UIN, NULL,
                               NULL, NULL, 
                               OCI_DEFAULT));
        
        while (OCI_SUCCESS ==
               (status = OCIStmtFetch2(ocip->stmt2p, ocip->errp, 1, OCI_DEFAULT,
                                       0, OCI_DEFAULT))) 
        {
          new_sid = (sid_pval_t *)malloc(sizeof(sid_pval_t));
          new_sid->sid = sidval.sid;
          new_sid->pval = new_elem;
          new_sid->type = APPLY_SERVER;
          new_sid->next = connp->sid_pval;
          
          connp->sid_pval = new_sid;
          connp->num_sid ++;
          new_elem->server_num++;
        }
      }
    }
    
    app.dequeue_scn_strlen = 128;
    OCICALL(ocip,
            OCINumberToText(ocip->errp, &app.dequeue_scn, 
                            (const oratext *)"TM9", 3, (oratext *)0, 0,
                            &app.dequeue_scn_strlen, app.dequeue_scn_str));

    app.hwm_scn_strlen = 128;
    OCICALL(ocip,
            OCINumberToText(ocip->errp, &app.hwm_scn, 
                            (const oratext *)"TM9", 3, (oratext *)0, 0,
                            &app.hwm_scn_strlen, app.hwm_scn_str));

    printf(" | ");

    if ((diff=(sb4)(app.txns_recvd - app.txns_assigned)) > 10)
    {
      printf(" <B>");          /* indicate apply as the potential bottleneck */
    }
    else
    {
      printf("  - ");                                       /* for alignment */
    }

    printf(" A%03u %5u %.*s", app.applynum, app.msgs_deqd,
           app.dequeue_scn_strlen, app.dequeue_scn_str);

    print_latency(app.dequeue_latency);

    printf(" %5u %5u %5u %.*s",
           app.txns_recvd, app.txns_assigned, app.txns_applied,
           app.hwm_scn_strlen, app.hwm_scn_str);

    print_latency(app.hwm_latency);

    if (connp->version >= VERSION_10g)
    {
      print_event_stats(opts, (void *)cur_app, APPLY_READER);
      print_event_stats(opts, (void *)cur_app, APPLY_SERVER);
    }
  }

  if (OCI_ERROR ==  status)
  {
    ocierror(ocip, (char *)" | A ", FALSE);
  }

  /* finish the cursor */
  OCICALL(ocip,
    OCIStmtFetch2(ocip->stmtp, ocip->errp, 0, OCI_DEFAULT, 0, OCI_DEFAULT));
}

#define MEM_STATS_10g "select frused_kwqbpmt from x$kwqbpmt"

static void print_mem_stats(options_t * opts, oci_t * ocip)
{
  OCIDefine  *defnp;
  ub2         mem_used;

  OCICALL(ocip,
          OCIStmtPrepare(ocip->stmtp, ocip->errp, (oratext *) MEM_STATS_10g,
                         sizeof(MEM_STATS_10g), OCI_NTV_SYNTAX, OCI_DEFAULT));
  
  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1, &mem_used,
                         sizeof(mem_used), SQLT_UIN, NULL,
                         NULL, NULL, OCI_DEFAULT));

  OCICALL(ocip,
          OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 1, 0,
                         (OCISnapshot *)0, (OCISnapshot *)0, OCI_EXACT_FETCH));

  printf(" | MEM %u %% ", mem_used);
  print_pool_size(ocip);
}


#define VERSION_SQL "select version, instance_name from v$instance"

static void get_version(connection_t * connp)
{
  OCIDefine  *defnp;
  ub2         i;
  oratext     tempstr[20];
  ub2         tempstrlen;
  oci_t      *ocip = connp->ocip;

  OCICALL(ocip,
          OCIStmtPrepare(ocip->stmtp, ocip->errp, (oratext *) VERSION_SQL,
                         sizeof(VERSION_SQL), OCI_NTV_SYNTAX, OCI_DEFAULT));
  
  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1,
                         (void *)connp->versionstr,
                         sizeof(connp->versionstr), SQLT_CHR, NULL,
                         &connp->versionstrlen, NULL, OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 2,
                         (void *)connp->instname,
                         sizeof(connp->instname), SQLT_CHR, NULL,
                         &connp->instnamelen, NULL, OCI_DEFAULT));

  OCICALL(ocip,
          OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 1, 0,
                         (OCISnapshot *)0, (OCISnapshot *)0, OCI_EXACT_FETCH));

  tempstrlen = 0;
  for (i=0; i<connp->versionstrlen; i++)
  {
    if (connp->versionstr[i] == '.')
    {
      continue;
    }
    tempstr[tempstrlen++] = connp->versionstr[i];
  }
  tempstr[tempstrlen] = '\0';
  sscanf((char *)tempstr,"%u", &connp->version);

  if (connp->version < VERSION_9iR2)
  {
    printf("Error: ORACLE version %.*s didn't have STREAMS\n",
           connp->versionstrlen, connp->versionstr);
    exit(1);
  }
}

static void print_latency(sb4 latency)
{
  if (latency < 0)
  {
    printf(" -");
  }
  else if (latency < 60)
  {
    printf(" %usec", latency);
  }
  else if (latency < 60*60)
  {
    printf(" %umin", (latency/60));
  }
  else if (latency < 60*60*24)
  {
    printf(" %uhr ", (latency/(60*60)));
  }
  else if (latency < 60*60*24*12)
  {
    printf(" %uday", (latency/(60*60*24)));
  }
  else if (latency < 60*60*24*12*30)
  {
    printf(" %umon", (latency/(60*60*24*12)));
  }
  else 
  {
    printf(" %uyr ", (latency/(60*60*24*12)));
  }
}

#define LGWR_STATS_SCN "select (LAST_WRITE_SCN_BAS + \
(4294967296*LAST_WRITE_SCN_WRP)) LAST_WRITE_SCN from x$kcrfws"

#define LGWR_STATS_BLKS "select value from v$sysstat where name =\
'redo blocks written'"

typedef struct lgwr
{
  OCINumber last_scn;
  OCINumber last_blk;
} lgwr_t;

static void print_log_stats(options_t * opts, oci_t * ocip, ub4 *lgwr_pval)
{

  lgwr_t      log;
  OCIBind    *bndp;
  OCIDefine  *defnp;
  sb4         redo_rate;
  OCINumber   result;
  ub4         int_result;
  ub4         diff;

  /* get the count of blocks written */
  OCICALL(ocip,
          OCIStmtPrepare(ocip->stmtp, ocip->errp, (oratext *) LGWR_STATS_BLKS,
                         sizeof(LGWR_STATS_BLKS), OCI_NTV_SYNTAX,
                         OCI_DEFAULT));
  
  defnp = (OCIDefine *)0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1, &log.last_blk,
                         sizeof(log.last_blk), SQLT_VNU, NULL, NULL, NULL, 
                         OCI_DEFAULT));
  
  OCICALL(ocip,
          OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 1, 0,
                   (OCISnapshot *)NULL, (OCISnapshot *)NULL, OCI_EXACT_FETCH));

  /* for long option, we need to get the last scn also and print out
   * the number of blocks and last scn as a string */
  if (!opts->short_opt)
  {
    oratext     last_scn_str[128];
    ub4         last_scn_strlen = 128;
    oratext     last_blk_str[128];
    ub4         last_blk_strlen = 128;

    /* get the last scn written */
    OCICALL(ocip,
            OCIStmtPrepare(ocip->stmtp, ocip->errp, (oratext *) LGWR_STATS_SCN,
                           sizeof(LGWR_STATS_SCN), OCI_NTV_SYNTAX,
                           OCI_DEFAULT));

    defnp = (OCIDefine *)0;
    OCICALL(ocip,
            OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1, &log.last_scn,
                           sizeof(log.last_scn), SQLT_VNU, NULL, NULL, NULL, 
                           OCI_DEFAULT));

    OCICALL(ocip,
            OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 1, 0,
                           (OCISnapshot *)NULL, (OCISnapshot *)NULL,
                           OCI_EXACT_FETCH));

    OCICALL(ocip,
            OCINumberToText(ocip->errp, &log.last_scn, 
                            (const oratext *)"TM9", 3, (oratext *)0, 0,
                            &last_scn_strlen, last_scn_str));

    OCICALL(ocip,
            OCINumberToText(ocip->errp, &log.last_blk,
                            (const oratext *)"TM9", 3, (oratext *)0, 0,
                            &last_blk_strlen, last_blk_str));

    printf(" | LOG %s %6s", last_scn_str, last_blk_str);
  }
  else
  {
    OCICALL(ocip,
      OCINumberToInt(ocip->errp, &log.last_blk, sizeof(int_result),
      OCI_NUMBER_UNSIGNED, &int_result));

    if (*lgwr_pval)
    {
      /* take care of the case where the block number wraps around */
      if (int_result < *lgwr_pval)
      {
        printf(" | LOG    -     ");
      }
      else
      {
        redo_rate = (sb4)(((int_result - *lgwr_pval)*512*100)/(opts->elapstim));

        printf(" | LOG ");
        print_bytes((ub8)redo_rate);
      }
    }

    /* update the previous value last block number */
    *lgwr_pval = int_result;
  }
}

#define NET_STATS_CLIENT "select sum(value) from v$sysstat where name like\
'bytes%client'"

#define NET_STATS_DBLINK "select sum(value) from v$sysstat where name like\
'bytes%dblink'"

static void print_net_stats(options_t * opts, connection_t * connp)
{
  oci_t      *ocip = connp->ocip;
  OCIBind    *bndp;
  OCIDefine  *defnp;
  OCINumber   client_bytes_num;
  OCINumber   dblink_bytes_num;
  ub8         client_bytes;
  ub8         dblink_bytes;

  /* get the client stats */
  OCICALL(ocip,
          OCIStmtPrepare(ocip->stmtp, ocip->errp, (oratext *) NET_STATS_CLIENT,
                         sizeof(NET_STATS_CLIENT), OCI_NTV_SYNTAX,
                         OCI_DEFAULT));

  defnp = (OCIDefine *)0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1, &client_bytes_num,
                         sizeof(client_bytes_num), SQLT_VNU, NULL, NULL, NULL, 
                         OCI_DEFAULT));
  
  OCICALL(ocip,
          OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 1, 0,
                   (OCISnapshot *)NULL, (OCISnapshot *)NULL, OCI_EXACT_FETCH));

  /* get the dblink stats */
  OCICALL(ocip,
          OCIStmtPrepare(ocip->stmtp, ocip->errp, (oratext *) NET_STATS_DBLINK,
                         sizeof(NET_STATS_DBLINK), OCI_NTV_SYNTAX,
                         OCI_DEFAULT));

  defnp = (OCIDefine *)0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmtp, &defnp, ocip->errp, 1, &dblink_bytes_num,
                         sizeof(dblink_bytes_num), SQLT_VNU, NULL, NULL, NULL, 
                         OCI_DEFAULT));
  
  OCICALL(ocip,
          OCIStmtExecute(ocip->svcp, ocip->stmtp, ocip->errp, 1, 0,
                   (OCISnapshot *)NULL, (OCISnapshot *)NULL, OCI_EXACT_FETCH));


  /* convert OCI Numbers to ub8s */
  OCICALL(ocip, OCINumberToInt(ocip->errp, &client_bytes_num,
                               sizeof(client_bytes), OCI_NUMBER_UNSIGNED,
                               &client_bytes));

  OCICALL(ocip, OCINumberToInt(ocip->errp, &dblink_bytes_num,
                               sizeof(dblink_bytes), OCI_NUMBER_UNSIGNED,
                               &dblink_bytes));

  if (opts->short_opt)
  {
    if (connp->client_bytes || connp->dblink_bytes)
    {
      printf(" | NET ");
      print_bytes((client_bytes - connp->client_bytes)*100/opts->elapstim);
      printf(" ");
      print_bytes((dblink_bytes - connp->dblink_bytes)*100/opts->elapstim);
    }

    connp->client_bytes = client_bytes;
    connp->dblink_bytes = dblink_bytes;
  }
  else
  {
    printf(" | NET %llu %llu", client_bytes, dblink_bytes);
  }
}


static void print_event_stats (options_t * opts, void* pval, process_type_t type) 
{
  ub8 idleTime = 0;
  ub8 flowControlTime = 0;
  ub8 maxEventTime = 0;

  ub8 idle_rate = 0;
  ub8 flowcontrol_rate = 0;
  ub8 max_event_rate = 0;
  ub8 sum_rate = 0;
  ub8 total_rate = 100;
  ub8 time_diff;
  
  event_pval_t** pevt;
  event_pval_t* cur = NULL;
  event_pval_t* maxEvt = NULL;
  ub2 i = 0;
  const char* idle_events;
  const char* flowcontrol_events;
  char event_name[800];
  OCIBind    *bndp;

  if (pval == NULL) 
    return;

  /*Construct the sqlcommand*/
  if (type == CAPTURE_PROCESS) 
  {
    cap_pval_t * cap_pval = (cap_pval_t *)pval;
    pevt = &(cap_pval->pevent);

    idle_events = capture_idle_events;
    flowcontrol_events = capture_flowcontrol_events;
  }

  else if (type == PR_SENDER_PROCESS) 
  {
    
    prop_pval_t * prop_pval = (prop_pval_t *)pval;
    pevt = &(prop_pval->pevent_sender);

    idle_events = propagation_sender_idle_events;
    flowcontrol_events = propagation_sender_flowcontrol_events;
  }
  else if (type == PR_RECEIVER_PROCESS) 
  {
    
    prop_pval_t * prop_pval = (prop_pval_t *)pval;
    pevt = &(prop_pval->pevent_receiver);

    idle_events = propagation_receiver_idle_events;
    flowcontrol_events = propagation_receiver_flowcontrol_events; 
  }
  else if (type == APPLY_READER) 
  {    
    app_pval_t * app_pval = (app_pval_t *)pval;
    pevt = &(app_pval->pevent_reader);

    printf(" AR:");

    idle_events = apply_reader_idle_events;
    flowcontrol_events = apply_reader_flowcontrol_events;
  }
  else if (type == APPLY_SERVER) 
  {    
    app_pval_t * app_pval = (app_pval_t *)pval;
    pevt = &(app_pval->pevent_server);

    /* For apply server, maxEvt contains the number of apply server
        process. total rate is 100 * number of apply server process.
    */
    if (app_pval->server_num > 0)
      total_rate = 100 * app_pval->server_num;
    
    printf(" AS(%d)", app_pval->server_num);
    
    idle_events = apply_server_idle_events;
    flowcontrol_events = apply_server_flowcontrol_events;
  }

  if (*pevt == NULL) return;
  
  for (cur = (*pevt); cur != NULL; cur = cur->next_event) 
  {

    sprintf(event_name, "|%.*s|\0",cur->event_namelen, cur->event_name); 
    time_diff = cur->time_in_micro_second - cur->time_in_micro_second_old;
    if (strstr(idle_events,event_name))
      {
        idleTime += time_diff;
        continue;
      }
    
    else if (strstr(flowcontrol_events,event_name))
      {
        flowControlTime += time_diff;
        continue;
      }
    else if (time_diff > maxEventTime) 
      {
        maxEventTime = time_diff;
        maxEvt = cur;
      }
  }

  /*  printf("\nidle: %llu, flowcontrol: %llu, maxeventtime: %llu \n", idleTime, flowControlTime, maxEventTime);
   */
  idle_rate = idleTime/opts->elapstim/100;
  
  flowcontrol_rate = flowControlTime/opts->elapstim/100;
  
  max_event_rate = maxEventTime/opts->elapstim/100;

  sum_rate = idle_rate + flowcontrol_rate + max_event_rate;
  
  if (sum_rate > total_rate) 
  {
    idle_rate = (idle_rate * total_rate) / sum_rate;
    flowcontrol_rate = (flowcontrol_rate * total_rate) / sum_rate;
    max_event_rate = (max_event_rate * total_rate) / sum_rate;
  }

  if ((opts->short_opt && max_event_rate < 5) || max_event_rate < 1) 
  {
    printf(" <%llu%%I %llu%%F -> ", idle_rate, flowcontrol_rate);
  }
  else 
    printf(" <%llu%%I %llu%%F %llu%%\"%.*s\"> ", idle_rate, flowcontrol_rate, max_event_rate, maxEvt->event_namelen, maxEvt->event_name);
 
  fflush(stdout);
    
}

#define WAIT_EVENT_STATS \
  "select sid, event, TIME_WAITED_MICRO from V$SESSION_EVENT s where sid in %s order by 1"

static void collect_event_data (options_t * opts, connection_t *connp)
{
  oci_t        *ocip = connp->ocip;
  OCIDefine     *defnp;
  event_pval_t   wevt;
  event_pval_t **pevt;
  event_pval_t  *cur = NULL;
  sword          status = OCI_SUCCESS;
  OCINumber      time_in_micro_second;
  ub2            found;
  char           eventquery[3072];
  ub4            eventquerylen;
  sid_pval_t    *sidpval;
  char          *curpos;
  ub4            in_process_sid = 0;
  
  if (connp->num_sid <= 0) return;
  
  if (connp->num_sid != connp->org_sid) {
    sidpval = connp->sid_pval;
    sprintf(connp->sidstr, "(%d", sidpval->sid);
    sidpval = sidpval->next;
    curpos = connp->sidstr + strlen(connp->sidstr);
    
    while (sidpval != NULL) {
      sprintf(curpos, ",%d", sidpval->sid);
      sidpval = sidpval->next;
      curpos = curpos + strlen(curpos);
    }
    
    sprintf(curpos, ")");
    
    connp->org_sid = connp->num_sid;

    /* Reinitial sidpval to NULL */
    sidpval = NULL;
    
  }
  
  (ub4)sprintf(eventquery, WAIT_EVENT_STATS, connp->sidstr);
  eventquerylen = strlen(eventquery);
  
  OCICALL(ocip,
          OCIStmtPrepare(ocip->stmt3p, ocip->errp,
                         (oratext *) eventquery,
                         eventquerylen, OCI_NTV_SYNTAX,
                         OCI_DEFAULT));  

  /* Start to query V$SESSION_EVENT to get the wait_event data.*/

  OCICALL(ocip,
          OCIStmtExecute(ocip->svcp, ocip->stmt3p, ocip->errp, 0, 0,
                         (OCISnapshot *)NULL, (OCISnapshot *)NULL, 
                         OCI_DEFAULT));
  
  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt3p, &defnp, ocip->errp, 1, &wevt.sidnum,
                         sizeof(wevt.sidnum), SQLT_UIN, NULL,
                         NULL, NULL, 
                         OCI_DEFAULT));

  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt3p, &defnp, ocip->errp, 2,
                         &wevt.event_name, sizeof(wevt.event_name),
                         SQLT_CHR, NULL, &wevt.event_namelen, NULL, OCI_DEFAULT));
  
  defnp = (OCIDefine *) 0;
  OCICALL(ocip,
          OCIDefineByPos(ocip->stmt3p, &defnp, ocip->errp, 3, &time_in_micro_second,
                         sizeof(time_in_micro_second), SQLT_VNU, NULL, NULL, NULL, 
                         OCI_DEFAULT));
  
  while(OCI_SUCCESS ==
        (status = OCIStmtFetch2(ocip->stmt3p, ocip->errp, 1, OCI_DEFAULT,
                                0, OCI_DEFAULT))) 
  {
    OCICALL(ocip,
            OCINumberToInt(ocip->errp, &time_in_micro_second, 8, OCI_NUMBER_UNSIGNED, &(wevt.time_in_micro_second)));


    if (wevt.sidnum != in_process_sid || sidpval == NULL) 
    {
      in_process_sid = wevt.sidnum;
      
      sidpval = connp->sid_pval;
      while ( sidpval != NULL && sidpval->sid != in_process_sid) 
      {
        sidpval = sidpval->next;
      }
    }
    
    if (sidpval == NULL) 
    {
      /* error out */
    }
    
    if (sidpval->type == CAPTURE_PROCESS) 
    {
      pevt = &(((cap_pval_t *)sidpval->pval)->pevent);
    }
    else if (sidpval->type == PR_SENDER_PROCESS)
    {
      pevt = &(((prop_pval_t *)sidpval->pval)->pevent_sender);      
    }
    else if (sidpval->type == PR_RECEIVER_PROCESS)
    {
      pevt = &(((prop_pval_t *)sidpval->pval)->pevent_receiver);      
    }
    else if (sidpval->type == APPLY_READER)
    {
      pevt = &(((app_pval_t *)sidpval->pval)->pevent_reader);      
    }
    else if (sidpval->type == APPLY_SERVER)
    {
      pevt = &(((app_pval_t *)sidpval->pval)->pevent_server);      
    }
    
    found = 0;
    
    for (cur = *pevt; cur != NULL; cur = cur->next_event) 
    {
      if ((cur->event_namelen == wevt.event_namelen) && !strncmp((const char *)cur->event_name, (const char *)wevt.event_name, wevt.event_namelen)) 
      {
        found = 1;
        break;
      }
    }

    if(found) 
    {
      if (opts->iters > cur->iters)
      {
        /* Save old time_in_micro_second */
        cur->time_in_micro_second_old = cur->time_in_micro_second;
      
        /* Update time_in_micro_second */
        cur->time_in_micro_second = wevt.time_in_micro_second;
        cur->iters = opts->iters;
      }
      /* Where there are multiple apply servers */
      else if ( opts->iters == cur->iters) 
      {
        cur->time_in_micro_second += wevt.time_in_micro_second;
      }
    }
    else {
      /* Two possibilities: First time monitor or new wait event */
      event_pval_t *evt = (event_pval_t *)malloc(sizeof(event_pval_t));
      
      evt->sidnum = wevt.sidnum;
      memcpy((void *)evt->event_name, (void *)wevt.event_name, (size_t)wevt.event_namelen);
      evt->event_namelen = wevt.event_namelen;

      evt->time_in_micro_second = wevt.time_in_micro_second;
      evt->time_in_micro_second_old = 0;
      evt->iters = opts->iters;
      evt->next_event = NULL;

      if (*pevt == NULL) 
      {
        *pevt = evt;
      }
      else {
        evt->next_event = *pevt;
        *pevt = evt;
      }
    }
  }
  if (OCI_ERROR ==  status)
  {
    ocierror(ocip, (char *)" | A ", FALSE);
  }
            
}

static void print_bytes(ub8 bytes)
{
  if (bytes < 1024)
  {
    printf("%llu", bytes);
  }
  else if (bytes < (1024*1024))
  {
    printf("%lluK", bytes/1024);
  }
  else if (bytes < 1000000000)
  {
    printf("%lluM", bytes/(1024*1024));
  }
  else
  {
    printf("%lluG", bytes/(1024*1024*1024));
  }
}

/* end of file strmmon.c */

