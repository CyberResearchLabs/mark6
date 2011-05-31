#ifndef MARK6_H_
#define MARK6_H_

// Common definitions.
typedef unsigned int u_int32_t;
typedef unsigned short int u_int16_t;
typedef unsigned char u_int8_t;

typedef signed int int32_t;
typedef signed short int int16_t;
typedef signed char int8_t;

// Framework includes.
#include <boost/date_time/posix_time/posix_time.hpp>
#include <log4cxx/logger.h>
#include <log4cxx/helpers/exception.h>
#include <log4cxx/propertyconfigurator.h>

using namespace std;
using namespace boost;
using namespace boost::posix_time;
using namespace boost::gregorian;
using namespace log4cxx;
using namespace log4cxx::helpers;

extern LoggerPtr logger;

// Convert CDR data/time to epoch.
extern time_t date_time_to_epoch(const string& start_date,
                          const string& start_time);

#endif /*MARK6_H_*/
