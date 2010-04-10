//
// cclog
//
// Copyright (C) 2009 FURUHASHI Sadayuki
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//        http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
//
#include "cclog_syslog.h"
#include <string.h>

cclog_syslog::cclog_syslog(level runtime_level, const char* ident, int facility, int option) :
	cclog(runtime_level)
{
	::openlog(ident, option, facility);
}

cclog_syslog::~cclog_syslog()
{
	::closelog();
}

void cclog_syslog::log_impl(level lv, std::string& str)
{
	int priority = LOG_DEBUG;
	switch(lv) {
	case TRACE:
	case DEBUG:
		priority = LOG_DEBUG;
		break;
	case INFO:
		priority = LOG_INFO;
		break;
	case WARN:
		priority = LOG_NOTICE;
		break;
	case ERROR:
		priority = LOG_ERR;
		break;
	case FATAL:
		priority = LOG_CRIT;
		break;
	}

	::syslog(priority, "%s", str.c_str());
}

