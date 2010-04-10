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
#ifndef CCLOG_SYSLOG_H__
#define CCLOG_SYSLOG_H__

#include "cclog.h"
#include <syslog.h>

class cclog_syslog : public cclog {
public:
	cclog_syslog(level runtime_level, const char* ident, int facility = LOG_USER, int option = 0);
	~cclog_syslog();

	void log_impl(level lv, std::string& str);
};

#endif /* cclog_syslog.h */

