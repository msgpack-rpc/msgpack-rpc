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
#include "cclog_tty.h"
#include <string.h>
#include <stdlib.h>

#define TTY_COLOR_RESET    "\033]R"
#define TTY_COLOR_CRE      "\033[K"
#define TTY_COLOR_CLEAR    "\033c"
#define TTY_COLOR_NORMAL   "\033[0;39m"
#define TTY_COLOR_RED      "\033[1;31m"
#define TTY_COLOR_GREEN    "\033[1;32m"
#define TTY_COLOR_YELLOW   "\033[1;33m"
#define TTY_COLOR_BLUE     "\033[1;34m"
#define TTY_COLOR_MAGENTA  "\033[1;35m"
#define TTY_COLOR_CYAN     "\033[1;36m"
#define TTY_COLOR_WHITE    "\033[1;37m"

static const char* const color_table[] = {
	TTY_COLOR_NORMAL,
	TTY_COLOR_WHITE,
	TTY_COLOR_GREEN,
	TTY_COLOR_YELLOW,
	TTY_COLOR_MAGENTA,
	TTY_COLOR_RED,
};

cclog_tty::cclog_tty(level runtime_level, std::ostream& stream) :
	cclog(runtime_level),
	m_stream(stream)
{ }

cclog_tty::~cclog_tty()
{ }

void cclog_tty::log_impl(level lv, std::string& str)
{
	// output atomically

	size_t sz =
		strlen(color_table[lv])
		+ str.size()
		+ strlen(TTY_COLOR_NORMAL "\n");

	char* buf = (char*)::malloc(sz);
	if(!buf) { throw std::bad_alloc(); }

	char* p = buf;
	memcpy(p, color_table[lv], strlen(color_table[lv]));
	p += strlen(color_table[lv]);
	memcpy(p, str.data(), str.size());
	p += str.size();
	memcpy(p, TTY_COLOR_NORMAL "\n", strlen(TTY_COLOR_NORMAL "\n"));

	try {
		m_stream.write(buf, sz) << std::flush;
	} catch (...) {
		free(buf);
		throw;
	}
	free(buf);
}


