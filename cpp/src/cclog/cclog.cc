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
#include "cclog.h"

cclog* cclog::s_logger;

void cclog::reset(cclog* lg)
{
	if(s_logger) { delete s_logger; }
	s_logger = lg;
}

void cclog::destroy()
{
	delete s_logger;
	s_logger = NULL;
}


cclog::cclog(level runtime_level) :
	m_runtime_level(runtime_level) {}

cclog::~cclog() {}

