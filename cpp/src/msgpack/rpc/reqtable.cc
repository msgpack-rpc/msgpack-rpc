//
// msgpack::rpc::reqtable - Cluster Communication Framework
//
// Copyright (C) 2009-2010 FURUHASHI Sadayuki
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
#include "reqtable.h"
#include "future_impl.h"

namespace msgpack {
namespace rpc {


void reqtable::insert(msgid_t msgid, shared_future f)
{
	mp::pthread_scoped_lock lk(m_mutex);
	m_map.insert( map_t::value_type(msgid, f) );
}

shared_future reqtable::take(msgid_t msgid)
{
	mp::pthread_scoped_lock lk(m_mutex);
	map_t::iterator found = m_map.find(msgid);
	if(found == m_map.end()) {
		return shared_future();
	} else {
		shared_future f = found->second;
		m_map.erase(found);
		return f;
	}
}

void reqtable::take_all(std::vector<shared_future>* all)
{
	mp::pthread_scoped_lock lk(m_mutex);
	for(map_t::iterator it(m_map.begin());
			it != m_map.end(); ) {
		shared_future& f = it->second;
		all->push_back(f);
		m_map.erase(it++);
	}
}

void reqtable::step_timeout(std::vector<shared_future>* timedout)
{
	mp::pthread_scoped_lock lk(m_mutex);
	for(map_t::iterator it(m_map.begin());
			it != m_map.end(); ) {
		shared_future& f = it->second;
		if(f->step_timeout()) {
			timedout->push_back(f);
			m_map.erase(it++);
		} else {
			++it;
		}
	}
}


size_t reqtable::size() const
{
	return m_map.size();
}


}  // namespace rpc
}  // namespace msgpack

