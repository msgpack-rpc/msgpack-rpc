//
// MessagePack-RPC for Java
//
// Copyright (C) 2010 FURUHASHI Sadayuki
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
package org.msgpack.rpc.config;

import java.util.HashMap;
import java.util.Map;

public abstract class ClientConfig {
    private Map<String, Object> options = new HashMap<String, Object>();
	protected int requestTimeout = 30;  // FIXME default timeout time

	public void setRequestTimeout(int sec) {
		this.requestTimeout = sec;
	}

	public int getRequestTimeout() {
		return this.requestTimeout;
	}
	
    public Object getOption(String key) {
        return options.get(key);
    }

    public Map<String, Object> getOptions() {
        return options;
    }
    
    public void setOption(String key, Object value) {
        options.put(key, value);
    }
}

