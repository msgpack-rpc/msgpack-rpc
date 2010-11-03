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
package org.msgpack.rpc.util.codegen;

public interface Constants extends org.msgpack.util.codegen.Constants {
    String POSTFIX_TYPE_NAME_INVOKER = "_$$_Invoker";

    String POSTFIX_TYPE_NAME_SYNCCLIENT = "_$$_SyncClient";

    String METHOD_NAME_INVOKE = "invoke";

    String STATEMENT_INVOKERS_TARGETFIELD_01 = "public %s _$$_target; ";

    String STATEMENT_INVOKERS_CONSTRUCTORBODY_01 = "{ _$$_target = $1; }";

    String STATEMENT_INVOKERS_INVOKEMETHODBODY_01 = "%s _$$_mpo = $1.getArguments(); ";

    String STATEMENT_INVOKERS_INVOKEMETHODBODY_02 = "%s _$$_mpos = _$$_mpo.asArray(); ";

    String STATEMENT_INVOKERS_INVOKEMETHODBODY_03 = "_$$_templates[%d].convert(_$$_mpos[%d])";

    String STATEMENT_INVOKERS_INVOKEMETHODBODY_04 = "%s _$$_%d = ((%s)%s).%s(); ";

    String STATEMENT_INVOKERS_INVOKEMETHODBODY_05 = "%s _$$_%d = (%s)%s; ";

    String STATEMENT_INVOKERS_INVOKEMETHODBODY_06 = "%s _$$_err = null; %s _$$_ret = null; ";

    String STATEMENT_INVOKERS_INVOKEMETHODBODY_07 = "%s %s_$$_target.%s(%s)%s;";

    String STATEMENT_INVOKERS_INVOKEMETHODBODY_08 = "try { %s } catch (%s _$$_t) { _$$_err = _$$_t; } ";

    String STATEMENT_INVOKERS_INVOKEMETHODBODY_09 = "$1.sendResponse(_$$_ret, _$$_err); ";

    String STATEMENT_CLIENT_CALLAPPLY_01 = "%s _$$_mpo = %s.callApply(\"%s\", $args); ";

    String STATEMENT_CLIENT_CALLAPPLY_02 = "%s.callApply(\"%s\", $args); ";

    String STATEMENT_CLIENT_CONVERT_01 = "return ($r)%s[%d].convert(_$$_mpo); ";

}