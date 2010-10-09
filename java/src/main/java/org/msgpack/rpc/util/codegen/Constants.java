package org.msgpack.rpc.util.codegen;

public interface Constants extends org.msgpack.util.codegen.Constants {
    String POSTFIX_TYPE_NAME_INVOKER = "_$$_Invoker";

    String POSTFIX_TYPE_NAME_SYNCCLIENT = "_$$_SyncClient";

    String POSTFIX_TYPE_NAME_ASYNCCLIENT = "_$$_AsyncClient";

    String VARIABLE_NAME_ARGS = "_$$_";

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

    String STATEMENT_CLIENT_CALLAPPLY_01 = "%s %s = %s.callApply(\"%s\", $args); ";

    String STATEMENT_CLIENT_CALLAPPLY_02 = "%s.callApply(\"%s\", $args); ";

    String STATEMENT_CLIENT_CALLASYNCAPPLY_01 = "%s %s = %s.callAsyncApply(\"%s\", $args); ";

    String STATEMENT_CLIENT_CALLASYNCAPPLY_02 = "%s.callAsyncApply(\"%s\", $args); ";

    String STATEMENT_CLIENT_CONVERT_01 = "return ($r)%s[%d].convert(%s); ";

}