package org.msgpack.rpc.util.codegen;

import org.msgpack.util.codegen.BasicConstants;

public interface InvokersGenConstants extends BasicConstants {
    String POSTFIX_TYPE_NAME_INVOKER = "_$$_Invoker";
    
    String FIELD_NAME_TARGET = "_$$_target";
    
//    String VARIABLE_NAME_TARGET = "_$$_t";

    String VARIABLE_NAME_ARGS = "_$$_";

    String VARIABLE_NAME_REQUEST = "_$$_r";

    String VARIABLE_NAME_MESSAGEPACKOBJECTS = "_$$_mpos";

    String VARIABLE_NAME_ERROR = "_$$_err";

    String VARIABLE_NAME_RET = "_$$_ret";

    String METHOD_NAME_INVOKE = "invoke";

    String METHOD_NAME_GETARGUMENTS = "getArguments";

    String METHOD_NAME_SENDRESPONSE = "sendResponse";

}