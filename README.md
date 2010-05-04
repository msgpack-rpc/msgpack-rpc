MessagePack-RPC
===============
http://github.com/msgpack/msgpack-rpc

## Overview

MessagePack-RPC is an inter-process messaging library that uses [MessagePack](http://msgpack.sourceforge.net/) for object serialization.

MessagePack enables very fast streaming serialization/deserialization, and communication between heterogeneous languages like Ruby <-> C++.

  - Asynchronous, synchronous and callback interfaces are supported.
  - Scalable event-driven architecture.


## Status

### Ruby

MessagePack-RPC for Ruby is available by RubyGems:

    $ gem install msgpack-rpc

### C++

See [cpp](http://github.com/msgpack/msgpack-rpc/tree/master/cpp/) directory.

### Java

[Maven2 repository](http://msgpack.sourceforge.net/maven2/) is available. Please add these lines to your pom.xml.

  <repositories>
    <repository>
    <id>msgpack.sourceforge.net</id>
    <name>MessagePack Repository for Maven</name> 
    <url>http://msgpack.sourceforge.net/maven2/</url>
    </repository>
  </repositories>
  <dependencies>
    <dependency>
      <groupId>org.msgpack</groupId>
      <artifactId>msgpack-rpc</artifactId>
      <version>0.1.0</version>
    </dependency>
  </dependencies>

## License

    Copyright (C) 2009-2010 FURUHASHI Sadayuki
    
       Licensed under the Apache License, Version 2.0 (the "License");
       you may not use this file except in compliance with the License.
       You may obtain a copy of the License at
    
           http://www.apache.org/licenses/LICENSE-2.0
    
       Unless required by applicable law or agreed to in writing, software
       distributed under the License is distributed on an "AS IS" BASIS,
       WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
       See the License for the specific language governing permissions and
       limitations under the License.

See also NOTICE file.

