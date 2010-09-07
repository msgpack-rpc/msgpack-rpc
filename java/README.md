MessagePack-RPC for Java
========================

## Overview

The Java implementation of MessagePack-RPC.

## Status

The current stable release is version 0.5.

## Implemented Features

Currently, these features are supported.

  - Asynchronous Call API
  - TCP/UDP Transport support
  - Scalable event-driven architecture

## Dependency

MessagePack-RPC for Java requires these softwares.
If you use Maven2 repository, these packages will be automatically downloaded.

  - JBoss XNIO

## Install

### Maven Repository

[Maven2 repository](http://msgpack.org/maven2/) is available.
If your project uses Maven, please add these lines to your pom.xml.

  <repositories>
    <repository>
    <id>msgpack.org</id>
    <name>MessagePack Repository for Maven</name> 
    <url>http://msgpack.org/maven2/</url>
    </repository>
  </repositories>
  <dependencies>
    <dependency>
      <groupId>org.msgpack</groupId>
      <artifactId>msgpack-rpc</artifactId>
      <version>0.5.0</version>
    </dependency>
  </dependencies>

## Build from the source

Maven2 is required to build this project.
The following command builds jar file.
Then you'll get the .jar file in target directory.

  mvn compile
  mvn package

## License

    Copyright (C) 2010 FURUHASHI Sadayuki
    
       Licensed under the Apache License, Version 2.0 (the "License");
       you may not use this file except in compliance with the License.
       You may obtain a copy of the License at
    
           http://www.apache.org/licenses/LICENSE-2.0
    
       Unless required by applicable law or agreed to in writing, software
       distributed under the License is distributed on an "AS IS" BASIS,
       WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
       See the License for the specific language governing permissions and
       limitations under the License.
    
