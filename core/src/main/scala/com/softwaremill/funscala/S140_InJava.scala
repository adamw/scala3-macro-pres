package com.softwaremill.funscala

import scala.annotation.StaticAnnotation

object S140_InJava:
  class Magic extends StaticAnnotation
  
  @Magic
  class MyApplication {
    // ...
  }
  
  // compile-time annotation processors: Lombok 
  // load-time bytecode manipulation: AOP
  // run-time bytecode manipulation: Spring, JEE
  
  /*
   - @Transactional
   - @GET @Path("/{param}")
   - @Inject
   */
