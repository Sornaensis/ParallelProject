Compiling the Java test requires Java 8  

Compile command:  
  javac SynchronousQueueTest.java
  
How to run:  
  Syntax: java SynchronousQueueTest <num producers> <num consumers> <num producer ops> <num consumer ops>
  
  Note: To ensure the test completes, the sum of producer ops should equal the sum of consumer ops.  
        Otherwise, the nature of the synchronous queue will cause execution to hang while at least one thread
        awaits fulfillment of its enqueue or dequeue operation.
        
  Run example:  
    java SynchronousQueueTest 1 2 100 50
