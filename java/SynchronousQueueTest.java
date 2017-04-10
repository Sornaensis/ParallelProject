import java.util.Random;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.SynchronousQueue;

class ThreadSchedule extends Thread
{
  Thread thread;
  boolean [] ops;
  boolean isProducer;
  int threadID, numOps;
  int [] enqVals;
  SynchronousQueue<Integer> syncQueue;

  ThreadSchedule(int threadID, SynchronousQueue<Integer> syncQueue, int numOps, 
                 boolean isProducer)
  {
    this.numOps = numOps;
    this.threadID = threadID;
    this.syncQueue = syncQueue;
    this.isProducer = isProducer;

    if (isProducer)
    {
      enqVals = new int[numOps];
      Random rand = new Random();

      for (int i = 0; i < numOps; i++)
        enqVals[i] = rand.nextInt(100);
    }
  }

  public void run()
  {
    if (isProducer)
    {
      for (int i : enqVals)
      {
        try
        {
          syncQueue.put(i);
        }
        catch (Exception e)
        {
          System.out.println(e);
        }
      }
    }
    else
    {
      while (numOps > 0)
      {
        try
        {
          syncQueue.take();
        }
        catch (Exception e)
        {
          System.out.println(e);
        }

        numOps--;
      }
    }
  }

  public void start()
  {
    if (thread == null)
    {
      thread = new Thread(this, Integer.toString(threadID));
      thread.start();
    }
  }
}

public class SynchronousQueueTest
{
  public static void main(String[] args)
  {
    if (args.length < 3)
    {
      System.out.print("Syntax: java SynchronousQueueTest <num_producers> <num_consumers> ");
      System.out.println("<num_producer_ops> <num_consumer_ops>");
      System.exit(1);
    }

    SynchronousQueue<Integer> syncQueue = new SynchronousQueue<Integer>();
    
    int numProducers = Integer.parseInt(args[0]);
    int numConsumers = Integer.parseInt(args[1]);
    int numProdOps   = Integer.parseInt(args[2]);
    int numConsOps   = Integer.parseInt(args[3]);

    int numThreads   = numProducers + numConsumers;

    System.out.println(numProducers + " thread(s) executing " + numProdOps + " enqueues each");
    System.out.println(numConsumers + " thread(s) executing " + numConsOps + " dequeues each");

    ThreadSchedule [] schedules = new ThreadSchedule[numThreads];

    // Create threads and assign them their generated list of operations
    for (int i = 0; i < schedules.length; i++)
    {
      if (numProducers > 0)
      {
        schedules[i] = new ThreadSchedule(i, syncQueue, numProdOps, true);
        numProducers--;
      }
      else
      {
        schedules[i] = new ThreadSchedule(i, syncQueue, numConsOps, false);
      }
    }

    // Shuffle start order of threads
    Collections.shuffle(Arrays.asList(schedules));

    // Start execution of each thread
    for (ThreadSchedule sched : schedules)
    {
      try
      {
        sched.start();
      }
      catch (Exception e)
      {
        System.out.println(e);
      }
    }

    // Join threads after they complete execution
    for (ThreadSchedule sched : schedules)
    {
      try
      {
        sched.join();
      }
      catch (Exception e)
      {
        System.out.println(e);
      }
    }
  }
}