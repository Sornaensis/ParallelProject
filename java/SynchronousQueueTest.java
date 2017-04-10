import java.util.Timer;
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
      System.out.println("Syntax: java SynchronousQueueTest <num_producers> <num_consumers> <num_ops_each>");
      System.exit(1);
    }

    SynchronousQueue<Integer> syncQueue = new SynchronousQueue<Integer>();
    
    int numProducers = Integer.parseInt(args[0]);
    int numConsumers = Integer.parseInt(args[1]);
    int numOps       = Integer.parseInt(args[2]);
    int numThreads   = numProducers + numConsumers;

    System.out.println("Num producers: " + numProducers);
    System.out.println("Num consumers: " + numConsumers);
    System.out.println("Ops per thread: " + numOps);
    System.out.println("Total ops: " + numOps * numThreads);

    ThreadSchedule [] schedules = new ThreadSchedule[numThreads];

    // Create threads and assign them their generated list of operations
    for (int i = 0; i < schedules.length; i++)
    {
      if (numProducers > 0)
      {
        schedules[i] = new ThreadSchedule(i, syncQueue, numOps, true);
        numProducers--;
      }
      else
      {
        schedules[i] = new ThreadSchedule(i, syncQueue, numOps, false);
      }
    }

    // Shuffle start order of threads
    Collections.shuffle(Arrays.asList(schedules));

    // Begin recording test time
    double startMs = System.currentTimeMillis();

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

    // Stop recording test time
    double endMs = System.currentTimeMillis();

    System.out.println("\n(Unreliable) Runtime: " + Double.toString(endMs - startMs) + " ms");
  }
}