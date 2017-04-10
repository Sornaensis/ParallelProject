import java.util.Timer;
import java.util.Random;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.SynchronousQueue;

class ThreadSchedule extends Thread
{
  Thread thread;
  boolean [] ops;
  int threadID, numOps;
  SynchronousQueue<Integer> syncQueue;

  ThreadSchedule(int threadID, SynchronousQueue<Integer> syncQueue, int numOps, boolean [] ops)
  {
    this.numOps = numOps;
    this.threadID = threadID;
    this.syncQueue = syncQueue;
    this.ops = ops;
  }

  public void run()
  {
    for (int i = 0; i < numOps; i++)
    {
      if (ops[i]) // Enqueue (w/ i as value to enqueue)
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
      else // Dequeue
      {
        try
        {
          syncQueue.take();
        }
        catch (Exception e)
        {
          System.out.println(e);
        }
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
      System.out.println("Syntax: java SynchronousQueueTest <num_threads> <num_ops_per_thread> <percent_enqueues>");
      System.exit(1);
    }

    SynchronousQueue<Integer> syncQueue = new SynchronousQueue<Integer>();
    
    int numThreads = Integer.parseInt(args[0]);
    int numOps     = Integer.parseInt(args[1]);
    int enqPercent = Integer.parseInt(args[2]);

    double enqCountD   = (numOps * numThreads) * (enqPercent / 100.0);
    int enqCount = (int) enqCountD;

    System.out.println("Num threads: " + numThreads);
    System.out.println("Num ops per thread: " + numOps);
    System.out.println("Enq percent: " + enqPercent);
    System.out.println("Enq count: " + enqCount);

    // Create boolean array of operations. True represents an enqueue, false represents a dequeue
    // Note: Use of 'Boolean' rather than 'boolean' allows for shuffling of array below
    Boolean [] allOps = new Boolean[numOps * numThreads];

    for (int i = 0; i < allOps.length; i++)
    {
      allOps[i] = (enqCount > 0);
      enqCount--;
    }

    // Randomly distribute enqueues and dequeues
    Collections.shuffle(Arrays.asList(allOps));

    // Create an array of operations for each thread to execute
    boolean [][] threadOps = new boolean[numThreads][numOps];
    int opIdx = 0;

    // Assign the randomly distributed operations
    for (int i = 0; i < numThreads; i++)
    {
      for (int j = 0; j < numOps; j++)
      {
        threadOps[i][j] = allOps[opIdx++];
      }
    }

    ThreadSchedule [] schedules = new ThreadSchedule[numThreads];

    // Create threads and assign them their generated list of operations
    for (int i = 0; i < schedules.length; i++)
    {
      schedules[i] = new ThreadSchedule(i, syncQueue, numOps, threadOps[i]);
    }

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

    System.out.println("Runtime: " + Double.toString(endMs - startMs) + " ms");
  }
}