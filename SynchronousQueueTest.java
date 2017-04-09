import java.util.Timer;
import java.util.Random;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.SynchronousQueue;

class QueueSchedule extends Thread
{
  Thread thread;
  boolean isProducer;
  int threadID, numOps;
  int [] enqVals;
  SynchronousQueue<Integer> syncQueue;

  QueueSchedule(int threadID, SynchronousQueue<Integer> syncQueue, int numOps, boolean isProducer)
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
          System.out.println(threadID + " enq: " + i);
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
           System.out.println(threadID + " deq: " + syncQueue.take());
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
    System.out.println("Starting " + threadID);

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
      System.out.println("Syntax: java SynchronousQueueTest <num_producers> <num_consumers> <num_ops>");
      System.exit(1);
    }

    SynchronousQueue<Integer> syncQueue = new SynchronousQueue<Integer>();
    
    int numProducers = Integer.parseInt(args[0]);
    int numConsumers = Integer.parseInt(args[1]);
    int numOps = Integer.parseInt(args[2]);

    QueueSchedule [] tests = new QueueSchedule[numProducers + numConsumers];

    for (int i = 0; i < tests.length; i++)
    {
      if (numProducers > 0)
      {
        tests[i] = new QueueSchedule(i, syncQueue, numOps, true); // Create producer
        numProducers--;
      }
      else if (numConsumers > 0)
      {
        tests[i] = new QueueSchedule(i, syncQueue, numOps, false); // Create consumer
        numConsumers--;
      }
    }

    // Randomly distribute producer(s) and enqueuer(s)
    Collections.shuffle(Arrays.asList(tests));

    double startMs = System.currentTimeMillis();

    for (QueueSchedule qs : tests)
    {
      try
      {
        qs.start();
      }
      catch (Exception e)
      {
        System.out.println(e);
      }
    }

    for (QueueSchedule qs : tests)
    {
      try
      {
        qs.join();
      }
      catch (Exception e)
      {
        System.out.println(e);
      }
    }

    double endMs = System.currentTimeMillis();

    System.out.println("Runtime: " + Double.toString(endMs - startMs) + " ms");
  }
}