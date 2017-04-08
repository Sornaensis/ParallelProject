import java.util.Timer;
import java.util.concurrent.SynchronousQueue;

class QueueSchedule extends Thread
{
  private Thread thread;
  private int threadID;
  private SynchronousQueue syncQueue;
  QueueSchedule(int threadID, SynchronousQueue syncQueue)
  {
    this.threadID = threadID;
    this.syncQueue = syncQueue;

    System.out.println("Created " + threadID);
  }
  public void run()
  {
    System.out.println("Running " + threadID);
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
    QueueSchedule [] tests = new QueueSchedule[5];

    for (int i = 0; i < tests.length; i++)
    {
      tests[i] = new QueueSchedule(i, new SynchronousQueue<>());
    }

    double startMs = System.currentTimeMillis();

    for (QueueSchedule qs : tests)
    {
      qs.start();
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