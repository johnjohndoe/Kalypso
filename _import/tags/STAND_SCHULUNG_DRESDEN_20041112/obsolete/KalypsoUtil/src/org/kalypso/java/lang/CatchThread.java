package org.kalypso.java.lang;

/**
 * @author belger
 */
public abstract class CatchThread extends Thread
{
  private Throwable m_throwable = null;

  /**
   * @see java.lang.Runnable#run()
   */
  public final void run()
  {
    try
    {
      runIntern();
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
      m_throwable = t;
    }
  }
  
  protected abstract void runIntern()  throws Throwable;

  public Throwable getThrown()
  {
    return m_throwable;
  }
}
