package org.kalypso.java.lang;

/**
 * <p>This thread catches all thrown exceptions and stores it into a variable</p>
 * <p>Implementors must implement {@link #runIntern()}.
 * 
 * @author belger
 */
public abstract class CatchThread extends Thread
{
  private Throwable m_throwable = null;

  public Throwable getThrowable()
  {
    return m_throwable;
  }
  
  /**
   * @see java.lang.Thread#run()
   */
  public void run()
  {
    try
    {
      runIntern();
    }
    catch( final Throwable t )
    {
      m_throwable = t;
    }
  }

  protected abstract void runIntern() throws Throwable;
}
