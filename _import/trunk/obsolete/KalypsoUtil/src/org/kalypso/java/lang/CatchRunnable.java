package org.kalypso.java.lang;

/**
 * <p>Helper-Klasse für Runnable's mit geworfenen Exceptions.</p>
 * <p>Implementierende Klassen müssen die Methode {@link #runIntern()} überschreiben.</p>
 * <p>Eventuell geworfene Exception könne n dann durch getThrown geholt werden</p>
 * 
 * @author belger
 */
public abstract class CatchRunnable implements Runnable
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
