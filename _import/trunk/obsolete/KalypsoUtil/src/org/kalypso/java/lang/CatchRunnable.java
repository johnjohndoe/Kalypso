package org.kalypso.java.lang;

/**
 * <p>Helper-Klasse f�r Runnable's mit geworfenen Exceptions.</p>
 * <p>Implementierende Klassen m�ssen die Methode {@link #runIntern()} �berschreiben.</p>
 * <p>Eventuell geworfene Exception k�nne n dann durch getThrown geholt werden</p>
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
