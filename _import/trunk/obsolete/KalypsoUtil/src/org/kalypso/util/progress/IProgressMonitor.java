package org.kalypso.util.progress;

/**
 * Dieses Interface dient zum Verfolgen von Fortschritt einer Operation.
 * Es ist praktisch identisch mit dem JFace Interface IProgressMonitor
 * 
 * @author Belger
 */
public interface IProgressMonitor
{
    public final static int UNKNOWN = -1;

    public void beginTask( final String name, final int totalWork );

    public void done();

    public boolean isCanceled();

    public void setCanceled( final boolean value );

    public void worked( final int work );
  }

