package org.kalypso.contribs.java.io;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Decorates an {@link java.io.OutputStream}and runs a {@link java.lang.Runnable}when the {@link #close()}Methdo is
 * called.}
 * 
 * @author belger
 */
public class RunAfterCloseOutputStream extends OutputStream
{
  private final OutputStream m_os;

  private final Runnable m_runAfterClose;

  public RunAfterCloseOutputStream( final OutputStream os, final Runnable runAfterClose )
  {
    m_os = os;
    m_runAfterClose = runAfterClose;
  }

  /**
   * @see java.io.OutputStream#close()
   */
  public void close() throws IOException
  {
    try
    {
      super.close();

      m_os.close();
    }
    finally
    {
      m_runAfterClose.run();
    }
  }

  /**
   * All write methods are overwritten, to use performance improvements of decorated streeam.
   * 
   * @see java.io.OutputStream#write(int)
   */
  public void write( final int b ) throws IOException
  {
    m_os.write( b );
  }

  /**
   * All write methods are overwritten, to use performance improvements of decorated streeam.
   * 
   * @see java.io.OutputStream#write(byte[])
   */
  public void write( byte[] b ) throws IOException
  {
    m_os.write( b );
  }

  /**
   * All write methods are overwritten, to use performance improvements of decorated streeam.
   * 
   * @see java.io.OutputStream#write(byte[], int, int)
   */
  public void write( byte[] b, int off, int len ) throws IOException
  {
    m_os.write( b, off, len );
  }

}
