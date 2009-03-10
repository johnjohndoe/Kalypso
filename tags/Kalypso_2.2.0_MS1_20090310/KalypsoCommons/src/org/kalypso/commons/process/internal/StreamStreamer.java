package org.kalypso.commons.process.internal;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class StreamStreamer extends Thread
{
  private final OutputStream m_os;

  private final InputStream m_is;

  public StreamStreamer( final InputStream is, final OutputStream os )
  {
    m_os = os;
    m_is = is;

    start();
  }

  @Override
  public void run( )
  {
    if( m_is == null )
      return;

    try
    {
      final byte[] stuff = new byte[2048];

      while( true )
      {
        final int read = m_is.read( stuff );
        if( read == -1 )
          break;

        if( m_os != null )
          m_os.write( stuff, 0, read );
      }
    }
    catch( final IOException ioe )
    {
      ioe.printStackTrace();
    }
  }
}