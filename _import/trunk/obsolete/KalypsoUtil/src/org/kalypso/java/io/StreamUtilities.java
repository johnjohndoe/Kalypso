package org.kalypso.java.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author belger
 */
public class StreamUtilities
{
  private StreamUtilities()
  {
  // diese Klasse wird nicht instantiiert
  }

  /**
   * Kopiert den Inhalt eines Streams in einen anderen. Die Streams werden nach
   * Ende der Operation geschlossen.
   */
  public static final void streamCopy( final InputStream is, final OutputStream os )
      throws IOException
  {
    final byte[] buffer = new byte[1024 * 16];
    while( true )
    {
      final int i = is.read( buffer );
      if( i == -1 )
        break;

      os.write( buffer, 0, i );
    }

    is.close();
    os.close();
  }
}