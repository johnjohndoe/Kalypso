package org.kalypso.java.io;

import java.io.IOException;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

/**
 * @author belger
 */
public class ReaderUtilities
{
  /** do not instantiate this class */
  private ReaderUtilities()
  {
  //
  }

  public final static String readStringFromReader( final Reader r ) throws IOException
  {
    final StringWriter sw = new StringWriter();

    readerCopy( r, sw );

    return sw.toString();
  }

  /**
   * Kopiert den Inhalt eines Readers in einen Writer. Beide werden nach Ende
   * der Operation geschlossen.
   */
  public static final void readerCopy( final Reader r, final Writer w ) throws IOException
  {
    final char[] buffer = new char[1024 * 16];
    while( true )
    {
      final int i = r.read( buffer );
      if( i == -1 )
        break;

      w.write( buffer, 0, i );
    }

    r.close();
    w.close();
  }

  /**
   * <p>Führt ein Pattern-Ersetzen durch, bevor die Gistableview geparst wird Jeder
   * key der Properties wird durch seinen value ersetzt. Funktioniert nur
   * zeilenweise, d.h.</p>
   * <p>Performance schlecht: nur für Reader mit wenig Inhalt verwenden</p>
   * 
   * @throws IOException
   */
  public static final String readAndReplace( final Reader r, final Properties replaceProps ) throws IOException
  {
    String content = ReaderUtilities.readStringFromReader( r );

    for( final Iterator iter = replaceProps.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Entry)iter.next();
      final String key = entry.getKey().toString();
      final String value = entry.getValue().toString();

      content = content.replaceAll( key, value );
    }
    
    return content;
  }

  public static void dumpAllAvailable( final Reader reader ) throws IOException
  {
    while( reader.ready() )
    {
      final char c = (char)reader.read();
      System.out.print( c );
    }
  }
}