package org.kalypso.eclipse.ui;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.eclipse.ui.IMemento;

/**
 * MementoUtils
 * 
 * @author schlienger
 */
public class MementoUtils
{
  public final static String ISO88591 = "ISO-8859-1";
  
  private MementoUtils( )
  {
    // do not instanciate
  }

  /**
   * Saves the given properties into the memento. The properties are simply
   * serialized using the load/save mechanims of the Properties class. The
   * string representation is then saved in the text data of the tag.
   * 
   * @param memento
   * @param props
   * @throws IOException
   */
  public static void saveProperties( final IMemento memento,
      final Properties props ) throws IOException
  {
    final ByteArrayOutputStream stream = new ByteArrayOutputStream();

    try
    {
      props.store( stream, "" );

      memento.putTextData( stream.toString( ISO88591 ) );
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }
  
  /**
   * The pendant to the saveProperties().
   * 
   * @param memento
   * @param props
   * @throws IOException
   */
  public static void loadProperties( final IMemento memento, final Properties props ) throws IOException
  {
    InputStream ins = null;
    try
    {
      ins = new ByteArrayInputStream( memento.getTextData().getBytes( ISO88591 ) );
    
      props.load( ins );
    }
    catch( UnsupportedEncodingException e )
    {
      e.printStackTrace();
      // ignored
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }
}