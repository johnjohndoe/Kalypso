package org.kalypso.java.xml;


import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.net.URL;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.io.IOUtils;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;


/**
 * Helper class for handling DOM stuff
 *
 * @author schlienger
 */
public class DomHelper
{
  /**
   * Loads a document from an InputStream. Closes the stream once finished.
   *
   * @param ins -
   *
   * @throws DomLoadException -
   */
  public static Document loadDocument( final InputStream ins ) throws DomLoadException
  {
    try
    {
      final DocumentBuilderFactory f = DocumentBuilderFactory.newInstance(  );
      final DocumentBuilder db = f.newDocumentBuilder(  );

      return db.parse( ins );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new DomLoadException( e );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }
  
  /**
   * Loads a document from an InputSource.
   *
   * @param ins -
   *
   * @throws DomLoadException -
   */
  public static Document loadDocument( final InputSource ins ) throws DomLoadException
  {
    try
    {
      final DocumentBuilderFactory f = DocumentBuilderFactory.newInstance(  );
      final DocumentBuilder db = f.newDocumentBuilder(  );

      return db.parse( ins );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new DomLoadException( e );
    }
  }

  /**
   * @see DomHelper#loadDocument(InputStream)
   */
  public static Document loadDocument( URL xmlURL ) throws DomLoadException
  {
    try
    {
      return loadDocument( xmlURL.openStream(  ) );
    }
    catch( IOException e )
    {
      throw new DomLoadException( e );
    }
  }

  /**
   * @see DomHelper#loadDocument(InputSource)
   */
  public static Document loadDocument( final String contents ) throws DomLoadException
  {
    return loadDocument( new InputSource( new StringReader( contents ) ) );
  }

  /**
   * Internal class that wraps different kind of exceptions.
   * 
   * @author schlienger
   */
  public static class DomLoadException extends Exception
  {
    public DomLoadException(  )
    {
      super(  );
    }

    public DomLoadException( String message )
    {
      super( message );
    }

    public DomLoadException( String message, Throwable cause )
    {
      super( message, cause );
    }

    public DomLoadException( Throwable cause )
    {
      super( cause );
    }
  }
}
