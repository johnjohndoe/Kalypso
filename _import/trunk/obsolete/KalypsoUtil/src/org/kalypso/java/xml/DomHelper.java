/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
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
