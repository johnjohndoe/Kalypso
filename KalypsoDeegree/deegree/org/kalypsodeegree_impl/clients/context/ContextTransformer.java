/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/

package org.deegree_impl.clients.context;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.deegree_impl.tools.Debug;

/**
 * Singleton class capsulating a <code>javax.xml.transform.Transformer</code>.
 * This class is used to transform a map context xml document in a html document
 * used the transformation (xslt) provided.
 * 
 * @author <a href="mailto:taddei@lat-lon.de">Ugo Taddei </a>
 *  
 */
public class ContextTransformer
{

  /**
   * The <code>Transformer</code> object used in the transformation of a map
   * context xml to html.
   */
  protected static Transformer transformer = null;

  private static final ContextTransformer INSTANCE = new ContextTransformer();

  // Forbid instanstiation
  private ContextTransformer()
  {}

  private static void initTransformer( InputStream is )
  {

    try
    {
      TransformerFactory tFactory = TransformerFactory.newInstance();
      transformer = tFactory.newTransformer( new StreamSource( is ) );
    }
    catch( TransformerConfigurationException e )
    {
      e.printStackTrace();
    }
    catch( TransformerFactoryConfigurationError e )
    {
      e.printStackTrace();
    }
  }

  public static ContextTransformer getInstance()
  {

    return INSTANCE;
  }

  /**
   * Transforms the context pointed to by <code>context</code> into html using
   * <code>xsltURL</code> (though this is currently fixed; there's really no
   * need to define one's wn xsl).
   * 
   * @param xsl
   *          the <code>InputStream</code> containing the xls
   * @param contxt
   *          the <code>InputStream</code> containing the context to be
   *          transformed
   */
  public String transformContext( InputStream xsl, InputStream contxt )
      throws TransformerException, IOException
  {
    Debug.debugMethodBegin();
    StringWriter sw = new StringWriter();

    if( transformer == null )
    { // needs to instatiate transformer
      initTransformer( xsl );
    }
    if( transformer == null )
    {
      System.out.println( "still null" );
    }
    StreamResult sr = new StreamResult( sw );
    StreamSource sSrc = new StreamSource( contxt );
    transformer.transform( sSrc, sr );

    try
    {
      sw.close();
    }
    catch( IOException e )
    {
      System.out.println( "Unable to close string writer.\n" );
      e.printStackTrace();
    }

    Debug.debugMethodEnd();
    return sw.toString();
  }

  public String toString()
  {
    return transformer.toString();
  }
}