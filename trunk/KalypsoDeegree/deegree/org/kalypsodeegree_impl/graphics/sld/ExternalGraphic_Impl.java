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
package org.deegree_impl.graphics.sld;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

import org.deegree.graphics.sld.ExternalGraphic;
import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.NetWorker;

import com.sun.media.jai.codec.MemoryCacheSeekableStream;

/**
 * The ExternalGraphic element allows a reference to be made to an external
 * graphic file with a Web URL. The OnlineResource sub-element gives the URL and
 * the Format sub-element identifies the expected document MIME type of a
 * successful fetch. Knowing the MIME type in advance allows the styler to
 * select the best- supported format from the list of URLs with equivalent
 * content.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class ExternalGraphic_Impl implements ExternalGraphic, Marshallable
{
  private BufferedImage image = null;

  private String format = null;

  private URL onlineResource = null;

  /**
   * Creates a new ExternalGraphic_Impl object.
   * 
   * @param format
   * @param onlineResource
   */
  ExternalGraphic_Impl( String format, URL onlineResource )
  {
    setFormat( format );
    setOnlineResource( onlineResource );
  }

  /**
   * the Format sub-element identifies the expected document MIME type of a
   * successful fetch.
   * 
   * @return Format of the external graphic
   */
  public String getFormat()
  {
    return format;
  }

  /**
   * sets the format (MIME type)
   * 
   * @param format
   *          Format of the external graphic
   */
  public void setFormat( String format )
  {
    this.format = format;
  }

  /**
   * The OnlineResource gives the URL of the external graphic
   * 
   * @return URL of the external graphic
   */
  public URL getOnlineResource()
  {
    return onlineResource;
  }

  /**
   * sets the online resource / URL of the external graphic
   * 
   * @param onlineResource
   *          URL of the external graphic
   */
  public void setOnlineResource( URL onlineResource )
  {
    Debug.debugMethodBegin( this, "setOnlineResource" );

    this.onlineResource = onlineResource;

    try
    {
      InputStream is = onlineResource.openStream();
      MemoryCacheSeekableStream mcss = new MemoryCacheSeekableStream( is );
      RenderedOp rop = JAI.create( "stream", mcss );
      image = rop.getAsBufferedImage();
      mcss.close();
    }
    catch( IOException e )
    {
      System.out.println( "Yikes: " + e );
    }

    Debug.debugMethodEnd();
  }

  /**
   * returns the external graphic as an image. this method is not part of the
   * sld specifications but it is added for speed up applications
   * 
   * @return the external graphic as BufferedImage
   */
  public BufferedImage getAsImage()
  {
    return image;
  }

  /**
   * sets the external graphic as an image.
   * 
   * @param image
   *          the external graphic as BufferedImage
   */
  public void setAsImage( BufferedImage image )
  {
    this.image = image;
  }

  /**
   * exports the content of the ExternalGraphic as XML formated String
   * 
   * @return xml representation of the ExternalGraphic
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 200 );
    sb.append( "<ExternalGraphic>" );
    sb.append( "<OnlineResource xmlns:xlink='http://www.w3.org/1999/xlink' " );
    sb.append( "xlink:type='simple' xlink:href='" );
    sb.append( NetWorker.url2String( onlineResource ) + "'/>" );
    sb.append( "<Format>" ).append( format ).append( "</Format>" );
    sb.append( "</ExternalGraphic>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }

}