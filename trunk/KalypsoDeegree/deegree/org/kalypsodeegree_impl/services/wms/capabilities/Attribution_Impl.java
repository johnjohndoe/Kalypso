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
package org.deegree_impl.services.wms.capabilities;

import java.net.MalformedURLException;
import java.net.URL;

import org.deegree.services.wms.capabilities.Attribution;
import org.deegree.services.wms.capabilities.LogoURL;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.NetWorker;

/**
 * The optional <Attribution>element provides a way to identify the source of
 * the map data used in a Layer or collection of Layers. Attribution encloses
 * several optional elements: <OnlineResource>states the data provider's URL;
 * <Title>is a human-readable string naming the data provider; <LogoURL>is the
 * URL of a logo image. Client applications may choose to display one or more of
 * these items. A <Format>element in LogoURL indicates the MIME type of the logo
 * image, and the attributes width and height state the size of the image in
 * pixels.
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
class Attribution_Impl implements Attribution, Marshallable
{
  private LogoURL logoURL = null;

  private String title = null;

  private URL onlineResource = null;

  /**
   * default constructor
   */
  Attribution_Impl()
  {}

  /**
   * constructor initializing the class with the attributes
   */
  Attribution_Impl( String title, URL onlineResource, LogoURL logoURL )
  {
    setTitle( title );
    setOnlineResource( onlineResource );
    setLogoURL( logoURL );
  }

  /**
   * a human-readable string naming the data providerreturns the title of the
   * attribution.
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * sets the title
   */
  public void setTitle( String title )
  {
    this.title = title;
  }

  /**
   * returns the data provider's URL
   */
  public URL getOnlineResource() throws MalformedURLException
  {
    return onlineResource;
  }

  /**
   * sets the data provider's URL
   */
  public void setOnlineResource( URL onlineResource )
  {
    this.onlineResource = onlineResource;
  }

  /**
   * returns the URL of a logo image
   */
  public LogoURL getLogoURL() throws MalformedURLException
  {
    return logoURL;
  }

  /**
   * sets the URL of a logo image
   */
  public void setLogoURL( LogoURL logoURL )
  {
    this.logoURL = logoURL;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "title = " + title + "\n";
    ret += ( "onlineResource = " + onlineResource + "\n" );
    ret += ( "logoURL = " + logoURL + "\n" );
    return ret;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<Attribution>" );

    if( title != null )
    {
      sb.append( "<Title>" ).append( XMLTools.validateCDATA( title ) ).append( "</Title>" );
    }

    if( onlineResource != null )
    {
      sb.append( "<OnlineResource " ).append( "xmlns:xlink=\"http://www.w3.org/1999/xlink\" " )
          .append( "xlink:type=\"simple\" xlink:href=\"" ).append(
              NetWorker.url2String( onlineResource ) ).append( "\"/>" );
    }

    if( logoURL != null )
    {
      sb.append( ( (Marshallable)logoURL ).exportAsXML() );
    }

    sb.append( "</Attribution>" );

    return sb.toString();
  }
}