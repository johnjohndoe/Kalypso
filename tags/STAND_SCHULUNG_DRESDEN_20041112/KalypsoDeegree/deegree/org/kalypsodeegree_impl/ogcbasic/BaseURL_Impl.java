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
package org.deegree_impl.ogcbasic;

import java.net.URL;

import org.deegree.ogcbasic.BaseURL;

/**
 * This is the base interface of all capabilities URL-fields. The <Format>
 * element in XXXXURL indicates the MIME type of the resource. The address is
 * represented by the <onlineResource>element.
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version 2002-03-01
 */
public class BaseURL_Impl implements BaseURL
{
  private String format = null;

  private URL onlineResource = null;

  /**
   * constructor initializing the class with the <BaseURL>
   */
  public BaseURL_Impl( String format, URL onlineResource )
  {
    setFormat( format );
    setOnlineResource( onlineResource );
  }

  /**
   * returns the MIME type of the resource
   */
  public String getFormat()
  {
    return format;
  }

  /**
   * sets the MIME type of the resource
   */
  public void setFormat( String format )
  {
    this.format = format;
  }

  /**
   * returns the address (URL) of the resource
   */
  public URL getOnlineResource()
  {
    return onlineResource;
  }

  /**
   * returns the address (URL) of the resource
   */
  public void setOnlineResource( URL onlineResource )
  {
    this.onlineResource = onlineResource;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "format = " + format + "\n";
    ret += ( "onlineResource = " + onlineResource + "\n" );
    return ret;
  }
}