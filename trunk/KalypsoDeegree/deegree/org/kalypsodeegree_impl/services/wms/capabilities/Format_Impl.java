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

import java.net.URL;

import org.deegree.services.wms.capabilities.Format;

/**
 * describes the format a WMS can serve for GetMap, GetFeatureInfo, GetStyle
 * etc. requests. Each request describes its own list of possible formats.
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class Format_Impl implements Format
{
  private String name = null;

  private URL url = null;

  /**
   * Creates a new Format_Impl object.
   * 
   * @param name
   *          name of the format
   *  
   */
  public Format_Impl( String name )
  {
    this( name, null );
  }

  /**
   * Creates a new Format_Impl object.
   * 
   * @param name
   *          name of the format
   * @param url
   *          <tt>URL</tt> from where the filter for transforming a
   *          GetFeatureInfo response can be accessed
   */
  public Format_Impl( String name, URL url )
  {
    this.name = name;
    this.url = url;
  }

  /**
   * returns the URL where a XSLT-script for filtering the result of a
   * GetFeatureInfo request can be accessed. For any other formats then those of
   * a GetFeatureInfo response the method returns null.
   * 
   * @return
   */
  public URL getFilter()
  {
    return url;
  }

  /**
   * returns the name of the format
   * 
   * @return
   */
  public String getName()
  {
    return name;
  }
}