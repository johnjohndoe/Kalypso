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

import java.net.URL;

import org.deegree.xml.Marshallable;

/**
 * Implements the description of the access to directories used by a deegree web
 * map context
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class DirectoryAccess implements Marshallable
{

  private URL onlineResource = null;

  private String directoryName = null;

  /**
   * @param directoryName
   * @param onlineResource
   */
  DirectoryAccess( String directoryName, URL onlineResource )
  {
    this.directoryName = directoryName;
    this.onlineResource = onlineResource;
  }

  /**
   * @return Returns the directoryName.
   */
  public String getDirectoryName()
  {
    return directoryName;
  }

  /**
   * @param directoryName
   *          The directoryName to set.
   */
  public void setDirectoryName( String directoryName )
  {
    this.directoryName = directoryName;
  }

  /**
   * @return Returns the onlineResource.
   */
  public URL getOnlineResource()
  {
    return onlineResource;
  }

  /**
   * @param onlineResource
   *          The onlineResource to set.
   */
  public void setOnlineResource( URL onlineResource )
  {
    this.onlineResource = onlineResource;
  }

  /**
   * @see org.deegree.xml.Marshallable#exportAsXML()
   */
  public String exportAsXML()
  {
    // TODO Auto-generated method stub
    return null;
  }

}