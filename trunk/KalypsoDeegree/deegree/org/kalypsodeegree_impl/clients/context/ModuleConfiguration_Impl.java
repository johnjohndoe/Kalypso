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

import org.deegree.clients.context.ModuleConfiguration;
import org.deegree.xml.Marshallable;

/**
 * provides the connection point where to access the configuration of a module
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class ModuleConfiguration_Impl implements ModuleConfiguration, Marshallable
{
  private URL onlineResource = null;

  /**
   * Creates a new ModuleConfiguration_Impl object.
   * 
   * @param onlineResource
   */
  public ModuleConfiguration_Impl( URL onlineResource )
  {
    setOnlineResource( onlineResource );
  }

  /**
   * returns the online resource where to access the configuration document for
   * a module
   * 
   * @return
   */
  public URL getOnlineResource()
  {
    return onlineResource;
  }

  /**
   * sets the online resource where to access the configuration document for a
   * module
   *  
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
    return null;
  }

}