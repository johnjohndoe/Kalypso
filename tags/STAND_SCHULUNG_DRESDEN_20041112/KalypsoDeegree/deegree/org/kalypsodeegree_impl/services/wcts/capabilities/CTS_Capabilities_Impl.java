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
package org.deegree_impl.services.wcts.capabilities;

import org.deegree.services.capabilities.Service;
import org.deegree.services.wcts.capabilities.CTS_Capabilities;
import org.deegree.services.wcts.capabilities.Capability;
import org.deegree_impl.services.capabilities.OGCWebServiceCapabilities_Impl;

/**
 * The CTS_Capabilities are the fundamental elements of the capability
 * documents. It contains two elements of which the first one serves the general
 * descritpion of the service and the second one desribes the operation which
 * the server can transact.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version 2002-07-10
 */
public class CTS_Capabilities_Impl extends OGCWebServiceCapabilities_Impl implements
    CTS_Capabilities
{
  private Capability capability = null;

  /**
   * constructor initializing the class with the <CTS_Capabilites>
   */
  CTS_Capabilities_Impl( String version, String updateSequence, Service service,
      Capability capability )
  {
    super( version, updateSequence, service );
    setCapability( capability );
  }

  /**
   * gets the capability
   */
  public Capability getCapability()
  {
    return capability;
  }

  /**
   * sets the capability
   */
  public void setCapability( Capability capability )
  {
    this.capability = capability;
  }

  /**
   * returns an XML representation of the capabilities
   *  
   */
  public String exportAsXML()
  {
    return null;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    return exportAsXML();
  }
}