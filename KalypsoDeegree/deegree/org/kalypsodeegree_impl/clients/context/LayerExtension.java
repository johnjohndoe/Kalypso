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

import org.deegree.xml.Marshallable;

/**
 * provides additional information about a layer described in a web map context
 * document. Additional description is not requiered so an instance of
 * <tt>org.deegree_impl.clients.context.Layer</tt> may doesn't provide an
 * instance of this class.
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class LayerExtension implements Marshallable
{
  private DataService dataService = null;

  private boolean masterLayer = true;

  /**
   * Creates a new LayerExtension object.
   * 
   * @param dataService
   *          description of the service/server behind a WMS layer
   * @param masterLayer
   *          true if a layer is one of the main layers of an application; false
   *          if it just provides background or additional informations.
   */
  public LayerExtension( DataService dataService, boolean masterLayer )
  {
    setDataService( dataService );
    setMasterLayer( masterLayer );
  }

  /**
   * returns a description of the service/server behind a WMS layer. The
   * returned value will be <tt>null</tt> if the WMS uses an internal
   * mechanism to access a layers data.
   * 
   * @return instance of <tt>DataService</tt>
   */
  public DataService getDataService()
  {
    return this.dataService;
    //return null;
  }

  /**
   * sets a description of the service/server behind a WMS layer. The returned
   * value will be <tt>null</tt> if the WMS uses an internal mechanism to
   * access a layers data.
   * 
   * @param dataService
   */
  public void setDataService( DataService dataService )
  {
    this.dataService = dataService;
  }

  /**
   * returns true if a layer is one of the main layers of an application;
   * returns false if it just provides background or additional informations.
   * 
   * @return
   */
  public boolean isMasterLayer()
  {
    return masterLayer;
  }

  /**
   * set to true if a layer is one of the main layers of an application; set to
   * false if it just provides background or additional informations.
   * 
   * @param masterLayer
   */
  public void setMasterLayer( boolean masterLayer )
  {
    this.masterLayer = masterLayer;
  }

  /**
   * 
   * 
   * @return
   */
  public String exportAsXML()
  {
    return null;
  }
}