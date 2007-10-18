/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.sobek.core.utils;

import nl.wldelft.fews.pi.LocationComplexType;
import nl.wldelft.fews.pi.ObjectFactory;
import nl.wldelft.fews.pi.LocationsComplexType.Location;

import org.kalypso.model.wspm.sobek.core.interfaces.IAbstractConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ILinkageNode;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.model.BoundaryNode;
import org.kalypso.model.wspm.sobek.core.model.LinkageNode;

/**
 * @author thuel2
 */
public class PiSobekModelUtils
{
  public static String SOBEK_NODES_CONNECTION = "Sobek.Nodes.Connection";

  public static String SOBEK_NODES_CONN_WITH_LAT_AND_STOR = "Sobek.Nodes.ConnWithLatAndStor";

  public static String SOBEK_NODES_BOUNDARY_Q = "Sobek.Nodes.Bound_Q";

  public static String SOBEK_NODES_BOUNDARY_W = "Sobek.Nodes.Bound_H";

  public static String SOBEK_NODES_LINKAGE = "Sobek.Nodes.Linkage";

  public static Location createLocationFromNode( ObjectFactory factory, INode node )
  {
    final LocationComplexType createLocationComplexType = factory.createLocationComplexType();
    final Location location = factory.createLocationsComplexTypeLocation();

    location.setLocationId( node.getId() );
    location.setStationName( node.getName() );
    location.setX( node.getLocation().getX() );
    location.setY( node.getLocation().getY() );

    if( node instanceof IAbstractConnectionNode )
    {
      if( node instanceof IConnectionNode )
      {
        location.setLocationType( SOBEK_NODES_CONNECTION );
      }
      else if( node instanceof ILinkageNode )
      {
        final LinkageNode ln = (LinkageNode)node;
        location.setLocationType( SOBEK_NODES_LINKAGE + "@" + ln.getLinkToBranch().getFeature().getId());
      }
      else if( node instanceof IBoundaryNode )
      {
// differenzieren zw. Boundary_H und Boundary_W
        BoundaryNode bn = (BoundaryNode) node;
       
      }

    }
    return location;
  }
}
