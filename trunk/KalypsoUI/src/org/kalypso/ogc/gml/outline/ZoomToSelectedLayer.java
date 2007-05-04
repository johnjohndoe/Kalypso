/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.action.IAction;
import org.kalypso.commons.list.IListManipulator;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 */
public class ZoomToSelectedLayer extends MapModellViewActionDelegate
{
  /**
   * @see org.eclipse.ui.actions.ActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  @Override
  public void run( final IAction action )
  {
    final IKalypsoTheme selectedElement = getSelectedTheme();
    
    MapPanel panel = getView().getMapPanel();
    
//    // REMARK: throws a class cast exeption. There should be a better solution!
//    final MapPanel panel  = (MapPanel) listManipulator;
    
    if( panel != null && selectedElement != null )
    {
      final GM_Envelope zoomBox = selectedElement.getBoundingBox();
      
      GM_Envelope wishBBox = null;

      final GM_Position zoomMax = zoomBox.getMax();
      final GM_Position zoomMin = zoomBox.getMin();

      final double newMaxX = zoomMax.getX() + (zoomMax.getX() - zoomMin.getX()) / 20;
      final double newMinX = zoomMin.getX() - (zoomMax.getX() - zoomMin.getX()) / 20;
      
      final double newMaxY = zoomMax.getY() + (zoomMax.getY() - zoomMin.getY()) / 20;
      final double newMinY = zoomMin.getY() - (zoomMax.getY() - zoomMin.getY()) / 20;

      final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, newMinY );
      final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, newMaxY );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );
      
      panel.setBoundingBox( wishBBox );
    }
  }

}
