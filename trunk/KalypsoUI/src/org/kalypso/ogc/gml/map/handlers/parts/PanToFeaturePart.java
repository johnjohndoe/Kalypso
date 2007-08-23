/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ogc.gml.map.handlers.parts;

import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ui.editor.mapeditor.actiondelegates.WidgetActionPart;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * This part pans to a feature.
 * 
 * @author Holger Albert
 */
public class PanToFeaturePart
{
  /**
   * A view part containing a map panel.
   */
  private IWorkbenchPart m_part;

  /**
   * The constructor.
   * 
   * @param part
   *            A view part containing a map panel.
   */
  public PanToFeaturePart( IWorkbenchPart part )
  {
    m_part = part;
  }

  /**
   * This function changes the extent of the map panel, so that it centers the centroid if the first geometry of the
   * given feature.
   * 
   * @param Feature
   *            The feature to center to.
   */
  public void panTo( Feature feature )
  {
    /* Get the map panel. */
    MapPanel mapPanel = (MapPanel) m_part.getAdapter( MapPanel.class );
    if( mapPanel == null )
      return;

    /* Get all geometrys. */
    GM_Object[] geometrys = feature.getGeometryProperties();

    /* If the feature has no geometrys, there is nothing to do. */
    if( geometrys.length == 0 )
      return;

    /* If there are some geometrys, zoom to the first one. */
    GM_Object geometry = geometrys[0];

    /* The center point. */
    GM_Point centroid = geometry.getCentroid();

    /* The extent of the map panel. */
    GM_Envelope boundingBox = mapPanel.getBoundingBox();

    /* Get the new paned bounding box to the centroid of the geometry. */
    GM_Envelope paned = boundingBox.getPaned( centroid );

    /* Finally set the bounding box. */
    WidgetActionPart part = new WidgetActionPart( m_part );
    part.postCommand( new ChangeExtentCommand( mapPanel, paned ), null );
  }
}