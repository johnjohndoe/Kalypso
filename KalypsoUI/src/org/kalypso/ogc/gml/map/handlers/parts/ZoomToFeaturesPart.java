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
package org.kalypso.ogc.gml.map.handlers.parts;

import java.util.List;

import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ui.editor.mapeditor.actiondelegates.WidgetActionPart;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * This part zoomes to a bounding box, containing all given features, so that all features could be seen on the map.
 * Furthermore a percent value can be specified, which will be used to increase the extend by exact this value. This is,
 * that a border can be created between the features and the border of the map.
 * 
 * @author Holger Albert
 */
public class ZoomToFeaturesPart
{
  /**
   * The part.
   */
  private IWorkbenchPart m_part;

  /**
   * The list of features, to zoom to.
   */
  private List<Feature> m_features;

  /**
   * This value specifies the amount of the new extend used to increase it for creating a border around the new extend.
   */
  private int m_percent;

  /**
   * The constructor.
   * 
   * @param mapPanel
   *          The map panel.
   * @param features
   *          The list of features, to zoom to.
   * @param percent
   *          This value specifies the amount of the new extend used to increase it for creating a border around the new
   *          extend.
   */
  public ZoomToFeaturesPart( IWorkbenchPart part, List<Feature> features, int percent )
  {
    m_part = part;
    m_features = features;
    m_percent = percent;
  }

  /**
   * This function will start the zoom action (boyaahh!).
   */
  public void zoomTo( )
  {
    if( m_features.size() == 0 )
      return;

    /* This is the envelope containing all features of this list. */
    GM_Envelope envelope = FeatureHelper.getEnvelope( m_features.toArray( new Feature[] {} ) );

    /* Get the positions of the envelope. */
    double min_x = envelope.getMin().getX();
    double min_y = envelope.getMin().getY();
    double max_x = envelope.getMax().getX();
    double max_y = envelope.getMax().getY();

    /* Recalculate the envelope. */
    if( m_percent > 0 )
    {
      /* The dimensions of the envelope. */
      double width = max_x - min_x;
      double height = max_y - min_y;

      /* The border size. */
      double border_x = m_percent * width / 100;
      double border_y = m_percent * height / 100;

      /* Add/Substract it to the positions of the extend. */
      min_x = min_x - border_x;
      min_y = min_y - border_y;
      max_x = max_x + border_x;
      max_y = max_y + border_y;
    }

    /* Create the new positions. */
    GM_Position newMin = GeometryFactory.createGM_Position( min_x, min_y );
    GM_Position newMax = GeometryFactory.createGM_Position( max_x, max_y );

    /* Create the new envelope. */
    GM_Envelope newEnvelope = GeometryFactory.createGM_Envelope( newMin, newMax );

    MapPanel mapPanel = (MapPanel) m_part.getAdapter( MapPanel.class );
    if( mapPanel == null )
      return;

    /* Finally set the bounding box. */
    WidgetActionPart part = new WidgetActionPart( m_part );
    part.postCommand( new ChangeExtentCommand( mapPanel, newEnvelope ), null );
  }
}