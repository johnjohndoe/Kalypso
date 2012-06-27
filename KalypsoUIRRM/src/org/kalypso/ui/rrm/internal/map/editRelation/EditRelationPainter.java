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
package org.kalypso.ui.rrm.internal.map.editRelation;

import java.awt.Graphics;
import java.awt.Point;
import java.net.URL;

import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.advanced.utils.SLDPainter2;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Paints the edited relation
 * 
 * @author Gernot Belger
 */
public class EditRelationPainter
{
  private final SLDPainter2 m_centerPainter = new SLDPainter2( new URL[] { getClass().getResource( "pointHover.sld" ) } ); //$NON-NLS-1$

  private final SLDPainter2 m_centerErrorPainter = new SLDPainter2( new URL[] { getClass().getResource( "pointHoverError.sld" ) } ); //$NON-NLS-1$

  private final SLDPainter2 m_arrowPainter = new SLDPainter2( new URL[] { getClass().getResource( "arrowHover.sld" ) } ); //$NON-NLS-1$

  private final SLDPainter2 m_arrowErrorPainter = new SLDPainter2( new URL[] { getClass().getResource( "arrowHoverError.sld" ) } ); //$NON-NLS-1$

  private final ToolTipRenderer m_sourceTooltip = ToolTipRenderer.createStandardTooltip();

  private final ToolTipRenderer m_targetTooltip = ToolTipRenderer.createStandardTooltip();

  private final ToolTipRenderer m_errorTooltip = ToolTipRenderer.createErrorTooltip();

  private final EditRelationData m_data;

  public EditRelationPainter( final EditRelationData data )
  {
    m_data = data;
  }

  public void paint( final Graphics g, final IMapPanel mapPanel )
  {
    if( mapPanel == null )
      return;

    final Feature sourceFeature = m_data.getSourceFeature();
    final Feature targetFeature = m_data.getTargetFeature();

    final String error = validateRelation( sourceFeature, targetFeature );

    final GM_Point sourceCenter = EditRelationUtils.getCenter( sourceFeature );
    final GM_Point targetCenter = EditRelationUtils.getCenter( targetFeature );

    final GeoTransform projection = mapPanel.getProjection();
    paintCenter( g, projection, sourceCenter, error != null );
    paintCenter( g, projection, targetCenter, error != null );
    final GM_Curve arrow = paintArrow( g, projection, sourceCenter, targetCenter, error );
    paintTooltip( g, projection, sourceFeature, sourceCenter, m_sourceTooltip );
    paintTooltip( g, projection, targetFeature, targetCenter, m_targetTooltip );
    if( arrow != null )
      paintError( g, projection, error, arrow.getCentroid() );
  }

  private String validateRelation( final Feature sourceFeature, final Feature targetFeature )
  {
    if( sourceFeature == null || targetFeature == null )
      return null;

    final IEditRelationType relation = m_data.getRelation();
    if( relation == null )
      return null;

    final EditRelationMode mode = m_data.getModificationMode();
    return relation.validate( sourceFeature, targetFeature, mode );
  }

  private void paintCenter( final Graphics g, final GeoTransform projection, final GM_Point center, final boolean isError )
  {
    if( center == null )
      return;

    if( isError )
      m_centerErrorPainter.paint( g, projection, center );
    else
      m_centerPainter.paint( g, projection, center );
  }

  private GM_Curve paintArrow( final Graphics g, final GeoTransform projection, final GM_Point sourceCenter, final GM_Point targetCenter, final String error )
  {
    if( sourceCenter == null || targetCenter == null )
      return null;

    try
    {
      final String srs = sourceCenter.getCoordinateSystem();
      final GM_Position[] points = new GM_Position[] { sourceCenter.getPosition(), targetCenter.getPosition() };
      final GM_Curve arrow = GeometryFactory.createGM_Curve( points, srs );
      if( error != null )
        m_arrowErrorPainter.paint( g, projection, arrow );
      else
        m_arrowPainter.paint( g, projection, arrow );

      return arrow;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private void paintError( final Graphics g, final GeoTransform projection, final String error, final GM_Point point )
  {
    m_errorTooltip.setTooltip( error );
    if( error == null )
      return;

    final GM_Position position = projection.getDestPoint( point.getPosition() );
    final Point pos = new Point( (int) position.getX(), (int) position.getY() );

    m_errorTooltip.paintToolTip( pos, g, g.getClipBounds() );
  }

  private void paintTooltip( final Graphics g, final GeoTransform projection, final Feature feature, final GM_Point center, final ToolTipRenderer tooltip )
  {
    final String label = feature == null ? null : EditRelationUtils.getFeatureLabel( feature );
    tooltip.setTooltip( label );
    if( label == null )
      return;

    final GM_Position position = projection.getDestPoint( center.getPosition() );
    final Point pos = new Point( (int) position.getX() + 5, (int) position.getY() );

    tooltip.paintToolTip( pos, g, g.getClipBounds() );
  }
}