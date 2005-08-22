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
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.command.SelectFeaturesCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

public abstract class AbstractSelectWidget extends AbstractWidget
{
  /*
   * 
   * @author doemming
   */
  public AbstractSelectWidget( String name, String toolTip )
  {
    super( name, toolTip );

  }

  /**
   * pixel coordinates
   */
  private Point m_endPoint = null;

  /**
   * pixel coordinates
   */
  private Point m_startPoint = null;

  /**
   * radius in pixel
   */
  private int m_radius = 20;

  abstract int getSelectionMode();

  abstract boolean allowOnlyOneSelectedFeature();

  public void dragged( Point p )
  {
    if( m_startPoint == null )
    {
      m_startPoint = p;
      m_endPoint = null;
    }
    else
      m_endPoint = p;
  }

  public void leftPressed( Point p )
  {
    m_startPoint = p;
    m_endPoint = null;
  }

  public void leftReleased( Point p )
  {
    if( m_endPoint != null ) // last update of endPoint
      m_endPoint = p;
    else
      m_startPoint = p;
    select();
  }

  public void paint( Graphics g )
  {
    if( m_startPoint != null && m_endPoint != null )
    {
      int px = (int)( m_startPoint.getX() < m_endPoint.getX() ? m_startPoint.getX() : m_endPoint.getX() );
      int py = (int)( m_startPoint.getY() < m_endPoint.getY() ? m_startPoint.getY() : m_endPoint.getY() );
      int dx = (int)Math.abs( m_endPoint.getX() - m_startPoint.getX() );
      int dy = (int)Math.abs( m_endPoint.getY() - m_startPoint.getY() );

      if( dx != 0 && dy != 0 )
        g.drawRect( px, py, dx, dy );
    }
  }

  public void perform()
  {
  // nothing
  }

  private void select()
  {
    // TODO: sollte diese ganze umrechnerei nicht einfach die view machen???
    final MapPanel mapPanel = getMapPanel();
    final GeoTransform transform = mapPanel.getProjection();

    final IKalypsoTheme activeTheme = getActiveTheme();
    if( activeTheme == null || !( activeTheme instanceof IKalypsoFeatureTheme ) )
    {
      m_startPoint = null;
      m_endPoint = null;
      return;
    }
    if( m_startPoint != null )
    {
      double g1x = transform.getSourceX( m_startPoint.getX() );
      double g1y = transform.getSourceY( m_startPoint.getY() );

      if( m_endPoint == null ) // not dragged
      {
        // TODO depend on featuretype
        // line and point with radius
        // polygon with without radius
        double gisRadius = Math.abs( transform.getSourceX( m_startPoint.getX() + m_radius ) - g1x );

        JMSelector selector = new JMSelector( getSelectionMode() );

        GM_Point pointSelect = GeometryFactory
            .createGM_Point( g1x, g1y, mapPanel.getMapModell().getCoordinatesSystem() );

        final Feature fe = selector.selectNearest( pointSelect, gisRadius, ( (IKalypsoFeatureTheme)activeTheme )
            .getFeatureListVisible( null ), false, activeTheme.getSelectionManager() );

        final List listFe = new ArrayList();
        if( fe != null )
          listFe.add( fe );
        //List listFe = selector.select( pointSelect, activeTheme,
        // mapPanel.getSelectionID() );
        if( !listFe.isEmpty() )
          fireCommand( listFe, (IKalypsoFeatureTheme)activeTheme );
      }
      else
      // dragged
      {
        double g2x = transform.getSourceX( m_endPoint.getX() );
        double g2y = transform.getSourceY( m_endPoint.getY() );
        boolean withinStatus = false;

        if( m_endPoint.getX() > m_startPoint.getX() && m_endPoint.getY() > m_startPoint.getY() )
          withinStatus = true;

        double minX = g1x < g2x ? g1x : g2x;
        double maxX = g1x > g2x ? g1x : g2x;
        double minY = g1y < g2y ? g1y : g2y;
        double maxY = g1y > g2y ? g1y : g2y;

        if( minX != maxX && minY != maxY )
        {
          final JMSelector selector = new JMSelector( getSelectionMode() );
          GM_Envelope envSelect = GeometryFactory.createGM_Envelope( minX, minY, maxX, maxY );
          List features = selector.select( envSelect, ( (IKalypsoFeatureTheme)activeTheme )
              .getFeatureListVisible( null ), withinStatus, activeTheme.getSelectionManager() );
          if( !features.isEmpty() )
            fireCommand( features, (IKalypsoFeatureTheme)activeTheme );
        }
      }
    }
    m_startPoint = null;
    m_endPoint = null;
  }

  private void fireCommand( final List features, final IKalypsoFeatureTheme activeTheme )
  {
    ICommand command = null;
    if( allowOnlyOneSelectedFeature() )
    {
      final Feature fe = (Feature)features.get( 0 );
      command = new SelectFeaturesCommand( activeTheme.getWorkspace(), fe, activeTheme.getSelectionManager(),ModellEvent.SELECTION_CHANGED );
    }
    else
      command = new SelectFeaturesCommand( activeTheme.getWorkspace(), (Feature[])features
          .toArray( new Feature[features.size()] ), activeTheme.getSelectionManager(), ModellEvent.SELECTION_CHANGED );
    //      command = new JMMarkSelectCommand( activeTheme.getWorkspace(), features, getSelectionMode() );

    postViewCommand( command, null );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish()
  {
    m_startPoint = null;
    m_endPoint = null;
    super.finish();
  }
}