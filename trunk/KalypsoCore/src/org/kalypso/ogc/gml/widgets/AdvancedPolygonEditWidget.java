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
package org.kalypso.ogc.gml.widgets;

import java.awt.Graphics;
import java.util.Map;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.widgets.tools.ISnappedPoint;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Dirk Kuch
 */
public class AdvancedPolygonEditWidget extends AbstractKeyListenerWidget implements IAdvancedEditWidget
{ 
  private enum EDIT_MODE
  {
    eMulti,
    eSingle;
  }

  private final EDIT_MODE m_mode = EDIT_MODE.eMulti;

  private final AdvancedEditModeMultiDelegate m_multi;

  private Point m_originPoint = null;

  private ISnappedPoint[] m_snappedPointsAtOrigin = null;

  private final IAdvancedEditWidgetDataProvider m_provider;

  public AdvancedPolygonEditWidget( final IAdvancedEditWidgetDataProvider provider )
  {
    super( "Editiere st‰dtebauliche Elemente" );
    m_provider = provider;

    m_multi = new AdvancedEditModeMultiDelegate( this, provider );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    paintToolTip( g );

    if( EDIT_MODE.eMulti.equals( m_mode ) )
    {
      m_multi.paint( g );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final java.awt.Point p )
  {
    final GM_Point gmp = getCurrentGmPoint();
    if( gmp == null )
      return;

    try
    {
      m_originPoint = (Point) JTSAdapter.export( gmp );

      final Feature[] features = m_provider.query( gmp, 20 );
      if( ArrayUtils.isEmpty( features ) )
        return;

      // highlight detected feature points
      final Map<Geometry, Feature> mapGeometries = m_provider.resolveJtsGeometries( features );

      m_snappedPointsAtOrigin = m_multi.resolveSnapPoints( mapGeometries );
    }
    catch( final GM_Exception e )
    {
      KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( final java.awt.Point p )
  {
    m_originPoint = null;
    m_snappedPointsAtOrigin = null;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#getToolTip()
   */
  @Override
  public String getToolTip( )
  {
    if( EDIT_MODE.eMulti.equals( m_mode ) )
      return "Editiermodus: Gemeinsames verschieben";
    else if( EDIT_MODE.eSingle.equals( m_mode ) )
      return "Editiermodus: Einzelnes verschieben";
    
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IAdvancedEditWidget#getOriginPoint()
   */
  @Override
  public Point getOriginPoint( )
  {
    return m_originPoint;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IAdvancedEditWidget#getSnappedPointsAtOrigin()
   */
  @Override
  public ISnappedPoint[] getSnappedPointsAtOrigin( )
  {
    return m_snappedPointsAtOrigin;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IAdvancedEditWidget#getIMapPanel()
   */
  @Override
  public IMapPanel getIMapPanel( )
  {
    return getMapPanel();
  }
}
