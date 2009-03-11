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
package org.kalypso.ogc.gml.widgets.aew;

import java.awt.Graphics;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.widgets.tools.GeometryPainter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Dirk Kuch
 */
public class AdvancedEditModePointsDelegate implements IAdvancedEditWidgetDelegate
{

  private final IAdvancedEditWidget m_widget;

  private final IAdvancedEditWidgetDataProvider m_provider;

  public AdvancedEditModePointsDelegate( final IAdvancedEditWidget widget, final IAdvancedEditWidgetDataProvider provider )
  {
    m_widget = widget;
    m_provider = provider;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.aew.IAdvancedEditWidgetDelegate#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {

    final GM_Point gmp = m_widget.getCurrentGmPoint();
    if( gmp == null )
      return;

    try
    {
      // highligth existing points
      final Point jtsPoint = (Point) JTSAdapter.export( gmp );

      final Feature[] features = m_provider.query( gmp, getRange() );
      if( ArrayUtils.isEmpty( features ) )
        return;

      /* find existing points */
      final Map<Geometry, Feature> mapGeometries = m_provider.resolveJtsGeometries( features );
      GeometryPainter.highlightPoints( g, m_widget.getIMapPanel(), mapGeometries.keySet().toArray( new Geometry[] {} ), VERTEX );

      /* find underlying geometry */
      final IAdvancedEditWidgetGeometry underlying = findUnderlyingGeometry( mapGeometries, jtsPoint );

    }
    catch( final GM_Exception e )
    {
      KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }

  }

  private IAdvancedEditWidgetGeometry findUnderlyingGeometry( final Map<Geometry, Feature> geometries, final Point point )
  {
    final Set<Entry<Geometry, Feature>> entries = geometries.entrySet();
    for( final Entry<Geometry, Feature> entry : entries )
    {
      if( !entry.getKey().intersection( point ).isEmpty() )
        return new IAdvancedEditWidgetGeometry()
        {
          @Override
          public Point getBasePoint( )
          {
            return point;
          }

          @Override
          public Feature getFeature( )
          {
            return entry.getValue();
          }

          @Override
          public Geometry getUnderlyingGeometry( )
          {
            return entry.getKey();
          }
        };
    }

    return null;
  }

  private double getRange( )
  {
    final IMapPanel mapPanel = m_widget.getIMapPanel();
    return mapPanel.getCurrentScale() * 4;
  }

}
