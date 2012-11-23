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
package org.kalypso.kalypsomodel1d2d.ui.map.dikeditchgen;

import java.awt.Color;
import java.awt.Graphics;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.gml.processes.constDelaunay.ConstraintDelaunayHelper;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizerUtils;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.elevation.ElevationException;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;
import org.kalypsodeegree_impl.graphics.sld.SurfacePolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.model.geometry.GM_MultiCurve_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Stefan Kurzbach
 */
public class TriangulationBuilder extends AbstractModelObject
{
  public static final String PROPERTY_MAX_AREA = "maxArea"; //$NON-NLS-1$

  public static final String PROPERTY_MIN_ANGLE = "minAngle"; //$NON-NLS-1$

  public static final String PROPERTY_NO_STEINER_ON_BOUNDARY = "noSteinerOnBoundary"; //$NON-NLS-1$

  public static final String PROPERTY_BOUNDARY = "boundary"; //$NON-NLS-1$

  public static final String PROPERTY_TIN = "tin"; //$NON-NLS-1$

  private double m_maxArea = 0;

  private double m_minAngle = 22;

  private boolean m_noSteinerOnBoundary = true;

  private boolean m_noSteiner = false;

  private GM_MultiCurve m_breaklines = null;

  private GM_Polygon m_boundaryGeom;

  private GM_TriangulatedSurface m_tin;

  private final SurfacePolygonSymbolizer m_tinSymb = new SurfacePolygonSymbolizer_Impl();

  private final PolygonSymbolizer m_polySymb = new PolygonSymbolizer_Impl();

  private final LineSymbolizer m_lineSymb = new LineSymbolizer_Impl();

  private IMapPanel m_mapPanel;

  public TriangulationBuilder( final IMapPanel mapPanel )
  {
    m_mapPanel = mapPanel;

    final Fill fill = StyleFactory.createFill( new Color( 255, 255, 255 ) );
    fill.setOpacity( 0.0 );
    final Stroke polyStroke = StyleFactory.createStroke( new Color( 255, 20, 20 ) );
    polyStroke.setWidth( 1.0 );
    m_polySymb.setFill( fill );
    m_polySymb.setStroke( polyStroke );

    final Stroke lineStroke = StyleFactory.createStroke( new Color( 20, 255, 20 ) );
    lineStroke.setWidth( 2 );
    lineStroke.setOpacity( 0.5 );
    m_lineSymb.setStroke( lineStroke );
  }

  public double getMaxArea( )
  {
    return m_maxArea;
  }

  public void setMaxArea( final double maxArea )
  {
    setMaxArea( maxArea, true );
  }

  public void setMaxArea( final double maxArea, final boolean buildImmediately )
  {
    final Double oldValue = m_maxArea;
    m_maxArea = maxArea;
    if( buildImmediately )
      finish();
    firePropertyChange( PROPERTY_MAX_AREA, oldValue, maxArea );
  }

  public double getMinAngle( )
  {
    return m_minAngle;
  }

  public void setMinAngle( final double minAngle )
  {
    setMinAngle( minAngle, true );
  }

  public void setMinAngle( final double minAngle, final boolean buildImmediately )
  {
    final double oldValue = m_minAngle;
    m_minAngle = minAngle;
    if( buildImmediately )
      finish();
    firePropertyChange( PROPERTY_MIN_ANGLE, oldValue, minAngle );
  }

  public boolean getNoSteinerOnBoundary( )
  {
    return m_noSteinerOnBoundary;
  }

  public void setNoSteiner( final boolean noSteiner )
  {
    m_noSteiner = noSteiner;
    setNoSteinerOnBoundary( noSteiner, false );
  }

  public void setNoSteinerOnBoundary( final boolean noSteiner )
  {
    setNoSteinerOnBoundary( noSteiner, true );
  }

  public void setNoSteinerOnBoundary( final boolean noSteiner, final boolean buildImmediately )
  {
    final boolean oldValue = m_noSteinerOnBoundary;
    m_noSteinerOnBoundary = noSteiner;
    if( buildImmediately )
      finish();
    else
      m_mapPanel.invalidateMap();
    firePropertyChange( PROPERTY_NO_STEINER_ON_BOUNDARY, oldValue, noSteiner );
  }

  public void addBreakLine( final GM_Curve breakline, final boolean buildImmediately )
  {
    if( m_breaklines == null )
      m_breaklines = new GM_MultiCurve_Impl( breakline.getCoordinateSystem() );
    m_breaklines.add( breakline );
    if( buildImmediately )
      finish();
    else
      m_mapPanel.invalidateMap();
  }

  public GM_MultiCurve getBreaklines( )
  {
    return m_breaklines;
  }

  public GM_Polygon getBoundary( )
  {
    return m_boundaryGeom;
  }

  public void setBoundary( final GM_Polygon boundaryGeom, final boolean buildImmediately )
  {
    m_boundaryGeom = boundaryGeom;
    if( buildImmediately )
      finish();
    else
      m_mapPanel.invalidateMap();
    firePropertyChange( PROPERTY_BOUNDARY, null, m_boundaryGeom );
  }

  public GM_TriangulatedSurface getTin( )
  {
    return m_tin;
  }

  public void finish( )
  {
    if( m_boundaryGeom == null )
      return;

    m_tin = null;
    try
    {
      // FIXME: encapsulate into a triangle.exe wrapper!
      final List<String> args = new ArrayList<>();
      if( m_maxArea > 0 )
        args.add( "-a" + m_maxArea ); //$NON-NLS-1$

      if( m_minAngle > 0 )
        args.add( "-q" + m_minAngle ); //$NON-NLS-1$

      if( m_noSteiner )
        args.add( "-YY" ); //$NON-NLS-1$
      else if( m_noSteinerOnBoundary )
        args.add( "-Y" ); //$NON-NLS-1$

      final GM_Triangle[] triangles = ConstraintDelaunayHelper.createGM_Triangles( m_boundaryGeom, m_breaklines == null ? null : m_breaklines.getAllCurves(), m_boundaryGeom.getCoordinateSystem(), args.toArray( new String[args.size()] ) );
      if( triangles != null && triangles.length > 0 )
      {
        final GM_TriangulatedSurface surface = GeometryFactory.createGM_TriangulatedSurface( triangles, triangles[0].getCoordinateSystem() );
        setTin( surface );
      }
      else
        setTin( null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  public void reset( )
  {
    m_breaklines = null;
    m_boundaryGeom = null;
    m_tin = null;
    m_mapPanel.invalidateMap();
  }

  public void paint( final Graphics g, final GeoTransform projection, IProgressMonitor pm )
  {
    final SubMonitor monitor = SubMonitor.convert( pm );
    monitor.beginTask( StringUtils.EMPTY, IProgressMonitor.UNKNOWN );
    try
    {
      if( m_tin != null && !m_tin.isEmpty() )
      {
        final DisplayElement de = DisplayElementFactory.buildSurfacePolygonDisplayElement( null, m_tin, m_tinSymb );
        de.paint( g, projection, monitor );
        final DisplayElement de2 = DisplayElementFactory.buildPolygonDisplayElement( null, m_tin, m_polySymb );
        de2.paint( g, projection, monitor );
      }

      if( m_boundaryGeom != null )
      {
        final DisplayElement de = DisplayElementFactory.buildPolygonDisplayElement( null, m_boundaryGeom, m_polySymb );
        de.paint( g, projection, monitor );
      }

      if( m_breaklines != null && !m_breaklines.isEmpty() )
      {
        final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( null, m_breaklines, m_lineSymb );
        de.paint( g, projection, monitor );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  public void setTin( final GM_TriangulatedSurface triangulatedSurface )
  {
    try
    {
      final Color fromColor = new Color( 0, 255, 100 );
      final Color toColor = new Color( 200, 20, 20 );
      final BigDecimal min = new BigDecimal( triangulatedSurface.getMinElevation() - 0.2 );
      final BigDecimal max = new BigDecimal( triangulatedSurface.getMaxElevation() + 0.2 );
      final PolygonColorMapEntry fromEntry = StyleFactory.createPolygonColorMapEntry( fromColor, fromColor, min, BigDecimal.ZERO );
      fromEntry.getFill().setOpacity( 0.5 );
      final PolygonColorMapEntry toEntry = StyleFactory.createPolygonColorMapEntry( toColor, toColor, BigDecimal.ZERO, max );
      toEntry.getFill().setOpacity( 0.5 );
      final PolygonColorMap_Impl colorMap = new PolygonColorMap_Impl();
      colorMap.replaceColorMap( PolygonSymbolizerUtils.createColorMap( fromEntry, toEntry, new BigDecimal( 0.2 ), min, max, false ) );
      m_tinSymb.setColorMap( colorMap );
    }
    catch( ElevationException e )
    {
      e.printStackTrace();
    }

    m_tin = triangulatedSurface;
    m_mapPanel.invalidateMap();
    // make sure that equals is not called on tin, always fires an event
    firePropertyChange( PROPERTY_TIN, null, m_tin );
  }
}