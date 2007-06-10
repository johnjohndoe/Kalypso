/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.geom.Area;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ColorModelIntervalSingleton;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitable;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitor;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * Provide display mechanism for asc terrain elevation model
 * 
 * @author Madanagopal
 * @author Patrice Congo
 */
public class SurfacePatchVisitableDisplayElement implements DisplayElementDecorator, SurfacePatchVisitor
{
  private IElevationColorModel colorModel;

  private final NativeTerrainElevationModelWrapper m_elevationModel;

  private boolean isHighlighted = false;

  private Graphics graphics = null;

  private boolean isSelected = false;

  private SurfacePatchVisitable/* ASCTerrainElevationModel */ascElevationModel;

  private final Symbolizer defaultSymbolizer = new PolygonSymbolizer_Impl();

  private DisplayElement m_decorated;

  private GeoTransform projection;

  public SurfacePatchVisitableDisplayElement( final NativeTerrainElevationModelWrapper elevationModel )
  {
    Assert.throwIAEOnNullParam( elevationModel, "elevationModel" );

    m_elevationModel = elevationModel;
    final IElevationProvider elevationProvider = elevationModel.getElevationProvider();
    // TODO continue
    if( elevationProvider instanceof SurfacePatchVisitable )
    {
      ascElevationModel = (SurfacePatchVisitable) elevationProvider;

      colorModel = ElevationColorControl.getColorModel( elevationProvider.getMinElevation(), elevationProvider.getMaxElevation() );
      ColorModelIntervalSingleton.getInstance().setInterval( colorModel.getDiscretisationInterval() );
    }
    else
    {
      throw new RuntimeException( "Can only handle asc ele model:" + elevationProvider );
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#doesScaleConstraintApply(double)
   */
  public boolean doesScaleConstraintApply( final double scale )
  {
    if( m_decorated != null )
    {
      return m_decorated.doesScaleConstraintApply( scale );
    }
    else
    {
      return false;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#getAssociateFeatureId()
   */
  public String getAssociateFeatureId( )
  {
    if( m_decorated != null )
    {
      return m_decorated.getAssociateFeatureId();
    }
    else
    {
      return m_elevationModel.getGmlID();
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#getFeature()
   */
  public Feature getFeature( )
  {
    if( m_decorated != null )
    {
      return m_decorated.getFeature();
    }
    else
    {
      return m_elevationModel.getFeature();
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#isHighlighted()
   */
  public boolean isHighlighted( )
  {
    if( m_decorated != null )
    {
      return m_decorated.isHighlighted();
    }
    else
    {
      return this.isHighlighted;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#isSelected()
   */
  public boolean isSelected( )
  {
    if( m_decorated != null )
    {
      return m_decorated.isSelected();
    }
    else
    {
      return this.isSelected;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#setHighlighted(boolean)
   */
  public void setHighlighted( final boolean highlighted )
  {
    if( m_decorated != null )
    {
      m_decorated.setHighlighted( highlighted );
    }
    this.isHighlighted = highlighted;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#setSelected(boolean)
   */
  public void setSelected( final boolean selected )
  {
    if( m_decorated != null )
    {
      m_decorated.setSelected( selected );
    }
    this.isSelected = selected;
  }

  public void paint( final Graphics g, @SuppressWarnings("hiding")
  final GeoTransform projection )
  {
    if( m_decorated != null )
    {
      m_decorated.paint( g, projection );
    }
    try
    {
      // TODO Patrice try to get the current paint box
      this.graphics = g;
      this.projection = projection;
      ascElevationModel.acceptSurfacePatches( projection.getSourceRect(), this );

    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      this.graphics = null;
      this.projection = null;
    }
  }

  /**
   * calculates the Area (image or screen coordinates) where to draw the surface.
   */
  private Area calcTargetCoordinates( @SuppressWarnings("hiding")
  final GeoTransform projection, final GM_Surface surface ) throws Exception
  {
    final PolygonSymbolizer sym = getSymbolizer();
    final Stroke stroke = sym.getStroke();
    float width = 1;
    if( stroke != null )
    {
      width = (float) stroke.getWidth( getFeature() );
    }

    try
    {
      final GM_SurfacePatch patch = surface.getSurfacePatchAt( 0 );
      final GM_Position[] ex = patch.getExteriorRing();
      final GM_Position[][] inner = patch.getInteriorRings();

      final Area areaouter = areaFromRing( projection, width, ex );
      if( inner != null )
      {
        for( final GM_Position[] innerRing : inner )
        {
          if( innerRing != null )
            areaouter.subtract( areaFromRing( projection, width, innerRing ) );
        }
      }

      return areaouter;
    }
    catch( final Exception e )
    {
      Debug.debugException( e, "" );
    }

    return null;
  }

  public static final Area areaFromRing( final GeoTransform projection, final float width, final GM_Position[] ex )
  {
    final int[] x = new int[ex.length];
    final int[] y = new int[ex.length];

    int k = 0;
    for( final GM_Position element : ex )
    {
      final GM_Position position = projection.getDestPoint( element );
      final int xx = (int) (position.getX() + 0.5);
      final int yy = (int) (position.getY() + 0.5);

      if( k > 0 && k < ex.length - 1 )
      {
        if( distance( xx, yy, x[k - 1], y[k - 1] ) > width )
        {
          x[k] = xx;
          y[k] = yy;
          k++;
        }
      }
      else
      {
        x[k] = xx;
        y[k] = yy;
        k++;
      }
    }

    final Polygon polygon = new Polygon( x, y, k - 1 );
    return new Area( polygon );
  }

  public static final double distance( final double x1, final double y1, final double x2, final double y2 )
  {
    return Math.sqrt( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) );
  }

  private PolygonSymbolizer getSymbolizer( )
  {
    m_decorated.doesScaleConstraintApply( 0 );
    return (PolygonSymbolizer) defaultSymbolizer;
  }

  public static final SurfacePatchVisitableDisplayElement createDisplayElement( final Feature feature )
  {
    if( feature == null )
    {
      return null;
    }
    final ITerrainElevationModel terrainElevationModel = (ITerrainElevationModel) feature.getAdapter( ITerrainElevationModel.class );
    if( terrainElevationModel instanceof NativeTerrainElevationModelWrapper )
    {
      return new SurfacePatchVisitableDisplayElement( (NativeTerrainElevationModelWrapper) terrainElevationModel );
    }
    else
    {
      System.out.println( "Could not adapt to a native terrain ele model:" + feature );
      return null;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator#getDecorated()
   */
  public DisplayElement getDecorated( )
  {
    return m_decorated;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator#setDecorated(org.kalypsodeegree.graphics.displayelements.DisplayElement)
   */
  public void setDecorated( final DisplayElement decorated )
  {
    m_decorated = decorated;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitor#accept(org.kalypsodeegree.model.geometry.GM_Surface,
   *      double)
   */
  public boolean visit( final GM_Surface surface, final double elevationSample )
  {
    paintThisSurface( surface, elevationSample );
    return true;
  }

  public GM_Position[] getGM_PositionForThisSurface( final GM_Surface surface )
  {

    GM_Position[] pos = null;
    GM_SurfacePatch patch;
    try
    {
      patch = surface.getSurfacePatchAt( 0 );
      pos = patch.getExteriorRing();
    }
    catch( final GM_Exception e1 )
    {
      e1.printStackTrace();
    }
    return pos;
  }

  private void paintThisSurface( final GM_Surface _surface, final double elevation )
  {
    try
    {
      final Area area = calcTargetCoordinates( this.projection, _surface );
      graphics.setColor( colorModel.getColor( elevation ) );
      ((Graphics2D) graphics).fill( area );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }
}
