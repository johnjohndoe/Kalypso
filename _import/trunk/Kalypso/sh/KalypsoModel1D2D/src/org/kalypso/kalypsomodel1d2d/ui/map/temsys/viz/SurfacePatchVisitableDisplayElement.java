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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.geom.Area;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.HMOTerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitable;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitor;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.TriangleDivider;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
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
  private IElevationColorModel/* SimpleElevationColorModel */colorModel;

  private NativeTerrainElevationModelWrapper m_elevationModel;

  private boolean isHighlighted = false;

  private Graphics graphics = null;

  private boolean isSelected = false;

  private SurfacePatchVisitable/* ASCTerrainElevationModel */ascElevationModel;

  private Symbolizer defaultSymbolizer = new PolygonSymbolizer_Impl();

  private DisplayElement m_decorated;

  private GeoTransform projection;

  private GM_Position[] ex;
  private TriangleDivider divider;

  private ArrayList<GM_Position[]> triangleList;

  private List<GM_Surface> name;
//  List<GM_Surface> toDivide = new ArrayList<GM_Surface>();
//  List<GM_Surface> notToDivide = new ArrayList<GM_Surface>();

  private ListRetriever _listRetriver;

  public SurfacePatchVisitableDisplayElement( NativeTerrainElevationModelWrapper elevationModel )
  {
    triangleList = new ArrayList<GM_Position[]>();
    Assert.throwIAEOnNullParam( elevationModel, "elevationModel" );
    m_elevationModel = elevationModel;
    IElevationProvider elevationProvider = elevationModel.getElevationProvider();
    // TODO continue
    if( elevationProvider instanceof SurfacePatchVisitable )
    {
      ascElevationModel = (SurfacePatchVisitable) elevationProvider;

      colorModel = ElevationColorControl.getColorModel( elevationProvider.getMinElevation(), elevationProvider.getMaxElevation() );
    }
    else
    {
      throw new RuntimeException( "Can only handle asc ele model:" + elevationProvider );
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#doesScaleConstraintApply(double)
   */
  public boolean doesScaleConstraintApply( double scale )
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
  public void setHighlighted( boolean highlighted )
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
  public void setSelected( boolean selected )
  {
    if( m_decorated != null )
    {
      m_decorated.setSelected( selected );
    }
    this.isSelected = selected;
  }

  public void paint( Graphics g, @SuppressWarnings("hiding")
  GeoTransform projection )
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

      ascElevationModel.acceptSurfacePatches( projection.getSourceRect(),// ascElevationModel.getBoundingBox(),
      this );

    }
    catch( GM_Exception e )
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
  GeoTransform projection, GM_Surface surface ) throws Exception
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
        for( GM_Position[] innerRing : inner )
        {
          if( innerRing != null )
            areaouter.subtract( areaFromRing( projection, width, innerRing ) );
        }
      }

      return areaouter;
    }
    catch( Exception e )
    {
      Debug.debugException( e, "" );
    }

    return null;
  }

  private Area areaFromRing( @SuppressWarnings("hiding")
  GeoTransform projection, float width, final GM_Position[] ex )
  {
    final int[] x = new int[ex.length];
    final int[] y = new int[ex.length];

    int k = 0;
    for( int i = 0; i < ex.length; i++ )
    {
      GM_Position position = projection.getDestPoint( ex[i] );
      int xx = (int) (position.getX() + 0.5);
      int yy = (int) (position.getY() + 0.5);

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

  private double distance( double x1, double y1, double x2, double y2 )
  {
    return Math.sqrt( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) );
  }

  /**
   * renders one surface to the submitted graphic context considering the also submitted projection
   */
  private void drawPolygon( Graphics gg, Area area ) throws FilterEvaluationException
  {
    Graphics2D g2d = (Graphics2D) gg;
    PolygonSymbolizer sym = getSymbolizer();
    org.kalypsodeegree.graphics.sld.Fill fill = sym.getFill();
    org.kalypsodeegree.graphics.sld.Stroke stroke = sym.getStroke();

    final Feature feature = getFeature();
    // only draw filled polygon, if Fill-Element is given
    if( fill != null )
    {
      double opacity = fill.getOpacity( feature );
      // is completly transparent
      // if not fill polygon
      if( opacity > 0.01 )
      {
        Color color = fill.getFill( feature );
        int alpha = (int) Math.round( opacity * 255 );
        int red = color.getRed();
        int green = color.getGreen();
        int blue = color.getBlue();

        try
        {

          g2d.fill( area );
        }
        catch( Exception e )
        {
          //  
        }
      }
    }
  }

  private PolygonSymbolizer getSymbolizer( )
  {
    m_decorated.doesScaleConstraintApply( 0 );
    return (PolygonSymbolizer) defaultSymbolizer;
  }

  public static final SurfacePatchVisitableDisplayElement createDisplayElement( Feature feature )
  {
    if( feature == null )
    {
      return null;
    }
    ITerrainElevationModel terrainElevationModel = (ITerrainElevationModel) feature.getAdapter( ITerrainElevationModel.class );
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
  public void setDecorated( DisplayElement decorated )
  {
    m_decorated = decorated;
  }



  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitor#accept(org.kalypsodeegree.model.geometry.GM_Surface,
   *      double)
   */
  public boolean visit( GM_Surface surface/*, double elevationSample */)
  { 
//    _listRetriver = ListRetriever.getInstance();
//    GM_SurfacePatch patch = null;  
//    //divider = new TriangleDivider();
//    try
//    {
//      patch = surfacePatch.getSurfacePatchAt( 0 );
//      ex = patch.getExteriorRing();    
//    }   
//    catch( GM_Exception e1 )
//    {
//      // TODO Auto-generated catch block
//      e1.printStackTrace();
//    }
//    
////    if (!_listRetriver.getListManager().containsKey( patch )) {
////      name = divider.visitThisDivisionSurface(ex);
////      _listRetriver.getListManager().put( patch, name );
////    }
////    else {
////      name = _listRetriver.getMyGM_SurfacePatch( patch );  
////    } 
//    
//    name = divider.visitThisDivisionSurface(ex);
//    
//    for (GM_Surface nam:name) {
//      paintThisSurface( nam, divider.calculateCenterCoOrdinate( nam ).getZ()  );
//    }
    
    paintThisSurface(surface);
    return true;
  }
  
  
  public GM_Position[] getGM_PositionForThisSurface(GM_Surface surface) {
    
    GM_Position[] pos = null;
    GM_SurfacePatch patch;    
    try
    {
      patch = surface.getSurfacePatchAt( 0 );
      pos = patch.getExteriorRing();    
    }   
    catch( GM_Exception e1 )
    {
      e1.printStackTrace();
    }    
    return pos;
  }

  private void paintThisSurface(GM_Surface _surface) {
    
    double elevation = calculateCenterCoOrdinate( getGM_PositionForThisSurface( _surface )).getZ();
    try
    {
      Area area = calcTargetCoordinates( this.projection, _surface);
      graphics.setColor( colorModel.getColor( elevation ) );
      ((Graphics2D) graphics).fill( area );
      
      java.awt.Stroke bs2 = new BasicStroke( 3 );
      ((Graphics2D) graphics).setStroke( bs2 );
      ((Graphics2D) graphics).draw( area );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }
  
  public GM_Position calculateCenterCoOrdinate( GM_Position[] coords )
  {
    
    double[] centerCo = new double[3];
    centerCo[0] = (coords[0].getX() + coords[1].getX() + coords[2].getX()) / 3;
    centerCo[1] = (coords[0].getY() + coords[1].getY() + coords[2].getY()) / 3;
    centerCo[2] = (coords[0].getZ() + coords[1].getZ() + coords[2].getZ()) / 3; 
    //centerCo[2] = computeZOfTrianglePlanePoint( coords, centerCo[0], centerCo[1] );
    
    return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position( centerCo ); 
  }



  
}

