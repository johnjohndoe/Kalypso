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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.geom.Area;
import java.util.List;

import org.deegree.model.geometry.GM_Point;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ASCTerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitor;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Provide display mechanism for asc terrain elevation model
 *  
 * @author Madanagopal 
 * @author Patrice Congo
 */
public class ASCDisplayElement 
                implements  DisplayElementDecorator,
                            SurfacePatchVisitor
{
  private SimpleElevationColorModel colorModel; 
    
  private NativeTerrainElevationModelWrapper elevationModel;
  
  private boolean isHighlighted=false;
 
  private Graphics graphics=null;
  
  private boolean isSelected=false;

  private ASCTerrainElevationModel ascElevationModel;

  private Symbolizer defaultSymbolizer = new PolygonSymbolizer_Impl();
  
  private DisplayElement decorated;

  private GeoTransform projection;
 
  
  public ASCDisplayElement(
                NativeTerrainElevationModelWrapper elevationModel )
  {
    Assert.throwIAEOnNullParam( elevationModel, "elevationModel" );
    this.elevationModel=elevationModel;
    IElevationProvider elevationProvider = 
              elevationModel.getElevationProvider();
    //TODO continue
    if(elevationProvider instanceof ASCTerrainElevationModel)
    {
      ascElevationModel=(ASCTerrainElevationModel)elevationProvider;
      colorModel = 
        new SimpleElevationColorModel(
            ascElevationModel.getMinElevation(),
            ascElevationModel.getMaxElevation(),
            Color.RED,
            0.10,
            0.80,
            Color.WHITE);
    }
    else
    {
      throw new RuntimeException("Can only handle asc ele model:"+elevationProvider);
    }
  }
  
  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#doesScaleConstraintApply(double)
   */
  public boolean doesScaleConstraintApply( double scale )
  {
    if(decorated!=null)
    {
      return decorated.doesScaleConstraintApply( scale );
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
    if(decorated!=null)
    {
      return decorated.getAssociateFeatureId();
    }
    else
    {
      return elevationModel.getGmlID();
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#getFeature()
   */
  public Feature getFeature( )
  {
    if(decorated!=null)
    {
      return decorated.getFeature();
    }
    else
    {
      return elevationModel.getFeature();
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#isHighlighted()
   */
  public boolean isHighlighted( )
  {
    if(decorated!=null)
    {
      return decorated.isHighlighted();
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
    if(decorated!=null)
    {
      return decorated.isSelected();
    }
    else
    {
      return this.isSelected;
    }
  }



  
  public void paint( 
                  Graphics g, 
                  GeoTransform p, 
                  double scale, 
                  GM_Envelope bbox, 
                  boolean selected,
                  IElevationColorModel colorModel)
  {
    paint( g, p, bbox );
  }
  
  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#setHighlighted(boolean)
   */
  public void setHighlighted( boolean highlighted )
  {
    if(decorated!=null)
    {
      decorated.setHighlighted( highlighted );
    }
    this.isHighlighted=highlighted;
  }

  
  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#setSelected(boolean)
   */
  public void setSelected( boolean selected )
  {
    if(decorated!=null)
    {
      decorated.setSelected( selected );
    }
    this.isSelected=selected;
  }
  
  public void paint( Graphics g, GeoTransform projection )
  {
    if(decorated!=null)
    {
      decorated.paint( g, projection );
    }
//    paint( g, projection, elevationModel.getBoundingBox() );
    try
    {
      //TODO Patrice try to get the current paint box
      this.graphics=g;
      this.projection=projection;
//      GM_Envelope worldRect = projection.getSourceRect();
//      
//      System.out.println("destRect="+worldRect);
      ascElevationModel.aceptSurfacePatches( 
                    ascElevationModel.getBoundingBox(), 
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
  
  
  
  public void paint( Graphics g, GeoTransform projection,GM_Envelope env )
  {
    try
    {
      
      Area area = null;
      List<GM_Surface> surfaces=null;
      List<GM_Position> cellLLCornerList = 
          this.ascElevationModel.getCellLLCornerIterator( env );
      //TODO patrice remove that from hier
      CS_CoordinateSystem crs=ascElevationModel.getCoordinateSystem();
      GM_Envelope bbox=null;
      double cellSize=ascElevationModel.getCellSize();
      Graphics graphics = g;//.create();
      SimpleElevationColorModel colorModel = 
            new SimpleElevationColorModel(
                        ascElevationModel.getMinElevation(),
                        ascElevationModel.getMaxElevation(),
                        Color.RED,
                        0.10,
                        0.80,
                        Color.WHITE);
//      g.setPaintMode();
      for(GM_Position position:cellLLCornerList)
      {
        double minx=position.getX();
        double miny=position.getY();
        bbox = GeometryFactory.createGM_Envelope( minx, miny, minx+cellSize, miny+cellSize ); 
        GM_Surface surface= GeometryFactory.createGM_Surface( bbox, crs );
        area = calcTargetCoordinates( projection, surface );
        g.setColor( colorModel.getColor( position.getZ() ) );
        drawPolygon( graphics, area );
      }
//      graphics.dispose();
    }
    catch (Exception e) 
    {
      e.printStackTrace();
    }
  }
  
  /**
   * calculates the Area (image or screen coordinates) where to draw the surface.
   */
  private Area calcTargetCoordinates( GeoTransform projection, GM_Surface surface ) throws Exception
  {
    final PolygonSymbolizer sym = (PolygonSymbolizer) getSymbolizer();
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
  
  private Area areaFromRing( GeoTransform projection, float width, final GM_Position[] ex )
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
    Graphics2D g2d=(Graphics2D) gg;
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
//        color = new Color( red*0, green*0, blue*1, alpha );
//        g2d.setColor( color );
//        g2d.setBackground( color );
        
        
//        
//        GraphicFill gFill = fill.getGraphicFill();
        
//        if( gFill != null )
//        {
//          BufferedImage texture = gFill.getGraphic().getAsImage( feature );
//          if( texture != null )
//          {
//            Rectangle anchor = new Rectangle( 0, 0, texture.getWidth( null ), texture.getHeight( null ) );
//            g2.setPaint( new TexturePaint( texture, anchor ));
//          }
//        }

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

    // only stroke outline, if Stroke-Element is given
//    if( stroke != null )
//    {
//      double opacity = stroke.getOpacity( feature );
//      if( opacity > 0.01 )
//      {
//        Color color = stroke.getStroke( feature );
//        int alpha = (int) Math.round( opacity * 255 );
//        int red = color.getRed();
//        int green = color.getGreen();
//        int blue = color.getBlue();
//        color = new Color( red, green, blue, alpha );
//
//        g2d.setColor( color );
//
//        float[] dash = stroke.getDashArray( feature );
//
//        // use a simple Stroke if dash == null or dash length < 2
//        BasicStroke bs2 = null;
//        float w = (float) stroke.getWidth( feature );
//
//        if( (dash == null) || (dash.length < 2) )
//        {
//          bs2 = new BasicStroke( w );
//        }
//        else
//        {
//          bs2 = new BasicStroke( w, stroke.getLineCap( feature ), stroke.getLineJoin( feature ), 10.0f, dash, stroke.getDashOffset( feature ) );
//        }
//
//        g2d.setStroke( bs2 );
//        try
//        {
//          g2d.draw( area );
//        }
//        catch( Exception e )
//        {
//          //  
//        }
//      }
//    }
  }

  private PolygonSymbolizer getSymbolizer( )
  {
    decorated.doesScaleConstraintApply( 0 );
    return (PolygonSymbolizer) defaultSymbolizer;
  }
  
  
  
  
  public static final ASCDisplayElement createDisplayElement(Feature feature)
  {
    if(feature == null)
    {
      return null;
    }
    ITerrainElevationModel terrainElevationModel=
            (ITerrainElevationModel) feature.getAdapter( ITerrainElevationModel.class );
    if(terrainElevationModel instanceof NativeTerrainElevationModelWrapper)
    {
      return new ASCDisplayElement(
                      (NativeTerrainElevationModelWrapper)terrainElevationModel);
    }
    else
    {
      System.out.println("Could not adapt to a native terrain ele model:"+feature);
      return null;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator#getDecorated()
   */
  public DisplayElement getDecorated( )
  {
    return decorated;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator#setDecorated(org.kalypsodeegree.graphics.displayelements.DisplayElement)
   */
  public void setDecorated( DisplayElement decorated )
  {
    this.decorated=decorated;    
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.SurfacePatchVisitor#accept(org.kalypsodeegree.model.geometry.GM_Surface, double)
   */
  public boolean visit( GM_Surface surfacePatch, double elevationSample )
  {
    try
    {
//        System.out.println(
//            "Drawing:"+
//            "\n\tsurface:"+surfacePatch+
//            "\n\televation:"+elevationSample);
        Area area = calcTargetCoordinates( this.projection, surfacePatch );
        graphics.setColor( colorModel.getColor( elevationSample ) );
//        drawPolygon( graphics, area );
        ((Graphics2D)graphics).fill( area );
    }
    catch (Exception e) 
    {
      e.printStackTrace();
    }
    return true;
  }

}
