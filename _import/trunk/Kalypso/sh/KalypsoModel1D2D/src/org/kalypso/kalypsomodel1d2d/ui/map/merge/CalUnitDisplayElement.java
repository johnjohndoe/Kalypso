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
package org.kalypso.kalypsomodel1d2d.ui.map.merge;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.geom.Area;
import java.util.LinkedList;
import java.util.List;


import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ILineElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.SurfacePatchVisitableDisplayElement;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * Provides the mechanism to paint a calculation unit.
 * 
 * @author Patrice Congo
 *
 */
@SuppressWarnings({"unchecked","hiding"})
public class CalUnitDisplayElement implements DisplayElementDecorator
{
  private ICalculationUnit<IFE1D2DElement> calUnit;
  
  private DisplayElement decorated;
  
  private boolean highlighted;
  private boolean selected;
  
  public CalUnitDisplayElement(ICalculationUnit<IFE1D2DElement> calUnit )
  {
    this.selected = false;
    this.highlighted = false;
    this.calUnit = calUnit;
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
   this.decorated = decorated; 
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#doesScaleConstraintApply(double)
   */
  public boolean doesScaleConstraintApply( double scale )
  {
    if( decorated!= null )
    {
      return decorated.doesScaleConstraintApply( scale );
    }
    else
    {
      return true;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#getAssociateFeatureId()
   */
  public String getAssociateFeatureId( )
  {
    if( decorated!= null )
    {
      return decorated.getAssociateFeatureId();
    }
    else
    {
      if(calUnit!=null)
      {
        return calUnit.getGmlID();
      }
      else
      {
        return null;
      }
    }
    
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#getFeature()
   */
  public Feature getFeature( )
  {
    if( calUnit != null )
    {
      return calUnit.getWrappedFeature();
    }
    else
    {
      return null;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#isHighlighted()
   */
  public boolean isHighlighted( )
  {
    if ( decorated != null )
    {
      return decorated.isHighlighted();
    }
    else
    {
      return highlighted;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#isSelected()
   */
  public boolean isSelected( )
  {
    if ( decorated != null )
    {
      return decorated.isSelected();
    }
    else
    {
      return selected;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#paint(java.awt.Graphics, org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( Graphics g, GeoTransform projection )
  {

    if( calUnit == null )
    {
      return;
    }
    
    GM_Envelope sourceRect = projection.getSourceRect();
    LinkedList<ICalculationUnit> calUnitTreeToDraw= new LinkedList<ICalculationUnit>();
    calUnitTreeToDraw.add( calUnit );
    while(!calUnitTreeToDraw.isEmpty())
    {
      final ICalculationUnit<IFE1D2DElement> currentUnit = calUnitTreeToDraw.removeFirst();
      final Color color = Color.lightGray;
      final IFeatureWrapperCollection<IFE1D2DElement> elements = currentUnit.getElements();
      List<IFE1D2DElement> visibleElements = elements.query( sourceRect );
      
      for(IFE1D2DElement element: visibleElements )
      {
        if( element instanceof IPolyElement )
        {
          try
          {
            GM_Surface surface = 
                (GM_Surface) element.recalculateElementGeometry();
            paintSurface( surface, color, (Graphics2D) g, projection );
          }
          catch( Exception e )
          {
            e.printStackTrace();
          }
        }
        else if( element instanceof ILineElement ||
                  element instanceof IElement1D )
        {
          try
          {
            GM_Curve curve = (GM_Curve) element.recalculateElementGeometry();
            paintLineString( curve, color, (Graphics2D) g, projection );
          }
          catch ( Exception e) 
          {
            e.printStackTrace();
            throw new RuntimeException(e);
          }
        }
        else
        {
          throw new RuntimeException("Unexpected type of element:"+element);
        }
      }
      
      //add sub unit for drawing 
      if( currentUnit instanceof ICalculationUnit1D2D )
      {
        calUnitTreeToDraw.addAll( 
            ((ICalculationUnit1D2D)currentUnit ).getSubUnits() );
      }
      
    }
  }
  
  public static final void paintSurface(
                            GM_Surface surface, 
                            Color color,
                            Graphics2D g2d, 
                            GeoTransform projection)
  {
    try
    {
      Area area = calcTargetCoordinates( projection, surface );
      g2d.setColor( color );
      g2d.fill( area );
      
      //shape drawing
      g2d.setColor( Color.BLACK );
      java.awt.Stroke bs2 = new BasicStroke( 1 );
      
      g2d.setStroke( bs2 );
      g2d.draw( area );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }
  
  public static final void paintLineString(
                              GM_Curve curve, 
                              Color color,
                              Graphics2D g2d, 
                              GeoTransform projection ) throws Exception
  {
   
      int[][] linePointCoords = calcTargetCoordinates( projection, curve );
      
      Stroke stroke = new BasicStroke(5);
      g2d.setStroke( stroke  );
      g2d.setColor( color );
      g2d.drawPolyline( 
          linePointCoords[0], 
          linePointCoords[1], 
          linePointCoords[2][0] );
  }
  
  /**
   * calculates the Area (image or screen coordinates) where to draw the surface.
   */
  public static final Area calcTargetCoordinates( 
                      GeoTransform projection, 
                      GM_Surface surface ) 
                      throws Exception
  {
    float width = 1;
    try
    {
      final GM_SurfacePatch patch = surface.getSurfacePatchAt( 0 );
      final GM_Position[] ex = patch.getExteriorRing();
      
      final Area areaouter = 
        SurfacePatchVisitableDisplayElement.areaFromRing( projection, width, ex );
     

      return areaouter;
    }
    catch( Exception e )
    {
      Debug.debugException( e, "" );
    }

    return null;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#setHighlighted(boolean)
   */
  public void setHighlighted( boolean highlighted )
  {
    this.highlighted = highlighted;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#setSelected(boolean)
   */
  public void setSelected( boolean selected )
  {
    this.selected = selected;
  }

  public static final CalUnitDisplayElement createDisplayElement( ICalculationUnit<IFE1D2DElement> calUnit )
  {
    Assert.throwIAEOnNullParam( calUnit, "calUnit" );
    final CalUnitDisplayElement calUnitDisplayElement = new CalUnitDisplayElement(calUnit);
    
    return calUnitDisplayElement; 
    
    
  }
  
  
  /**
   * Copied and modified from {@link org.kalypsodeegree.graphics.displayelements.LineStringDisplayElement}
   * 
   * calculates the coordintes (image or screen coordinates) where to draw
   * the curve.
   */
  public static final int[][] calcTargetCoordinates( 
                                  GeoTransform projection, 
                                  GM_Curve curve
                                  )
                                 throws Exception 
  {
    GM_LineString lineString = curve.getAsLineString();
      float width = 1;
      int count = lineString.getNumberOfPoints();
      int[][] pos = new int[3][];
      pos[0] = new int[count];
      pos[1] = new int[count];
      pos[2] = new int[1];
      
      int k = 0;
      for ( int i = 0; i < count; i++ ) {
          GM_Position position = lineString.getPositionAt( i );  
          double tx = projection.getDestX( position.getX() );
          double ty = projection.getDestY( position.getY() );
          if ( i > 0 && i < count-1) {                
              if ( distance( tx, ty, pos[0][k-1], pos[1][k-1] ) > width ) {
                  pos[0][k] = (int)( tx + 0.5 );
                  pos[1][k] = (int)( ty + 0.5 );
                  k++;
              }
          } else {                
              pos[0][k] = (int)( tx + 0.5 );
              pos[1][k] = (int)( ty + 0.5 );
              k++;
          }            
      }
      pos[2][0] = k;        

      return pos;
  }
  
  /**
   * Copied  from {@link org.kalypsodeegree.graphics.displayelements.LineStringDisplayElement}
   *  
   */
  public static final double distance( 
                double x1, double y1, double x2, double y2 ) 
  {
    return Math.sqrt( (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) );
  }
  
}
