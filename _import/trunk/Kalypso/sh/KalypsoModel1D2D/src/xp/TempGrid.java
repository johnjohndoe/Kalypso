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
package xp;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.opengis.cs.CS_CoordinateSystem;

//TODO model grid mechanism to here
/**
 * This class is the container for the grid points calculated 
 * on the fly.
 * 
 * @author Patrice Congo
 */
public class TempGrid 
{
  /**
   * Stores the count of points which this geometry must have. If it is 0, there is no rule.
   */
  private int m_cnt_points;

  private CS_CoordinateSystem m_crs;
  
  private int[][] drawPoints;
  
  private GM_Point[][] gridPoints;
  
  private int pointRectSize = 6;
  
  /**
   * The constructor.
   * 
   * @param cnt_points
   *          If >0 the the geometry will be finished, if the count of points is reached. If 0 no rule regarding the
   *          count of the points will apply.
   * @param targetCrs
   *          The target coordinate system.
   */
  public TempGrid()
  {
       //yes it is empty
  }


  public void setCoodinateSystem (final CS_CoordinateSystem targetCrs )
  {
       this.m_crs = targetCrs;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( 
                  final Graphics g, 
                  final GeoTransform projection)
  {
    
    // IMPORTANT: we remeber GM_Points (not Point's) and retransform them for painting
    // because the projection depends on the current map-extent, so this builder
    // is stable in regard to zoom in/out
    if(gridPoints.length!=0)
    {
      
        //draw a line
        final int[][] points = getPointArrays( projection);
        final int[] arrayX = points[0];
        final int[] arrayY = points[1];
  
        /* Paint a linestring. */
//        g.drawPolyline( arrayX, arrayY, arrayX.length );
        drawHandles( g, arrayX, arrayY, pointRectSize);        
        drawPoints=points;   
    }
      
  }

  private int[][] getPointArrays( 
                      final GeoTransform projection )
  {
    final List<Integer> xArray = new ArrayList<Integer>();
    final List<Integer> yArray = new ArrayList<Integer>();

    for( GM_Point points[]:gridPoints)
    {
      for(GM_Point point:points)
      {
        if(point!=null)
        {
          final int x = (int) projection.getDestX( point.getX() );
          final int y = (int) projection.getDestY( point.getY() );
    
          xArray.add( new Integer( x ) );
          yArray.add( new Integer( y ) );
        }
      }
    }

    

    final int[] xs = ArrayUtils.toPrimitive( xArray.toArray( new Integer[xArray.size()] ) );
    final int[] ys = ArrayUtils.toPrimitive( yArray.toArray( new Integer[yArray.size()] ) );

    return new int[][] { xs, ys };
  }

  private static final void drawHandles( 
                            final Graphics g, 
                            final int[] x, 
                            final int[] y ,
                            final int pointRectWidth)
  {
    final Color oldColor=g.getColor();
    g.setColor( oldColor.darker() );
    //int sizeOuter = 4;
    int halfRectWidth=pointRectWidth/2;
    
    for( int i = 0; i < y.length; i++ )
    {
//      g.drawRect( 
//            x[i] - halfRectWidth, 
//            y[i] - halfRectWidth, 
//            pointRectWidth, 
//            pointRectWidth );
      g.fill3DRect( 
            x[i]-halfRectWidth, 
            y[i]-halfRectWidth, 
            pointRectWidth, 
            pointRectWidth, 
            true//raised
            );
    }    
    g.setColor( oldColor );    
  }
  
   
  void resetTempGrid(CS_CoordinateSystem crs)
  {
    gridPoints= new GM_Point[0][0];
    m_crs=crs;
  }
  
  void setTempGrid(GM_Point[][] gridPoints)
  {
    Assert.throwIAEOnNullParam( gridPoints, "gridPoint" );
    this.gridPoints=gridPoints;
    drawPoints=null;
  }
  
  
}