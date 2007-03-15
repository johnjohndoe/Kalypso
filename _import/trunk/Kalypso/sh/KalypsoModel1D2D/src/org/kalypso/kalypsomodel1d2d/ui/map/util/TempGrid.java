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
package org.kalypso.kalypsomodel1d2d.ui.map.util;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.commons.command.ICommand;
import org.kalypso.jts.QuadMesher.JTSQuadMesher;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddElementCmdFromNodeCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddNodeCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeDiscretiationModelCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.grid.LinePointCollector;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.IncompatibleGeometryTypeException;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

//TODO model grid mechanism to here
/**
 * This class provide the mechanism to calculate the grid finit element model
 * node and to display them.
 * The point are supposed to be in the same coordinate reference system so that no
 * no reference system convertion is done
 * 
 * @author Patrice Congo
 */
public class TempGrid 
{
//  /**
//   * Stores the count of points which this geometry must have. If it is 0, there is no rule.
//   */
//  private int m_cnt_points;

  /**
   * The target coodinate reference system for the created grid point 
   */
  private CS_CoordinateSystem m_crs;
  
  /**
   * Cache for screen point coordinate
   */
  private int[][] drawPoints;
  
  /**
   * Cache for grid computed grid points
   */
  private GM_Point[][] gridPoints;
  
  
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


  /**
   * Set the target {@link CS_CoordinateSystem}. All points this grid is coping
   * with are required to reside in that system and no reference system conversion 
   * made in by the {@link TempGrid}
   * @param targetCrs the target {@link CS_CoordinateSystem}
   * @throws IllegalArgumentException is targetCrs is null
   */
  public void setCoodinateSystem (final CS_CoordinateSystem targetCrs )
                                  throws IllegalArgumentException
  {
      Assert.throwIAEOnNullParam( targetCrs, "targetCrs" );
       this.m_crs = targetCrs;
  }

  /**
   * Class this to display the {@link TempGrid} on the screen.
   * @param g the display graphic context
   * @param projection the geoprojection for projecting {@link GM_Point} to
   *    screen points
   * @param pointRectSize the side lengtth for square representing the points
   */
  public void paint( 
                  final Graphics g, 
                  final GeoTransform projection,
                  final int pointRectSize)
  {
    
    // IMPORTANT: we remember GM_Points (not Point's) and retransform them for painting
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
        
        //cache draw points
        drawPoints=points; 
        try
        {
          paintEdges( g, projection );
        }
        catch( GM_Exception e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
        catch( IncompatibleGeometryTypeException e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
    }
      
  }

  /**
   * compute screen points from gm points
   */
  private int[][] getPointArrays( 
                      final GeoTransform projection )
  {
    if(drawPoints!=null)
    {
      return drawPoints;
    }
    
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

  /**
   * draws the temp grid point on the screen
   */
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
  
  private void paintEdges( final Graphics g, GeoTransform projection ) throws GM_Exception, IncompatibleGeometryTypeException
  {
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );
    Stroke defaultstroke = new Stroke_Impl( new HashMap(), null, null );
    defaultstroke = symb.getStroke();
    Color grey = new Color( 100, 100, 100 );

    stroke.setWidth( 1 );
    stroke.setStroke( grey );
    symb.setStroke( stroke );
    DisplayElement de;
    
    
    
    GM_Position[] pos = new GM_Position[2];

    for( int i = 0; i < gridPoints.length; i++ )
    {
      org.kalypsodeegree_impl.model.geometry.GeometryFactory factory = new org.kalypsodeegree_impl.model.geometry.GeometryFactory();
      for( int j = 0; j < gridPoints[i].length - 1; j++ )
      {
        pos[0] = gridPoints[i][j].getPosition();
        pos [1] = gridPoints[i][j+1].getPosition();
        final GM_Curve curve = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Curve( pos, m_crs );
        de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
        de.paint( g, projection );
      }
    }
    for( int j = 0; j < gridPoints[0].length; j++ )
    {
      GeometryFactory factory = new GeometryFactory();
      for( int i = 0; i < gridPoints.length - 1; i++ )
      {
        pos[0] = gridPoints[i][j].getPosition();
        pos [1] = gridPoints[i+1][j].getPosition();
        final GM_Curve curve = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Curve( pos, m_crs );
        de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
        de.paint( g,projection );
      }
    }
  }

  /**
   * Reset this {@link TempGrid}.
   * It is empty, i.e. contains no points after reset
   * @param crs the target coordinate reference system for the grid
   * @see #setCoodinateSystem(CS_CoordinateSystem)
   * @throws IllegalArgumentException if crs is null
   */ 
  public void resetTempGrid(CS_CoordinateSystem crs)
                    throws IllegalArgumentException
  {
    gridPoints= new GM_Point[0][0];
    m_crs=crs;
  }
  
  /**
   * config the with its side points.
   * @param topSidePoints the collector containing the top side points
   * @param bottomSidePoints the collector containing the bottom side points
   * @param leftSidePoints the collector containing the to left side points
   * @param rightSidePoints the collector containing the to right side points
   * @throws IllegalArgumentException if one the the side point collector is null
   */
  public void setTempGrid(
          LinePointCollector topSidePoints, 
          LinePointCollector bottomSidePoints, 
          LinePointCollector leftSidePoints, 
          LinePointCollector rightSidePoints) throws GM_Exception
  {
    Assert.throwIAEOnNullParam( topSidePoints, "topSidePoints" );
    Assert.throwIAEOnNullParam( bottomSidePoints, "bottomSidePoints" );
    Assert.throwIAEOnNullParam( leftSidePoints, "leftSidePoints" );
    Assert.throwIAEOnNullParam( leftSidePoints, "leftSidePoints" );
    this.gridPoints=
        computeMesh( 
              topSidePoints, 
              bottomSidePoints, 
              leftSidePoints, rightSidePoints );
    drawPoints=null;
  }
  
  /**
   * in case that the data comes allready as mesh  
   */
  public void importMesh( GM_Point[][] importedGridPoints)
  {
    this.gridPoints  = importedGridPoints;
  }
  
  /**
   * Computes the grid points from its side points
   */
  private final GM_Point[][] computeMesh(
                          LinePointCollector topSidePoints, 
                          LinePointCollector bottomSidePoints, 
                          LinePointCollector leftSidePoints, 
                          LinePointCollector rightSidePoints) 
                          throws GM_Exception
  {
    //GeometryFactory geometryFactory= new GeometryFactory();
    final LineString topLine = pointToLineString( topSidePoints );
    final LineString bottomLine = pointToLineString( bottomSidePoints );
    final LineString leftLine = pointToLineString( leftSidePoints );
    final LineString rightLine = pointToLineString( rightSidePoints );
    
    //compute mesh points
    JTSQuadMesher mesher= 
    new JTSQuadMesher(
            topLine,
            bottomLine,
            leftLine,
            rightLine);
    Coordinate[][] coordinates=mesher.calculateMesh();
    GeometryFactory geometryFactory= new GeometryFactory();
    GM_Point points2D[][] = new GM_Point[coordinates.length][]; 
    for(int i=0;i<coordinates.length;i++)
    {
      Coordinate[] line=coordinates[i];
      GM_Point[] points1D= 
              new GM_Point[line.length];
      points2D[i]=points1D;
      for(int j=0;j<line.length;j++)
      {
        Coordinate coord=line[j];
        points1D[j]=    
          (GM_Point)JTSAdapter.wrap( 
                  geometryFactory.createPoint(coord));
      }
    }
    return points2D;
  }
  
  /**
   * To get an {@link ICommand} that can be use to hat the temp
   * grid to the model
   */
  public ICommand getAddToModelCommand(
      MapPanel mapPanel,
      IFEDiscretisationModel1d2d model,
      CommandableWorkspace commandableWorkspace,
      double searchRectWidth) 
      throws GM_Exception
  {
    ChangeDiscretiationModelCommand compositeCommand = 
          new ChangeDiscretiationModelCommand(
              commandableWorkspace,
              model);// new CompositeCommand("Grid Command");
    //compute Points
    GM_Point[][] points2D=gridPoints;//computeMesh();
    final int DIM_X=points2D.length;
    if(DIM_X==0)
    {
      System.out.println("DimX is null");
      return compositeCommand;
    }
    
    final int DIM_Y=points2D[0].length;
    //add nodes
    AddNodeCommand[][] newNodesArray2D= 
       new AddNodeCommand[DIM_X][DIM_Y];
    addNodesFromPoints( 
        model, 
        newNodesArray2D, 
        compositeCommand, 
        points2D,
        searchRectWidth);    
    
    addElementsFromNodes( model, newNodesArray2D, compositeCommand );
    return compositeCommand;
  }
  /**
   * Make the add node command
   */
  private final void  addNodesFromPoints(
      IFEDiscretisationModel1d2d model,
      AddNodeCommand[][] newNodesArray2D,
      ChangeDiscretiationModelCommand compositeCommand,
      GM_Point[][] points2D,
      double searchRectWidth)
  {
    for(int i=0;i<points2D.length;i++)
    {
      GM_Point[] points1D=points2D[i];
//      AddNodeCommand[] newNodesArray1D= 
//              new AddNodeCommand[points1D.length];
//      newNodesArray2D[i]=newNodesArray1D;
      for(int j=0;j<points1D.length;j++)
      {
        //TODO check node for existance
        AddNodeCommand nodeCommand=
          new AddNodeCommand(
                model,
                points1D[j], 
                searchRectWidth );
        newNodesArray2D[i][j]=nodeCommand;//newNodesArray1D[j]=nodeCommand;
        compositeCommand.addCommand( nodeCommand );
      }
    }
  }
  
  /**
   * make the add element command. element are added based on their nodes
   */
  private final void  addElementsFromNodes(
      IFEDiscretisationModel1d2d model,
      AddNodeCommand[][] newNodesArray2D,
      ChangeDiscretiationModelCommand compositeCommand)
  {
      final int LAST_INDEX_I = newNodesArray2D.length-2;
      for(int i=0; i<=LAST_INDEX_I/*i<addEdgeH2D.length-1*/;i++)
      {
        final int LAST_INDEX_J=newNodesArray2D[0].length-2;
        for(int j=0;j<=LAST_INDEX_J;j++)
        {
          AddNodeCommand node0 = newNodesArray2D[i][j];
          AddNodeCommand node1 = newNodesArray2D[i][j+1];
          AddNodeCommand node2 = newNodesArray2D[i+1][j+1];
          AddNodeCommand node3 = newNodesArray2D[i+1][j];
          AddNodeCommand node4 = newNodesArray2D[i][j];
          
          AddElementCmdFromNodeCmd addElementCommand=
            new AddElementCmdFromNodeCmd(
                model,
                new AddNodeCommand[]{node0,node1,node2,node3, node4});
          compositeCommand.addCommand( addElementCommand );
        }
      }
  }
  
  /**
   * get the {@link LinePointCollector} points as {@link LineString}
   */
  private LineString pointToLineString(
      LinePointCollector lineGeometryBuilder)
  {
    final int SIZE=lineGeometryBuilder.getCurrentPointCnt();
    Coordinate coordinates[] = new Coordinate[SIZE];
    for(int i=0;i<SIZE;i++)
    {
      coordinates[i]=
      JTSAdapter.export(  
            lineGeometryBuilder.getPointAt( i ).getPosition());
    }
    //CoordinateSequence 
    //JTSAdapter
    GeometryFactory geometryFactory=new GeometryFactory();
    LineString lineString=
    geometryFactory.createLineString( coordinates );
    return lineString;
  }
}