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
import org.kalypso.commons.command.ICommand;
import org.kalypso.jts.QuadMesher.JTSQuadMesher;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

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
  
//  private int pointRectSize = 6;
  
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
                  final GeoTransform projection,
                  final int pointRectSize)
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
  
  void setTempGrid(
          LinePointCollector topSidePoints, 
          LinePointCollector bottomSidePoints, 
          LinePointCollector leftSidePoints, 
          LinePointCollector rightSidePoints) throws GM_Exception
  {
    Assert.throwIAEOnNullParam( gridPoints, "gridPoint" );
    this.gridPoints=
        computeMesh( 
              topSidePoints, 
              bottomSidePoints, 
              leftSidePoints, rightSidePoints );
    drawPoints=null;
  }
  
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