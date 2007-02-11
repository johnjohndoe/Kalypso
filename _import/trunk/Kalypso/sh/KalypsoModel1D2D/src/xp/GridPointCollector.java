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
package xp;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.commons.command.ICommand;
import org.kalypso.jts.QuadMesher.JTSQuadMesher;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import test.org.kalypso.kalypsomodel1d2d.TestWorkspaces;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;


@SuppressWarnings("unchecked")
class GridPointCollector implements IGeometryBuilder
{
  private static final Logger logger=
        Logger.getLogger( GridPointCollector.class.getName() );
  
  public static final int SIDE_MAX_NUM=4;
  public static final int SIDE_TOP=0;
  public static final int SIDE_BOTTOM=2;
  public static final int SIDE_LEFT=1;
  public static final int SIDE_RIGHT=3;
  
//  private LineGeometryBuilder builder;
  
  private int actualSideKey;
  
  private boolean hasAllSides=false;
  
  private LineGeometryBuilder sides[] = new LineGeometryBuilder[SIDE_MAX_NUM];
  
  private Color colors[] = {Color.BLUE,Color.DARK_GRAY,Color.RED, Color.GREEN};
  
  private GM_Curve[] oppossites= new GM_Curve[2]; 
  
  public GridPointCollector()
  {
    
  }
  
//  public void setSideToBuild(int sideKey, CS_CoordinateSystem targetCrs)
//  {
//    checkSideKey( sideKey );
//    actualSideKey=sideKey;
////    builder = new LineGeometryBuilder( 0, targetCrs );     
//    sides[actualSideKey]=new LineGeometryBuilder( 0, targetCrs );//builder;
//  }
  
//  public void resetSideToBuild(CS_CoordinateSystem targetCrs)
//  {
//    builder = new LineGeometryBuilder( 0, targetCrs );      
//  }
  
  public void reset(CS_CoordinateSystem targetCrs)
  {
    
//    builder = new LineGeometryBuilder( 0, targetCrs );
//    setSideToBuild( 0, targetCrs );
    for(LineGeometryBuilder b:sides)
    {
      if(b!=null)
      {
        b.clear();
      }
    }
    actualSideKey=0;
    if(sides[actualSideKey]==null)
    {
      sides[actualSideKey]=new LineGeometryBuilder( 0, targetCrs );
    }
    
  }
  
//  private final void checkSideKey(int sideKey)
//  {
//    if(sideKey<0 || sideKey>=SIDE_MAX_NUM)
//    {
//      throw new IllegalArgumentException(
//          "Legal value for sides are 0,1,2,3 but this value passsed:"+sideKey);
//    }
//  }
  
  
  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder#addPoint(org.kalypsodeegree.model.geometry.GM_Point)
   */
  public GM_Object addPoint( GM_Point p ) throws Exception
  {
    if(actualSideKey>=SIDE_MAX_NUM)
    {
      return null;
    }
    Assert.throwIAEOnNull( 
        sides[actualSideKey], 
        "builder not available for adding a point" );
    
    GM_Point lastAdded= (GM_Point)sides[actualSideKey].addPoint( p );
    GM_Point autocompleted=autoComplete();
    if(autocompleted!=null)
    {
      return this.finishSide();
    }
    else
    {
      return lastAdded;//(autocompleted==null)?lastAdded:autocompleted;
    }
    
  }

  /**
   * Auto complete this line collector and returns the 
   * completing point if done.
   * Auto completion is only done for the last side because
   * 
   * @return the auto completion point 
   */
  public GM_Point autoComplete() throws Exception
  {
    if(actualSideKey==3)
    {
      System.out.println("Autocompleting:");
      if(sides[actualSideKey].getRemainingPointCnt()==1)
      {
        GM_Point point= sides[0].getFirstPoint();
        if(point instanceof MutableGMPoint)
        {
          sides[actualSideKey].addPoint( point );
          return point;
        }
        else
        {
          throw new RuntimeException("Mutable point expected");
//          return null;
        }
      }
      else
      {
        return null;
      }
    }
    else
    {
      if(sides[actualSideKey].getRemainingPointCnt()==0)
      {
        return (GM_Point)sides[actualSideKey].finish();//getLastPoint();
      }
      else
      {
        return null;
      }
    }
  }
  public GM_Point getLastPoint() throws Exception
  {
    if(actualSideKey>=SIDE_MAX_NUM)
    {
      return null;
    }
    Assert.throwIAEOnNull( 
        sides[actualSideKey], 
        "builder not available for adding a point" );
    return sides[actualSideKey].getLastPoint();
  }
  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder#finish()
   */
  public GM_Object finish( ) throws Exception
  {
    return null;
//    Assert.throwIAEOnNull( 
//            builder, 
//            "builder not available" );
//          
//    GM_Object gmObject=builder.finish();
//    sides[actualSideKey]=(GM_Curve)gmObject;
//    
//    if(actualSideKey<(SIDE_MAX_NUM-1))
//    {
//      actualSideKey++;
//      builder=
//        new LineGeometryBuilder( 0, gmObject.getCoordinateSystem());      
//    }
//    logger.info( "Curve="+((GM_Curve)gmObject).getAsLineString()+ 
//                "\n\tactualSide="+actualSideKey );
//    return gmObject;      
  }
  
  public GM_Object finishSide( ) throws Exception
  {
    Assert.throwIAEOnNull( 
            sides[actualSideKey], 
            "builder not available" );
    if(actualSideKey>=SIDE_MAX_NUM)
    {
      return null;
    }
    LineGeometryBuilder oldBuilder=sides[actualSideKey];      
    GM_Object gmObject=sides[actualSideKey].finish();
    
    actualSideKey++;
    if(actualSideKey<SIDE_MAX_NUM)
    {
      //actualSideKey++;
      LineGeometryBuilder newSide=sides[actualSideKey];
      if(newSide==null)
      {
        newSide=oldBuilder.getNewBuilder();
        sides[actualSideKey]= newSide;
        GM_Point lastP=oldBuilder.getLastPoint();  
        newSide.setCntPoints( computeSize() );
        if(lastP!=null)
        {
          newSide.addPoint( lastP );
        }
        else
        {
          logger.warning( "Last point is null" );
        }
        
      }
    }
    else
    {
      hasAllSides=true;
    }
//    logger.info( "Curve="+((GM_Curve)gmObject).getAsLineString()+ 
//                "\n\tactualSide="+actualSideKey );
    return gmObject;      
  }
  
  private final int computeSize()
  {
    if(actualSideKey==0 || actualSideKey==1)
    {
      return 0; 
    }
    else if(actualSideKey==2)
    {
      return sides[0].getCurrentPointCnt();
    }
    else if(actualSideKey==3)
    {
      return sides[1].getCurrentPointCnt();
    }
    else
    {
      return 0;
    }
  }
  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder#paint(java.awt.Graphics, org.kalypsodeegree.graphics.transformation.GeoTransform, java.awt.Point)
   */
  public void paint( Graphics g, GeoTransform projection, Point currentPoint )
  {
    LineGeometryBuilder builder=null;
    if(actualSideKey<SIDE_MAX_NUM)
    {
      builder=sides[actualSideKey];
      Assert.throwIAEOnNull( 
          builder, "builder null, therfore not available for drawing" );
    }
    
//    logger.info( "Curves="+Arrays.asList( sides ) );
    final Color curColor=g.getColor();
    
    int i=0;
    for(LineGeometryBuilder b:sides)
    {
        if(b==null)
        {
          continue;
        }
        g.setColor( colors[i]);
        if(b!=builder)
        {
          b.paint( g, projection, null);
          
        }
        else
        {
          b.paint( g, projection, currentPoint );
        }
        i++;
    }
    
    g.setColor( curColor );
   
  }  
  
  public void clearCurrent()
  {
    if(actualSideKey>=SIDE_MAX_NUM)
    {
      return ;
    }
    
    LineGeometryBuilder builder=
                  sides[actualSideKey];
    if(actualSideKey>0)
    {
      LineGeometryBuilder previousBuilder=sides[actualSideKey-1];
      if(previousBuilder!=null)
      {
        GM_Point point=previousBuilder.getLastPoint();
        if(point!=null)
        {
          
          try
          {
            builder.addPoint( point );
          }
          catch( Exception e )
          {
            logger.log(Level.INFO, "Error while setting first point from previous builder", e );
          }
        }
      }
    }
   
  }
  
  public void gotoPreviousSide()
  {
    LineGeometryBuilder curBuilder;
    if(actualSideKey>=SIDE_MAX_NUM)
    {
      //empty
    }
    else
    {
      curBuilder= sides[actualSideKey];
      if(curBuilder!=null)
      {
        curBuilder.clear();
      }
    }
    if(actualSideKey>0)
    {
      actualSideKey--;
    }
  }
  
  public void removeLastPoint()
  {
    if(actualSideKey>=SIDE_MAX_NUM)
    {
      //goto to the last line builder
      actualSideKey=SIDE_MAX_NUM-1;
    }
    
    LineGeometryBuilder builder=
                  sides[actualSideKey];
    
    builder.removeLastPoint( actualSideKey==0 );   
  }
  
  public void replaceLastPoint(GM_Point point)
  {
    if(actualSideKey>=SIDE_MAX_NUM)
    {
      return;
    }
    //TODO check going to previous side
    LineGeometryBuilder builder=
                  sides[actualSideKey];
    
    builder.replaceLastPoint( point );   
  }
  
  /**
   * Tells this classes that to given curves are opposed
   * 
   */
  public void setOpossites(GM_Curve curve1, GM_Curve curve2)
  {
    //Checks opposites
    oppossites[0]=curve1;
    oppossites[1]=curve2;
  }
  
  public void selectNext()
  {
    if(actualSideKey<SIDE_MAX_NUM)
    {
      sides[actualSideKey].setSelected( false );
    }
    
    actualSideKey=(actualSideKey+1) %SIDE_MAX_NUM;
    sides[actualSideKey].setSelected( true );
  }
  
  public void selectPoint(
                  GM_Point squareCenter, 
                  double squareWidth)
  {
    if(actualSideKey<SIDE_MAX_NUM)
    {
      sides[actualSideKey].selectPoint( squareCenter, squareWidth );   
    }
    else
    {
      return;   
    }
  }
  
  public void changeSelectedPoint(  GM_Point newPosition )
  {
    
    if(actualSideKey<SIDE_MAX_NUM)
    {
      sides[actualSideKey].changeSelected( newPosition );
    }
    else
    {
      return;      
    }
    
  }
  
  public GM_Point getSelectedPoint(  )
  {
    
    if(actualSideKey<SIDE_MAX_NUM)
    {
      return sides[actualSideKey].getSelectedPoint();
    }
    else
    {
      return null;      
    }
    
  }
  
  public boolean getHasAllSides()
  {
    return hasAllSides;
  }
  
  public void enableOnTheFlyMesh()
  {
    
  }
  
  private final GM_Point[][] computeMesh() throws GM_Exception
  {
//    GM_Point[][] points= new GM_Point[4][3];
//    for(int i=0;i<4;i++)
//    {
//      for(int j=0;j<3;j++)
//      {
//        points[i][j]= 
//          org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( 
//                i, j, TestWorkspaces.getGaussKrueger() );
//      }
//    }
//    if(true)
//    {
//      return points;
//    }
    //GeometryFactory geometryFactory= new GeometryFactory();
    final LineString topLine = pointToLineString( sides[0] );
    final LineString bottomLine = pointToLineString( sides[2] );
    final LineString leftLine = pointToLineString( sides[1] );
    final LineString rightLine = pointToLineString( sides[3] );
    
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
      IFEDiscretisationModel1d2d model,
      CommandableWorkspace commandableWorkspace) 
      throws GM_Exception
  {
    ChangeDiscretiationModelCommand compositeCommand = 
          new ChangeDiscretiationModelCommand(
              commandableWorkspace,
              model);// new CompositeCommand("Grid Command");
    //compute Points
    GM_Point[][] points2D=computeMesh();
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
        compositeCommand, points2D );    
    
    //add edges
    AddEdgeCommand addEdgeH2D[][]= 
      new AddEdgeCommand[DIM_X][DIM_Y];
    AddEdgeCommand addEdgeV2D[][]= 
          new AddEdgeCommand[DIM_X][DIM_Y];
    AddEdgeInvCommand addEdgeH2DInv[][]= 
      new AddEdgeInvCommand[DIM_X][DIM_Y];
    AddEdgeInvCommand addEdgeV2DInv[][]= 
          new AddEdgeInvCommand[DIM_X][DIM_Y];
    
//    addEdgesH( 
//        model, newNodesArray2D, 
//        compositeCommand, 
//        addEdgeH2D, addEdgeH2DInv);
//    addEdgesV( 
//        model, 
//        newNodesArray2D, compositeCommand, 
//        addEdgeV2D, addEdgeV2DInv );
    
    //add elements
//     addElements( 
//         model, newNodesArray2D, 
//         compositeCommand, 
//         addEdgeH2D, addEdgeH2DInv, 
//         addEdgeV2D, addEdgeV2DInv );
    addElementsFromNodes( model, newNodesArray2D, compositeCommand );
    return compositeCommand;
  }
  
  private final void  addNodesFromPoints(
      IFEDiscretisationModel1d2d model,
      AddNodeCommand[][] newNodesArray2D,
      ChangeDiscretiationModelCommand compositeCommand,
      GM_Point[][] points2D)
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
            points1D[j]);
        newNodesArray2D[i][j]=nodeCommand;//newNodesArray1D[j]=nodeCommand;
        compositeCommand.addCommand( nodeCommand );
      }
    }
  }
  

  
  
  //TODO separate in create horizontal and vertical edges
  //TODO introduce inverted edges along
  //TODO create test case for this
  //TODO Prevent premature double click
  //TODO Caching 
  //TODO on the fly meching plus displaying
  //TODO get the map to be redrawn after last point
 
  private final void  addEdgesH(
      IFEDiscretisationModel1d2d model,
      AddNodeCommand[][] newNodesArray2D,
      ChangeDiscretiationModelCommand compositeCommand,
      AddEdgeCommand[][] addEdgeH2D,
      AddEdgeInvCommand[][] addEdgeH2DInv
      )
  {
    final int LAST_INDEX_I=newNodesArray2D.length-1;
    for(int i=0;i<newNodesArray2D.length;i++)
    {
      final int LAST_INDEX_J=newNodesArray2D[0].length-2;
      AddNodeCommand[] addNodeLine1=newNodesArray2D[i];
      for(int j=0; j<=LAST_INDEX_J;j++)
      {
        //horidonzal edges
        
        if(i==0 )
        {//first line only one direction
          AddEdgeCommand edgeCommand=
              new AddEdgeCommand(model, addNodeLine1[j],addNodeLine1[j+1]);
          compositeCommand.addCommand( edgeCommand );
          addEdgeH2D[i][j]=edgeCommand;
        }
        else if(i==LAST_INDEX_I)
        {//last line back direction
          AddEdgeCommand edgeCommand=
                    new AddEdgeCommand(
                        model,addNodeLine1[j+1], addNodeLine1[j]);
          compositeCommand.addCommand( edgeCommand );
          addEdgeH2D[i][j]=edgeCommand;
        }            
        else
        {
          AddEdgeCommand edgeCommand=
                new AddEdgeCommand(model, addNodeLine1[j],addNodeLine1[j+1]);
          compositeCommand.addCommand( edgeCommand );
          addEdgeH2D[i][j]=edgeCommand;
          
          ////create inverted
            AddEdgeInvCommand edgeInvCommand = 
                  new AddEdgeInvCommand(model,edgeCommand);
            addEdgeH2DInv[i][j] =edgeInvCommand;
            compositeCommand.addCommand( edgeInvCommand );
        }          
      }
    }    
  }
  private final void  addEdgesV(
                  IFEDiscretisationModel1d2d model,
                  AddNodeCommand[][] newNodesArray2D,
                  ChangeDiscretiationModelCommand compositeCommand,
                  AddEdgeCommand[][] addEdgeV2D,
                  AddEdgeInvCommand[][] addEdgeV2DInv)
  {
    for(int i=0;i<newNodesArray2D.length-1;i++)
    {
      final int LENGTH = newNodesArray2D[0].length;
      final int LAST_INDEX=LENGTH-1;
      for(int j=0; j<LENGTH;j++)
      {
              
        if(j==0 )
        {
          AddEdgeCommand edgeCommand=
            new AddEdgeCommand(
                model, 
                newNodesArray2D[i+1][j],
                newNodesArray2D[i][j]);
          compositeCommand.addCommand( edgeCommand );
          addEdgeV2D[i][j]=edgeCommand;
        }
        else if(j==LAST_INDEX)
        {
          AddEdgeCommand edgeCommand=
                      new AddEdgeCommand(
                          model,
                          newNodesArray2D[i][j], 
                          newNodesArray2D[i+1][j]);
          compositeCommand.addCommand( edgeCommand );
          addEdgeV2D[i][j]=edgeCommand;
        }            
        else
        {
          AddEdgeCommand edgeCommand=
            new AddEdgeCommand(
                model, 
                newNodesArray2D[i][j],
                newNodesArray2D[i+1][j]);
          compositeCommand.addCommand( edgeCommand );
          addEdgeV2D[i][j]=edgeCommand;
          
          //create inverted
          AddEdgeInvCommand edgeInvCommand=
            new AddEdgeInvCommand(model, edgeCommand);
          addEdgeV2DInv[i][j] =edgeInvCommand;
          compositeCommand.addCommand( edgeInvCommand );
        }          
      }
    }    
  }
  
  private final void addElements(
      IFEDiscretisationModel1d2d model,
      AddNodeCommand[][] newNodesArray2D,
      ChangeDiscretiationModelCommand compositeCommand,
      AddEdgeCommand[][] addEdgeH2D,
      AddEdgeInvCommand[][] addEdgeH2DInv,
      AddEdgeCommand[][] addEdgeV2D,
      AddEdgeInvCommand[][] addEdgeV2DInv)
  {
      final int LAST_INDEX_I = addEdgeH2D.length-2;
      for(int i=0; i<=LAST_INDEX_I/*i<addEdgeH2D.length-1*/;i++)
      {
        AddEdgeCommand[] aeHCnds0=addEdgeH2D[i];
        final int LAST_INDEX_J=addEdgeH2D[0].length-2;
        for(int j=0;j<=LAST_INDEX_J/*j<aeHCnds0.length-1*/;j++)
        {
          IDiscrMode1d2dlChangeCommand edge0 = addEdgeH2D[i][j];
          IDiscrMode1d2dlChangeCommand edge1 = addEdgeV2D[i+1][j+1];
          IDiscrMode1d2dlChangeCommand edge2 = addEdgeH2DInv[i][j];
          IDiscrMode1d2dlChangeCommand edge3 = 
                        (j==0)?addEdgeV2D[i][j]:addEdgeV2DInv[i][j];
          if(edge0==null || edge1==null || edge2==null|| edge3==null )
          {
            System.out.println("An edge is null");
            continue;
          }
          AddElementCommand addElementCommand=
            new AddElementCommand(
                model,
                new IDiscrMode1d2dlChangeCommand[]{edge0,edge1,edge2,edge3});
          compositeCommand.addCommand( addElementCommand );
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
  public void getAddToModelVV(
                      IFEDiscretisationModel1d2d model) 
                      throws GM_Exception
  {
    GeometryFactory geometryFactory= new GeometryFactory();
    final LineString topLine = pointToLineString( sides[0] );
    final LineString bottomLine = pointToLineString( sides[2] );
    final LineString leftLine = pointToLineString( sides[1] );
    final LineString rightLine = pointToLineString( sides[3] );
    
    JTSQuadMesher mesher= 
              new JTSQuadMesher(
                        topLine,
                        bottomLine,
                        leftLine,
                        rightLine);
    Coordinate[][] coordinates=mesher.calculateMesh();  
    IFE1D2DNode[][] newNodesArray2D= 
                   new IFE1D2DNode[coordinates.length][];
    IFeatureWrapperCollection<IFE1D2DNode> nodes = model.getNodes();
   
    //Create the nodes
    for(int i=0;i<coordinates.length;i++)
    {
      Coordinate[] line=coordinates[i];
      IFE1D2DNode[] newNodesArray1D= 
                        new IFE1D2DNode[line.length];
      newNodesArray2D[i]=newNodesArray1D;
      for(int j=0;j<line.length;j++)
      {
        Coordinate coord=line[j];
        //TODO check node for existance
        IFE1D2DNode node=
          nodes.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
        
        node.setPoint( 
              (GM_Point)JTSAdapter.wrap( 
                  geometryFactory.createPoint(coord)));
        newNodesArray1D[j]=node;
      }
    }
      
//  add edges
    IFeatureWrapperCollection<IFE1D2DEdge> edges = model.getEdges();
    for(int i=0;i<newNodesArray2D.length;i++)
    {
      IFE1D2DNode[] nodeLine1=newNodesArray2D[i];
      IFE1D2DNode[] nodeLine2=
        (i+1<newNodesArray2D.length)?newNodesArray2D[i+1]:null;
      for(int j=0; j<nodeLine1.length-1;j++)
      {
        //horidonzal edges
        IFE1D2DEdge edge = edges.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
        //todo add vertical edge
        edge.addNode( nodeLine1[j].getGmlID() );
        edge.addNode( nodeLine1[j+1].getGmlID() );
        
        if(nodeLine2!=null)
        {
//          edge=edges.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
//          edge.addNode( nodeLine2[j].getGmlID() );
//          edge.addNode( nodeLine2[j+1].getGmlID() );
        }
        
      }
    }
    model.getWrappedFeature().getWorkspace().fireModellEvent( null );
    
  }
  
  private LineString pointToLineString(
                LineGeometryBuilder lineGeometryBuilder)
  {
    final int SIZE=lineGeometryBuilder.getCurrentPointCnt();
      Coordinate coordinates[] = new Coordinate[SIZE];
      for(int i=0;i<SIZE;i++)
      {
        coordinates[i]=
          JTSAdapter.export(  
                lineGeometryBuilder.getPointAt( i ).getPosition());
      }
//    CoordinateSequence 
//    JTSAdapter
    GeometryFactory geometryFactory=new GeometryFactory();
    LineString lineString=
      geometryFactory.createLineString( coordinates );
    return lineString;
  }
}