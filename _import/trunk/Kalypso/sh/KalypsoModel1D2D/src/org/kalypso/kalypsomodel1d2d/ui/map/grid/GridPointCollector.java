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
package org.kalypso.kalypsomodel1d2d.ui.map.grid;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.opengis.cs.CS_CoordinateSystem;



@SuppressWarnings("unchecked")
public class GridPointCollector /*implements IGeometryBuilder*/
{
  private static final Logger logger=
        Logger.getLogger( GridPointCollector.class.getName() );
  
  public static final int SIDE_MAX_NUM=4;
  public static final int SIDE_TOP=0;
  public static final int SIDE_BOTTOM=2;
  public static final int SIDE_LEFT=1;
  public static final int SIDE_RIGHT=3;
  
  private int actualSideKey;
  
  private boolean hasAllSides=false;
  
  private LinePointCollector sides[] = new LinePointCollector[SIDE_MAX_NUM];
  
  private LinePointCollectorConfig lpcConfigs[]= new LinePointCollectorConfig[SIDE_MAX_NUM];
  
  
  
  private TempGrid tempGrid = new TempGrid(true);
  
  private List<IGridPointCollectorStateListener> stateListeners=
        new ArrayList<IGridPointCollectorStateListener>();
  
  public GridPointCollector()
  {
    initLPCConfigs();
  }
  
  private final void initLPCConfigs()
  {
    Color colors[] = GridWidgetFace.getLineColors();//{Color.BLUE,Color.DARK_GRAY,Color.RED, Color.GREEN};
    int pointRectSize = GridWidgetFace.getPointRectSize();
    for(int i=0;i<sides.length;i++)
    {
      lpcConfigs[i]= 
        new LinePointCollectorConfig("Linie "+(i+1),colors[i],sides[i]);
      lpcConfigs[i].setPointRectSize( pointRectSize );
    }
  }
  
  
  
  
  public void reset(CS_CoordinateSystem targetCrs)
  {
    for(LinePointCollector b:sides)
    {
      if(b!=null)
      {
        b.reset( targetCrs );        
      }
      else
      {
        b=new LinePointCollector(0,targetCrs);
      }
    }
    actualSideKey=0;
    hasAllSides=false;
    tempGrid.resetTempGrid(targetCrs);
    if(sides[actualSideKey]==null)
    {
      sides[actualSideKey]=new LinePointCollector( 0, targetCrs );
      lpcConfigs[actualSideKey].setConfigLinePointCollector( sides[actualSideKey] );
    }
    fireStateChanged();    
  }
  
  public GM_Object addPoint( GM_Point p ) throws Exception
  {
    if(hasAllSides)
    {
      System.out.println("Trying to add point but hasAllsides");
      return null;
    }
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
      fireStateChanged();
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
  
 
  public GM_Object finishSide( ) throws Exception
  {
    Assert.throwIAEOnNull( 
            sides[actualSideKey], 
            "builder not available" );
    if(actualSideKey>=SIDE_MAX_NUM)
    {
      return null;
    }
    LinePointCollector oldBuilder=sides[actualSideKey];      
    GM_Object gmObject=sides[actualSideKey].finish();
    if(gmObject==null)
    {
      //not finish
      return null;
    }
    actualSideKey++;
    if(actualSideKey<SIDE_MAX_NUM)
    {
      //actualSideKey++;
      LinePointCollector newSide=sides[actualSideKey];
      if(newSide==null)
      {
        newSide=oldBuilder.getNewBuilder();
        sides[actualSideKey]= newSide;     
        lpcConfigs[actualSideKey].setConfigLinePointCollector(newSide);
      }
      
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
    else
    {
      hasAllSides=true;
      updateTempGrid();
    }
//    logger.info( "Curve="+((GM_Curve)gmObject).getAsLineString()+ 
//                "\n\tactualSide="+actualSideKey );
    fireStateChanged();
    return gmObject;      
  }
  
  private final void updateTempGrid() throws GM_Exception
  {
    tempGrid.setTempGrid( 
        sides[SIDE_TOP],
        sides[SIDE_BOTTOM], 
        sides[SIDE_LEFT], 
        sides[SIDE_RIGHT] );
    
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
    LinePointCollector builder=null;
    if(actualSideKey<SIDE_MAX_NUM)
    {
      if ( sides[actualSideKey] == null )
        return;
     
      builder=sides[actualSideKey];
      Assert.throwIAEOnNull( 
      builder, "builder null, therfore not available for drawing" );
    }
    
//    logger.info( "Curves="+Arrays.asList( sides ) );
    final Color curColor=g.getColor();
    
    int i=0;
    for(LinePointCollector b:sides)
    {
        if(b==null)
        {
          continue;
        }
        g.setColor( lpcConfigs[i].getColor());
        int pointRectSize=lpcConfigs[i].getPointRectSize();
        if(b!=builder)
        {
          b.paint( g, projection, null, pointRectSize);
        }
        else
        {
          b.paint( g, projection, currentPoint, pointRectSize );
        }
        i++;
    }
    /* draw selected line */
    if(actualSideKey<SIDE_MAX_NUM)
    {
      builder=sides[actualSideKey];
      builder.paintLine( g, projection, i, lpcConfigs[actualSideKey].getColor() );
      g.setColor( curColor );     
    }
    
    /* draw temp grid */
    tempGrid.paint( g, projection,lpcConfigs[0].getPointRectSize());
   
  }  
  
  public void clearCurrent()
  {
    if(actualSideKey>=SIDE_MAX_NUM)
    {
      return ;
    }
    
    LinePointCollector builder=
                  sides[actualSideKey];
    builder.clear();
    if(actualSideKey>0)
    {
      LinePointCollector previousBuilder=sides[actualSideKey-1];
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
    hasAllSides=false;
    fireStateChanged();
  }
  
  public void gotoPreviousSide()
  {
    LinePointCollector curBuilder;
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
      sides[actualSideKey].removeLastPoint( false );
      if ( actualSideKey < 2 )
        sides[actualSideKey].removeMaxNum(  );
    }
    
    fireStateChanged();
  }
  
  public void removeLastPoint()
  {
    if(actualSideKey>=SIDE_MAX_NUM)
    {
      //goto to the last line builder
      actualSideKey=SIDE_MAX_NUM-1;
    }
    
    LinePointCollector builder=
                  sides[actualSideKey];
    
    builder.removeLastPoint( actualSideKey==0 );   
    fireStateChanged(  );
  }
  
  public void replaceLastPoint(GM_Point point)
  {
    if(actualSideKey>=SIDE_MAX_NUM)
    {
      return;
    }
    //TODO check going to previous side
    LinePointCollector builder=
                  sides[actualSideKey];
    
    builder.replaceLastPoint( point );   
  }
  
  
  public void selectNext()
  {
    if(actualSideKey<SIDE_MAX_NUM)
    {
      sides[actualSideKey].setSelected( false );
      
    }
    
    actualSideKey=(actualSideKey+1) %SIDE_MAX_NUM;
    sides[actualSideKey].setSelected( true );
    fireStateChanged();
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
  
  public void changeSelectedPoint(  GM_Point newPosition ) throws GM_Exception
  {
    
    if(actualSideKey<SIDE_MAX_NUM)
    {
      sides[actualSideKey].changeSelected( newPosition );
      updateTempGrid();
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
  
//  public void enableOnTheFlyMesh()
//  {
//    
//  }
//  
  
  
  
  
  

  
  
  //TODO separate in create horizontal and vertical edges
  //TODO introduce inverted edges along
  //TODO create test case for this
  //TODO Prevent premature double click
  //TODO Caching 
  //TODO on the fly meching plus displaying
  //TODO get the map to be redrawn after last point
 
//  private final void  addEdgesH(
//      IFEDiscretisationModel1d2d model,
//      AddNodeCommand[][] newNodesArray2D,
//      ChangeDiscretiationModelCommand compositeCommand,
//      AddEdgeCommand[][] addEdgeH2D,
//      AddEdgeInvCommand[][] addEdgeH2DInv
//      )
//  {
//    final int LAST_INDEX_I=newNodesArray2D.length-1;
//    for(int i=0;i<newNodesArray2D.length;i++)
//    {
//      final int LAST_INDEX_J=newNodesArray2D[0].length-2;
//      AddNodeCommand[] addNodeLine1=newNodesArray2D[i];
//      for(int j=0; j<=LAST_INDEX_J;j++)
//      {
//        //horidonzal edges
//        
//        if(i==0 )
//        {//first line only one direction
//          AddEdgeCommand edgeCommand=
//              new AddEdgeCommand(model, addNodeLine1[j],addNodeLine1[j+1]);
//          compositeCommand.addCommand( edgeCommand );
//          addEdgeH2D[i][j]=edgeCommand;
//        }
//        else if(i==LAST_INDEX_I)
//        {//last line back direction
//          AddEdgeCommand edgeCommand=
//                    new AddEdgeCommand(
//                        model,addNodeLine1[j+1], addNodeLine1[j]);
//          compositeCommand.addCommand( edgeCommand );
//          addEdgeH2D[i][j]=edgeCommand;
//        }            
//        else
//        {
//          AddEdgeCommand edgeCommand=
//                new AddEdgeCommand(model, addNodeLine1[j],addNodeLine1[j+1]);
//          compositeCommand.addCommand( edgeCommand );
//          addEdgeH2D[i][j]=edgeCommand;
//          
//          ////create inverted
//            AddEdgeInvCommand edgeInvCommand = 
//                  new AddEdgeInvCommand(model,edgeCommand);
//            addEdgeH2DInv[i][j] =edgeInvCommand;
//            compositeCommand.addCommand( edgeInvCommand );
//        }          
//      }
//    }    
//  }
  
//  private final void  addEdgesV(
//                  IFEDiscretisationModel1d2d model,
//                  AddNodeCommand[][] newNodesArray2D,
//                  ChangeDiscretiationModelCommand compositeCommand,
//                  AddEdgeCommand[][] addEdgeV2D,
//                  AddEdgeInvCommand[][] addEdgeV2DInv)
//  {
//    for(int i=0;i<newNodesArray2D.length-1;i++)
//    {
//      final int LENGTH = newNodesArray2D[0].length;
//      final int LAST_INDEX=LENGTH-1;
//      for(int j=0; j<LENGTH;j++)
//      {
//              
//        if(j==0 )
//        {
//          AddEdgeCommand edgeCommand=
//            new AddEdgeCommand(
//                model, 
//                newNodesArray2D[i+1][j],
//                newNodesArray2D[i][j]);
//          compositeCommand.addCommand( edgeCommand );
//          addEdgeV2D[i][j]=edgeCommand;
//        }
//        else if(j==LAST_INDEX)
//        {
//          AddEdgeCommand edgeCommand=
//                      new AddEdgeCommand(
//                          model,
//                          newNodesArray2D[i][j], 
//                          newNodesArray2D[i+1][j]);
//          compositeCommand.addCommand( edgeCommand );
//          addEdgeV2D[i][j]=edgeCommand;
//        }            
//        else
//        {
//          AddEdgeCommand edgeCommand=
//            new AddEdgeCommand(
//                model, 
//                newNodesArray2D[i][j],
//                newNodesArray2D[i+1][j]);
//          compositeCommand.addCommand( edgeCommand );
//          addEdgeV2D[i][j]=edgeCommand;
//          
//          //create inverted
//          AddEdgeInvCommand edgeInvCommand=
//            new AddEdgeInvCommand(model, edgeCommand);
//          addEdgeV2DInv[i][j] =edgeInvCommand;
//          compositeCommand.addCommand( edgeInvCommand );
//        }          
//      }
//    }    
//  }
  
  public ICommand getAddToModelCommand(
      MapPanel mapPanel,
      IFEDiscretisationModel1d2d model,
      CommandableWorkspace commandableWorkspace) throws GM_Exception
  {
    int pointRectSize=lpcConfigs[0].getPointRectSize();
    double searchRectWidth = 
        sides[0].getHandleWidthAsWorldDistance( mapPanel,pointRectSize);
    ICommand addGridCommand=
      tempGrid.getAddToModelCommand( 
                      mapPanel, model, 
                      commandableWorkspace, 
                      searchRectWidth );
    return addGridCommand;
  }
  
  
  public LinePointCollectorConfig[] getSideconfigsAsArray()
  {
    LinePointCollectorConfig[] cloneCollectorConfigs = lpcConfigs.clone();
    return cloneCollectorConfigs;
  }
  
  public LinePointCollectorConfig getCurrentLPCConfig()
  {
    if(actualSideKey<SIDE_MAX_NUM)
    {
      return lpcConfigs[actualSideKey];
    }
    else
    {
      return null;
    }
  }
  
  
  /**
   * To get the with for the square that are drawn to show 
   * point. 
   * if ther is an active {@link LinePointCollectorConfig} its 
   * actual point rect size  is resturn otherwise the point 
   * square size of the first {@link LinePointCollectorConfig}
   *  
   * @return Returns the with for the square that are drawn to show 
   * point
   */
  public int getPointRectSize()
  {
    if(actualSideKey<SIDE_MAX_NUM)
    {
      return lpcConfigs[actualSideKey].getPointRectSize();
    }
    else
    {
      return lpcConfigs[0].getPointRectSize();
    }
  }
  
  
  public void addGridPointCollectorStateChangeListener(
                              IGridPointCollectorStateListener listener)
  {
    Assert.throwIAEOnNullParam( listener, "listener" );
    if(stateListeners.contains( listener ))
    {
      return;
    }
    else
    {
      stateListeners.add( listener );
    }
  }
  
  private final void fireStateChanged(GridPointColectorChangeEvent event)
  {
    for(IGridPointCollectorStateListener listener:stateListeners)
    {
      listener.stateChanged( event );
    }    
  }
  
  private final void fireStateChanged()
  {
    for(IGridPointCollectorStateListener listener:stateListeners)
    {
      listener.stateChanged(null);
    }    
  }
  
  public void setPointRectSize(int pointRectSize)
  {
    if(pointRectSize<=0)
    {
      throw new IllegalArgumentException("pointrectSize must positive:"+pointRectSize);
    }
    else
    {
      for(LinePointCollectorConfig config:lpcConfigs)
      {
       config.setPointRectSize( pointRectSize ) ;
      }
    }
  }
  
  public void setColor(int lineIndex, Color lineColor)
  {
    lpcConfigs[lineIndex].setColor( lineColor );
    fireStateChanged();
  }
}