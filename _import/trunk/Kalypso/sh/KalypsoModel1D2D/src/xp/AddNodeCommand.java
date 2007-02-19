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


import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;


/**
 * Undoable command to add node to a simulation model
 * Broken links are not removed
 * 
 * @author Patrice Congo
 */
public class AddNodeCommand implements IDiscrModel1d2dChangeCommand
{
  
  private IFE1D2DNode<IFE1D2DEdge>  addedNode;
  private GM_Point nodePoint;
  private IFEDiscretisationModel1d2d discretisationModel;
  private boolean notCreated[]= new boolean[1];
  private double searchRectWidth;
  
  public AddNodeCommand(
              IFEDiscretisationModel1d2d model, 
              GM_Point nodePoint,
              double searchRectWidth)
  {
    this.discretisationModel=model;
    //FIXME point z coordinate causes problem
    this.nodePoint=
      GeometryFactory.createGM_Point(
              nodePoint.getX(),
              nodePoint.getY(),
              nodePoint.getCoordinateSystem());
    this.searchRectWidth=searchRectWidth;
  }
  
  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Adding Node";
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    addedNode=discretisationModel.createNode( 
                    nodePoint,searchRectWidth, notCreated );
//    <System.out.println("Adding node from command:"+addedNode+" "+notCreated[0]);
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    if(addedNode==null)
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    if(notCreated[0])
    {
      return;
    }
    else
    {
      //TODO check broken links issue
      discretisationModel.getNodes().remove( addedNode.getGmlID() );
      addedNode=null;
    }
    
    
  }

  public IFE1D2DNode<IFE1D2DEdge> getAddedNode( )
  {
    return addedNode;
  }
  
  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper getChangedFeature( )
  {
    return addedNode;
  }
  
  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return discretisationModel;
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    StringBuffer buf= new StringBuffer();
    buf.append( "AddNodeCommand[" );
    buf.append( nodePoint );
    buf.append( ']' );
    return buf.toString();
  }
}
