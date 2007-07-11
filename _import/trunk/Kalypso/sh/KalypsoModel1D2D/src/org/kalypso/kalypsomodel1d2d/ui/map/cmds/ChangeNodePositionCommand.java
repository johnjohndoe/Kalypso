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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds;


import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;


/**
 * Undoable command to change the position of a node.
 * the change can be specified as a point or a change 
 * elevation
 * 
 * 
 * @author Patrice Congo
 */
public class ChangeNodePositionCommand implements IDiscrModel1d2dChangeCommand
{
  
  private IFE1D2DNode<IFE1D2DEdge>  movedNode;
  private GM_Point newNodePoint;
  private IFEDiscretisationModel1d2d discretisationModel;
  private GM_Point oldPosition = null;
  
  public ChangeNodePositionCommand(
              IFEDiscretisationModel1d2d model,
              IFE1D2DNode nodeToChange,
              GM_Point newNodePoint1)
  {
    this.discretisationModel=model;
    this.movedNode=nodeToChange;
    if (newNodePoint1.getCoordinateDimension()==3)
    {
      this.newNodePoint=
        GeometryFactory.createGM_Point(
                newNodePoint1.getX(),
                newNodePoint1.getY(),
                newNodePoint1.getZ(),
                newNodePoint1.getCoordinateSystem());
    }
    else
    {
      this.newNodePoint=
        GeometryFactory.createGM_Point(
                newNodePoint1.getX(),
                newNodePoint1.getY(),
                newNodePoint1.getCoordinateSystem());
    }    
  }
  
  public ChangeNodePositionCommand(
                IFEDiscretisationModel1d2d model,
                IFE1D2DNode nodeToChange,
                double elevation)
  {
    this.discretisationModel=model;
    this.movedNode=nodeToChange;
    GM_Point oldPos=nodeToChange.getPoint();
    if (Double.isNaN( elevation ))
    {
      this.newNodePoint=
        GeometryFactory.createGM_Point(
                oldPos.getX(),
                oldPos.getY(),
                oldPos.getCoordinateSystem());
    }
    else
    {
      this.newNodePoint=
        GeometryFactory.createGM_Point(
                oldPos.getX(),
                oldPos.getY(),
                elevation,
                oldPos.getCoordinateSystem());
    }    
  }
  
  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Changing node positiom";
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
    oldPosition=movedNode.getPoint();
    movedNode.setPoint( newNodePoint );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    if(oldPosition==null)
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    if(oldPosition!=null)
    {
      movedNode.setPoint( oldPosition );
      oldPosition = null;
    }
  }

//  public IFE1D2DNode<IFE1D2DEdge> getAddedNode( )
//  {
//    return movedNode;
//  }
  
  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return new IFeatureWrapper2[]{movedNode};
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
    buf.append( "ChangeNodePositionCommand[" );
    buf.append( newNodePoint );
    buf.append( ']' );
    return buf.toString();
  }

  public IFE1D2DNode<IFE1D2DEdge> getMovedNode( )
  {
    return movedNode;
  }
}
