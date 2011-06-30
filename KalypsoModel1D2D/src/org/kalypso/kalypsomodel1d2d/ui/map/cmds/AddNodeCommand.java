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

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Undoable command to add node to a simulation model Broken links are not removed
 * 
 * @author Patrice Congo
 */
public class AddNodeCommand implements IDiscrModel1d2dChangeCommand
{
  private IFE1D2DNode addedNode;

  private GM_Point m_nodePoint;

  private IFEDiscretisationModel1d2d discretisationModel;

  private boolean notCreated[] = new boolean[1];

  private final double m_searchRectWidth;

  private final boolean ignoreZCoordinate;

  /**
   * Adds a node at the given point if there is no node within the specified rectangle
   * 
   * @param model
   *            the model to add the new node to
   * @param searchRectWidth
   *            the width of the search rectangle , its center if given by nodePoint
   * @param nodePoint
   *            the position of the node
   *            <ul>
   *            <li/>true to have the set coordinate of the point ignored (meaning a 2D point is created) <li/>false to
   *            use the position without change
   *            </ul>
   * 
   * 
   */
  public AddNodeCommand( IFEDiscretisationModel1d2d model, GM_Point nodePoint, double searchRectWidth )
  {
    this( model, nodePoint, searchRectWidth, false );
  }

  /**
   * Adds a node at the given point if there is no node within the specified rectangle
   * 
   * @param model
   *            the model to add the new node to
   * @param nodePoint
   *            the position of the node
   * @param searchRectWidth
   *            the width of the search rectangle , its center if given by nodePoint
   * @param ignoreZCoordinate
   *            <ul>
   *            <li/>true to have the set coordinate of the point ignored (meaning a 2D point is created) <li/>false to
   *            use the position without change
   *            </ul>
   */
  public AddNodeCommand( IFEDiscretisationModel1d2d model, GM_Point nodePoint, double searchRectWidth, boolean ignoreZCoordinate )
  {
    this.ignoreZCoordinate = ignoreZCoordinate;
    discretisationModel = model;
    m_searchRectWidth = searchRectWidth;
    if( ignoreZCoordinate )
    {
      m_nodePoint = GeometryFactory.createGM_Point( nodePoint.getX(), nodePoint.getY(), nodePoint.getCoordinateSystem() );
    }
    else
    {
      if( nodePoint.getCoordinateDimension() == 3 )
      {
        m_nodePoint = GeometryFactory.createGM_Point( nodePoint.getX(), nodePoint.getY(), nodePoint.getZ(), nodePoint.getCoordinateSystem() );
      }
      else
      {
        m_nodePoint = GeometryFactory.createGM_Point( nodePoint.getX(), nodePoint.getY(), nodePoint.getCoordinateSystem() );
      }
    }
    // PERFORMANCE-BUGFIX: first search for all nodes, then add it
    addedNode = model.findNode( nodePoint, searchRectWidth );
    if( addedNode != null )
    {
      notCreated[0] = true;
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddNodeCommand.0"); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  // public static boolean[] dummyNotCreated = new boolean[1];

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    if( addedNode == null )
    {
      addedNode = discretisationModel.createNode( m_nodePoint, -1, notCreated );
      System.out.println( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddNodeCommand.1") + addedNode + Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddNodeCommand.2") + notCreated[0] ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    if( addedNode == null )
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
    if( notCreated[0] )
    {
      return;
    }
    else
    {
      // TODO check broken links issue
      discretisationModel.getNodes().remove( addedNode.getGmlID() );
      addedNode = null;
    }

  }

  public IFE1D2DNode getAddedNode( )
  {
    return addedNode;
  }

  public GM_Point getNodePoint( )
  {
    return m_nodePoint;
  }

  public double getSearchRectWidth( )
  {
    return m_searchRectWidth;
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return new IFeatureWrapper2[] { addedNode };
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getDiscretisationModel1d2d()
   */
  @Override
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
    StringBuffer buf = new StringBuffer();
    buf.append( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddNodeCommand.3") ); //$NON-NLS-1$
    buf.append( m_nodePoint );
    buf.append( ']' );
    return buf.toString();
  }
}
