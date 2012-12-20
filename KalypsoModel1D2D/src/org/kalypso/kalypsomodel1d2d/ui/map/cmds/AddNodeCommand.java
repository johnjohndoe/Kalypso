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
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Undoable command to add node to a simulation model Broken links are not removed
 * 
 * @author Patrice Congo
 */
public class AddNodeCommand implements IFeatureChangeCommand
{
  private IFE1D2DNode m_addedNode;

  private GM_Point m_nodePoint;

  private final IFEDiscretisationModel1d2d m_discretisationModel;

  private final double m_searchRectWidth;

  /**
   * Adds a node at the given point if there is no node within the specified rectangle
   * 
   * @param model
   *          the model to add the new node to
   * @param searchRectWidth
   *          the width of the search rectangle , its center if given by nodePoint
   * @param nodePoint
   *          the position of the node
   *          <ul>
   *          <li/>true to have the set coordinate of the point ignored (meaning a 2D point is created)
   *          <li/>false to use the position without change
   *          </ul>
   */
  public AddNodeCommand( final IFEDiscretisationModel1d2d model, final GM_Point nodePoint, final double searchRectWidth )
  {
    this( model, nodePoint, searchRectWidth, false );
  }

  /**
   * Adds a node at the given point if there is no node within the specified rectangle
   * 
   * @param model
   *          the model to add the new node to
   * @param nodePoint
   *          the position of the node
   * @param searchRectWidth
   *          the width of the search rectangle , its center if given by nodePoint
   * @param ignoreZCoordinate
   *          <ul>
   *          <li/>true to have the set coordinate of the point ignored (meaning a 2D point is created)
   *          <li/>false to use the position without change
   *          </ul>
   */
  public AddNodeCommand( final IFEDiscretisationModel1d2d model, final GM_Point nodePoint, final double searchRectWidth, final boolean ignoreZCoordinate )
  {
    // this.ignoreZCoordinate = ignoreZCoordinate;
    m_discretisationModel = model;
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
    m_addedNode = model.findNode( nodePoint, searchRectWidth );
  }

  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddNodeCommand.0" ); //$NON-NLS-1$
  }

  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  @Override
  public void process( ) throws Exception
  {
    if( m_addedNode == null )
    {
      m_addedNode = m_discretisationModel.createNode( m_nodePoint );
    }
  }

  @Override
  public void redo( ) throws Exception
  {
    if( m_addedNode == null )
    {
      process();
    }
  }

  @Override
  public void undo( ) throws Exception
  {
  }

  public IFE1D2DNode getAddedNode( )
  {
    return m_addedNode;
  }

  public GM_Point getNodePoint( )
  {
    return m_nodePoint;
  }

  public double getSearchRectWidth( )
  {
    return m_searchRectWidth;
  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    return new Feature[] { m_addedNode };
  }

  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer();
    buf.append( "AddNodeCommand[" ); //$NON-NLS-1$
    buf.append( m_nodePoint );
    buf.append( ']' );
    return buf.toString();
  }
}
