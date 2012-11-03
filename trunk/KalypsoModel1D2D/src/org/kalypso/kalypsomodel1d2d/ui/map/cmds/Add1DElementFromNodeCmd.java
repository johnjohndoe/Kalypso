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

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Undoable Add 1D fe element command from add node cmds
 * 
 * @author Patrice Congo
 */
public class Add1DElementFromNodeCmd implements IFeatureChangeCommand
{
  private IElement1D m_addedElement;

  private final AddNodeCommand m_elementNodeCmds[];

  private final IFEDiscretisationModel1d2d m_model;

  /**
   * @param model
   * @param elementEdgeCmds
   *          an array the command used to create the edges of the element to be created by this command. the array
   *          must contains only {@link AddEdgeCommand} and {@link AddEdgeInvCommand} commands
   */
  public Add1DElementFromNodeCmd( final IFEDiscretisationModel1d2d model, final AddNodeCommand[] elementNodeCmds )
  {
    Assert.throwIAEOnNullParam( model, "Modell" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( elementNodeCmds, "elementEdgeCmds" ); //$NON-NLS-1$

    for( final IFeatureChangeCommand cmd : elementNodeCmds )
    {
      if( cmd == null )
      {
        throw new IllegalArgumentException( "elementNodeCmds must only contains non null node cmds: " + elementNodeCmds ); //$NON-NLS-1$
      }
    }
    if( elementNodeCmds.length != 2 )
    {
      throw new IllegalArgumentException( "This create 1D element command required 2 create node command\r\n\tbut got=" + elementNodeCmds.length + " node Command" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

    m_model = model;

    m_elementNodeCmds = elementNodeCmds;
  }

  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.6" ); //$NON-NLS-1$
  }

  @Override
  public boolean isUndoable( )
  {
    // TODO implement undo using delete command
    return false;
  }

  @Override
  public void process( ) throws Exception
  {
    if( m_addedElement == null )
    {
      IFE1D2DEdge curEdge;

      final IFE1D2DNode node0 = m_elementNodeCmds[0].getAddedNode();
      final IFE1D2DNode node1 = m_elementNodeCmds[1].getAddedNode();
      if( node0 == null || node1 == null )
      {
        throw new IllegalStateException( "One of the base node command returns a null node: \r\n\tnode0= " + node0 + "\r\n\tnode1= " + node1 ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      }
      if( node0.equals( node1 ) )
      {
        throw new UnsupportedOperationException( "Building an edge from one node not supported" ); //$NON-NLS-1$
      }

      curEdge = m_model.findEdge( node0, node1 );
      if( curEdge == null )
      {
        curEdge = m_model.createEdge( node0, node1 );
      }
      else
      {
        // test whether the edge is in an element
        if( curEdge.getLinkedElements().length == 0 )
        {
          return;
        }
      }

      m_addedElement = m_model.createElement1D( curEdge );
    }
  }

  @Override
  public void redo( ) throws Exception
  {
    if( m_addedElement == null )
    {
      process();
    }
  }

  @Override
  public void undo( ) throws Exception
  {
    if( m_addedElement != null )
    {
      // TODO remove element and links to it edges
    }
  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    return new Feature[] { m_addedElement };
  }

  /**
   * Returns the newly created element.
   * 
   * @return <code>null</code>, if the command was not yet processed.
   */
  public IElement1D getAddedElement( )
  {
    return m_addedElement;
  }
}
