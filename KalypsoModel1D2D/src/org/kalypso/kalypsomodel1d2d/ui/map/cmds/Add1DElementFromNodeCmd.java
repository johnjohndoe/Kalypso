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

import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Undoable Add 1D fe element command from add node cmds
 * 
 * @author Patrice Congo
 */
public class Add1DElementFromNodeCmd implements IDiscrModel1d2dChangeCommand
{
  private IElement1D addedElement;

  private final AddNodeCommand elementNodeCmds[];

  private final IFEDiscretisationModel1d2d model;

  /**
   * @param model
   * @param elementEdgeCmds
   *            an array the command used to create the edges of the element to be created by this command. the array
   *            must contains only {@link AddEdgeCommand} and {@link AddEdgeInvCommand} commands
   */
  public Add1DElementFromNodeCmd( final IFEDiscretisationModel1d2d model, final AddNodeCommand[] elementNodeCmds )
  {
    Assert.throwIAEOnNullParam( model, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.0") ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( elementNodeCmds, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.1") ); //$NON-NLS-1$
    for( final IDiscrModel1d2dChangeCommand cmd : elementNodeCmds )
    {
      if( cmd == null )
      {
        throw new IllegalArgumentException( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.2") + elementNodeCmds ); //$NON-NLS-1$
      }
    }
    if( elementNodeCmds.length != 2 )
    {
      throw new IllegalArgumentException( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.3") + Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.4") + elementNodeCmds.length + Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.5") ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

    this.model = model;

    this.elementNodeCmds = elementNodeCmds;

  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.6"); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    // TODO implement undo using delete command
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    if( addedElement == null )
    {
      IFE1D2DEdge curEdge;

      final IFE1D2DNode node0 = elementNodeCmds[0].getAddedNode();
      final IFE1D2DNode node1 = elementNodeCmds[1].getAddedNode();
      if( node0 == null || node1 == null )
      {
        throw new IllegalStateException( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.7") + Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.8") + node0 + Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.9") + node1 ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      }
      if( node0.equals( node1 ) )
      {
        throw new UnsupportedOperationException( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.10") ); //$NON-NLS-1$
      }

      curEdge = model.findEdge( node0, node1 );
      if( curEdge == null )
      {
        final int size1 = model.getEdges().size();
        curEdge = FE1D2DEdge.createFromModel( model, node0, node1 );
        final int size2 = model.getEdges().size();
        if( size2 - size1 != 1 )
        {
          throw new IllegalStateException( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd.11") ); //$NON-NLS-1$
        }
      }
      else
      {
        // test whether the edge is in an element
        if( ModelOps.isContainedInAnElement( curEdge ) )
        {

          return;
        }
      }

      addedElement = ModelOps.createElement1d( model, curEdge );
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    if( addedElement == null )
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
    if( addedElement != null )
    {
      // TODO remove element and links to it edges
    }
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return new IFeatureWrapper2[] { addedElement };
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getDiscretisationModel1d2d()
   */
  @Override
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return model;
  }

  /**
   * Returns the newly created element.
   * 
   * @return <code>null</code>, if the command was not yet processed.
   */
  public IElement1D getAddedElement( )
  {
    return addedElement;
  }
}
