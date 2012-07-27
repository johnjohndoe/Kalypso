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

import java.util.ArrayList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Undoable Add fe element command
 *
 * @author Patrice Congo
 */
public class AddElementCommand implements IFeatureChangeCommand
{
  // TODO donot forget firering update events
  private IFE1D2DElement m_addedElement;

  private final IFeatureChangeCommand m_elementEdgeCmds[];

  private final IFEDiscretisationModel1d2d m_model;

  /**
   * @param model
   * @param elementEdgeCmds
   *            an array the command used to create the edges of the element to be created by this command. the array
   *            must contains only {@link AddEdgeCommand} and {@link AddEdgeInvCommand} commands
   */
  public AddElementCommand( final IFEDiscretisationModel1d2d model, final IFeatureChangeCommand[] elementEdgeCmds )
  {
    Assert.throwIAEOnNullParam( model, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddElementCommand.0") ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( elementEdgeCmds, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddElementCommand.1") ); //$NON-NLS-1$

    for( final IFeatureChangeCommand cmd : elementEdgeCmds )
    {
      if( !(cmd instanceof AddEdgeCommand) )
      {
        throw new IllegalArgumentException( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddElementCommand.2") + cmd ); //$NON-NLS-1$
      }
    }

    this.m_model = model;

    this.m_elementEdgeCmds = elementEdgeCmds;

  }

  @Override
  public String getDescription( )
  {
    return Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddElementCommand.3"); //$NON-NLS-1$
  }

  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  @Override
  public void process( ) throws Exception
  {
    if( m_addedElement == null )
    {
      final List<IFE1D2DEdge> edges = new ArrayList<IFE1D2DEdge>();
      IFE1D2DEdge curEdge;
      for( final IFeatureChangeCommand edgeCmd : m_elementEdgeCmds )
      {
        curEdge = (IFE1D2DEdge) edgeCmd.getChangedFeatures()[0];
        if( curEdge != null )
        {
          edges.add( curEdge );
        }

      }
      m_addedElement = ModelOps.createElement2d( m_model, edges );
      System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddElementCommand.4" ) + m_addedElement ); //$NON-NLS-1$
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
}
