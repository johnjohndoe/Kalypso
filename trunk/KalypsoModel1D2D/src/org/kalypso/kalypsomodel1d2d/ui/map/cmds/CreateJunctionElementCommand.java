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

import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

public class CreateJunctionElementCommand implements IFeatureChangeCommand
{
  private IJunctionElement m_junctionElement;

  private final IFEDiscretisationModel1d2d m_model;

  private final List<IContinuityLine1D> m_lines;

  public CreateJunctionElementCommand( final IFEDiscretisationModel1d2d model, final List<IContinuityLine1D> lines ) throws IllegalArgumentException
  {
    m_model = model;
    m_lines = lines;
  }

  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  @Override
  public void process( ) throws Exception
  {
    if( m_junctionElement == null )
    {
      try
      {
        m_junctionElement = m_model.createJunctionElement();
        for( final IContinuityLine1D line : m_lines )
          m_junctionElement.addLinkedItem( line );
        final Feature feature = m_junctionElement;
        feature.setEnvelopesUpdated();
        final GMLWorkspace workspace = feature.getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, feature.getOwner(), feature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        throw e;
      }
    }
  }

  @Override
  public void redo( ) throws Exception
  {
    if( m_junctionElement == null )
    {
      process();
    }
  }

  @Override
  public void undo( ) throws Exception
  {
    if( m_junctionElement != null )
    {
      // TODO remove element and links to it edges
    }
  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    return new Feature[] { m_junctionElement };
  }

  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.CreateJunctionElementCommand.0" ); //$NON-NLS-1$
  }
}
