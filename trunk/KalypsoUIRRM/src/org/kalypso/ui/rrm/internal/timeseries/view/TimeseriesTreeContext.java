/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.rrm.internal.timeseries.view;

import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * @author Gernot Belger
 */
public class TimeseriesTreeContext
{
  private final StructuredViewer m_viewer;

  private final CommandableWorkspace m_workspace;

  public TimeseriesTreeContext( final StructuredViewer viewer, final CommandableWorkspace workspace )
  {
    m_viewer = viewer;
    m_workspace = workspace;
  }

  public void postCommand( final ICommand command ) throws Exception
  {
    m_workspace.postCommand( command );
  }

  public void setSelection( final TimeseriesNode... selection )
  {
    m_viewer.setSelection( new StructuredSelection( selection ) );
  }

  public void addModellListener( final ModellEventListener modelListener )
  {
    m_workspace.addModellListener( modelListener );
  }

  public void removeModellListener( final ModellEventListener modelListener )
  {
    m_workspace.removeModellListener( modelListener );
  }
}