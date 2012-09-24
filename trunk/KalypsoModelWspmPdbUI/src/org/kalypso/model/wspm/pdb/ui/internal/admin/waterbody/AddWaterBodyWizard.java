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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.command.ExecutorRunnable;
import org.kalypso.model.wspm.pdb.connect.command.SaveObjectOperation;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.WaterBodyControl.Mode;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class AddWaterBodyWizard extends Wizard
{
  private final WaterBody m_waterBody = new WaterBody();

  private final WaterBody[] m_existingWaterbodies;

  private final IConnectionViewer m_viewer;

  public AddWaterBodyWizard( final WaterBody[] existingWaterBodies, final IConnectionViewer viewer )
  {
    m_existingWaterbodies = existingWaterBodies;
    m_viewer = viewer;

    setWindowTitle( Messages.getString( "AddWaterBodyAction.0" ) ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
  }

  @Override
  public void addPages( )
  {
    addPage( new EditWaterBodyPage( "editWaterBody", m_waterBody, m_existingWaterbodies, Mode.NEW ) ); //$NON-NLS-1$
  }

  @Override
  public boolean performFinish( )
  {
    final SaveObjectOperation operation = new SaveObjectOperation( m_waterBody );

    final IPdbConnection connection = m_viewer.getConnection();
    final ExecutorRunnable runnable = new ExecutorRunnable( connection, operation );
    final IStatus result = RunnableContextHelper.execute( getContainer(), true, true, runnable );
    if( !result.isOK() )
      new StatusDialog( getShell(), result, getWindowTitle() ).open();

    final ElementSelector selector = new ElementSelector();
    selector.addWaterBodyName( m_waterBody.getName() );
    m_viewer.reload( selector );
    return true;
  }
}