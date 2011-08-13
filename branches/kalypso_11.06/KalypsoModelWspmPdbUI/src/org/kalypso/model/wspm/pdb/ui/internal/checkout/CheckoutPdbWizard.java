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
package org.kalypso.model.wspm.pdb.ui.internal.checkout;

import java.net.URI;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.internal.wspm.CheckoutDataMapping;
import org.kalypso.model.wspm.pdb.internal.wspm.CheckoutPdbOperation;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
public class CheckoutPdbWizard extends Wizard
{
  private final CheckoutPdbData m_data;

  public CheckoutPdbWizard( final CheckoutPdbData data )
  {
    m_data = data;

    setNeedsProgressMonitor( true );
    setDialogSettings( DialogSettingsUtils.getDialogSettings( WspmPdbUiPlugin.getDefault(), getClass().getName() ) );
  }

  public void init( final IStructuredSelection selection, final CommandableWorkspace workspace, final TuhhWspmProject project )
  {
    m_data.init( getDialogSettings() );

    final CheckoutDataSearcher searcher = new CheckoutDataSearcher();
    searcher.search( selection );

    final WaterBody[] waterBodies = searcher.getWaterBodies();
    final State[] states = searcher.getStates();
    final CrossSection[] crossSections = searcher.getCrossSections();
    final Event[] events = searcher.getEvents();

    final CheckoutDataMapping mapping = new CheckoutDataMapping( waterBodies, states, crossSections, events, workspace, project );
    m_data.setMapping( mapping );
  }

  @Override
  public void addPages( )
  {
    addPage( new CheckoutPdbPreviewPage( "previewPage", m_data ) ); //$NON-NLS-1$

    // TODO: ask, if all local data should be replaced (or at least an existing copy of the reach should be replaced)
  }

  @Override
  public boolean performFinish( )
  {
    final URI documentBase = m_data.getDocumentBase();
    final CheckoutPdbOperation operation = new CheckoutPdbOperation( m_data.getMapping(), documentBase );
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
    if( !status.isOK() )
      new StatusDialog2( getShell(), status, getWindowTitle() ).open();

    return !status.matches( IStatus.ERROR );
  }
}