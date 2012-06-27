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
package org.kalypso.model.km.internal.ui.kmupdate;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.km.internal.core.KMUpdateOperation;
import org.kalypso.model.km.internal.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;


/**
 * @author doemming
 */
public class KMUpdateWizard extends Wizard
{
  private final KMUpdateWizardPage m_kmUpdatePage;

  public KMUpdateWizard( final CommandableWorkspace workspace, final Feature[] initialSelection )
  {
    m_kmUpdatePage = new KMUpdateWizardPage( workspace, initialSelection );

    setWindowTitle( Messages.getString("KMUpdateWizard_0") ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );

    addPage( m_kmUpdatePage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final boolean savePage = m_kmUpdatePage.finish();
    if( !savePage )
      return false;

    final KMUpdateOperation kmUpdateOperation = m_kmUpdatePage.createOperation();

    final IStatus result = RunnableContextHelper.execute( getContainer(), true, false, kmUpdateOperation );
    if( result.matches( IStatus.CANCEL ) )
      return false;

    final StatusDialog statusDialog = new StatusDialog( getShell(), result, getWindowTitle() );
    statusDialog.open();
    return !result.matches( IStatus.ERROR );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    // FIXME: no page should handle that itself
    return m_kmUpdatePage.isPageComplete();
  }
}
