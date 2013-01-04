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
package org.kalypso.ui.wizards.lengthsection;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ui.wizards.results.SelectResultData;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;

/**
 * Wizard to show length sections to the chart view.
 * 
 * @author Thomas Jung
 */
public class SelectLengthSectionWizard extends Wizard
{
  private final static String PAGE_SELECT_RESULTS_NAME = "selectLengthSection"; //$NON-NLS-1$

  private final IScenarioResultMeta m_resultModel;

  private final IWorkbenchWindow m_window;

  public SelectLengthSectionWizard( final IScenarioResultMeta resultModel, final IWorkbenchWindow window )
  {
    m_resultModel = resultModel;
    m_window = window;

    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.0" ) ); //$NON-NLS-1$
    setDialogSettings( DialogSettingsUtils.getDialogSettings( Kalypso1d2dProjectPlugin.getDefault(), getClass().getName() ) );
  }

  @Override
  public void addPages( )
  {
    final String title = Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.2" ); //$NON-NLS-1$

    final SelectResultData data = new SelectResultData( m_resultModel );
    final SelectResultWizardPage selectResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_RESULTS_NAME, title, data );

    selectResultWizardPage.setFilter( new LengthSectionViewerFilter() );

    addPage( selectResultWizardPage );
  }

  @Override
  public boolean performFinish( )
  {
    final SelectResultWizardPage page = (SelectResultWizardPage)getPage( PAGE_SELECT_RESULTS_NAME );
    final IResultMeta[] results = page.getSelectedResults();

    /* Start */
    final ShowLengthSectionOperation operation = new ShowLengthSectionOperation( results, m_window );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, operation );
    if( !status.isOK() )
      StatusDialog.open( getShell(), status, getWindowTitle() );
//    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.12" ), status ); //$NON-NLS-1$

    return !status.matches( IStatus.ERROR );
  }
}