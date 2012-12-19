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
package org.kalypso.ui.wizards.results;

import java.io.File;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.swt.widgets.FileDialogUtils;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypso1d2d.internal.I2DContants;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class ImportResultAction extends Action
{
  private static final String SETTINGS_IMPORT_DIR = "externalRsultImportDir"; //$NON-NLS-1$

  private final SelectResultWizardPage m_page;

  private final IScenarioDataProvider m_modelProvider;

  public ImportResultAction( final SelectResultWizardPage page, final IScenarioDataProvider modelProvider )
  {
    m_page = page;

    m_modelProvider = modelProvider;

    setToolTipText( Messages.getString("ImportResultAction_0") ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    setImageDescriptor( imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.IMPORT_EXTERNAL_RESULT ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final IWizard wizard = m_page.getWizard();
    final IDialogSettings dialogSettings = wizard.getDialogSettings();

    final ICoreRunnableWithProgress importOperation = askForFiles( shell, dialogSettings );
    if( importOperation == null )
    {
      // cancelled
      return;
    }

    final IStatus status = RunnableContextHelper.execute( wizard.getContainer(), true, true, importOperation );
    new StatusDialog( shell, status, wizard.getWindowTitle() ).open();

    /* handle tree */
    final CheckboxTreeViewer treeViewer = m_page.getTreeViewer();
    ViewerUtilities.refresh( treeViewer, true );
  }

  private ICoreRunnableWithProgress askForFiles( final Shell shell, final IDialogSettings settings )
  {
    final File lastDir = findLastDir( settings );

    final FileDialog dialog = new FileDialog( shell, SWT.OPEN | SWT.MULTI );
    if( lastDir != null )
      dialog.setFilterPath( lastDir.getAbsolutePath() );

    FileDialogUtils.addFilter( dialog, I2DContants.STR_FILTERNAME_2D, I2DContants.EXTENSION_2D );

    final File[] files = FileDialogUtils.open( dialog );
    if( files == null )
      return null;

    final String selectedExtension = FileDialogUtils.getSelectedExtension( dialog );
    return createImportOperation( selectedExtension, files );
  }

  private File findLastDir( final IDialogSettings settings )
  {
    if( settings == null )
      return null;

    final String path = settings.get( SETTINGS_IMPORT_DIR );
    if( StringUtils.isBlank( path ) )
      return null;

    return new File( path );
  }

  private ICoreRunnableWithProgress createImportOperation( final String selectedExtension, final File[] files )
  {
    if( I2DContants.EXTENSION_2D.equals( selectedExtension ) )
      return new Import2DResultsOperation( files, m_modelProvider );

    throw new IllegalStateException();
  }
}
