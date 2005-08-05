/*--------------- Kalypso-Header ------------------------------------------

This file is part of kalypso.
Copyright (C) 2004, 2005 by:

Technical University Hamburg-Harburg (TUHH)
Institute of River and coastal engineering
Denickestr. 22
21073 Hamburg, Germany
http://www.tuhh.de/wb

and

Bjoernsen Consulting Engineers (BCE)
Maria Trost 3
56070 Koblenz, Germany
http://www.bjoernsen.de

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Contact:

E-Mail:
belger@bjoernsen.de
schlienger@bjoernsen.de
v.doemming@tuhh.de

--------------------------------------------------------------------------*/

package org.kalypso.metadoc.ui;

import java.lang.reflect.InvocationTargetException;

import org.apache.commons.configuration.BaseConfiguration;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.internal.UIPlugin;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.java.lang.DisposeHelper;
import org.kalypso.metadoc.IExportTarget;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.IExportableObjectFactory;
import org.kalypso.metadoc.KalypsoMetaDocPlugin;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.configuration.PublishingConfiguration;


public final class ExportWizard extends Wizard
{
  private final Shell m_shell;
  private final WorkspaceModifyOperation m_operation;
  private final IExportTarget m_target;

  public ExportWizard( final IExportTarget target, final IExportableObjectFactory factory, final Shell shell ) throws CoreException
  {
    m_target = target;
    m_shell = shell;

    final IPublishingConfiguration configuration = new PublishingConfiguration( new BaseConfiguration() );

    // one settings-entry per target and factory
    final String settingsName = target.getClass().toString() + "_" + factory.getClass().toString();

    final IDialogSettings workbenchSettings = UIPlugin.getDefault().getDialogSettings();
    IDialogSettings section = workbenchSettings.getSection( settingsName );//$NON-NLS-1$
    if( section == null )
      section = workbenchSettings.addNewSection( settingsName );//$NON-NLS-1$
    setDialogSettings( section );

    final IWizardPage[] factoryPages = factory.createWizardPages( configuration );
    final IWizardPage[] targetPages = target.createWizardPages( configuration );
    for( int i = 0; i < factoryPages.length; i++ )
      addPage( factoryPages[i] );
    for( int i = 0; i < targetPages.length; i++ )
      addPage( targetPages[i] );

    // operation which will be called for finish
    m_operation = new WorkspaceModifyOperation()
    {
      protected void execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException,
          InterruptedException
      {
        try
        {
          final IExportableObject[] objects = factory.createExportableObjects( configuration );

          monitor.beginTask( "", objects.length );

          final IStatus[] stati = new IStatus[objects.length];
          for( int i = 0; i < objects.length; i++ )
          {
            try
            {
              stati[i] = target.commitDocument( objects[i], configuration, new SubProgressMonitor( monitor, 1 ) );
            }
            catch( final Exception e )
            {
              stati[i] = RunnableContextHelper.statusFromThrowable( e );
            }
          }
          
          final IStatus status;
          if( stati.length == 0 )
            status = new Status( IStatus.INFO, KalypsoMetaDocPlugin.getId(), 0, "Es wurden keine Dokumente erzeugt.", null );
          else
            status = stati.length > 1 ? new MultiStatus( KalypsoMetaDocPlugin.getId(), 0, stati, "siehe Details", null ) : stati[0];
          if( !status.isOK() )
            throw new CoreException( status );
        }
        finally
        {
          monitor.done();
        }
      }
    };
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#dispose()
   */
  public void dispose()
  {
    new DisposeHelper( getPages() ).dispose();

    super.dispose();
  }

  public boolean performFinish()
  {
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, m_operation );
    final Throwable exception = status.getException();
    if( exception != null && !status.isOK() )
      exception.printStackTrace();
    ErrorDialog.openError( m_shell, m_target.getName(), "Probleme beim Export", status );

    return status.isOK();
  }
}