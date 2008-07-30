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

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.configuration.BaseConfiguration;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.java.lang.DisposeHelper;
import org.kalypso.metadoc.IExportTarget;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.IExportableObjectFactory;
import org.kalypso.metadoc.KalypsoMetaDocPlugin;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.configuration.PublishingConfiguration;

/**
 * The export wizard takes care of creating the pages using the given target and exportable-object-factory.
 * 
 * @author schlienger
 */
public final class ExportWizard extends Wizard
{
  private final Shell m_shell;
  private final WorkspaceModifyOperation m_operation;
  private final IExportTarget m_target;

  public ExportWizard( final IExportTarget target, final IExportableObjectFactory factory, final Shell shell,
      final ImageDescriptor defaultImage, final String windowTitle ) throws CoreException
  {
    m_target = target;
    m_shell = shell;

    setNeedsProgressMonitor( true );
    setWindowTitle( windowTitle );

    final IPublishingConfiguration configuration = new PublishingConfiguration( new BaseConfiguration() );

    // one settings-entry per target and factory
    final String settingsName = target.getClass().toString() + "_" + factory.getClass().toString();

    final IDialogSettings workbenchSettings = KalypsoMetaDocPlugin.getDefault().getDialogSettings();
    IDialogSettings section = workbenchSettings.getSection( settingsName );//$NON-NLS-1$
    if( section == null )
      section = workbenchSettings.addNewSection( settingsName );//$NON-NLS-1$
    setDialogSettings( section );

    // use the target image as default image for this wizard
    final IWizardPage[] factoryPages = factory.createWizardPages( configuration, defaultImage );
    final IWizardPage[] targetPages = target.createWizardPages( configuration );
    for( int i = 0; i < factoryPages.length; i++ )
      addPage( factoryPages[i] );
    for( int i = 0; i < targetPages.length; i++ )
      addPage( targetPages[i] );

    // operation which will be called for finish
    m_operation = new WorkspaceModifyOperation()
    {
      @Override
      protected void execute( final IProgressMonitor monitor ) throws CoreException, InterruptedException
      {
        final List<IStatus> stati = new ArrayList<IStatus>();

        try
        {
          final IExportableObject[] objects = factory.createExportableObjects( configuration );

          monitor.beginTask( "Export", objects.length );

          for( int i = 0; i < objects.length; i++ )
          {
            if( monitor.isCanceled() )
              throw new InterruptedException();

            final IExportableObject exportableObject = objects[i];
            monitor.subTask( exportableObject.getPreferredDocumentName() );
            
            IStatus status;
            try
            {
              status = target.commitDocument( exportableObject, configuration, new SubProgressMonitor( monitor, 1 ) );
            }
            catch( final Exception e )
            {
              status = StatusUtilities.statusFromThrowable( e );
              
              KalypsoMetaDocPlugin.getDefault().getLog().log( status );
            }

            stati.add( status );
          }
        }
        catch( final CoreException e )
        {
          KalypsoMetaDocPlugin.getDefault().getLog().log( e.getStatus() );
          
          stati.add( e.getStatus() );
        }
        finally
        {
          monitor.done();
        }

        final IStatus status;
        if( stati.size() == 0 )
          status = new Status( IStatus.INFO, KalypsoMetaDocPlugin.getId(), 0, "Es wurden keine Dokumente erzeugt.",
              null );
        else
          status = StatusUtilities.createStatus( stati, "Siehe Details" );
        if( !status.isOK() )
          throw new CoreException( status );
      }
    };
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#dispose()
   */
  @Override
  public void dispose()
  {
    new DisposeHelper( getPages() ).dispose();

    super.dispose();
  }

  @Override
  public boolean performFinish( )
  {
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, m_operation );
    final Throwable exception = status.getException();
    if( exception != null && !status.isOK() )
      exception.printStackTrace();
    ErrorDialog.openError( m_shell, m_target.getName(), "Export-Probleme", status );

    return !status.matches( IStatus.ERROR );
  }
}