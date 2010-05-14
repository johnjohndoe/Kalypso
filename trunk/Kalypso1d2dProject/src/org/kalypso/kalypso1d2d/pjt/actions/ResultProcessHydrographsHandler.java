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
package org.kalypso.kalypso1d2d.pjt.actions;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.Date;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialogWithToggle;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultsAcessor;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;

/**
 * @author Gernot Belger
 */
public class ResultProcessHydrographsHandler extends AbstractHandler
{
  private static final String DIALOG_TITEL = Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ResultProcessHydrographsHandler.0" ); //$NON-NLS-1$

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final IFolder scenarioFolder = (IFolder) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

    // find all timesteps to process

    /* Ask user if and how to proceed */
    final IPreferenceStore prefStore = null;
    final String prefKey = null;
    final String message = Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ResultProcessHydrographsHandler.1" ); //$NON-NLS-1$
    final String toggleMessage = Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ResultProcessHydrographsHandler.2" ); //$NON-NLS-1$
    final MessageDialogWithToggle msgDialog = MessageDialogWithToggle.openOkCancelConfirm( shell, DIALOG_TITEL, message, toggleMessage, true, prefStore, prefKey );
    if( msgDialog.getReturnCode() == Window.CANCEL )
      return Status.CANCEL_STATUS;

    final boolean overwriteExistingHydrographs = msgDialog.getToggleState();
    final ResultsAcessor resultsAcessor = new ResultsAcessor( scenarioFolder );
    final IFolder resultsFolder = resultsAcessor.getResultsFolder();
    final IFile hydrographFile = resultsAcessor.getHydrographFile();

    final WorkspaceModifyOperation operation = new WorkspaceModifyOperation( resultsFolder )
    {
      @SuppressWarnings("synthetic-access")
      @Override
      protected void execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        try
        {
          monitor.beginTask( DIALOG_TITEL, 10 );

          /* Load hydrograph gml via pool, maybe its already loaded. */
          final URL hydrographUrl = ResourceUtilities.createURL( hydrographFile );
          final PoolableObjectType hydrographKey = new PoolableObjectType( "gml", hydrographUrl.toExternalForm(), hydrographUrl ); //$NON-NLS-1$
          final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
          final GMLWorkspace hydrographWorkspace = (GMLWorkspace) pool.getObject( hydrographKey );
          monitor.worked( 1 );

          final Map<Date, IFile> wspTimestepResults = resultsAcessor.getTimestepsFiles();

          final IHydrographCollection graphs = (IHydrographCollection) hydrographWorkspace.getRootFeature().getAdapter( IHydrographCollection.class );
          processHydrographs( graphs, wspTimestepResults, overwriteExistingHydrographs );

          // save hydrograph.gml
          GmlSerializer.serializeWorkspace( hydrographFile.getLocation().toFile(), hydrographWorkspace, "UTF-8" ); //$NON-NLS-1$
          monitor.worked( 2 );
          hydrographFile.refreshLocal( IResource.DEPTH_ONE, new SubProgressMonitor( monitor, 1 ) );
        }
        catch( final Throwable e )
        {
          throw new InvocationTargetException( e );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    final IStatus status = ProgressUtilities.busyCursorWhile( operation, null );
    ErrorDialog.openError( shell, DIALOG_TITEL, Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.ResultProcessHydrographsHandler.5" ), status ); //$NON-NLS-1$
    return status;
  }

  protected void processHydrographs( final IHydrographCollection graphs, final Map<Date, IFile> wspTimestepResults, final boolean overwriteExistingHydrographs )
  {
    // TODO Auto-generated method stub
  }
}
