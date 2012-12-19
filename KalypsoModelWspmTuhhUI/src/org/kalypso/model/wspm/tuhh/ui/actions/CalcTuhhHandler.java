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
package org.kalypso.model.wspm.tuhh.ui.actions;

import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.util.pool.KeyInfo;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.loader.LoaderException;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.simulation.ui.calccase.ModelNature;
import org.kalypso.util.command.WaitForFeatureChanges;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 */
public class CalcTuhhHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final ISelection selection = HandlerUtil.getCurrentSelectionChecked( event );

    final IFeatureSelection featureSelection = selection instanceof IFeatureSelection ? (IFeatureSelection) selection : null;
    if( featureSelection == null )
      throw new ExecutionException( "Selection must be a feature-selection" ); //$NON-NLS-1$

    /* Save the workspace first: we assume that all features are from the same workspace. */
    // FIXED: Wait for all feature changes to be commited, else the gml workspace might still being changed.
    final Feature[] features = FeatureSelectionHelper.getFeatures( featureSelection );
    final ICoreRunnableWithProgress commandWaiter = new WaitForFeatureChanges();
    ProgressUtilities.busyCursorWhile( commandWaiter );

    if( !saveFeatures( shell, features, featureSelection ).isOK() )
      return null;

    for( final Feature feature : features )
    {
      // FIXME: use new result provider api
      final TuhhCalculation calculation = (TuhhCalculation) feature;
      final URL context = feature.getWorkspace().getContext();
      final IFile gmlFile = ResourceUtilities.findFileFromURL( context );
      if( gmlFile == null )
      {
        // Context is not local, what shall we do?
        // Ignore it for now.
        continue;
      }

      // Only one calculation may run at the same time
      // Still a problem if the user start several task
      // Setting a mutext does not help, because we already have a rule
      final Job calcJob = new Job( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhHandler.0", calculation.getName() ) ) //$NON-NLS-1$
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          try
          {
            final String calcxpath = String.format( "id('%s')", calculation.getId() ); //$NON-NLS-1$
            final String resultPath = calculation.getResultFolder().toPortableString();

            /* Check path */
            final String calcName = calculation.getName();
            if( calcName.endsWith( "." ) ) //$NON-NLS-1$
            {
              // REMARK: very special case (windows only)?: if path ends with '.', the file system will remove that
              // automatically later.
              // This leads to the bug, that results are not found, even if present.
              final String message = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhHandler.1", calcName ); //$NON-NLS-1$
              return new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), message );
            }

            final ModelNature nature = (ModelNature) gmlFile.getProject().getNature( ModelNature.ID );
            if( nature == null )
            {
              final String message = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhHandler.5" ) + ModelNature.ID; //$NON-NLS-1$
              return new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), message );
            }

            final Map<String, Object> properties = new HashMap<>();
            properties.put( "calc.xpath", calcxpath ); //$NON-NLS-1$
            properties.put( "result.path", resultPath ); //$NON-NLS-1$

            return nature.launchAnt( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhHandler.8" ), "calc", properties, gmlFile.getParent(), monitor ); //$NON-NLS-1$ //$NON-NLS-2$
          }
          catch( final OperationCanceledException e )
          {
            return Status.CANCEL_STATUS;
          }
          catch( final Throwable t )
          {
            return StatusUtilities.statusFromThrowable( t );
          }
        }
      };
      calcJob.setUser( true );
      calcJob.schedule();

      // Only one calculation may run at the same time
      // Still a problem if the user start several task
      // Setting a mutext does not help, because we already have a rule
      break;
    }

    return null;
  }

  /**
   * Asks the user which corresponding workspaces should be saved.
   */
  private IStatus saveFeatures( final Shell shell, final Feature[] features, final IFeatureSelection selection )
  {
    final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();

    final Set<CommandableWorkspace> objectsToSave = new HashSet<>( features.length );
    for( final Feature feature : features )
    {
      final CommandableWorkspace workspace = selection.getWorkspace( feature );
      final KeyInfo info = pool.getInfo( workspace );
      if( info.isDirty() )
      {
        objectsToSave.add( workspace );
      }
    }

    if( objectsToSave.size() == 0 )
      return Status.OK_STATUS;

    final StringBuffer msg = new StringBuffer( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhHandler.10" ) ); //$NON-NLS-1$
    for( final CommandableWorkspace workspace : objectsToSave )
    {
      final KeyInfo info = pool.getInfo( workspace );
      msg.append( info.getKey().getLocation() );
      msg.append( "\n" ); //$NON-NLS-1$
    }

    if( MessageDialog.openConfirm( shell, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhHandler.12" ), msg.toString() ) ) //$NON-NLS-1$
    {
      final Job job = new Job( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhHandler.13" ) ) //$NON-NLS-1$
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          final MultiStatus resultStatus = new MultiStatus( PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() ), 0, "", null ); //$NON-NLS-1$

          monitor.beginTask( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhHandler.15" ), objectsToSave.size() ); //$NON-NLS-1$

          for( final CommandableWorkspace workspace : objectsToSave )
          {
            try
            {
              pool.saveObject( workspace, new SubProgressMonitor( monitor, 1 ) );
            }
            catch( final LoaderException e )
            {
              final IStatus status = StatusUtilities.statusFromThrowable( e );
              resultStatus.add( status );
            }
          }

          return resultStatus;
        }
      };

      job.setUser( true );
      job.schedule();

      try
      {
        job.join();
      }
      catch( final InterruptedException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModelWspmTuhhUIPlugin.getDefault().getLog().log( status );
      }

      return job.getResult();
    }

    return Status.CANCEL_STATUS;
  }
}