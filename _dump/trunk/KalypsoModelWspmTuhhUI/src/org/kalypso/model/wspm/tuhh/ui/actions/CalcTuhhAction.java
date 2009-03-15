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
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
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
import org.kalypso.simulation.core.simspec.Modeldata.ClearAfterCalc;
import org.kalypso.simulation.core.simspec.Modeldata.Input;
import org.kalypso.simulation.core.simspec.Modeldata.Output;
import org.kalypso.simulation.ui.calccase.ModelNature;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 */
public class CalcTuhhAction implements IActionDelegate
{
  private IFeatureSelection m_selection;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final Feature[] features = FeatureSelectionHelper.getFeatures( m_selection );

    /* Save the workspace first: we assume that all features are from the same workspace */
    final Shell activeShell = Display.getCurrent().getActiveShell();
    if( !saveFeatures( activeShell, features ).isOK() )
      return;

    for( final Feature feature : features )
    {
      final TuhhCalculation calculation = new TuhhCalculation( feature );

      final URL context = feature.getWorkspace().getContext();
      final IFile gmlFile = ResourceUtilities.findFileFromURL( context );
      if( gmlFile == null )
      {
        // context is not local, what shall we do?

        // ignore it for now
        continue;
      }

      final Job calcJob = new Job( Messages.getFormatString("org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhAction.0", calculation.getName() )) //$NON-NLS-1$
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          try
          {
            final String calcxpath = Messages.getFormatString("org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhAction.2", calculation.getFeature().getId()); //$NON-NLS-1$
            final String resultPath = "Ergebnisse/" + calculation.getName(); //$NON-NLS-1$

            final ModelNature nature = (ModelNature) gmlFile.getProject().getNature( ModelNature.ID );
            if( nature == null )
              return StatusUtilities.createWarningStatus( Messages.getString("org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhAction.5") + ModelNature.ID ); //$NON-NLS-1$

            final Map<String, Object> properties = new HashMap<String, Object>();
            properties.put( "calc.xpath", calcxpath ); //$NON-NLS-1$
            properties.put( "result.path", resultPath ); //$NON-NLS-1$

            return nature.launchAnt( Messages.getString("org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhAction.8"), "calc", properties, gmlFile.getParent(), monitor ); //$NON-NLS-1$ //$NON-NLS-2$
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
  }

  /**
   * Asks the user which corresponding workspaces should be saved.
   */
  private IStatus saveFeatures( final Shell shell, final Feature[] features )
  {
    final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();

    final Set<CommandableWorkspace> objectsToSave = new HashSet<CommandableWorkspace>( features.length );
    for( final Feature feature : features )
    {
      final CommandableWorkspace workspace = m_selection.getWorkspace( feature );
      final KeyInfo info = pool.getInfo( workspace );
      if( info.isDirty() )
        objectsToSave.add( workspace );
    }

    if( objectsToSave.size() == 0 )
      return Status.OK_STATUS;

    final StringBuffer msg = new StringBuffer( Messages.getString("org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhAction.10") ); //$NON-NLS-1$
    for( final CommandableWorkspace workspace : objectsToSave )
    {
      final KeyInfo info = pool.getInfo( workspace );
      msg.append( info.getKey().getLocation() );
      msg.append( "\n" ); //$NON-NLS-1$
    }

    if( MessageDialog.openConfirm( shell, Messages.getString("org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhAction.12"), msg.toString() ) ) //$NON-NLS-1$
    {
      final Job job = new Job( Messages.getString("org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhAction.13") ) //$NON-NLS-1$
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          final MultiStatus resultStatus = new MultiStatus( PluginUtilities.id( KalypsoModelWspmTuhhUIPlugin.getDefault() ), 0, "", null ); //$NON-NLS-1$

          monitor.beginTask( Messages.getString("org.kalypso.model.wspm.tuhh.ui.actions.CalcTuhhAction.15"), objectsToSave.size() ); //$NON-NLS-1$

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

  protected void addInput( final List<Input> list, final String id, final String path, final boolean relative )
  {
    final Input input = ModelNature.OF_SPEC.createModeldataInput();
    input.setId( id );
    input.setPath( path );
    input.setRelativeToCalcCase( relative );
    list.add( input );
  }

  protected void addOutput( final List<Output> list, final String id, final String path, final boolean relative )
  {
    final Output output = ModelNature.OF_SPEC.createModeldataOutput();
    output.setId( id );
    output.setPath( path );
    output.setRelativeToCalcCase( relative );
    list.add( output );
  }

  protected void addClearAfterCalc( final List<ClearAfterCalc> list, final String path, final boolean relative )
  {
    final ClearAfterCalc clear = ModelNature.OF_SPEC.createModeldataClearAfterCalc();
    clear.setPath( path );
    clear.setRelativeToCalcCase( relative );
    list.add( clear );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_selection = selection instanceof IFeatureSelection ? (IFeatureSelection) selection : null;

    final boolean enable = m_selection != null && m_selection.size() == 1;
    action.setEnabled( enable );
  }

}
