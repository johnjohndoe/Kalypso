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
package org.kalypso.model.wspm.ui.action;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.ActionDelegate;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Action which flips the selected profiles.
 * 
 * @author Thomas Jung
 */
public class FlipProfileAction extends ActionDelegate
{
  private static final String STR_DIALOG_TITLE = Messages.FlipProfileAction_0;

  private IFeatureSelection m_selection;

  private CommandableWorkspace m_workspace;

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_selection = selection instanceof IFeatureSelection ? (IFeatureSelection) selection : null;

    if( action != null )
      action.setEnabled( m_selection != null );
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#runWithEvent(org.eclipse.jface.action.IAction,
   *      org.eclipse.swt.widgets.Event)
   */
  @Override
  public void runWithEvent( final IAction action, final Event event )
  {
    final Shell shell = event.display.getActiveShell();

    final List<Feature> features = getProfiles();

    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      @SuppressWarnings("synthetic-access") //$NON-NLS-1$
      public IStatus execute( final IProgressMonitor monitor )
      {
        final String id = PluginUtilities.id( KalypsoModelWspmUIPlugin.getDefault() );
        final MultiStatus resultStatus = new MultiStatus( id, 1, Messages.FlipProfileAction_2, null );

        final List<FeatureChange> featureChanges = new ArrayList<FeatureChange>();

        monitor.beginTask( Messages.FlipProfileAction_3, features.size() );

        for( int i = 0; i < features.size(); i++ )
        {
          final Feature feature = features.get( i );

          final IProfil profile = ((IProfileFeature)feature).getProfil();
          ProfilUtil.flipProfile( profile );

          monitor.worked( 1 );
          if( monitor.isCanceled() )
            return new Status( IStatus.CANCEL, id, 1, Messages.FlipProfileAction_4, null );

          featureChanges.addAll( Arrays.asList( ProfileFeatureFactory.toFeatureAsChanges( profile, feature ) ) );
        }

        if( m_workspace != null )
        {
          final ChangeFeaturesCommand command = new ChangeFeaturesCommand( m_workspace, featureChanges.toArray( new FeatureChange[0] ) );
          try
          {
            m_workspace.postCommand( command );
          }
          catch( Exception e )
          {
            e.printStackTrace();
            return StatusUtilities.statusFromThrowable( e, Messages.FlipProfileAction_5 );
          }
        }
        else
          return StatusUtilities.createErrorStatus( Messages.FlipProfileAction_6 );

        return resultStatus;
      }
    };
    final IStatus status = ProgressUtilities.busyCursorWhile( op, Messages.FlipProfileAction_7 );
    ErrorDialog.openError( shell, STR_DIALOG_TITLE, Messages.FlipProfileAction_8, status, IStatus.ERROR | IStatus.WARNING | IStatus.CANCEL );
  }

  /**
   * @return profiles
   */
  @SuppressWarnings( { "unchecked" }) //$NON-NLS-1$
  private final List<Feature> getProfiles( )
  {
    /* retrieve selected profiles, abort if none */
    final Set<Feature> foundProfiles = new HashSet<Feature>();

    Object[] objects = m_selection.toArray();

    for( int i = 0; i < objects.length; i++ )
    {
      final Object fe = objects[i];

      if( fe instanceof FeatureAssociationTypeElement )
      {
        final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) m_selection.getFirstElement();
        final IRelationType rt = fate.getAssociationTypeProperty();
        final Feature parentFeature = fate.getParentFeature();
        if( rt.isList() )
          foundProfiles.addAll( (FeatureList) parentFeature.getProperty( rt ) );
        else
        {
          foundProfiles.add( (Feature) parentFeature.getProperty( rt ) );
        }
        m_workspace = m_selection.getWorkspace( parentFeature );
      }
      else if( fe instanceof Feature )
      {
        m_workspace = m_selection.getWorkspace( (Feature) fe );
        foundProfiles.add( (Feature) fe );
      }
    }
    final List<Feature> profiles = new LinkedList<Feature>();

    for( final Object object : foundProfiles )
    {
      final Feature feature = FeatureHelper.getFeature( m_workspace, object );
      if( feature != null )
      {
        profiles.add( feature );
      }
    }

    return profiles;
  }

}
