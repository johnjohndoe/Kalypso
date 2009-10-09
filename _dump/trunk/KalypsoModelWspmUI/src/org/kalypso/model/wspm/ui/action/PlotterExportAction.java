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

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate2;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.actions.ActionDelegate;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.core.profil.serializer.ProfilSerializerUtilitites;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Action wich exports the selected profile as .prf files.
 * <p>
 * TODO: better use a wizard and let the user choose 1) the profiles to export 2) a name pattern for the generated files
 * 
 * @author Gernot Belger
 */
public class PlotterExportAction extends ActionDelegate implements IObjectActionDelegate, IActionDelegate2
{
  private static final String STR_DIALOG_TITLE = ".prf Export"; //$NON-NLS-1$

  private static final String SETTINGS_FILTER_PATH = "prfImportInitialDirectory"; //$NON-NLS-1$

  private IFeatureSelection m_selection;

  private CommandableWorkspace m_workspace;

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
  }

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

    final String plotterExe = findPlotter( shell );

    final String id = PluginUtilities.id( KalypsoModelWspmUIPlugin.getDefault() );

    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor )
      {
        monitor.beginTask( "Export to Plotter", features.size() );

        IProfilSink sink;
        try
        {
          sink = KalypsoModelWspmCoreExtensions.createProfilSink( "prf" );
        }
        catch( CoreException e1 )
        {
          return new Status( IStatus.ERROR, id, 1, "Profilexport konnte nicht gestartet werden", null );
        }

        for( int i = 0; i < features.size(); i++ )
        {
          final Feature feature = features.get( i );

          final IProfil profile = ((IProfileFeature) feature).getProfil();
          final File file = new File( System.getProperty( "java.io.tmpdir" ), "exportTmp" + i + ".prf" );//$NON-NLS-1$ //$NON-NLS-2$// $NON-NLS-3$
          try
          {
            ProfilSerializerUtilitites.writeProfile( sink, profile, file );
            Runtime.getRuntime().exec( "\"" + plotterExe + "\" \"" + file.getPath() + "\"" );//$NON-NLS-1$ //$NON-NLS-2$// $NON-NLS-3$
          }
          catch( IOException e )
          {
            e.printStackTrace();
            return StatusUtilities.statusFromThrowable( e, "an error occured starting the plotter" );
          }

          monitor.worked( 1 );
          if( monitor.isCanceled() )
            return new Status( IStatus.CANCEL, id, 1, "program abortion through user", null );

        }

        return new Status( IStatus.OK, id, "" );
      }
    };

    ProgressUtilities.busyCursorWhile( op, "could not export cross section profile" );

  }

  private final String findPlotter( final Shell shell )
  {

    String plotterPath = KalypsoCorePlugin.getDefault().getPreferenceStore().getString( IWspmConstants.WSPWIN_PLOTTER_PATH );

    if( !plotterPath.equals( "" ) )
      return plotterPath;

    final FileDialog dlg = new FileDialog( shell );
    plotterPath = dlg.open();
    if( plotterPath == null )
      return "";
    KalypsoCorePlugin.getDefault().getPreferenceStore().setValue( IWspmConstants.WSPWIN_PLOTTER_PATH, plotterPath );
    return plotterPath;
  }

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
