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
package org.kalypso.model.wspm.ui.action;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate2;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.actions.ActionDelegate;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.core.profil.serializer.ProfilSerializerUtilitites;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Action wich exports the selected profile as .prf files.
 * <p>
 * TODO: better use a wizard and let the user choose 1) the profiles to export 2) a name pattern for the generated files
 * 
 * @author Gernot Belger
 */
public class ExportProfilePrfAction extends ActionDelegate implements IObjectActionDelegate, IActionDelegate2
{
  private static final String STR_DIALOG_TITLE = ".prf Export"; //$NON-NLS-1$

  private static final String SETTINGS_FILTER_PATH = "prfImportInitialDirectory"; //$NON-NLS-1$

  private IFeatureSelection m_selection;

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

    /* open file dialog and choose profile files */
    final File dir = askForDir( shell );
    if( dir == null )
      return;

    try
    {
      final IProfilSink sink = KalypsoModelWspmCoreExtensions.createProfilSink( "prf" ); //$NON-NLS-1$
      final Map<IProfil, String> profiles = getProfiles();

      final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
      {
        public IStatus execute( final IProgressMonitor monitor )
        {
          final String id = PluginUtilities.id( KalypsoModelWspmUIPlugin.getDefault() );
          final MultiStatus resultStatus = new MultiStatus( id, 1, org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.action.ExportProfilePrfAction.0"), null ); //$NON-NLS-1$

          monitor.beginTask( org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.action.ExportProfilePrfAction.1"), profiles.size() ); //$NON-NLS-1$

          for( final Map.Entry<IProfil, String> entry : profiles.entrySet() )
          {
            final IProfil profile = entry.getKey();
            final String name = entry.getValue();
            monitor.subTask( name );

            final File file = new File( dir, name );

            try
            {
              ProfilSerializerUtilitites.writeProfile( sink, profile, file );
            }
            catch( final IOException e )
            {
              final IStatus status = StatusUtilities.statusFromThrowable( e );
              resultStatus.add( status );
            }

            monitor.worked( 1 );
            if( monitor.isCanceled() )
              return new Status( IStatus.CANCEL, id, 1, org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.action.ExportProfilePrfAction.2"), null ); //$NON-NLS-1$
          }
          return resultStatus;
        }
      };
      final IStatus status = ProgressUtilities.busyCursorWhile( op, org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.action.ExportProfilePrfAction.3") ); //$NON-NLS-1$
      ErrorDialog.openError( shell, STR_DIALOG_TITLE, org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.action.ExportProfilePrfAction.4"), status, IStatus.ERROR | IStatus.WARNING | IStatus.CANCEL ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      ErrorDialog.openError( shell, STR_DIALOG_TITLE, org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.action.ExportProfilePrfAction.5"), status ); //$NON-NLS-1$
    }
  }

  /**
   * @return profil to filename
   */
  private final Map<IProfil, String> getProfiles( )
  {
    final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) m_selection.getFirstElement();
    final IRelationType rt = fate.getAssociationTypeProperty();

    final Feature parentFeature = fate.getParentFeature();
    final GMLWorkspace workspace = parentFeature.getWorkspace();

    final List foundFeatures;
    if( rt.isList() )
      foundFeatures = (FeatureList) parentFeature.getProperty( rt );
    else
    {
      foundFeatures = new ArrayList<Feature>();
      foundFeatures.add( parentFeature.getProperty( rt ) );
    }

    final Map<IProfil, String> profiles = new HashMap<IProfil, String>( foundFeatures.size() );

    for( final Object object : foundFeatures )
    {
      final Feature feature = FeatureHelper.getFeature( workspace, object );
      if( feature != null )
      {
        final String label = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
        final String filename = convertToWspwinFilename( label ) + ".prf"; //$NON-NLS-1$

        final IProfil profile = ((IProfileFeature)feature).getProfil();

        profiles.put( profile, filename );
      }
    }

    return profiles;
  }

  private String convertToWspwinFilename( final String label )
  {
    String result = label;
    result = label.replace( '#', '_' );
    result = label.replace( ':', '_' );
    result = label.replace( ' ', '_' );
    result = label.replace( ' ', '_' );
    return result;
  }

  private File askForDir( final Shell shell )
  {
    final IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() );
    final String initialFilterPath = dialogSettings.get( SETTINGS_FILTER_PATH );

    final DirectoryDialog dialog = new DirectoryDialog( shell );
    dialog.setText( STR_DIALOG_TITLE );
    dialog.setMessage( org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.action.ExportProfilePrfAction.6") ); //$NON-NLS-1$
    dialog.setFilterPath( initialFilterPath );

    final String result = dialog.open();
    if( result == null )
      return null;

    final String filterPath = dialog.getFilterPath();
    dialogSettings.put( SETTINGS_FILTER_PATH, filterPath );

    return new File( result );
  }

}
