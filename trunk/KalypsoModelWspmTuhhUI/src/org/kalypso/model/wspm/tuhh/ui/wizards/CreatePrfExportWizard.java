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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.core.profil.serializer.ProfilSerializerUtilitites;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kimwerner
 */
public class CreatePrfExportWizard extends Wizard
{

  final private ArrayChooserPage m_profileChooserPage;

  final private List<Feature> m_profiles;

  final private List<Feature> m_selectedProfiles;

  final private CommandableWorkspace m_workspace;

  public CreatePrfExportWizard( final CommandableWorkspace workspace, final List<Feature> profiles, final List<Feature> selection )
  {
    m_workspace = workspace;
    m_profiles = profiles;
    m_selectedProfiles = selection;
    setWindowTitle( "Export als .prf Datei(en)" );
    setNeedsProgressMonitor( true );
    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );
    m_profileChooserPage = new ArrayChooserPage( m_profiles, new Object[0], m_selectedProfiles.toArray(), 1, "profilesChooserPage", "Profile w‰hlen", null );
    m_profileChooserPage.setLabelProvider( new GMLLabelProvider() );
    m_profileChooserPage.setMessage( "Profile w‰hlen" ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    super.addPages();
    addPage( m_profileChooserPage );
  }

  private IProfil[] toProfiles( final Object[] features )
  {

    final IProfil[] choosenProfiles = new IProfil[features.length];
    for( int i = 0; i < features.length; i++ )
    {
      final IProfileFeature wspmProfile = (IProfileFeature) features[i];
      choosenProfiles[i] = wspmProfile.getProfil();
    }
    return choosenProfiles;
  }
  private File askForDir( final Shell shell )
  {
    final IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() );
    final String initialFilterPath = dialogSettings.get( "SETTINGS_FILTER_PATH" );

    final DirectoryDialog dialog = new DirectoryDialog( shell );
    dialog.setText( "DialogTitle");
    dialog.setMessage( "DialogMessage" ); 
    dialog.setFilterPath( initialFilterPath );

    final String result = dialog.open();
    if( result == null )
      return null;

    final String filterPath = dialog.getFilterPath();
    dialogSettings.put( "SETTINGS_FILTER_PATH", filterPath );

    return new File( result );
  }
  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final Object[] profilFeatures = m_profileChooserPage.getChoosen();
    final IProfil[] choosenProfiles = toProfiles( profilFeatures );
//    final List<FeatureChange> featureChanges = new ArrayList<FeatureChange>();
// for( int i = 0; i < choosenProfiles.length; i++ )
// {
// ProfilUtil.flipProfile( choosenProfiles[i],true );
// featureChanges.addAll( Arrays.asList( ProfileFeatureFactory.toFeatureAsChanges( choosenProfiles[i], (Feature)
    // profilFeatures[i] ) ) );
// }
    final Shell shell = getShell();

    /* open file dialog and choose profile files */
    final File dir = askForDir(shell );
    if( dir == null )
      return false;

    try
    {
      final IProfilSink sink = KalypsoModelWspmCoreExtensions.createProfilSink( "prf" ); //$NON-NLS-1$
      // final Map<IProfil, String> profiles = getProfiles();

      final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
      {
        public IStatus execute( final IProgressMonitor monitor )
        {
          final String id = PluginUtilities.id( KalypsoModelWspmUIPlugin.getDefault() );
          final MultiStatus resultStatus = new MultiStatus( id, 1, "Fehlermeldung", null ); //$NON-NLS-1$

          monitor.beginTask( "TaskTitle", choosenProfiles.length );

          for( final IProfil profile : choosenProfiles )
          {

            final String name = String.format( "%.4f", profile.getStation() );
            monitor.subTask( name );
            File file = new File( dir, name + ".prf" );
            int i = 0;
            while( file.exists() )
            {
              file = new File( dir, name + i++ + ".prf" );
            }
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
              return new Status( IStatus.CANCEL, id, 1, "Statusmeldung", null );
          }
          return resultStatus;
        }
      };
      final IStatus status = ProgressUtilities.busyCursorWhile( op,"org.kalypso.model.wspm.ui.action.ExportProfilePrfAction.3"  ); 
      ErrorDialog.openError( shell, "STR_DIALOG_TITLE", "org.kalypso.model.wspm.ui.action.ExportProfilePrfAction.4" , status, IStatus.ERROR | IStatus.WARNING | IStatus.CANCEL ); //$NON-NLS-1$
    }
    catch( final CoreException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      ErrorDialog.openError( shell, "STR_DIALOG_TITLE","org.kalypso.model.wspm.ui.action.ExportProfilePrfAction.5", status ); 
    }
    return true;
  }
}
