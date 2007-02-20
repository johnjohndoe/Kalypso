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
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.actions.ActionDelegate;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSource;
import org.kalypso.model.wspm.core.profil.serializer.ProfilSerializerUtilitites;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;

/**
 * @author Gernot Belger
 */
public class ImportProfilePrfAction extends ActionDelegate implements IObjectActionDelegate
{
  private IAction m_action;

  private IFeatureSelection m_selection;

  private static final String SETTINGS_FILTER_PATH = "fileDialogPath";

  private static final DateFormat DF = DateFormat.getDateInstance( DateFormat.LONG );

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#init(org.eclipse.jface.action.IAction)
   */
  @Override
  public void init( final IAction action )
  {
    m_action = action;
  }

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
    m_action = action;
  }

  /**
   * @see org.eclipse.ui.actions.ActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_action = action;
    m_selection = selection instanceof IFeatureSelection ? (IFeatureSelection) selection : null;

    if( action != null )
      action.setEnabled( m_selection != null && m_selection.getFirstElement() instanceof FeatureAssociationTypeElement );
  }

  public IAction getAction( )
  {
    return m_action;
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
    final File[] files = askForFiles( shell );
    if( files == null || files.length == 0 )
      return;

    /* read profiles, show warnings */
    final List<IProfil> profiles = new ArrayList<IProfil>( files.length );
    final MultiStatus prfReadStatus = readProfiles( action, shell, files, profiles );

    if( profiles.size() == 0 )
    {
      MessageDialog.openInformation( shell, action.getText(), "Es konnten keine Profile gelesen werden." );
      return;
    }

    if( !prfReadStatus.isOK() )
    {
      if( !MessageDialog.openConfirm( shell, action.getText(), "Trotz Fehler beim Lesen der .prf Dateien fortfahren?" ) )
        return;
    }

    /* convert them into the profile-list */
    try
    {
      loadIntoGml( profiles );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      ErrorDialog.openError( shell, m_action.getText(), "Fehler beim Konvertieren der Profile", status );
    }
  }

  private MultiStatus readProfiles( final IAction action, final Shell shell, final File[] files, final List<IProfil> profiles )
  {
    final MultiStatus prfReadStatus = new MultiStatus( PluginUtilities.id( KalypsoModelWspmUIPlugin.getDefault() ), -1, "Eine oder mehrere der .rpf Dateien konnten nicht gelesen werden.", null );
    final Date today = new Date();
    final String todayString = DF.format( today );
    for( final File file : files )
    {
      try
      {
        final IProfilSource prfSource = KalypsoModelWspmCoreExtensions.createProfilSource( "prf" );
        final IProfil profil = ProfilSerializerUtilitites.readProfile( prfSource, file, "org.kalypso.model.wspm.tuhh.profiletype" );

        profil.setProperty( IWspmConstants.PROFIL_PROPERTY_NAME, "Import" );

        final String description = String.format( "Importiert am %s aus %s", todayString, file.getAbsolutePath() );
        profil.setProperty( IWspmConstants.PROFIL_PROPERTY_KOMMENTAR, description );

        profiles.add( profil );
      }
      catch( final IOException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e, file.getName() + ": " );
        prfReadStatus.add( status );
      }
      catch( final ProfilDataException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e, file.getName() + ": " );
        prfReadStatus.add( status );
      }
      catch( final CoreException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e, file.getName() + ": " );
        prfReadStatus.add( status );
      }
    }

    ErrorDialog.openError( shell, action.getText(), "Fehler beim Import der .prf Dateien", prfReadStatus );
    return prfReadStatus;
  }

  private File[] askForFiles( final Shell shell )
  {
    final IDialogSettings dialogSettings = PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() );
    final String initialFilterPath = dialogSettings.get( SETTINGS_FILTER_PATH );

    final FileDialog dialog = new FileDialog( shell, SWT.OPEN | SWT.MULTI );
    dialog.setText( ".prf Import" );
    dialog.setFilterExtensions( new String[] { "*.prf", "*.*" } );
    dialog.setFilterPath( initialFilterPath );

    final String result = dialog.open();
    if( result == null )
      return null;

    final String filterPath = dialog.getFilterPath();
    dialogSettings.put( SETTINGS_FILTER_PATH, filterPath );

    final String[] fileNames = dialog.getFileNames();
    final File[] results = new File[fileNames.length];
    for( int i = 0; i < fileNames.length; i++ )
    {
      final String name = fileNames[i];
      results[i] = new File( filterPath, name );
    }

    return results;
  }

  private void loadIntoGml( final List<IProfil> profiles ) throws Exception
  {
    final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) m_selection.getFirstElement();

    final ICommand command = new ICommand()
    {
      private Feature[] m_addedFeatures = null;

      private FeatureList m_profileList = null;

      private GMLWorkspace m_workspace;

      private Feature m_waterFeature;

      public String getDescription( )
      {
        return "Profile importieren";
      }

      public boolean isUndoable( )
      {
        return true;
      }

      public void process( ) throws Exception
      {
        m_waterFeature = fate.getParentFeature();
        m_profileList = (FeatureList) m_waterFeature.getProperty( WspmWaterBody.QNAME_PROP_PROFILEMEMBER );

        final WspmWaterBody water = new WspmWaterBody( m_waterFeature );
        final List<Feature> newFeatureList = new ArrayList<Feature>();
        try
        {
          for( final IProfil profile : profiles )
          {
            final WspmProfile gmlProfile = water.createNewProfile();
            ProfileFeatureFactory.toFeature( profile, gmlProfile.getFeature() );

            newFeatureList.add( gmlProfile.getFeature() );
          }
        }
        catch( final GMLSchemaException e )
        {
          // should never happen, just log
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
        }
        finally
        {
          m_addedFeatures = newFeatureList.toArray( new Feature[newFeatureList.size()] );
          m_workspace = m_waterFeature.getWorkspace();
          final ModellEvent event = new FeatureStructureChangeModellEvent( m_workspace, m_waterFeature, m_addedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
          m_workspace.fireModellEvent( event );
        }
      }

      @SuppressWarnings("unchecked")
      public void redo( ) throws Exception
      {
        m_profileList.addAll( Arrays.asList( m_addedFeatures ) );
        final ModellEvent event = new FeatureStructureChangeModellEvent( m_workspace, m_waterFeature, m_addedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
        m_workspace.fireModellEvent( event );
      }

      @SuppressWarnings("unchecked")
      public void undo( ) throws Exception
      {
        m_profileList.removeAll( Arrays.asList( m_addedFeatures ) );
        final ModellEvent event = new FeatureStructureChangeModellEvent( m_workspace, m_waterFeature, m_addedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE );
        m_workspace.fireModellEvent( event );
      }
    };

    final CommandableWorkspace workspace = m_selection.getWorkspace( fate.getParentFeature() );
    if( workspace != null )
      workspace.postCommand( command );
  }

}
