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
package org.kalypso.model.wspm.ui.profil.wizard.propertyEdit;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IllegalProfileOperationException;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.observation.result.IComponent;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author kimwerner
 */
public class PropertyEditWizard extends Wizard
{

  private String m_profiletype = "";

  final private ArrayChooserPage m_profileChooserPage;

  private ArrayChooserPage m_propertyChooserPage;

  final private IProfil m_profile;

  final private List<Feature> m_profiles;

  final private List<Feature> m_selectedProfiles;

  private OperationChooserPage m_operationChooserPage;

  final private CommandableWorkspace m_workspace;

  public PropertyEditWizard( final CommandableWorkspace workspace, final List<Feature> profiles, final List<Feature> selection )
  {
    m_profile = null;
    m_workspace = workspace;
    m_profiles = profiles;
    m_selectedProfiles = selection;

    m_profiletype = (String) profiles.get( 0 ).getProperty( ProfileFeatureFactory.QNAME_TYPE );
    setWindowTitle( "Profileigenschaften zuweisen" );
    setNeedsProgressMonitor( true );
    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );
    m_profileChooserPage = new ArrayChooserPage( m_profiles, new Object[0], m_selectedProfiles.toArray(), 1, "profilesChooserPage", "Profile ausw‰hlen", null );
    m_profileChooserPage.setLabelProvider( new GMLLabelProvider() );
    m_profileChooserPage.setMessage( "Bitte w‰hlen Sie aus, welchen Profilen Werte zugeweisen werden sollen." );
  }

  public PropertyEditWizard( final IProfil profile)
  {
    m_profile = profile;
    m_workspace = null;
    m_profiles = null;
    m_selectedProfiles = null;
    m_profiletype = m_profile.getType();
    m_profileChooserPage = null;
    setNeedsProgressMonitor( true );
    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    super.addPages();
    if( m_profile == null )
      addPage( m_profileChooserPage );

    final IProfilPointPropertyProvider[] ppps = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( m_profiletype );
    final List<IComponent> properties = new ArrayList<IComponent>();
    for( final IProfilPointPropertyProvider ppp : ppps )
    {
      for( final IComponent property : ppp.getPointProperties() )
      {
        properties.add( property );
      }
    }
    m_propertyChooserPage = new ArrayChooserPage( properties.toArray( new IComponent[0] ), new Object[0], new Object[0], 1, "profilePropertiesChooserPage", "Profileigenschaften ausw‰hlen", null );
    m_propertyChooserPage.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( element instanceof IComponent )
          return ((IComponent) element).getName();
        else
          return element.toString();
      }
    } );
    m_propertyChooserPage.setMessage( "Bitte w‰hlen Sie aus, welche Werte ge‰ndert werden sollen." );
    m_operationChooserPage = new OperationChooserPage();
    m_operationChooserPage.setPageComplete( false );

    
    addPage( m_propertyChooserPage );
    addPage( m_operationChooserPage );
  
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    return m_operationChooserPage.isPageComplete();
  }

  private IProfil[] toProfiles( final Object[] features )
  {
    if( features == null )
      return new IProfil[] {m_profile };
    final IProfil[] choosenProfiles = new IProfil[features.length];
    for( int i = 0; i < features.length; i++ )
    {
      final WspmProfile wspmProfile = new WspmProfile( (Feature) features[i] );
      choosenProfiles[i] = wspmProfile.getProfil();
    }
    return choosenProfiles;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final Object[] profilFeatures = m_profile == null ? m_profileChooserPage.getChoosen() : null;
    final IProfil[] choosenProfiles = toProfiles( profilFeatures );
    final Object[] choosenProperties = m_propertyChooserPage.getChoosen();
    final List<FeatureChange> featureChanges = new ArrayList<FeatureChange>();
    IProfilChange[] profilChanges = null;

    for( int i = 0; i < choosenProfiles.length; i++ )
    {
      profilChanges = m_operationChooserPage.changeProfile( choosenProfiles[i], choosenProperties );
      if( m_profile == null )
      {
        try
        {
          for( final IProfilChange change : profilChanges )
          {
            change.doChange( null );
          }
        }
        catch( final IllegalProfileOperationException e )
        {
          KalypsoModelWspmUIPlugin.getDefault().getLog().log( new Status( Status.ERROR, KalypsoModelWspmUIPlugin.getDefault().id(), e.getMessage() ) );
        }
        featureChanges.addAll( Arrays.asList( ProfileFeatureFactory.toFeatureAsChanges( choosenProfiles[i], (Feature) profilFeatures[i] ) ) );
      }
    }
    if( m_profile == null )
    {
      final GMLWorkspace workspace = m_profiles.get( 0 ).getWorkspace();
      final ChangeFeaturesCommand command = new ChangeFeaturesCommand( workspace, featureChanges.toArray( new FeatureChange[0] ) );
      try
      {
        m_workspace.postCommand( command );
      }
      catch( final Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
    else
    {
      final ProfilOperation operation = new ProfilOperation( "Profilpunkteigenschaften ‰ndern", m_profile, profilChanges, true );
      new ProfilOperationJob( operation ).schedule();
    }

    return true;
  }
}
