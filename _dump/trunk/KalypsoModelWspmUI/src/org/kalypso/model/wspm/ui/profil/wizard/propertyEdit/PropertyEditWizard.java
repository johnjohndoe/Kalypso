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

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointProperty;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditorLabelProvider2;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author kimwerner
 */
public class PropertyEditWizard extends Wizard
{

  private String m_profiletype = "";

  private ArrayChooserPage m_profileChooserPage;

  private ArrayChooserPage m_propertyChooserPage;

  private List<Feature> m_profiles;

  private List<Feature> m_selectedProfiles;

  private OperationChooserPage m_operationChooserPage;
  
  private CommandableWorkspace m_workspace;

  public PropertyEditWizard(CommandableWorkspace workspace, final List<Feature> profiles, final List<Feature> selection )
  {
    m_workspace = workspace;
    m_profiles = profiles;
    m_selectedProfiles = selection;
    m_profiletype = (String) profiles.get( 0 ).getProperty( ProfileFeatureFactory.QNAME_TYPE );
    setWindowTitle( "Profileigenschaften zuweisen" );
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

    m_profileChooserPage = new ArrayChooserPage( m_profiles, new Object[0], m_selectedProfiles.toArray(), 1, "profilesChooserPage", "Profile ausw‰hlen", null );
    m_profileChooserPage.setLabelProvider( new GMLEditorLabelProvider2() );
    m_profileChooserPage.setMessage( "Bitte w‰hlen Sie aus, welchen Profilen Werte zugeweisen werden sollen." );

    final IProfilPointPropertyProvider[] ppps = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( m_profiletype );
    final List<IProfilPointProperty> properties = new ArrayList<IProfilPointProperty>();
    for( final IProfilPointPropertyProvider ppp : ppps )
    {
      for( final String property : ppp.getPointProperties() )
      {
        properties.add( ppp.getPointProperty( property ) );
      }
    }
    m_propertyChooserPage = new ArrayChooserPage( properties.toArray( new IProfilPointProperty[0] ), new Object[0], new Object[0], 1, "profilePropertiesChooserPage", "Profileigenschaften ausw‰hlen", null );
    m_propertyChooserPage.setLabelProvider( new LabelProvider()
    {

      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( Object element )
      {
        if( element instanceof IProfilPointProperty )
          return ((IProfilPointProperty) element).getLabel();
        else
          return element.toString();
      }
    } );
    m_propertyChooserPage.setMessage( "Bitte w‰hlen Sie aus, welche Werte ge‰ndert werden sollen." );

    m_operationChooserPage = new OperationChooserPage();
    m_operationChooserPage.setPageComplete( false );
    addPage( m_profileChooserPage );

    addPage( m_operationChooserPage );
    addPage( m_propertyChooserPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    return m_operationChooserPage.isPageComplete();
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final Object[] choosenProfiles = m_profileChooserPage.getChoosen();
    final Object[] choosenProperties =  m_propertyChooserPage.getChoosen();
    final List<FeatureChange> changes = new ArrayList<FeatureChange>();
    if( (choosenProfiles != null) && (choosenProfiles.length != 0) )
    {
      for( final Object objProfile : choosenProfiles )
      {
        final Feature p = (Feature) objProfile;
        final WspmProfile wspmProfile = new WspmProfile( p );
        final IProfil profile = wspmProfile.getProfil();
        m_operationChooserPage.changeProfile( profile, choosenProperties );
        changes.addAll( Arrays.asList( ProfileFeatureFactory.toFeatureAsChanges( profile, p ) ) );
      }
      GMLWorkspace workspace = m_profiles.get( 0 ).getWorkspace();

      final ChangeFeaturesCommand command = new ChangeFeaturesCommand( workspace, changes.toArray( new FeatureChange[0] ) );
      try
      {
        m_workspace.postCommand( command );
      }
      catch( Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
    return true;
  }
}
