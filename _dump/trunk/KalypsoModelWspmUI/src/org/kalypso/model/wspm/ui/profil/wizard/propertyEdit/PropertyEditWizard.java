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
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureBinding;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IllegalProfileOperationException;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.i18n.Messages;
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
  private final IPageChangedListener m_pageChangedListener = new IPageChangedListener()
  {
    @Override
    public void pageChanged( final PageChangedEvent event )
    {
      handlePageChanged( event.getSelectedPage() );
    }
  };

  final private ArrayChooserPage m_profileChooserPage;

  private ArrayChooserPage m_propertyChooserPage;

  final private IProfil m_profile;

  final private List<Feature> m_profiles;

  final private List<Feature> m_selectedProfiles;

  private OperationChooserPage m_operationChooserPage;

  final private CommandableWorkspace m_workspace;

  final private IStructuredSelection m_selectedPoints;

  public PropertyEditWizard( final CommandableWorkspace workspace, final List<Feature> profiles, final List<Feature> selection )
  {
    m_profile = null;
    m_workspace = workspace;
    m_profiles = profiles;
    m_selectedProfiles = selection;
    m_selectedPoints = null;

    setWindowTitle( org.kalypso.model.wspm.ui.i18n.Messages.getString( "org.kalypso.model.wspm.ui.profil.wizard.propertyEdit.PropertyEditWizard.0" ) ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );
    m_profileChooserPage = new ArrayChooserPage( m_profiles, new Object[0], m_selectedProfiles.toArray(), 1, "profilesChooserPage", Messages.getString( "org.kalypso.model.wspm.ui.profil.wizard.propertyEdit.PropertyEditWizard.1" ), null, false ); //$NON-NLS-1$
    m_profileChooserPage.setLabelProvider( new GMLLabelProvider() );
    m_profileChooserPage.setMessage( "Choose profiles to be changed" );
  }

  public PropertyEditWizard( final IProfil profile, final ISelection selection )
  {
    m_selectedPoints = selection instanceof IStructuredSelection ? (IStructuredSelection) selection : null;
    m_profile = profile;
    m_workspace = null;
    m_profiles = null;
    m_selectedProfiles = null;
    m_profileChooserPage = null;
    setNeedsProgressMonitor( true );
    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#setContainer(org.eclipse.jface.wizard.IWizardContainer)
   */
  @Override
  public void setContainer( final IWizardContainer wizardContainer )
  {
    final IWizardContainer oldContainer = getContainer();
    if( oldContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider) oldContainer).removePageChangedListener( m_pageChangedListener );

    super.setContainer( wizardContainer );

    if( wizardContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider) wizardContainer).addPageChangedListener( m_pageChangedListener );
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

    m_propertyChooserPage = new ArrayChooserPage( null, null, null, 1, "profilePropertiesChooserPage", Messages.getString( "org.kalypso.model.wspm.ui.profil.wizard.propertyEdit.PropertyEditWizard.1" ), null, true ); //$NON-NLS-1$ //$NON-NLS-2$
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

        return element.toString();
      }
    } );
    m_propertyChooserPage.setMessage( Messages.getString( "org.kalypso.model.wspm.ui.profil.wizard.propertyEdit.PropertyEditWizard.2" ) ); //$NON-NLS-1$

    m_operationChooserPage = new OperationChooserPage( m_selectedPoints, Messages.getString( "org.kalypso.model.wspm.ui.profil.wizard.propertyEdit.PropertyEditWizard.1" ) );
    m_operationChooserPage.setPageComplete( false );
    m_operationChooserPage.setMessage( "Select how the profiles are to be changed" );

    addPage( m_propertyChooserPage );
    addPage( m_operationChooserPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    return super.canFinish();
  }

  private IProfil[] toProfiles( final Object[] features )
  {
    if( features == null )
      return new IProfil[] { m_profile };
    final IProfil[] choosenProfiles = new IProfil[features.length];
    for( int i = 0; i < features.length; i++ )
    {
      final IProfileFeature wspmProfile = (IProfileFeature) features[i];
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
      final ProfilOperation operation = new ProfilOperation( org.kalypso.model.wspm.ui.i18n.Messages.getString( "org.kalypso.model.wspm.ui.profil.wizard.propertyEdit.PropertyEditWizard.3" ), m_profile, profilChanges, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }

    return true;
  }

  protected void handlePageChanged( final Object selectedPage )
  {
    if( selectedPage == m_propertyChooserPage )
    {
      final Collection<IComponent> properties = new LinkedHashSet<IComponent>();
      final Object[] profiles = m_profile == null ? m_profileChooserPage.getChoosen() : new Object[] { m_profile };
      for( final Object object : profiles )
      {
        final IProfil profile;
        if( object instanceof IProfil )
          profile = (IProfil) object;
        else if( object instanceof ProfileFeatureBinding )
          profile = ((ProfileFeatureBinding) object).getProfil();
        else
          continue;

        for( final IComponent property : profile.getPointProperties() )
        {
          if( !profile.isPointMarker( property.getId() ) )
            properties.add( property );
        }
      }

      m_propertyChooserPage.setInput( properties );
    }
  }
}
