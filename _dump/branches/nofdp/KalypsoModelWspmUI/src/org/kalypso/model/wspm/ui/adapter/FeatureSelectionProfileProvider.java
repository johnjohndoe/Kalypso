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
package org.kalypso.model.wspm.ui.adapter;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.ProfileFeatureProvider;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.result.IStationResult;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.profil.AbstractProfilProvider2;
import org.kalypso.model.wspm.ui.profil.IProfilProvider2;
import org.kalypso.model.wspm.ui.profil.validation.ValidationProfilListener;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class FeatureSelectionProfileProvider extends AbstractProfilProvider2 implements IProfilProvider2, ISelectionChangedListener, IProfilListener, ModellEventListener
{
  /**
   * @see org.kalypso.model.wspm.ui.profil.IProfilProvider2#getProfil()
   */
  public IProfil getProfil( )
  {
    return m_profile;
  }

  private final ProfilViewData m_viewData = new ProfilViewData();

  private final ISelectionProvider m_provider;

  private IFile m_file;

  private IProfil m_profile = null;

  private Feature m_feature;

  private CommandableWorkspace m_workspace;

  /** Flag to prevent update when source of model change is this */
  private boolean m_lockNextModelChange = false;

  private ValidationProfilListener m_profilValidator;

  public FeatureSelectionProfileProvider( final ISelectionProvider provider )
  {
    m_provider = provider;

    if( m_provider != null )
    {
      m_provider.addSelectionChangedListener( this );
      selectionChanged( new SelectionChangedEvent( m_provider, m_provider.getSelection() ) );
    }
  }

  public void dispose( )
  {
    if( m_provider != null )
      m_provider.removeSelectionChangedListener( this );

    unhookListeners();
  }

  /* find all results connected to this water */
  private IStationResult[] findResults( final WspmProfile profileMember )
  {
    final WspmWaterBody water = profileMember.getWater();
    if( water == null )
      return new IStationResult[] {};

    final GMLWorkspace workspace = water.getFeature().getWorkspace();

    final List<IStationResult> results = new ArrayList<IStationResult>();

    /* Waterlevel fixations */
    final List< ? > wspFixations = water.getWspFixations();
    for( final Object wspFix : wspFixations )
    {
      final Feature feature = FeatureHelper.getFeature( workspace, wspFix );

      final IStationResult result = new ObservationStationResult( feature, profileMember.getStation() );
      results.add( result );
    }

    /* Calculated results. */
    // TRICKY: this depends currently on the concrete model
    // so we need to know the model-type (such as tuhh) and
    // delegate the search for results to model-specific code.
    return results.toArray( new IStationResult[results.size()] );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.IProfilProvider2#getEventManager()
   */
  public IProfil getProfile( )
  {
    return m_profile;
  }

  /**
   * @see com.bce.profil.ui.view.IProfilProvider2#getFile()
   */
  public IFile getFile( )
  {
    return m_file;
  }

  public ISelectionProvider getSelectionProvider( )
  {
    return m_provider;
  }

  /**
   * @see com.bce.profil.ui.view.IProfilProvider2#getViewData()
   */
  public ProfilViewData getViewData( )
  {
    return m_viewData;
  }

  /**
   * If the feature changes, write it back to the profile.
   * 
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( m_lockNextModelChange )
    {
      m_lockNextModelChange = false;
      return;
    }

    // do no react to my own event, beware of recursion
    if( m_feature == null )
      return;

    if( modellEvent.isType( ModellEvent.FEATURE_CHANGE ) )
    {
      try
      {
        final IProfil profil = ProfileFeatureFactory.toProfile( m_feature );
        /**
         * TODO implement IStationResult to profiles
         */
        final IStationResult[] results = null;
        setProfile( profil, results, m_feature, m_workspace );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
      }
    }
  }

  /**
   * If the profile changes, write it back to the feature.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( m_profile != null && m_feature != null )
    {
      try
      {
        if( hint.isObjectChanged() || hint.isObjectDataChanged() || hint.isMarkerDataChanged() || hint.isMarkerMoved() || hint.isPointPropertiesChanged() || hint.isPointsChanged()
            || hint.isPointValuesChanged() || hint.isProfilPropertyChanged() )
        {
          final FeatureChange[] featureChanges = ProfileFeatureFactory.toFeatureAsChanges( m_profile, m_feature );

          final ChangeFeaturesCommand command = new ChangeFeaturesCommand( m_feature.getWorkspace(), featureChanges );
          m_lockNextModelChange = true;
          m_workspace.postCommand( command );
        }
      }
      catch( final Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
      }
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProblemMarkerChanged(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public void onProblemMarkerChanged( final IProfil source )
  {
    // Nothing to do: this class is probably the source for the event anyway
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    final ISelection selection = event.getSelection();
    if( !(selection instanceof IFeatureSelection) )
      return;

    final IFeatureSelection fs = (IFeatureSelection) selection;
    final EasyFeatureWrapper[] features = fs.getAllFeatures();

    WspmProfile profileMember = null;
    try
    {
      for( final EasyFeatureWrapper eft : features )
      {
        final Feature feature = eft.getFeature();

        if( feature != null )
        {
          profileMember = ProfileFeatureProvider.findProfile( feature );
          if( profileMember != null )
          {
            final Feature pf = profileMember.getFeature();

            // HACK: If type not set, force it to be the tuhh-profile. We need this, as tuhh-profile are created via
            // the gml-tree which knows nothing about profiles... Everyone else should create profile programatically
            // and directly set the prefered type.
            if( ProfileFeatureFactory.getProfileType( pf ) == null )
              ProfileFeatureFactory.setProfileType( pf, "org.kalypso.model.wspm.tuhh.profiletype" ); //$NON-NLS-1$

            break;
          }
        }
      }
    }
    catch( final Exception e )
    {
      final KalypsoModelWspmUIPlugin wspmPlugin = KalypsoModelWspmUIPlugin.getDefault();
      wspmPlugin.getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }

    // Check if this is the current feature, if true, do not set the profile agin
    final Feature profileFeature = profileMember == null ? null : profileMember.getFeature();

    if( ObjectUtils.equals( m_feature, profileFeature ) )
      return;

    if( profileMember == null )
    {
      setProfile( null, null, null, null );
      return;
    }

    final Feature feature = profileMember.getFeature();
    final CommandableWorkspace workspace = fs.getWorkspace( feature );
    final URL workspaceContext = workspace == null ? null : workspace.getContext();
    m_file = workspaceContext == null ? null : ResourceUtilities.findFileFromURL( workspaceContext );

    IProfil profile = ProfileFeatureFactory.toProfile( profileFeature );
    IStationResult[] results = findResults( profileMember );
    setProfile( profile, results, profileFeature, workspace );
  }

  private void setProfile( final IProfil profil, final IStationResult[] results, final Feature feature, final CommandableWorkspace workspace )
  {
    final IProfil oldProfile = m_profile;

    unhookListeners();

    m_feature = feature;
    m_workspace = workspace;

    m_profile = profil;

    if( m_profile != null )
    {
      m_profile.addProfilListener( this );

      if( m_profile != null && m_file != null )
      {
        m_profilValidator = new ValidationProfilListener( m_profile, m_file, null, m_feature.getId() );

        m_profile.addProfilListener( m_profilValidator );
      }
    }

    if( m_feature != null )
      m_feature.getWorkspace().addModellListener( this );

    fireOnProfilProviderChanged( this, oldProfile, m_profile, m_viewData, m_viewData );
  }

  private void unhookListeners( )
  {
    if( m_feature != null )
    {
      m_feature.getWorkspace().removeModellListener( this );
      m_feature = null;
    }

    if( m_profile != null )
    {
      if( m_profilValidator != null )
      {
        m_profile.removeProfilListener( m_profilValidator );
        m_profilValidator.dispose();
        m_profilValidator = null;
      }

      m_profile.removeProfilListener( this );
      m_profile = null;
    }
  }

}
