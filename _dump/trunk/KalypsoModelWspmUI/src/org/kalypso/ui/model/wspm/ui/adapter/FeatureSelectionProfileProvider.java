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
package org.kalypso.ui.model.wspm.ui.adapter;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.model.wspm.IWspmConstants;
import org.kalypso.ui.model.wspm.KalypsoUIModelWspmPlugin;
import org.kalypso.ui.model.wspm.abstraction.WspmProfile;
import org.kalypso.ui.model.wspm.abstraction.WspmReachProfileSegment;
import org.kalypso.ui.model.wspm.core.profile.ProfileFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilEventManager;
import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.changes.ProfilChangeHint;
import com.bce.eind.core.profil.impl.ProfilEventManager;
import com.bce.profil.ui.view.AbstractProfilProvider2;
import com.bce.profil.ui.view.IProfilProvider2;
import com.bce.profil.ui.view.ProfilViewData;

/**
 * @author Gernot Belger
 */
public class FeatureSelectionProfileProvider extends AbstractProfilProvider2 implements IProfilProvider2, ISelectionChangedListener, IProfilListener, ModellEventListener
{
  private final ModellEventProvider m_mep = new ModellEventProviderAdapter();

  private final ProfilViewData m_viewData = new ProfilViewData();

  private final ISelectionProvider m_provider;

  private IProfilEventManager m_pem = null;

  private final IFile m_file;

  private Feature m_feature;

  public FeatureSelectionProfileProvider( final IFile file, final ISelectionProvider provider )
  {
    m_file = file;
    m_provider = provider;

    m_provider.addSelectionChangedListener( this );
    selectionChanged( new SelectionChangedEvent( m_provider, m_provider.getSelection() ) );
  }

  public void dispose( )
  {
    m_provider.removeSelectionChangedListener( this );

    unhookListeners();
  }

  /**
   * @see com.bce.profil.ui.view.IProfilProvider2#getEventManager()
   */
  public IProfilEventManager getEventManager( )
  {
    return m_pem;
  }

  /**
   * @see com.bce.profil.ui.view.IProfilProvider2#getViewData()
   */
  public ProfilViewData getViewData( )
  {
    return m_viewData;
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
    final Feature feature = FeatureSelectionHelper.getSelectedFeature( fs );

    IProfil profile = null;
    WspmProfile profileMember = null;
    try
    {
      if( feature != null )
      {

        if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), new QName( IWspmConstants.NS_WSPMPROF, "Profile" ) ) )
          profileMember = new WspmProfile( feature );
        else if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), new QName( IWspmConstants.NS_WSPM, "ProfileReachSegment" ) ) )
        {
          final WspmReachProfileSegment segment = new WspmReachProfileSegment( feature );
          profileMember = segment.getProfileMember();
        }

        if( profileMember != null )
        {
          profile = ProfileFeatureFactory.toProfile( profileMember.getFeature() );
        }
      }
    }
    catch( final ProfilDataException e )
    {
      final KalypsoUIModelWspmPlugin wspmPlugin = KalypsoUIModelWspmPlugin.getDefault();
      wspmPlugin.getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }

    final Feature profileFeature = profileMember == null ? null : profileMember.getFeature();
    setProfile( profile, profileFeature );
  }

  private void unhookListeners( )
  {
    if( m_feature != null )
    {
      m_feature.getWorkspace().removeModellListener( this );
      m_feature = null;
    }

    if( m_pem != null )
    {
      m_pem.removeProfilListener( this );
      m_pem = null;
    }
  }

  /**
   * @see com.bce.profil.ui.view.IProfilProvider2#getFile()
   */
  public IFile getFile( )
  {
    return m_file;
  }

  /**
   * If the profile changes, write it back to the feature.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    final IProfil profil = m_pem.getProfil();
    if( profil != null && m_feature != null )
    {
      ProfileFeatureFactory.toFeature( profil, m_feature );
      // use m_mep as ModellEventProvider to remember where the event came from
      m_feature.getWorkspace().fireModellEvent( new ModellEvent( m_mep, ModellEvent.FEATURE_CHANGE ) );
    }
  }

  /**
   * If the feature changes, write it back to the profile.
   * 
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    // do no react to my own event, beware of recursion
    if( modellEvent.getEventSource() == m_mep )
      return;

    if( modellEvent.isType( ModellEvent.FEATURE_CHANGE ) )
    {
      try
      {
        final IProfil profil = ProfileFeatureFactory.toProfile( m_feature );
        setProfile( profil, m_feature );
      }
      catch( ProfilDataException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
  }

  private void setProfile( final IProfil profil, final Feature feature )
  {
    final IProfilEventManager oldPem = m_pem;

    unhookListeners();

    if( profil != null )
      m_pem = new ProfilEventManager( profil );
    m_feature = feature;

    if( m_pem != null )
      m_pem.addProfilListener( this );

    if( m_feature != null )
      m_feature.getWorkspace().addModellListener( this );

    fireOnProfilProviderChanged( this, oldPem, m_pem, m_viewData, m_viewData );
  }
}
