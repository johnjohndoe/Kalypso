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
package org.kalypso.model.wspm.ui.profil.validation;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.core.profil.validator.ValidatorRuleSet;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.preferences.PreferenceConstants;

/**
 * Profil-listener which repairs and validates the profile, each time it changes.
 * 
 * @author belger
 */
public class ValidationProfilListener implements IProfilListener
{
  private ValidatorRuleSet m_rules = KalypsoModelWspmCorePlugin.getValidatorSet( "Pasche" );

//  private ReparatorRuleSet m_reparatorRules = KalypsoModelWspmCorePlugin.getDefault().createReparatorRuleSet();

  private final IProfilEventManager m_pem;

  private IPropertyChangeListener m_propertyListener;

  private IPreferenceStore m_preferenceStore;

  private final IFile m_file;

  private final String m_editorID;

  private final ResourceValidatorMarkerCollector m_collector;

  public ValidationProfilListener( final IProfilEventManager profil, final IFile file, final String editorID )
  {
    // validierung (re-)initialisieren
    // TODO: das (WspWin-)Projekt weiss, welcher Art es ist

    m_pem = profil;
    m_file = file;
    m_editorID = editorID;

    m_collector = new ResourceValidatorMarkerCollector( m_file, m_editorID );

    m_propertyListener = new IPropertyChangeListener()
    {
      public void propertyChange( final PropertyChangeEvent event )
      {
        if( PreferenceConstants.P_VALIDATE_PROFILE.equals( event.getProperty() ) || PreferenceConstants.P_VALIDATE_RULES_TO_EXCLUDE.equals( event.getProperty() ) )
          revalidate();
      }
    };
    m_preferenceStore = KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore();
    m_preferenceStore.addPropertyChangeListener( m_propertyListener );

    revalidate();
  }

  public void dispose( )
  {
    try
    {
      m_collector.reset();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

    KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore().removePropertyChangeListener( m_propertyListener );
  }

  protected void revalidate( )
  {
    // bisserl hack, aber der richtige zeitpunkt, um das profil zu validieren
//    if( m_reparatorRules.repairProfile( m_pem.getProfil() ) )
//      return;

    final boolean validate = m_preferenceStore.getBoolean( PreferenceConstants.P_VALIDATE_PROFILE );
    final String excludes = m_preferenceStore.getString( PreferenceConstants.P_VALIDATE_RULES_TO_EXCLUDE );

    final ValidatorRuleSet rules = m_rules;
    final IProfil profil = m_pem.getProfil();
    final IValidatorMarkerCollector collector = m_collector;

    final WorkspaceJob job = new WorkspaceJob( "Profil wird validiert" )
    {
      @Override
      public IStatus runInWorkspace( final IProgressMonitor monitor )
      {
        try
        {
          collector.reset();
        }
        catch( final CoreException e )
        {
          return e.getStatus();
        }

        return rules.validateProfile( profil, collector, validate, excludes.split( ";" ) );
      }
    };
    job.schedule();
    try
    {
      // Join the job, because we only want to return, when validation has finished
      job.join();
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }
  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    // only revalidate if rellay data has changed
    if( hint.isBuildingChanged() || hint.isBuildingDataChanged() || hint.isDeviderDataChanged() || hint.isDeviderMoved() || hint.isPointPropertiesChanged() || hint.isPointsChanged()
        || hint.isPointValuesChanged() || hint.isProfilPropertyChanged() )
      revalidate();
  }
}
