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
import org.eclipse.core.resources.IMarker;
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
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.core.profil.validator.ValidatorRuleSet;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.preferences.PreferenceConstants;

/**
 * Profil-listener which repairs and validates the profile, each time it changes.
 * 
 * @author belger
 */
public class ValidationProfilListener implements IProfilListener
{
  private final IPropertyChangeListener m_propertyListener;

  private final WorkspaceJob m_validateJob;

  private final String m_featureID;

  public ValidationProfilListener( final IProfil profile, final IFile file, final String editorID, final String featureID )
  {
    final String profiletype = profile.getType();

    final ValidatorRuleSet rules = KalypsoModelWspmCorePlugin.getValidatorSet( profiletype );

    m_featureID = featureID;

    m_validateJob = new WorkspaceJob( Messages.ValidationProfilListener_0 )
    {
      @Override
      public IStatus runInWorkspace( final IProgressMonitor monitor )
      {
        final IPreferenceStore preferenceStore = KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore();
        final boolean validate = preferenceStore.getBoolean( PreferenceConstants.P_VALIDATE_PROFILE );
        final String excludes = preferenceStore.getString( PreferenceConstants.P_VALIDATE_RULES_TO_EXCLUDE );

        final IValidatorMarkerCollector collector = new ResourceValidatorMarkerCollector( file, editorID, ""+profile.getStation(), m_featureID ); //$NON-NLS-1$

        try
        {
          // TODO: only reset markers of this profile
          collector.reset( featureID );

          // TODO: use monitor and check for cancel
          final IStatus status = rules.validateProfile( profile, collector, validate, excludes.split( ";" ) ); //$NON-NLS-1$

          final IMarker[] markers = collector.getMarkers();
          profile.setProblemMarker( markers );

          return status;
        }
        catch( final CoreException e )
        {
          return e.getStatus();
        }
      }
    };
    m_validateJob.setRule( file.getWorkspace().getRuleFactory().markerRule( file ) );

    m_propertyListener = new IPropertyChangeListener()
    {
      public void propertyChange( final PropertyChangeEvent event )
      {
        if( PreferenceConstants.P_VALIDATE_PROFILE.equals( event.getProperty() ) || PreferenceConstants.P_VALIDATE_RULES_TO_EXCLUDE.equals( event.getProperty() ) )
          revalidate(); // TODO: validate all profiles... in that case!
      }
    };

    KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore().addPropertyChangeListener( m_propertyListener );

    revalidate();
  }

  public void dispose( )
  {
    KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore().removePropertyChangeListener( m_propertyListener );
  }

  protected void revalidate( )
  {
    m_validateJob.cancel(); // Just in case, to avoid too much validations
    m_validateJob.schedule( 100 );
  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    // only revalidate if rellay data has changed
    if( hint.isObjectChanged() || hint.isObjectDataChanged() || hint.isMarkerDataChanged() || hint.isMarkerMoved() || hint.isPointPropertiesChanged() || hint.isPointsChanged()
        || hint.isPointValuesChanged() || hint.isProfilPropertyChanged() )
      revalidate();
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProblemMarkerChanged(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public void onProblemMarkerChanged( final IProfil source )
  {
    // Ignored
  }
}
