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
package org.kalypso.model.wspm.ui.wizard;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Helper class for wizards on {@link IKalypsoFeatureTheme}s.
 * 
 * @author Gernot Belger
 */
public class FeatureThemeWizardUtilitites
{
  private FeatureThemeWizardUtilitites( )
  {
    // helper class, don't instantiate
  }

  public static class FOUND_PROFILES
  {
    public IKalypsoFeatureTheme theme;

    public Feature[] foundProfiles;

    public Feature[] selectedProfiles;
  }

  /**
   * Returns the profile-features from the first feature-theme in the selection.
   * 
   * @return null, if no theme was found. returnValue[0]: all found profile; returnValue[1]: all found selectedProfiles
   */
  public static FOUND_PROFILES getProfileFeaturesFromThemeSelection( final ISelection selection )
  {
    /* retrieve selected profiles, abort if none */
    if( selection instanceof IStructuredSelection )
    {
      for( final Object selectedObject : ((IStructuredSelection) selection).toList() )
      {
        if( selectedObject instanceof IKalypsoFeatureTheme )
        {
          final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) selectedObject;

          final List<Feature> foundProfiles = new ArrayList<Feature>();
          final List<Feature> selectedProfiles = new ArrayList<Feature>( foundProfiles );
          final Set<Object> selectedFeatures = new HashSet<Object>( theme.getSelectionManager().toList() );

          final FeatureList featureList = theme.getFeatureList();
          for( final Object object : featureList )
          {
            final IProfileFeature profile = (IProfileFeature) FeatureHelper.getFeature( theme.getWorkspace(), object );
            if( profile != null )
            {
              foundProfiles.add( profile );

              if( selectedFeatures.contains( profile ) )
                selectedProfiles.add( profile );
            }
          }

          final FOUND_PROFILES found_profiles = new FOUND_PROFILES();

          found_profiles.theme = theme;
          found_profiles.foundProfiles = foundProfiles.toArray( new Feature[foundProfiles.size()] );
          found_profiles.selectedProfiles = selectedProfiles.toArray( new Feature[selectedProfiles.size()] );

          return found_profiles;
        }
      }
    }

    return null;
  }

}
