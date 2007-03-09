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
package org.kalypso.model.wspm.ui;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.IPointsSource;
import org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.IPointsTarget;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;

public class KalypsoModelWspmUIExtensions
{
  /** Only the Plugin may instantiate me */
  KalypsoModelWspmUIExtensions( )
  {
  }

  public static IPointsTarget[] createProfilPointTargets( )
  {
    return createExtensions( "org.kalypso.model.wspm.ui.profilPointsTarget", new IPointsTarget[0] );
  }

  public static IPointsSource[] createProfilPointSources( )
  {
    return createExtensions( "org.kalypso.model.wspm.ui.profilPointsSource", new IPointsSource[0] );
  }

  @SuppressWarnings("unchecked")
  public static <T> T[] createExtensions( final String extensionPoint, final T[] a )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] elements = registry.getConfigurationElementsFor( extensionPoint );

    // elements[0].getAttribute( "id" );
    // elements[0].createExecutableExtension( "class" );

    final Collection<T> targets = new ArrayList<T>( elements.length );
    for( int i = 0; i < elements.length; i++ )
    {
      final IConfigurationElement element = elements[i];
      try
      {
        targets.add( (T) element.createExecutableExtension( "class" ) );
      }
      catch( final CoreException e )
      {
        KalypsoModelWspmUIPlugin.getDefault().getLog().log( e.getStatus() );
      }
    }

    return targets.toArray( a );
  }
  /**
   * @return the LayerProvider for the given profiletype and providerId or null
   */
  public static IProfilLayerProvider createProfilLayerProvider( final String profiletype, final String providerId )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] elements = registry.getConfigurationElementsFor( "org.kalypso.model.wspm.ui.profilChartLayerProvider" );

    for( final IConfigurationElement element : elements )
    {
      final String type = element.getAttribute( "profiletype" );
      final String id = element.getAttribute( "id" );
      if( type.equals( profiletype ) && id.equals(providerId ) )
      {
        try
        {
          final Object layerProvider = element.createExecutableExtension( "provider" );
          if( layerProvider instanceof IProfilLayerProvider )
            return (IProfilLayerProvider) layerProvider;
        }
        catch( final CoreException e )
        {
          KalypsoModelWspmUIPlugin.getDefault().getLog().log( e.getStatus() );
        }
      }
    }
    final IStatus status = StatusUtilities.createWarningStatus( "No profile layer provider for type: " + profiletype );
    KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
    return null;

  }
/**
 * @return the first LayerProvider for the given profiletype
 */
  public static IProfilLayerProvider createProfilLayerProvider( final String profiletype )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] elements = registry.getConfigurationElementsFor( "org.kalypso.model.wspm.ui.profilChartLayerProvider" );

    for( final IConfigurationElement element : elements )
    {
      final String type = element.getAttribute( "profiletype" );
      if( type.equals( profiletype )  )
      {
        try
        {
          final Object layerProvider = element.createExecutableExtension( "provider" );
          if( layerProvider instanceof IProfilLayerProvider )
            return (IProfilLayerProvider) layerProvider;
        }
        catch( final CoreException e )
        {
          KalypsoModelWspmUIPlugin.getDefault().getLog().log( e.getStatus() );
        }
      }
    }
    final IStatus status = StatusUtilities.createWarningStatus( "No profile layer provider for type: " + profiletype );
    KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
    return null;
  }
}
