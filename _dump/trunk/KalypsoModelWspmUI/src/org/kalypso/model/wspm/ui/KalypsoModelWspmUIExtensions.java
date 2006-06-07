package org.kalypso.model.wspm.ui;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.IPointsSource;
import org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.IPointsTarget;


public class KalypsoModelWspmUIExtensions
{
  /** Only the Plugin may instantiate me */
  KalypsoModelWspmUIExtensions( )
  {
  }

  public static IPointsTarget[] createProfilPointTargets( )
  {
    
    return  createExtensions( "org.kalypso.model.wspm.ui.profil.profilPointsTarget",new IPointsTarget[0]);
  }

  public static IPointsSource[] createProfilPointSources( )
  {
    return createExtensions( "org.kalypso.model.wspm.ui.profil.profilPointsSource", new IPointsSource[0] );
  }

  @SuppressWarnings("unchecked")
  public static <T> T[] createExtensions( final String extensionPoint, final T[] a )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] elements = registry.getConfigurationElementsFor( extensionPoint );

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
}
