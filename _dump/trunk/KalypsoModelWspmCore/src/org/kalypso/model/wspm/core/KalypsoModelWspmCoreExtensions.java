package org.kalypso.model.wspm.core;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.model.wspm.core.profil.reparator.IProfilReparator;


/** Helper class to read extension points of this plugin. */
public class KalypsoModelWspmCoreExtensions
{
  public static IProfilReparator[] createReaparatorRules( )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] elements = registry
        .getConfigurationElementsFor( "org.kalypso.model.wspm.core.reparatorrule" );

    final Collection<IProfilReparator> reparators = new ArrayList<IProfilReparator>(
        elements.length );
    final Collection<IStatus> stati = new ArrayList<IStatus>( elements.length );

    for( final IConfigurationElement element : elements )
    {
      try
      {
        final IProfilReparator rule = (IProfilReparator)element.createExecutableExtension( "class" );
        reparators.add( rule );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
        
        stati.add( e.getStatus() );
      }
    }
    
    if( stati.size() > 0 )
    {
      final IStatus[] childrens = stati.toArray( new IStatus[stati.size()] );
      final IStatus status = new MultiStatus( KalypsoModelWspmCorePlugin.getID(), 0, childrens, "Ein oder mehrere Reparatoren konnten nicht initialisiert werden.", null );
      if ( status != null )
      {
        // TODO: what to do whith this status?
      }
    }

    return reparators.toArray( new IProfilReparator[reparators.size()] );
  }
}
