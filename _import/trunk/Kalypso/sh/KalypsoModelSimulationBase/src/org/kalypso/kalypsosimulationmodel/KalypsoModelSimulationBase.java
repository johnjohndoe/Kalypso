package org.kalypso.kalypsosimulationmodel;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.kalypso.kalypsosimulationmodel.extension.IKalypsoModule;
import org.osgi.framework.Bundle;

public class KalypsoModelSimulationBase extends Plugin
{
  private static List<IKalypsoModule> KALYPSO_MODULES = null;

  private final static String KALYPSO_MODULES_EXTENSION_POINT = "org.kalypso.ModelSimulationBase.kalypsoModule";

  public KalypsoModelSimulationBase( )
  {
  }

  /**
   * @return list of feature binding handlers, handling a special featureType qname
   */
  public synchronized static IKalypsoModule[] getKalypsoModules( )
  {
    // fill binding map
    if( KALYPSO_MODULES == null )
    {

      KALYPSO_MODULES = new ArrayList<IKalypsoModule>();
      /* get extension points */
      final IExtensionRegistry registry = Platform.getExtensionRegistry();
      final IConfigurationElement[] elements = registry.getConfigurationElementsFor( KALYPSO_MODULES_EXTENSION_POINT );

      for( final IConfigurationElement element : elements )
      {
        try
        {
          final String pluginid = element.getContributor().getName();
          final Bundle bundle = Platform.getBundle( pluginid );
          final Class< ? extends IKalypsoModule> featureClass = bundle.loadClass( element.getAttribute( "module" ) );
          final Constructor< ? extends IKalypsoModule> constructor = featureClass.getConstructor();

          final IKalypsoModule instance = constructor.newInstance();
          KALYPSO_MODULES.add( instance );
        }
        catch( final Throwable e )
        {
          e.printStackTrace();
        }
      }

      final Comparator<IKalypsoModule> comparator = new Comparator<IKalypsoModule>()
      {
        @Override
        public int compare( final IKalypsoModule o1, final IKalypsoModule o2 )
        {
          return o1.getModuleEnteringPage().getHeader().compareTo( o2.getModuleEnteringPage().getHeader() );
        }
      };

      Collections.sort( KALYPSO_MODULES, comparator );
    }

    return KALYPSO_MODULES.toArray( new IKalypsoModule[] {} );
  }
}
