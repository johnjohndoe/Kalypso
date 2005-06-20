package org.kalypso.repository;

import java.util.Vector;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.kalypso.repository.conf.RepositoryFactoryConfig;
import org.kalypso.repository.factory.IRepositoryFactory;

/**
 * Helper class that delves into the extensions of the extension point org.kalypso.repositories.
 * 
 * @author schlienger
 */
public class RepositoriesExtensions
{
  public final static String REPOSITORIES_EXTENSION_POINT = "org.kalypso.repository.factories";

  public final static String ATT_NAME = "name";

  public final static String ATT_FACTORY = "factory";

  public final static String ATT_CONF = "conf";

  public final static String ATT_RO = "readOnly";

  /**
   * Uses the platform extension registry to retrieve all extensions for the repositories extension point.
   * <p>
   * For each extension, a RepositoryFactoryConfig is created which can be used in your application.
   * 
   * @return array of config items
   * @throws CoreException
   */
  public static RepositoryFactoryConfig[] retrieveExtensions() throws CoreException
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( REPOSITORIES_EXTENSION_POINT );

    if( extensionPoint == null )
      return new RepositoryFactoryConfig[0];

    final IExtension[] extensions = extensionPoint.getExtensions();

    final Vector items = new Vector();

    for( int i = 0; i < extensions.length; i++ )
    {
      final IExtension extension = extensions[i];
      final IConfigurationElement[] elements = extension.getConfigurationElements();

      for( int j = 0; j < elements.length; j++ )
      {
        final IConfigurationElement element = elements[j];

        final String name = element.getAttribute( ATT_NAME );
        final String conf = element.getAttribute( ATT_CONF );
        final boolean ro = Boolean.valueOf( element.getAttribute( ATT_RO ) ).booleanValue();

        final IRepositoryFactory factory = (IRepositoryFactory)element.createExecutableExtension( ATT_FACTORY );

        items.add( new RepositoryFactoryConfig( factory, name, conf, ro ) );
      }
    }

    return (RepositoryFactoryConfig[])items.toArray( new RepositoryFactoryConfig[items.size()] );
  }

  /**
   * Returns the corresponding factory config for the given repository factory classname
   * 
   * @param factoryClassName
   * @param repositoryName
   * @param conf
   * @param readOnly
   * @throws CoreException
   */
  public static RepositoryFactoryConfig retrieveExtensionFor( String factoryClassName, String repositoryName,
      String conf, boolean readOnly ) throws CoreException
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( REPOSITORIES_EXTENSION_POINT );

    if( extensionPoint == null )
      return null;

    final IExtension[] extensions = extensionPoint.getExtensions();

    for( int i = 0; i < extensions.length; i++ )
    {
      final IExtension extension = extensions[i];
      final IConfigurationElement[] elements = extension.getConfigurationElements();

      for( int j = 0; j < elements.length; j++ )
      {
        final IConfigurationElement element = elements[j];

        if( factoryClassName.equals( element.getAttribute( ATT_FACTORY ) ) )
        {
          final IRepositoryFactory factory = (IRepositoryFactory)element.createExecutableExtension( ATT_FACTORY );

          return new RepositoryFactoryConfig( factory, repositoryName, conf, readOnly );
        }
      }
    }

    return null;
  }
}