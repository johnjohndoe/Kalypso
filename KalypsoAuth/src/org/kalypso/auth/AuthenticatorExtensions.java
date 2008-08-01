package org.kalypso.auth;

import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.kalypso.auth.login.IAuthenticator;

/**
 * Helper class that delves into the extensions of the extension point org.kalypso.auth.login.IAuthenticator
 * 
 * @author schlienger
 */
public class AuthenticatorExtensions
{
  public final static String EXTENSION_POINT = "org.kalypso.auth.authenticator";

  public final static String ATT_AUTHENTICATOR = "class";

  /**
   * Uses the platform extension registry to retrieve all extensions for the authenticators extension point.
   * <p>
   * For each extension, an instance of IAuthenticator is created which can be used in your application.
   */
  public static IAuthenticator[] retrieveExtensions() throws CoreException
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( EXTENSION_POINT );

    if( extensionPoint == null )
      return new IAuthenticator[0];

    final IExtension[] extensions = extensionPoint.getExtensions();

    final ArrayList<IAuthenticator> items = new ArrayList<IAuthenticator>( extensions.length + 1 );

    for( int i = 0; i < extensions.length; i++ )
    {
      final IExtension extension = extensions[i];
      final IConfigurationElement[] elements = extension.getConfigurationElements();

      for( int j = 0; j < elements.length; j++ )
      {
        final IConfigurationElement element = elements[j];

        final IAuthenticator auth = (IAuthenticator)element.createExecutableExtension( ATT_AUTHENTICATOR );

        items.add( auth );
      }
    }
    
    return items.toArray( new IAuthenticator[items.size()] );
  }
}