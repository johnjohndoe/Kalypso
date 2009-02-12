package org.kalypso.services.user.impl;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.framework.internal.core.FrameworkProperties;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.services.user.IUserRightsProvider;

/**
 * Delivers the rights from a simple property file
 * 
 * @author Jessica Huebsch
 */
@SuppressWarnings("restriction")
public class DefaultRightsProvider implements IUserRightsProvider, IExecutableExtension
{
  public final static String PROP_RIGHTSFILE_URL = "kalypso.user.defaultProvider.rightsFile";

  private final Properties m_rights = new Properties();

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data ) throws CoreException
  {
    InputStream ins = null;

    try
    {
      final String locURLString = FrameworkProperties.getProperty( PROP_RIGHTSFILE_URL, null );
      if( locURLString == null )
        throw new CoreException( StatusUtilities.createErrorStatus( "No rights file set for default-rights-provider. Use property '" + PROP_RIGHTSFILE_URL + "' to define the rights file." ) );

      final URL configUrl = Platform.getConfigurationLocation().getURL();
      final URL locURL = new URL( configUrl, locURLString );

      ins = new BufferedInputStream( locURL.openStream() );

      m_rights.load( ins );
      ins.close();
    }
    catch( final IOException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  /**
   * @see org.kalypso.services.user.IUserRightsProvider#dispose()
   */
  public void dispose( )
  {
    // empty
  }

  /**
   * @see org.kalypso.services.user.IUserRightsProvider#getRights(java.lang.String, java.lang.String)
   */
  public String[] getRights( final String username, final String scenarioId )
  {
    return m_rights.getProperty( username, "" ).split( "," );
  }

  /**
   * @see org.kalypso.services.user.IUserRightsProvider#getRights(java.lang.String, java.lang.String, java.lang.String)
   */
  public String[] getRights( final String username, final String password, final String scenarioId )
  {
    return getRights( username, scenarioId );
  }

}
