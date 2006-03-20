package org.kalypso.services.user.impl;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.kalypso.services.user.IUserRightsProvider;
import org.kalypso.services.user.UserRightsException;

/**
 * Delivers the rights from a simple property file
 * 
 * @author Jessica Huebsch
 */
public class DefaultRightsProvider implements IUserRightsProvider
{
  public final static String PROP_RIGHTSFILE_URL = "RIGHTSFILE_URL";

  private final Properties m_rights = new Properties();

  /**
   * @see org.kalypso.services.user.IUserRightsProvider#init(java.util.Properties)
   */
  public void init( final Properties props ) throws UserRightsException
  {
    final String locURLString = props.getProperty( PROP_RIGHTSFILE_URL );

    InputStream ins = null;
    try
    {
      final URL locURL = new URL( locURLString );
      ins = new BufferedInputStream( locURL.openStream() );

      m_rights.load( ins );
      ins.close();
    }
    catch( final IOException e )
    {
      throw new UserRightsException( e );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  /**
   * @see org.kalypso.services.user.IUserRightsProvider#dispose()
   */
  public void dispose()
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
