package org.kalypso.services.user.impl;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.kalypso.users.IUserRightsProvider;
import org.kalypso.users.UserRightsException;

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
   * @see org.kalypso.users.IUserRightsProvider#init(java.util.Properties)
   */
  public void init( final Properties props ) throws UserRightsException
  {
    final String loc = props.getProperty( PROP_RIGHTSFILE_URL );
    final File file = new File( loc );

    InputStream ins = null;
    try
    {
      ins = new BufferedInputStream( new FileInputStream( file ) );

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
   * @see org.kalypso.users.IUserRightsProvider#dispose()
   */
  public void dispose()
  {
  // empty
  }

  /**
   * @see org.kalypso.users.IUserRightsProvider#getRights(java.lang.String)
   */
  public String[] getRights( final String username )
  {
    return m_rights.getProperty( username, "" ).split( "," );
  }

  /**
   * @see org.kalypso.users.IUserRightsProvider#getRights(java.lang.String, java.lang.String)
   */
  public String[] getRights( String username, String password )
  {
    return getRights( username );
  }
}
