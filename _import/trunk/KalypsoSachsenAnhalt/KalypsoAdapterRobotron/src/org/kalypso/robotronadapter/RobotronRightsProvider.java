/*
 * --------------- Kalypso-Header --------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ----------------------------------------------------------------------------
 */
package org.kalypso.robotronadapter;

import java.util.Arrays;
import java.util.Hashtable;
import java.util.Properties;
import java.util.logging.Logger;

import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.Attributes;
import javax.naming.directory.DirContext;
import javax.naming.directory.InitialDirContext;

import org.kalypso.auth.user.UserRights;
import org.kalypso.services.user.IUserRightsProvider;
import org.kalypso.services.user.UserRightsException;

/**
 * RobotronRightsProvider
 * 
 * @author schlienger (13.05.2005)
 */
public class RobotronRightsProvider implements IUserRightsProvider
{
  private final static Logger LOG = Logger.getLogger( RobotronRightsProvider.class.getName() );

  /** Example "ldap://193.23.163.115:389/dc=hvz,dc=lhw,dc=mlu,dc=lsa-net,dc=de" */
  private String m_url;

  /** Example "cn=admin,dc=hvz,dc=lhw,dc=mlu,dc=lsa-net,dc=de" */
  private String m_principal;

  /** Example "geheim" */
  private String m_crendentials;

  /**
   * The properties should contain following information:
   * <p>
   * <ul>
   * <li>LDAP_CONNECTION: the url of the ldap service. Example:
   * "LDAP_CONNECTION=ldap://193.23.163.115:389/dc=hvz,dc=lhw,dc=mlu,dc=lsa-net,dc=de"
   * <li>LDAP_PRINCIPAL: the principal of the ldap. Example:
   * "LDAP_PRINCIPAL=cn=admin,dc=hvz,dc=lhw,dc=mlu,dc=lsa-net,dc=de"
   * <li>LDAP_CRENDENTIALS: the password to use along the principal. Example: "LDAP_CREDENTIALS=geheim"
   * 
   * @see IUserRightsProvider#init(java.util.Properties)
   */
  public void init( Properties props ) throws UserRightsException
  {
    m_url = props.getProperty( "LDAP_CONNECTION" );
    m_principal = props.getProperty( "LDAP_PRINCIPAL" );
    m_crendentials = props.getProperty( "LDAP_CREDENTIALS" );

    LOG.info( "RobotronRightsProvider initialised" );
  }

  private DirContext getDirContext() throws NamingException
  {
    final Hashtable env = new Hashtable();

    // set the parameters for the intial context
    env.put( Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory" );
    env.put( Context.PROVIDER_URL, m_url );
    env.put( Context.SECURITY_PRINCIPAL, m_principal );
    env.put( Context.SECURITY_CREDENTIALS, m_crendentials );

    return new InitialDirContext( env );
  }

  public void dispose()
  {
  // empty
  }

  public String[] getRights( String username ) throws UserRightsException
  {
    // this method doesn't make sense for the Sachsen-Anhalt project
    throw new UserRightsException( "Method not implemented" );
  }

  public String[] getRights( final String username, final String password ) throws UserRightsException
  {
    DirContext dirCtxt = null;
    
    try
    {
      dirCtxt = getDirContext();
      
      final Attributes userAtts = dirCtxt.getAttributes( "cn=" + username + ",ou=benutzer", new String[]
      { "gidNumber", "userPassword" } );

      final byte[] pw1 = (byte[])userAtts.get( "userPassword" ).get();
      final byte[] pw2 = password.getBytes();
      if( !Arrays.equals( pw1, pw2 ) )
      {
        LOG.info( "Passwords do not match for user: " + username );
        return UserRights.NO_RIGHTS;
      }

      final String groupName = (String)userAtts.get( "gidNumber" ).get();
      LOG.info( "User " + username + " found in group: " + groupName );

      final Attributes rightsAtt = dirCtxt.getAttributes( "gidNumber=" + groupName + ",ou=gruppen" );

      final NamingEnumeration rightsEnum = rightsAtt.get( "recht" ).getAll();
      while( rightsEnum.hasMore() )
      {
        final String right = rightsEnum.next().toString();

        // in Kalypso Sachsen-Anhalt the "Modellierung"-Right specifies whether
        // a user is allowed to use Kalypso or not
        if( "Modellierung".equalsIgnoreCase( right ) )
          return UserRights.FULL_RIGHTS;
      }

      // Benutzer existiert, aber darf nicht Modellieren: Vorhersage
      return new String[]
      { UserRights.RIGHT_PROGNOSE };
    }
    catch( final NamingException e )
    {
      LOG.throwing( getClass().getName(), "getRights", e );
      LOG.finest( "Returning NO_RIGHTS due to previous errors" );

      return UserRights.NO_RIGHTS;
    }
    finally
    {
      try
      {
        if( dirCtxt != null )
          dirCtxt.close();
      }
      catch( final NamingException ignored )
      {
        // empty
      }
    }
  }
}
