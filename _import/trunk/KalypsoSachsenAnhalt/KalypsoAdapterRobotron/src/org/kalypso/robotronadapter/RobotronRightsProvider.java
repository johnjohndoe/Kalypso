/*
 * --------------- Kalypso-Header --------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ----------------------------------------------------------------------------
 */
package org.kalypso.robotronadapter;

import java.util.Hashtable;
import java.util.logging.Logger;

import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.Attributes;
import javax.naming.directory.DirContext;
import javax.naming.directory.InitialDirContext;

import org.kalypso.users.IUserRightsProvider;
import org.kalypso.users.UserRightsException;
import org.kalypso.users.UserServiceConstants;

/**
 * RobotronRightsProvider
 * <p>
 * 
 * created by
 * 
 * @author schlienger (13.05.2005)
 */
public class RobotronRightsProvider implements IUserRightsProvider
{
  private DirContext m_dirCtxt;

  /** "ldap://193.23.163.115:389/dc=hvz,dc=lhw,dc=mlu,dc=lsa-net,dc=de" */
  private final String m_url;

  /** "cn=admin,dc=hvz,dc=lhw,dc=mlu,dc=lsa-net,dc=de" */
  private final String m_principal;

  /** "geheim" */
  private final String m_crendentials;

  public RobotronRightsProvider( final String url, final String principal,
      final String crendentials )
  {
    m_url = url;
    m_principal = principal;
    m_crendentials = crendentials;
  }

  private DirContext getDirContext()
  {
    if( m_dirCtxt != null )
      return m_dirCtxt;

    final Hashtable env = new Hashtable();
    // set the parameters for the intial context
    env.put( Context.INITIAL_CONTEXT_FACTORY,
        "com.sun.jndi.ldap.LdapCtxFactory" );
    env.put( Context.PROVIDER_URL, m_url );
    env.put( Context.SECURITY_PRINCIPAL, m_principal );
    env.put( Context.SECURITY_CREDENTIALS, m_crendentials );

    try
    {
      m_dirCtxt = new InitialDirContext( env );
    }
    catch( final NamingException e )
    {
      e.printStackTrace();

      throw new IllegalStateException( e.getLocalizedMessage()
          + " Explanation: " + e.getExplanation() );
    }

    return m_dirCtxt;
  }

  /**
   * @see org.kalypso.users.IUserRightsProvider#dispose()
   */
  public void dispose()
  {
    if( m_dirCtxt != null )
    {
      try
      {
        m_dirCtxt.close();
      }
      catch( final NamingException e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * @see org.kalypso.users.IUserRightsProvider#getRights(java.lang.String)
   */
  public String[] getRights( String username ) throws UserRightsException
  {
    // this method doesn't make sense for the Sachsen-Anhalt project
    throw new UserRightsException( "Method not implemented" );
  }

  /**
   * @see org.kalypso.users.IUserRightsProvider#getRights(java.lang.String,
   *      java.lang.String)
   */
  public String[] getRights( String username, String password )
      throws UserRightsException
  {
    try
    {
      final Attributes userAtts = getDirContext().getAttributes(
          "uid=" + username + ",ou=benutzer", new String[] {
              "gruppe",
              "userPassword" } );

      final Logger logger = Logger.getLogger( getClass().getName() );

      // read the retrieved attribute values
      final String groupName = (String)userAtts.get( "gruppe" ).get();

      logger.info( "User [" + username + "] exists in group: " + groupName );

      final Attributes rightsAtt = m_dirCtxt.getAttributes( "cn=" + groupName
          + ",ou=gruppen" );

      final NamingEnumeration rightsEnum = rightsAtt.get( "recht" ).getAll();
      while( rightsEnum.hasMore() )
      {
        final String right = rightsEnum.next().toString();

        // in Kalypso Sachsen-Anhalt the "Modellierung"-Right specifies whether
        // a user is allowed to use Kalypso or not
        if( "Modellierung".equals( right ) )
          return UserServiceConstants.FULL_RIGHTS;
      }

      return UserServiceConstants.NO_RIGHTS;
    }
    catch( final NamingException e )
    {
      e.printStackTrace();

      return UserServiceConstants.NO_RIGHTS;
    }
  }
}
