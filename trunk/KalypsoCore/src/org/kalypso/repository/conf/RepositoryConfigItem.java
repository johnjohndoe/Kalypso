/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.repository.conf;

import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.repository.IRepositoryFactory;

/**
 * Using such an item you can create the <code>IRepositoryFactory</code> for
 * which it delivers the initial configuration.
 * 
 * @author schlienger
 */
public class RepositoryConfigItem
{
  private final String m_className;

  private final String m_conf;

  private final boolean m_readOnly;

  private final static String SEPARATOR = ";";
  
  /**
   * Constructor with:
   * 
   * @param className
   *          name of the IRepositoryFactory class
   * @param conf
   *          configuration used when instanciating the factory class
   * @param readOnly
   *          when true repository should be read only
   *  
   */
  public RepositoryConfigItem( final String className, final String conf, final boolean readOnly )
  {
    m_className = className;
    m_conf = conf;
    m_readOnly = readOnly;
  }

  /**
   * Constructor with factory.
   * @param factory
   */
  public RepositoryConfigItem( final IRepositoryFactory factory )
  {
    this( factory.getClass().getName(), factory.getConfiguration(), factory.isReadOnly() );
  }

  /**
   * Creates the underlying factory.
   * @param cl
   * @return factory
   * @throws ClassUtilityException
   */
  public IRepositoryFactory createFactory( final ClassLoader cl ) throws ClassUtilityException
  {
    final IRepositoryFactory rf = (IRepositoryFactory)ClassUtilities.newInstance( m_className,
        IRepositoryFactory.class, cl );

    rf.setReadOnly( m_readOnly );

    rf.setConfiguration( m_conf );

    return rf;
  }

  /**
   * Saves the state of this object in a simple string representation.
   */
  public String saveState()
  {
    final StringBuffer bf = new StringBuffer();

    bf.append( m_className ).append( SEPARATOR ).append( m_conf ).append( SEPARATOR ).append( String.valueOf( m_readOnly ) );

    return bf.toString();
  }

  /**
   * Restores a <code>RepositoryConfigItem</code> from the state provided as a
   * string. This is the pendant to the saveState() method.
   * @param state
   * @return a repository config item
   */
  public static RepositoryConfigItem restore( final String state )
  {
    final String[] splits = state.split( SEPARATOR );
    
    if( splits.length != 3 )
      return null;
    
    return new RepositoryConfigItem( splits[0], splits[1], Boolean.valueOf( splits[2] ).booleanValue() );
  }
}