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

import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.contribs.java.lang.reflect.ClassUtilityException;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoriesExtensions;
import org.kalypso.repository.factory.IRepositoryFactory;

/**
 * Using such an object you can create the <code>IRepositoryFactory</code> for which it delivers the initial
 * configuration.
 * 
 * @author schlienger
 */
public class RepositoryFactoryConfig
{
  private final String m_name;

  private final String m_factory;

  private final String m_conf;

  private final boolean m_readOnly;

  private final static String SEPARATOR = ";";

  /** factory can be specified in constructor */
  private IRepositoryFactory m_rf = null;

  /**
   * Constructor with:
   * 
   * @param name
   *          display name of the repository
   * @param factory
   *          name of the IRepositoryFactory class
   * @param conf
   *          configuration used when instanciating
   * @param readOnly
   *          when true repository should be read only
   */
  protected RepositoryFactoryConfig( String name, String factory, String conf, boolean readOnly )
  {
    m_name = name;
    m_factory = factory;
    m_conf = conf;
    m_readOnly = readOnly;
  }

  /**
   * Constructor with repository
   */
  public RepositoryFactoryConfig( final IRepository rep )
  {
    this( rep.getName(), rep.getFactory(), rep.getConfiguration(), rep.isReadOnly() );
  }

  /**
   * Shortcut constructor with factory. if createFactory() is called, it will return this given factory configured with
   * the given arguments.
   */
  public RepositoryFactoryConfig( IRepositoryFactory rf, String name, String conf, boolean ro )
  {
    this( name, rf.getClass().getName(), conf, ro );

    m_rf = rf;
  }

  /**
   * Creates the underlying factory.
   */
  public IRepositoryFactory createFactory( final ClassLoader cl ) throws ClassUtilityException
  {
    final IRepositoryFactory rf;

    // if member factory is defined, no need to create a new instance, just use
    // it
    if( m_rf != null )
      rf = m_rf;
    else
      rf = (IRepositoryFactory)ClassUtilities.newInstance( m_factory, IRepositoryFactory.class, cl );

    rf.setReadOnly( m_readOnly );
    rf.setConfiguration( m_conf );
    rf.setRepositoryName( m_name );

    return rf;
  }

  /**
   * Saves the state of this object in a simple string representation.
   * 
   * @return state
   */
  public String saveState()
  {
    final StringBuffer bf = new StringBuffer();

    bf.append( m_name ).append( SEPARATOR ).append( m_factory ).append( SEPARATOR ).append( m_conf ).append( SEPARATOR )
        .append( String.valueOf( m_readOnly ) );

    return bf.toString();
  }

  /**
   * Restores a <code>RepositoryConfigItem</code> from the state provided as a string. This is the pendant to the
   * saveState() method.
   * 
   * @return a repository config item
   */
  public static RepositoryFactoryConfig restore( final String state ) throws CoreException
  {
    final String[] splits = state.split( SEPARATOR );

    if( splits.length != 4 )
      return null;

    final String repositoryName = splits[0];
    final String factoryClassName = splits[1];
    final String conf = splits[2];
    final boolean readOnly = Boolean.valueOf( splits[3] ).booleanValue();

    // retrieve instance using extension mechanism,
    // else class might not be found (ClassLoadingException)
    return RepositoriesExtensions.retrieveExtensionFor( factoryClassName, repositoryName, conf, readOnly );
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    if( m_conf != null && m_conf.length() > 0 )
      return m_name + " (" + m_conf + ")";

    return m_name;
  }
}