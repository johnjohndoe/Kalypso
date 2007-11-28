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
package org.kalypso.repository;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Vector;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.commons.java.util.PropertiesHelper;

/**
 * Abstract implementation of <code>IRepository</code> to provide basic functionality.
 * 
 * @author schlienger
 */
public abstract class AbstractRepository implements IRepository
{
  private final String m_name;

  private final String m_factory;

  private boolean m_readOnly;

  private final List m_listeners;

  private final Properties m_properties;

  private final String m_conf;

  public AbstractRepository( String name, String factory, String conf, boolean readOnly )
  {
    m_name = name;
    m_factory = factory;
    m_conf = conf;
    m_readOnly = readOnly;

    m_listeners = new Vector();
    m_properties = new Properties();
  }

  /**
   * @see org.kalypso.repository.IRepository#dispose()
   */
  public void dispose()
  {
    m_listeners.clear();
    m_properties.clear();
  }

  public String getFactory()
  {
    return m_factory;
  }

  public String getConfiguration()
  {
    return m_conf;
  }

  public boolean isReadOnly()
  {
    return m_readOnly;
  }

  public void setReadOnly( final boolean ro )
  {
    m_readOnly = ro;
  }

  /**
   * @see org.kalypso.repository.IRepository#getDescription()
   */
  public String getDescription()
  {
    return "";
  }

  /**
   * @see org.kalypso.repository.IRepository#addRepositoryListener(org.kalypso.repository.IRepositoryListener)
   */
  public void addRepositoryListener( final IRepositoryListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.repository.IRepository#fireRepositoryStructureChanged()
   */
  public void fireRepositoryStructureChanged()
  {
    for( Iterator iter = m_listeners.iterator(); iter.hasNext(); )
    {
      IRepositoryListener element = (IRepositoryListener)iter.next();

      element.onRepositoryStructureChanged();
    }
  }

  /**
   * @see org.kalypso.repository.IRepository#removeRepositoryListener(org.kalypso.repository.IRepositoryListener)
   */
  public void removeRepositoryListener( final IRepositoryListener l )
  {
    m_listeners.remove( l );
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent()
  {
    return null;
  }

  /**
   * This default implementation uses recursion to find an item with the requested id. Subclasses may use this method if
   * they want to implement findItem using recursion.
   * 
   * @return item if found, else null
   */
  protected final IRepositoryItem findItemRecursive( final IRepositoryItem item, final String id )
      throws RepositoryException
  {
    if( item.getIdentifier().equalsIgnoreCase( id ) )
      return item;

    final IRepositoryItem[] items = item.getChildren();
    if( items == null )
      return null;

    for( int i = 0; i < items.length; i++ )
    {
      final IRepositoryItem item2 = findItemRecursive( items[i], id );

      if( item2 != null )
        return item2;
    }

    return null;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    final String desc = getDescription();
    if( desc != null && desc.length() > 0 )
      return getName() + " (" + desc + ")";

    return getName();
  }

  /**
   * This default implementation always returns null.
   */
  public Object getAdapter( final Class anotherClass )
  {
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository()
  {
    return this;
  }

  /**
   * @see org.kalypso.repository.IRepository#dumpStructure(java.io.Writer, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void dumpStructure( final Writer writer, final IProgressMonitor monitor ) throws RepositoryException,
      InterruptedException
  {
    dumpRecursive( writer, this, "", monitor );
  }

  /**
   * Dumps the contents of this item and all its children using recursion
   */
  private static void dumpRecursive( final Writer writer, final IRepositoryItem item, final String indent,
      final IProgressMonitor monitor ) throws RepositoryException, InterruptedException
  {
    if( monitor.isCanceled() )
      throw new InterruptedException();

    if( item == null )
      return;

    monitor.subTask( item.getIdentifier() );

    try
    {
      // let's look if the item can be adapted to properties. In the positive,
      // we dump the properties too.
      final Properties props = (Properties)item.getAdapter( Properties.class );
      if( props != null )
        writer.write( indent + item.toString() + " Properties: " + PropertiesHelper.format( props, ';' ) );
      else
        writer.write( indent + item.toString() );

      writer.write( "\n" );
    }
    catch( final IOException e )
    {
      throw new RepositoryException( e );
    }

    final String recIndent = indent + "\t";

    final IRepositoryItem[] items = item.getChildren();
    if( items == null )
      return;

    for( int i = 0; i < items.length; i++ )
      dumpRecursive( writer, items[i], recIndent, monitor );

    monitor.worked( 1 );
  }

  /**
   * @see org.kalypso.repository.IRepository#getProperty(java.lang.String, java.lang.String)
   */
  public String getProperty( final String name, final String defaultValue )
  {
    return m_properties.getProperty( name, defaultValue );
  }

  /**
   * @see org.kalypso.repository.IRepository#getProperty(java.lang.String)
   */
  public String getProperty( final String name )
  {
    return m_properties.getProperty( name );
  }

  /**
   * @see org.kalypso.repository.IRepository#getProperties()
   */
  public Properties getProperties()
  {
    return m_properties;
  }

  /**
   * @see org.kalypso.repository.IRepository#setProperties(java.util.Properties)
   */
  public void setProperties( final Properties props )
  {
    m_properties.clear();
    m_properties.putAll( props );
  }

  /**
   * @see org.kalypso.repository.IRepository#setProperty(java.lang.String, java.lang.String)
   */
  public void setProperty( final String name, final String value )
  {
    m_properties.setProperty( name, value );
  }
}