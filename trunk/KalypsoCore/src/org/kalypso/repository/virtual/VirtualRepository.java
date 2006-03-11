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
package org.kalypso.repository.virtual;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.zml.repository.virtual.ItemType;
import org.kalypso.zml.repository.virtual.LevelType;
import org.kalypso.zml.repository.virtual.ObjectFactory;
import org.kalypso.zml.repository.virtual.VirtualRepositoryType;
import org.xml.sax.InputSource;

/**
 * VirtualRepository
 * 
 * @author schlienger
 */
public class VirtualRepository extends AbstractRepository
{
  private static final JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  /** child items */
  private IRepositoryItem[] m_children;

  /** stores the mapping between ids and items */
  private final Map<String, IRepositoryItem> m_idMap = new Hashtable<String, IRepositoryItem>();

  private final String m_identifier;

  private final String m_location;

  /**
   * Constructor
   * 
   * @param factory
   * @param identifier
   *          uniquely identifies this repository among the list of repositories used in a given context. The identifier
   *          is also used as name.
   * @param location
   *          location of the specification file (xml)
   * @param readOnly
   * @throws RepositoryException
   */
  public VirtualRepository( String factory, String identifier, String location, boolean readOnly ) throws RepositoryException
  {
    super( identifier, factory, location, readOnly );
    m_identifier = identifier;
    m_location = location;
    try
    {
      buildRepository();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new RepositoryException( e );
    }
  }

  private final void buildRepository( ) throws JAXBException, FileNotFoundException
  {
    final InputSource specSource = new InputSource( new FileInputStream( new File( m_location ) ) );
    final VirtualRepositoryType vrt = (VirtualRepositoryType) JC.createUnmarshaller().unmarshal( specSource );
    m_children = buildStructure( null, vrt.getLevel() ).toArray( new IRepositoryItem[0] );
  }

  private final List<IRepositoryItem> buildStructure( final VirtualRepositoryItem parent, final List<LevelType> levels )
  {
    final List<IRepositoryItem> rItems = new ArrayList<IRepositoryItem>( levels.size() );
    for( final LevelType level : levels )
    {
      final VirtualRepositoryItem rItem = new VirtualRepositoryItem( this, level.getName(), level.getId(), parent );
      final List<IRepositoryItem> children = new ArrayList<IRepositoryItem>();
      children.addAll( buildStructure( rItem, level.getLevel() ) );
      children.addAll( buildItems( rItem, level.getItem() ) );
      rItem.setChildren( children );
      m_idMap.put( rItem.getIdentifier(), rItem );
      rItems.add( rItem );
    }
    return rItems;
  }

  private final List<IRepositoryItem> buildItems( final VirtualRepositoryItem parent, final List<ItemType> items )
  {
    final List<IRepositoryItem> rItems = new ArrayList<IRepositoryItem>( items.size() );
    for( final ItemType item : items )
    {
      final VirtualRepositoryItem rItem = new VirtualRepositoryItem( this, item.getName(), item.getId(), parent );
      rItem.setFilterType( item.getFilter().getValue() );
      m_idMap.put( rItem.getIdentifier(), rItem );
      rItems.add( rItem );
    }
    return rItems;
  }

  /**
   * @see org.kalypso.repository.IRepository#dispose()
   */
  @Override
  public void dispose( )
  {
    super.dispose();
    m_idMap.clear();
    m_children = null;
  }

  /**
   * @see org.kalypso.repository.IRepository#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return m_location;
  }

  /**
   * @see org.kalypso.repository.IRepository#findItem(java.lang.String)
   */
  public IRepositoryItem findItem( final String id )
  {
    return m_idMap.get( id );
  }

  /**
   * @see org.kalypso.repository.IRepository#reload()
   */
  public void reload( ) throws RepositoryException
  {
    m_children = null;
    m_idMap.clear();
    try
    {
      buildRepository();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new RepositoryException( e );
    }
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier( )
  {
    return m_identifier + "://";
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( )
  {
    return m_children != null && m_children.length > 0;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( )
  {
    return m_children;
  }
}