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

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Vector;


/**
 * Beinhaltet eine Liste von Repositories und verwaltet Listeners.
 * 
 * @author schlienger
 */
public class DefaultRepositoryContainer implements IRepositoryContainer
{
  private final List m_reps = new Vector();

  private final List m_listeners = new Vector();

  public DefaultRepositoryContainer()
  {
    this( new IRepository[0] );
  }

  public DefaultRepositoryContainer( IRepository[] repositories )
  {
    m_reps.addAll( Arrays.asList( repositories ) );
  }

  public void addRepository( final IRepository rep )
  {
     m_reps.add( rep );

    fireRepositoryChanged();
  }

  private void fireRepositoryChanged()
  {
    for( Iterator iter = m_listeners.iterator(); iter.hasNext(); )
    {
      IRepositoryContainerListener element = (IRepositoryContainerListener)iter.next();
      
      element.onRepositoryContainerChanged();
    }
  }

  public void removeRepository( IRepository rep )
  {
    m_reps.remove( rep );
    
    fireRepositoryChanged();
  }

  public int getRepositoriesCount()
  {
    return m_reps.size();
  }

  /**
   * @see org.kalypso.repository.IRepositoryContainer#addRepositoryContainerListener(org.kalypso.repository.IRepositoryContainerListener)
   */
  public void addRepositoryContainerListener( IRepositoryContainerListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.repository.IRepositoryContainer#removeRepositoryContainerListener(org.kalypso.repository.IRepositoryContainerListener)
   */
  public void removeRepositoryContainerListener( IRepositoryContainerListener l )
  {
    m_listeners.remove( l );
  }

  /**
   * @see org.kalypso.repository.IRepositoryContainer#getRepositories()
   */
  public List getRepositories()
  {
    return m_reps;
  }

  /**
   * @see org.kalypso.repository.IRepositoryContainer#findItem(java.lang.String)
   */
  public IRepositoryItem findItem( final String id ) throws NoSuchElementException
  {
    for( final Iterator it = m_reps.iterator(); it.hasNext(); )
    {
      final IRepository rep = (IRepository)it.next();

      if( rep.getIdentifier().equals( id ) )
        return rep;
      
      try
      {
        return rep.findItem( id );
      }
      catch( RepositoryException e )
      {
        // ignored, try with next repository
      }
    }
    
    throw new NoSuchElementException( "Item not found: " + id );
  }
}