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
package org.kalypso.ogc.sensor.template;

import java.awt.Color;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.PoolableObjectWaiter;

/**
 * @author schlienger
 */
public abstract class ObsView implements IObsViewEventProvider
{
  /** Additional hints for new items */
  public static class ItemData
  {
    public final Color color;

    public final boolean editable;

    public ItemData( final boolean editable, final Color color )
    {
      this.color = color;
      this.editable = editable;
    }
  }

  private final List m_items = new ArrayList();

  private final List m_listeners = new ArrayList();

  public void dispose()
  {
    removeAllItems();
  }

  public boolean waitUntilLoaded( final int sleepTime, final int maxLoops )
  {
    for( int i = 0; i < maxLoops; i++ )
    {
      if( !isLoading() )
        return true;

      try
      {
        Thread.sleep( sleepTime );
      }
      catch( InterruptedException e )
      {
        e.printStackTrace();
      }
    }

    return false;
  }

  private boolean isLoading()
  {
    synchronized( m_items )
    {
      for( final Iterator iter = m_items.iterator(); iter.hasNext(); )
      {
        if( ( (ObsViewItem)iter.next() ).isLoading() )
          return true;
      }

      return false;
    }
  }

  public void removeAllItems()
  {
    synchronized( m_items )
    {
      for( final Iterator iter = m_items.iterator(); iter.hasNext(); )
      {
        final ObsViewItem element = (ObsViewItem)iter.next();
        element.dispose();
      }
      m_items.clear();
    }

    fireObsViewChanged( new ObsViewEvent( this, ObsViewEvent.TYPE_REMOVE_ALL ) );
  }

  public final void addItem( final ObsViewItem item )
  {
    synchronized( m_items )
    {
      m_items.add( item );
    }

    fireObsViewChanged( new ObsViewEvent( item, ObsViewEvent.TYPE_ADD ) );
  }

  public final void removeItem( final ObsViewItem item )
  {
    synchronized( m_items )
    {
      m_items.remove( item );
    }

    fireObsViewChanged( new ObsViewEvent( item, ObsViewEvent.TYPE_REMOVE ) );
  }

  public ObsViewItem[] getItems()
  {
    synchronized( m_items )
    {
      return (ObsViewItem[])m_items.toArray( new ObsViewItem[m_items.size()] );
    }
  }

  public final void refresh( final ObsViewItem item )
  {
    fireObsViewChanged( new ObsViewEvent( item, ObsViewEvent.TYPE_REFRESH ) );
  }

  public final void refresh()
  {
    fireObsViewChanged( new ObsViewEvent( this, ObsViewEvent.TYPE_REFRESH ) );
  }

  public void addObsViewEventListener( IObsViewEventListener l )
  {
    synchronized( m_listeners )
    {
      m_listeners.add( l );
    }
  }

  protected void fireObsViewChanged( ObsViewEvent evt )
  {
    synchronized( m_listeners )
    {
      for( final Iterator it = m_listeners.iterator(); it.hasNext(); )
      {
        final IObsViewEventListener l = (IObsViewEventListener)it.next();
        l.onObsViewChanged( evt );
      }
    }
  }

  public void removeObsViewListener( IObsViewEventListener l )
  {
    synchronized( m_listeners )
    {
      m_listeners.remove( l );
    }
  }

  /** Laods an observation into this view and sets default values */
  public void loadObservation( final URL context, final String href,
      final boolean ignoreExceptions, final String ignoreType, final String tokenizedName,
      final ItemData data )
  {
    final PoolableObjectType k = new PoolableObjectType( "zml", href, context, ignoreExceptions );

    new PoolableObjectWaiter( k, new Object[]
    {
        this,
        data,
        ignoreType,
        tokenizedName }, false )
    {
      protected void objectLoaded( final IPoolableObjectType key, final Object newValue )
      {
        final IObsProvider provider = new PooledObsProvider( key, null );
        try
        {
          ( (ObsView)m_data[0] ).addObservation( provider, (String)m_data[3], (String)m_data[2],
              (ObsView.ItemData)m_data[1] );
        }
        finally
        {
          provider.dispose();
        }
      }
    };
  }

  /**
   * Implementors of this class must ensure, that there is a 1:1 relationship
   * between provider and added item. So the given provider should be disposed
   * and for each added item a copy of the given provider should be made.
   */
  protected abstract void addObservation( final IObsProvider provider, final String tokenizedName,
      final String ignoreType, final ItemData data );

  public static Map mapItems( final ObsViewItem[] items )
  {
    // obs -> columns
    final Map obsmap = new HashMap();
    for( int i = 0; i < items.length; i++ )
    {
      final IObservation observation = items[i].getObservation();
      if( !obsmap.containsKey( observation ) )
        obsmap.put( observation, new ArrayList() );

      ( (List)obsmap.get( observation ) ).add( items[i] );
    }

    return obsmap;
  }
}