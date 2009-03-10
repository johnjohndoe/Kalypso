/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.ui.map.facedata;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.kalypso.kalypsosimulationmodel.core.Assert;

/**
 * @author Madanagopal
 * @author Patrice Congo
 * 
 */
public class KeyBasedDataModel
{
  private static final int NO_POS = -1;

  final Set<KeyBasedDataModelChangeListener> m_listeners = new HashSet<KeyBasedDataModelChangeListener>();

  final String[] m_keys;

  final Object[] m_data;

  final IDataModelCheck[] m_dataChecks;

  private IDataModelCheck m_modelCheck;

  public KeyBasedDataModel( final String[] keys, final IDataModelCheck modelCheck )
  {
    Assert.throwIAEOnNullParam( keys, "keys" ); //$NON-NLS-1$

    final String[] tempKeys = new String[keys.length];
    try
    {
      for( int i = keys.length - 1; i >= 0; i-- )
      {
        final String currentKey = keys[i];
        tempKeys[i] = Assert.throwIAEOnNullOrEmpty( currentKey );
      }
    }
    catch( final IllegalArgumentException e )
    {
      e.printStackTrace();
      throw e;
    }

    m_keys = tempKeys;
    m_data = new Object[keys.length];
    m_dataChecks = new IDataModelCheck[keys.length];
    m_modelCheck = modelCheck;
  }

  public Object getData( String key )
  {
    key = Assert.throwIAEOnNullOrEmpty( key );
    final int pos = findPosition( key );
    if( pos == NO_POS )
    {
      return null;
    }
    else
    {
      return m_data[pos];
    }
  }

  @SuppressWarnings("unchecked")
  public <T> T getData( final Class<T> dataType, final String key )
  {
    final Object obj = getData( key );
    if( obj == null )
    {
      return null;
    }
    else if( dataType.isInstance( obj ) )
    {
      return (T) obj;
    }
    else
    {
      final String message = String.format( "Illegal type in model:" + " \n\texpected=%s" + "\n\tcurrent=%s" + "\n\tkey=%s", dataType, obj.getClass(), key ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      throw new RuntimeException( message );
    }

  }

  public IDataModelCheck getDataCheck( String key )
  {

    key = Assert.throwIAEOnNullOrEmpty( key );
    final int pos = findPosition( key );
    if( pos == NO_POS )
    {
      return null;
    }
    else
    {
      return m_dataChecks[pos];
    }
  }

  public void setData( final String key, final Object newEntry )
  {
    setData( key, newEntry, true );
  }

  public void setData( String key, final Object newEntry, final boolean doNotify )
  {
    key = Assert.throwIAEOnNullOrEmpty( key );
    final int pos = findPosition( key );
    if( pos == NO_POS )
      throw new IllegalArgumentException( "Key not available:" + "\n\tCurrent key=" + key + "\n\tavailablekeys=" + Arrays.asList( m_keys ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    // Don't notify or check if value does not change
    if( m_data[pos] == newEntry )
      return;

    m_data[pos] = newEntry;
    if( m_dataChecks[pos] != null )
      m_dataChecks[pos].update( key, newEntry, this );

    if( m_modelCheck != null )
      m_modelCheck.update( key, newEntry, this );

    if( doNotify )
      fireDataChanged( key, newEntry );
  }

  public void setDataCheck( String key, final IDataModelCheck newEntry )
  {
    key = Assert.throwIAEOnNullOrEmpty( key );
    final int pos = findPosition( key );
    if( pos == NO_POS )
    {
      throw new IllegalArgumentException( "Key not available:" + "\n\tCurrent key=" + key + "\n\tavailablekeys=" + Arrays.asList( m_keys ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
    else
    {
      m_dataChecks[pos] = newEntry;
    }
  }

  public IDataModelCheck getModelCheck( )
  {
    return m_modelCheck;
  }

  public void setModelCheck( final IDataModelCheck modelCheck )
  {
    m_modelCheck = modelCheck;
  }

  // TODO: ?!? Use hashmap for such stuff! This is potentially slow
  private final int findPosition( final String key )
  {
    for( int i = 0; i < m_keys.length; i++ )
    {
      if( key.equals( m_keys[i] ) )
      {
        return i;
      }
    }

    return NO_POS;
  }

  public void addKeyBasedDataChangeListener( final KeyBasedDataModelChangeListener newListener )
  {
    Assert.throwIAEOnNullParam( newListener, "newListener" ); //$NON-NLS-1$
    m_listeners.add( newListener );
  }

  public void removeKeyBasedDataChangeListener( final KeyBasedDataModelChangeListener newListener )
  {
    Assert.throwIAEOnNullParam( newListener, "newListener" ); //$NON-NLS-1$
    m_listeners.remove( newListener );
  }

  public void fireDataChanged( final String key, final Object newValue )
  {
    for( final KeyBasedDataModelChangeListener l : m_listeners )
    {
      l.dataChanged( key, newValue );
    }
  }

  public void removeAllListeners( )
  {
    m_listeners.clear();
  }
}
