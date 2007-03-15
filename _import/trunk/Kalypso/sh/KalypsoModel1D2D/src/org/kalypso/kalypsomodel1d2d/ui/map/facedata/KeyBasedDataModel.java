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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.Assert;

/**
 * @author Madanagopal
 * @author Patrice Congo
 *
 */
public class KeyBasedDataModel
{
  private static final int NO_POS = -1;

  final List<KeyBasedDataModelChangeListener> listeners = 
                    new ArrayList<KeyBasedDataModelChangeListener>();
  final String[] keys;
  
  final Object[] data;
  
  public KeyBasedDataModel(String[] keys)
  {
    Assert.throwIAEOnNullParam( keys, "keys" );
    String[] tempKeys = new String[keys.length];
    String currentKey;
    try
    {
      for(int i=keys.length-1;i>=0;i-- )
      {
        currentKey = keys[i];
        tempKeys[i] = Assert.throwIAEOnNullOrEmpty( currentKey );
      }
    }
    catch( IllegalArgumentException e )
    {
      e.printStackTrace();
      throw e;
    }
    
    this.keys = tempKeys;
    this.data = new Object[keys.length];
  }
  
  public Object getData(String key)
  {
    
    key=Assert.throwIAEOnNullOrEmpty(key);
    int pos=findPosition( key );
    if(pos==NO_POS)
    {
      return null;
    }
    else
    {
      return data[pos];
    }
  }
  
  public void setData(String key, Object newEntry)
  {
    key=Assert.throwIAEOnNullOrEmpty(key);
    int pos=findPosition( key );
    if(pos==NO_POS)
    {
      throw new IllegalArgumentException(
          "Key not available:"+
          "\n\tCurrent key="+key+
          "\n\tavailablekeys="+Arrays.asList( keys ));
    }
    else
    {
      data[pos]=newEntry;
      fireDataChanged( key, newEntry );
    }
  }
  
  private final int findPosition(String key)
  {
    for(int i=0;i<keys.length;i++)
    {
      if(key.equals( keys[i] ))
      {
        return i;
      }      
    }
    
    return NO_POS;
  }
  
  public void addKeyBasedDataChangeListener(
                    KeyBasedDataModelChangeListener newListener)
  {
    Assert.throwIAEOnNullParam( newListener, "newListener" );
    if(!listeners.contains( newListener ))
    {
      listeners.add( newListener );
    }
  }
  
  public void fireDataChanged(String key, Object newValue)
  {
    
    for(KeyBasedDataModelChangeListener l:listeners)
    {
      l.dataChanged( key, newValue );
    }
  }
}
