/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.metadoc.configuration;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.apache.commons.configuration.Configuration;

/**
 * Default implementation of the IPublishingConfiguration interface
 * 
 * @author schlienger
 */
public class PublishingConfiguration implements IPublishingConfiguration
{
  private final Configuration m_conf;
  private final List m_listeners;

  public PublishingConfiguration( final Configuration conf )
  {
    m_conf = conf;
    m_listeners = new ArrayList( 10 );
  }

  private void fireConfigurationChanged( final String key )
  {
    final IConfigurationListener[] listeners = (IConfigurationListener[])m_listeners.toArray( new IConfigurationListener[m_listeners.size()] );
    for( int i = 0; i < listeners.length; i++ )
    {
      try
      {
        listeners[i].configurationChanged( this, key );
      }
      catch( final Throwable t )
      {
        t.printStackTrace();
      }
    }
  }

  /**
   * @see org.kalypso.metadoc.configuration.IPublishingConfiguration#addListener(org.kalypso.metadoc.configuration.IConfigurationListener)
   */
  public void addListener( final IConfigurationListener listener )
  {
    m_listeners.add( listener );
  }

  /**
   * @see org.kalypso.metadoc.configuration.IPublishingConfiguration#removeListener(org.kalypso.metadoc.configuration.IConfigurationListener)
   */
  public void removeListener( final IConfigurationListener listener )
  {
    m_listeners.remove( listener );
  }

  public void addProperty( final String key, Object arg1 )
  {
    m_conf.addProperty( key, arg1 );
    
    fireConfigurationChanged( key );
  }

  public void clear()
  {
    m_conf.clear();
    
    fireConfigurationChanged( null );
  }

  public void clearProperty( final String key )
  {
    m_conf.clearProperty( key );
    
    fireConfigurationChanged( key );
  }

  public boolean containsKey( String arg0 )
  {
    return m_conf.containsKey( arg0 );
  }

  public boolean equals( Object obj )
  {
    return m_conf.equals( obj );
  }

  public BigDecimal getBigDecimal( String arg0 )
  {
    return m_conf.getBigDecimal( arg0 );
  }

  public BigDecimal getBigDecimal( String arg0, BigDecimal arg1 )
  {
    return m_conf.getBigDecimal( arg0, arg1 );
  }

  public BigInteger getBigInteger( String arg0 )
  {
    return m_conf.getBigInteger( arg0 );
  }

  public BigInteger getBigInteger( String arg0, BigInteger arg1 )
  {
    return m_conf.getBigInteger( arg0, arg1 );
  }

  public boolean getBoolean( String arg0 )
  {
    return m_conf.getBoolean( arg0 );
  }

  public boolean getBoolean( String arg0, boolean arg1 )
  {
    return m_conf.getBoolean( arg0, arg1 );
  }

  public Boolean getBoolean( String arg0, Boolean arg1 ) throws NoClassDefFoundError
  {
    return m_conf.getBoolean( arg0, arg1 );
  }

  public byte getByte( String arg0 )
  {
    return m_conf.getByte( arg0 );
  }

  public byte getByte( String arg0, byte arg1 )
  {
    return m_conf.getByte( arg0, arg1 );
  }

  public Byte getByte( String arg0, Byte arg1 )
  {
    return m_conf.getByte( arg0, arg1 );
  }

  public double getDouble( String arg0 )
  {
    return m_conf.getDouble( arg0 );
  }

  public double getDouble( String arg0, double arg1 )
  {
    return m_conf.getDouble( arg0, arg1 );
  }

  public Double getDouble( String arg0, Double arg1 )
  {
    return m_conf.getDouble( arg0, arg1 );
  }

  public float getFloat( String arg0 )
  {
    return m_conf.getFloat( arg0 );
  }

  public float getFloat( String arg0, float arg1 )
  {
    return m_conf.getFloat( arg0, arg1 );
  }

  public Float getFloat( String arg0, Float arg1 )
  {
    return m_conf.getFloat( arg0, arg1 );
  }

  public int getInt( String arg0 )
  {
    return m_conf.getInt( arg0 );
  }

  public int getInt( String arg0, int arg1 )
  {
    return m_conf.getInt( arg0, arg1 );
  }

  public Integer getInteger( String arg0, Integer arg1 )
  {
    return m_conf.getInteger( arg0, arg1 );
  }

  public Iterator getKeys()
  {
    return m_conf.getKeys();
  }

  public Iterator getKeys( String arg0 )
  {
    return m_conf.getKeys( arg0 );
  }

  public List getList( String arg0 )
  {
    return m_conf.getList( arg0 );
  }

  public List getList( String arg0, List arg1 )
  {
    return m_conf.getList( arg0, arg1 );
  }

  public long getLong( String arg0 )
  {
    return m_conf.getLong( arg0 );
  }

  public Long getLong( String arg0, Long arg1 )
  {
    return m_conf.getLong( arg0, arg1 );
  }

  public long getLong( String arg0, long arg1 )
  {
    return m_conf.getLong( arg0, arg1 );
  }

  public Properties getProperties( String arg0 )
  {
    return m_conf.getProperties( arg0 );
  }

  public Object getProperty( String arg0 )
  {
    return m_conf.getProperty( arg0 );
  }

  public short getShort( String arg0 )
  {
    return m_conf.getShort( arg0 );
  }

  public Short getShort( String arg0, Short arg1 )
  {
    return m_conf.getShort( arg0, arg1 );
  }

  public short getShort( String arg0, short arg1 )
  {
    return m_conf.getShort( arg0, arg1 );
  }

  public String getString( String arg0 )
  {
    return m_conf.getString( arg0 );
  }

  public String getString( String arg0, String arg1 )
  {
    return m_conf.getString( arg0, arg1 );
  }

  public String[] getStringArray( String arg0 )
  {
    return m_conf.getStringArray( arg0 );
  }

  public int hashCode()
  {
    return m_conf.hashCode();
  }

  public boolean isEmpty()
  {
    return m_conf.isEmpty();
  }

  public void setProperty( final String key, Object arg1 )
  {
    m_conf.setProperty( key, arg1 );

    fireConfigurationChanged( key );
  }

  public Configuration subset( final String prefix )
  {
    return m_conf.subset( prefix );
  }

  public String toString()
  {
    return m_conf.toString();
  }
}
