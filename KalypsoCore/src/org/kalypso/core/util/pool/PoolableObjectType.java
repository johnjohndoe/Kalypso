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
package org.kalypso.core.util.pool;

import java.net.URL;

/**
 * @author vdoemming
 */
public class PoolableObjectType implements IPoolableObjectType
{
  private final String m_type;

  private final URL m_context;

  private final String m_source;

  private final boolean m_ignoreExceptions;

  /**
   * Constructor. Defaults ignoreExceptions to false.
   *
   * @param type
   *          type of object to load
   * @param source
   *          location of the object
   * @param context
   *          context into which object is loaded
   */
  public PoolableObjectType( final String type, final String source, final URL context )
  {
    this( type, source, context, false );
  }

  /**
   * Constructor.
   *
   * @param type
   *          type of object to load
   * @param source
   *          location of the object
   * @param context
   *          context into which object is loaded
   * @param ignoreExceptions
   *          when true, exceptions occuring during load process are ignored, object won't get pooled
   */
  public PoolableObjectType( final String type, final String source, final URL context, final boolean ignoreExceptions )
  {
    m_type = type;
    m_source = source;
    m_context = context;
    m_ignoreExceptions = ignoreExceptions;
  }

  /**
   * @see org.kalypso.util.pool.IPoolableObjectType#getType()
   */
  public String getType()
  {
    return m_type;
  }

  /**
   * @see org.kalypso.util.pool.IPoolableObjectType#getLocation()
   */
  public String getLocation()
  {
    return m_source;
  }

  /**
   * @see org.kalypso.util.pool.IPoolableObjectType#getContext()
   */
  public URL getContext()
  {
    return m_context;
  }

  /**
   * @return Returns the ignoreExceptions.
   */
  public boolean isIgnoreExceptions()
  {
    return m_ignoreExceptions;
  }

  @Override
  public boolean equals( final Object obj )
  {
    if( !( obj instanceof IPoolableObjectType ) )
      return false;

    final IPoolableObjectType other = (IPoolableObjectType)obj;
    if( !getType().equals( other.getType() ) )
      return false;
    if( !getLocation().equals( other.getLocation() ) )
      return false;

    if( getContext() == null && other.getContext() == null )
      return true;

    if( ( getContext() != null && !getContext().equals( other.getContext() ) ) )
      return false;

    return true;
  }

  @Override
  public int hashCode()
  {
    return toString().hashCode();
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return "Location=" + getLocation() + " Type=" + getType() + " Context= " + getContext(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }
}