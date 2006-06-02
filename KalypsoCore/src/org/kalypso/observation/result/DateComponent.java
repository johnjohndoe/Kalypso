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
package org.kalypso.observation.result;

import java.util.TimeZone;

import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * @author schlienger
 */
public class DateComponent extends Component
{
  private final String m_timezoneName;
  
  public DateComponent( final int pos, final String name, final String description, final QName valueTypeName )
  {
    this( pos, name, description, valueTypeName, TimeZone.getDefault().getID() );
  }
  
  public DateComponent( final int pos, final String name, final String description, final QName valueTypeName, final String timezoneName )
  {
    this( pos, name, description, valueTypeName, null, timezoneName );
  }

  public DateComponent( final int pos, final String name, final String description, final QName valueTypeName, final Object defaultValue, final String timezoneName )
  {
    super( pos, name, description, valueTypeName, defaultValue );
    
    m_timezoneName = timezoneName;
  }
  
  public String getTimezoneName( )
  {
    return m_timezoneName;
  }
  
  /**
   * @see org.kalypso.observation.result.Component#fillEqualsBilder(org.kalypso.observation.result.IComponent, org.apache.commons.lang.builder.EqualsBuilder)
   */
  @Override
  protected void fillEqualsBilder( final IComponent comp, final EqualsBuilder builder )
  {
    super.fillEqualsBilder( comp, builder );
    
    // this cast is safe (see super class implementation)
    final DateComponent dc = (DateComponent) comp;
    
    builder.append( dc.getTimezoneName(), m_timezoneName );
  }
  
  /**
   * @see org.kalypso.observation.result.Component#fillHashCodeBuilder(org.apache.commons.lang.builder.HashCodeBuilder)
   */
  @Override
  protected void fillHashCodeBuilder( HashCodeBuilder builder )
  {
    super.fillHashCodeBuilder( builder );
    
    builder.append( m_timezoneName );
  }
}
