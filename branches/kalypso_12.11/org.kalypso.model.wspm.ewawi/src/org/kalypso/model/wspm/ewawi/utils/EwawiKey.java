/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.ewawi.utils;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * @author Holger Albert
 */
public class EwawiKey
{
  private final String m_pe;

  private final String m_alias;

  private final String m_modelId;

  private final String m_freeText;

  public EwawiKey( final String pe, final String alias, final String modelId, final String freeText )
  {
    m_pe = pe;
    m_alias = alias;
    m_modelId = modelId;
    m_freeText = freeText;
  }

  public String getPe( )
  {
    return m_pe;
  }

  public String getAlias( )
  {
    return m_alias;
  }

  public String getModelId( )
  {
    return m_modelId;
  }

  public String getFreeText( )
  {
    return m_freeText;
  }

  @Override
  public boolean equals( final Object obj )
  {
    if( !(obj instanceof EwawiKey) )
      return false;

    final EwawiKey other = (EwawiKey)obj;

    final EqualsBuilder builder = new EqualsBuilder();
    builder.append( m_pe, other.getPe() );
    builder.append( m_alias, other.getAlias() );
    builder.append( m_modelId, other.getModelId() );
    builder.append( m_freeText, other.getFreeText() );

    return builder.isEquals();
  }

  @Override
  public int hashCode( )
  {
    final HashCodeBuilder builder = new HashCodeBuilder();
    builder.append( m_pe );
    builder.append( m_alias );
    builder.append( m_modelId );
    builder.append( m_freeText );

    return builder.hashCode();
  }
}