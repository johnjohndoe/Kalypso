/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
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
package org.kalypso.model.wspm.tuhh.core.profile.energyloss;

import java.math.BigDecimal;

/**
 * @author Holger Albert
 */
public class Energyloss
{
  private final String m_type;

  private final String m_description;

  private final BigDecimal m_value;

  public Energyloss( final String type, final String description, final BigDecimal value )
  {
    m_type = type;
    m_description = description;
    m_value = value;
  }

  public String getType( )
  {
    return m_type;
  }

  public Energyloss setType( final String type )
  {
    return new Energyloss( type, m_description, m_value );
  }

  public String getDescription( )
  {
    return m_description;
  }

  public Energyloss setDescription( final String description )
  {
    return new Energyloss( m_type, description, m_value );
  }

  public BigDecimal getValue( )
  {
    return m_value;
  }

  public Energyloss setValue( final BigDecimal value )
  {
    return new Energyloss( m_type, m_description, value );
  }
}