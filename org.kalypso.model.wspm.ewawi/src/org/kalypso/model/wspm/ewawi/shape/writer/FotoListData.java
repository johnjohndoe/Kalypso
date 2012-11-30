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
package org.kalypso.model.wspm.ewawi.shape.writer;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @author Holger Albert
 */
public class FotoListData
{
  private final String m_filename;

  private final BigDecimal m_rechtswert;

  private final BigDecimal m_hochwert;

  private final BigDecimal m_hoehe;

  private final Date m_datum;

  public FotoListData( final String filename, final BigDecimal rechtswert, final BigDecimal hochwert, final BigDecimal hoehe, final Date datum )
  {
    m_filename = filename;
    m_rechtswert = rechtswert;
    m_hochwert = hochwert;
    m_hoehe = hoehe;
    m_datum = datum;
  }

  public String getFilename( )
  {
    return m_filename;
  }

  public BigDecimal getRechtswert( )
  {
    return m_rechtswert;
  }

  public BigDecimal getHochwert( )
  {
    return m_hochwert;
  }

  public BigDecimal getHoehe( )
  {
    return m_hoehe;
  }

  public Date getDatum( )
  {
    return m_datum;
  }
}