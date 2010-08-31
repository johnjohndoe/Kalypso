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
package org.kalypso.wspwin.core.prf.datablock;

/**
 * @author Gernot Belger
 */
public enum DataBlockDescription
{
  STATION,
  SOHLHOEHE,
  BOESCHUNG_LI("BOESCHUNG-LI"), //$NON-NLS-1$
  BOESCHUNG_RE("BOESCHUNG-RE"), //$NON-NLS-1$
  OK_WEHRS("OK-WEHRS"), //$NON-NLS-1$
  DECKENOBERK,
  DECKENUNTERK,
  // BRIDGE_WIDTH,
  // ROHR_DN,
  TEXT(12),
  WASSERSPIEGEL("Wasserspiegel NN+m"); //$NON-NLS-1$

  private final int m_specialDataBlockId;

  private final String m_name;

  private DataBlockDescription( )
  {
    this( null, 0 );
  }

  private DataBlockDescription( final String name )
  {
    this( name, 0 );
  }

  private DataBlockDescription( final int sepcialDataBlockId )
  {
    this( null, sepcialDataBlockId );
  }

  private DataBlockDescription( final String name, final int sepcialDataBlockId )
  {
    m_name = name;
    m_specialDataBlockId = sepcialDataBlockId;
  }

  /**
   * The name (i.e. type) of the data block. Used as first line of the db in .prf.
   */
  public String getName( )
  {
    if( m_name == null )
      return name();

    return m_name;
  }

  public int getSpecialDataBlockId( )
  {
    return m_specialDataBlockId;
  }

}
