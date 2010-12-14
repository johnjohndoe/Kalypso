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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

class WVQInfo
  {
    private final String m_formattedObservation;

// private final double m_vMax;
//
// private final double m_vMin;

    private final int m_numberOfEntries;

    protected WVQInfo( final String formattedObservation, @SuppressWarnings("unused") final double vMax, @SuppressWarnings("unused") final double vMin, final int numberOfEntries )
    {
      m_formattedObservation = formattedObservation;
// m_vMax = vMax;
// m_vMin = vMin;
      m_numberOfEntries = numberOfEntries;
    }

    protected String getFormattedObservation( )
    {
      return m_formattedObservation;
    }

// // TODO: to discuss: usage of min/max from the timeserie, or those explicitly defined by user in user interface
// protected double getMaxVolume( )
// {
// return m_vMax;
// }
//
// protected double getMinVolume( )
// {
// return m_vMin;
// }

    protected int getNumberOfEntries( )
    {
      return m_numberOfEntries;
    }
  }