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
package org.kalypso.model.hydrology.internal.preprocessing.hydrotope;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.kalypso.model.hydrology.binding.model.Catchment;

/**
 * Infos about all hydrotops belonging to one catchment.
 * 
 * @author Gernot Belger
 */
public class CatchmentInfo
{
  private final List<HydrotopeInfo> m_hydrotopes = new ArrayList<>();

  private final Catchment m_catchment;

  private Sealing m_totalSealing = null;

  private final String m_label;

  private final String m_lzsID;

  public CatchmentInfo( final Catchment catchment, final String label, final String lzsID )
  {
    this( catchment, label, lzsID, new HydrotopeInfo[0], null );
  }

  public CatchmentInfo( final Catchment catchment, final String label, final String lzsID, final HydrotopeInfo[] hydrotopes, final Sealing totalSealing )
  {
    m_catchment = catchment;
    m_label = label;
    m_lzsID = lzsID;
    m_totalSealing = totalSealing;

    m_hydrotopes.addAll( Arrays.asList( hydrotopes ) );
  }

  public void add( final HydrotopeInfo hydrotopeInfo )
  {
    m_hydrotopes.add( hydrotopeInfo );
  }

  public String getHydroFeatureId( final int pos )
  {
    return m_hydrotopes.get( pos ).getFeatureId();
  }

  public Sealing getTotalSealing( )
  {
    if( m_totalSealing == null )
      m_totalSealing = HydrotopeInfo.calculateSealing( m_hydrotopes );

    return m_totalSealing;
  }

  public List<HydrotopeInfo> getHydrotopes( )
  {
    return Collections.unmodifiableList( m_hydrotopes );
  }

  public Catchment getCatchment( )
  {
    return m_catchment;
  }

  public String getLabel( )
  {
    return m_label;
  }

  public String getLzsId( )
  {
    return m_lzsID;
  }
}