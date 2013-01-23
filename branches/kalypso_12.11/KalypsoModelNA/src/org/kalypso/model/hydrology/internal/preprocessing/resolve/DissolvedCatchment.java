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
package org.kalypso.model.hydrology.internal.preprocessing.resolve;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.kalypso.model.hydrology.binding.OverlayElement;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.CatchmentInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydrotopeInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.Sealing;

/**
 * Dissolves the hydrotopes of one catchment.
 * 
 * @author Gernot Belger
 */
class DissolvedCatchment
{
  /* hyrotopes get dissolved via this hash */
  private final Map<String, HydrotopeInfo> m_hydrotopeHash = new LinkedHashMap<>();

  private final List<HydrotopeInfo> m_hydrotopes = new ArrayList<>();

  private final Catchment m_catchment;

  private Sealing m_totalSealing;

  private final OverlayElement m_overlay;

  public DissolvedCatchment( final Catchment catchment, final OverlayElement overlay )
  {
    Assert.isNotNull( catchment );

    m_overlay = overlay;
    m_catchment = catchment;
  }

  public Catchment getCatchment( )
  {
    return m_catchment;
  }

  public double getHydrotopeArea( )
  {
    return getTotalSealing().getArea();
  }

  private Sealing getTotalSealing( )
  {
    if( m_totalSealing == null )
      m_totalSealing = HydrotopeInfo.calculateSealing( m_hydrotopes );

    return m_totalSealing;
  }

  public String getLzsID( )
  {
    final String catchmentID = m_catchment.getId();

    if( !hasDrainageFunction() )
      return catchmentID;

    final String overlayID = m_overlay.getId();
    return String.format( "%s#%s", catchmentID, overlayID );
  }

  public String getLabel( )
  {
    final String catchmentLabel = m_catchment.getName();

    if( !hasDrainageFunction() )
      return catchmentLabel;

    final String overlayLabel = m_overlay.getName();
    return String.format( "%s - Overlay %s", catchmentLabel, overlayLabel );
  }

  private boolean hasDrainageFunction( )
  {
    if( m_overlay == null )
      return false;

    return m_overlay.hasDrainageFunction();
  }

  public boolean hasDrwbm( )
  {
    if( m_overlay == null )
      return false;

    return m_overlay.hasDrainageFunction();
  }

  public void add( final HydrotopeInfo hydrotopeInfo )
  {
    final String attributeHashKey = buildHydrotopeHash( hydrotopeInfo );

    if( m_hydrotopeHash.containsKey( attributeHashKey ) )
    {
      final HydrotopeInfo existingInfo = m_hydrotopeHash.get( attributeHashKey );
      existingInfo.addArea( hydrotopeInfo );
    }
    else
    {
      m_hydrotopeHash.put( attributeHashKey, hydrotopeInfo );
      m_hydrotopes.add( hydrotopeInfo );
    }
  }

  private String buildHydrotopeHash( final HydrotopeInfo hydrotopeInfo )
  {
    final StringBuilder buffer = new StringBuilder();

    buffer.append( hydrotopeInfo.getGwFactor() );
    buffer.append( '#' );
    buffer.append( hydrotopeInfo.getLanduseShortName() );
    buffer.append( '#' );
    buffer.append( hydrotopeInfo.getMaxPercolationRate() );
    buffer.append( '#' );
    buffer.append( hydrotopeInfo.getTotalSealingRate() );
    buffer.append( '#' );
    buffer.append( hydrotopeInfo.getSoilType().getId() );

    return buffer.toString();
  }

  /**
   * Associates all hydrotopes of this dissolver to its own catchment and return a {@link CatchmentInfo} for this constellation.
   */
  public CatchmentInfo createInfo( )
  {
    return createInfo( m_catchment );
  }

  /**
   * Associates all hydrotopes of this dissolver to the given catchment and return a {@link CatchmentInfo} for this constellation.
   */
  public CatchmentInfo createInfo( final Catchment catchment )
  {
    final String label = getLabel();
    final String lzsID = getLzsID();
    final Sealing totalSealing = getTotalSealing();

    final HydrotopeInfo[] hydrotopes = m_hydrotopeHash.values().toArray( new HydrotopeInfo[m_hydrotopeHash.size()] );

    return new CatchmentInfo( catchment, label, lzsID, hydrotopes, totalSealing );
  }
}