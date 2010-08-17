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

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class LanduseHash
{
  private final static String PLC_LANDUSE_NAME_FORMAT = "PLC_%05d"; //$NON-NLS-1$

  private int m_plcLanduseCounter = 1;

  private final Map<String, String> m_landuseLongNamesMap = new HashMap<String, String>();

  private final Map<String, Double> m_landuseSealingRateMap = new HashMap<String, Double>();

  private final Logger m_logger;

  public LanduseHash( final Parameter parameter, final Logger logger )
  {
    m_logger = logger;

    if( parameter != null )
      initHash( parameter );
  }

  private void initHash( final Parameter parameter )
  {
    final List< ? > landuseList = (List< ? >) parameter.getProperty( NaModelConstants.PARA_PROP_LANDUSE_MEMBER );
    for( final Object landuseElement : landuseList )
    {
      final Feature landuseFE = (Feature) landuseElement;

      final Feature linkedSealingFE = FeatureHelper.resolveLink( landuseFE, NaModelConstants.PARA_LANDUSE_PROP_SEALING_LINK );

      final Double sealingRate = (Double) linkedSealingFE.getProperty( NaModelConstants.PARA_LANDUSE_PROP_SEALING );
      final String landuseName = getLanduseShortName( landuseFE );
      if( m_landuseSealingRateMap.containsKey( landuseName ) )
        m_logger.log( Level.WARNING, Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.0", landuseName ) ); //$NON-NLS-1$
      else
        m_landuseSealingRateMap.put( landuseName, sealingRate );
    }
  }

  public String getLanduseShortName( final Feature landuseFE )
  {
    return getLanduseFeatureShortedName( landuseFE.getName() );
  }

  public Double getSealingRate( final String landuseName )
  {
    final String landuseFeatureShortedName = getLanduseFeatureShortedName( landuseName );
    return m_landuseSealingRateMap.get( landuseFeatureShortedName );
  }

  /**
   * Returns landuse name that is compatible with the calculation core; mappings are stored so once given ID is used
   * again if requested; for null gml names, the new ID is given without storing it
   */
  // TODO: move this to LanduseHash or to NutzungsManager
  public final String getLanduseFeatureShortedName( final String landuseName )
  {
    if( landuseName != null && landuseName.length() < 10 )
      return landuseName;

    final String shortName = String.format( Locale.US, PLC_LANDUSE_NAME_FORMAT, m_plcLanduseCounter++ );

    System.out.println( "Created " + shortName + " for " + landuseName ); //$NON-NLS-1$ //$NON-NLS-2$

    if( landuseName == null )
      return shortName;

    final String string = m_landuseLongNamesMap.get( landuseName );
    if( string == null )
    {
      m_landuseLongNamesMap.put( landuseName, shortName );
      return shortName;
    }
    return string;
  }

}
