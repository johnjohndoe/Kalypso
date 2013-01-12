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
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.binding.parameter.Soiltype;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class ParameterHash
{
  private final Map<Feature, String> m_landuseNameMap = new HashMap<>();

  private final Map<String, Feature> m_landuseClassMap = new HashMap<>();

  private final Map<String, Soiltype> m_soilTypeNameHash = new HashMap<>();

  private final IStatusCollector m_log = new StatusCollector( ModelNA.PLUGIN_ID );

  private final Parameter m_parameter;

  public ParameterHash( final Parameter parameter ) throws NAPreprocessorException
  {
    m_parameter = parameter;

    if( parameter != null )
    {
      initLanduseHash( parameter );
      initSoilTypeHash( parameter );
    }
  }

  public IStatus getStatus( )
  {
    return m_log.asMultiStatusOrOK( Messages.getString("ParameterHash.1") ); //$NON-NLS-1$
  }

  /** Build soiltype hash for faster lookup later */
  private void initSoilTypeHash( final Parameter parameter )
  {
    final IFeatureBindingCollection<Soiltype> soiltypes = parameter.getSoiltypes();
    for( final Soiltype soiltype : soiltypes )
    {
      final String name = soiltype.getName();
      if( m_soilTypeNameHash.containsKey( name ) )
        m_log.add( IStatus.WARNING, String.format( Messages.getString("ParameterHash.2"), name ) ); //$NON-NLS-1$
      else
        m_soilTypeNameHash.put( name, soiltype );
    }
  }

  private void initLanduseHash( final Parameter parameter ) throws NAPreprocessorException
  {
    int shortNameCounter = 0;

    final List< ? > landuseList = (List< ? >)parameter.getProperty( NaModelConstants.PARA_PROP_LANDUSE_MEMBER );
    for( final Object landuseElement : landuseList )
    {
      final Feature landuseClass = (Feature)landuseElement;
      final String landuseName = landuseClass.getName();

      if( StringUtils.isBlank( landuseName ) )
        throw new NAPreprocessorException( Messages.getString( "ParameterHash.0" ) ); //$NON-NLS-1$

      if( m_landuseClassMap.containsKey( landuseName ) )
        m_log.add( IStatus.WARNING, Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.0", landuseName ) ); //$NON-NLS-1$
      else
      {
        final String shortName = shortenLanduseName( landuseName, shortNameCounter );
        if( !shortName.equals( landuseName ) )
          shortNameCounter++;

        m_landuseClassMap.put( landuseName, landuseClass );
        m_landuseNameMap.put( landuseClass, shortName );
      }
    }
  }

  private String shortenLanduseName( final String landuseName, final int counter )
  {
    if( landuseName.length() < 10 )
      return landuseName;

    return String.format( "class%d", landuseName, counter ); //$NON-NLS-1$
  }

  public Double getSealingRate( final Feature landuseClass )
  {
    final Feature linkedSealingFE = FeatureHelper.resolveLink( landuseClass, NaModelConstants.PARA_LANDUSE_PROP_SEALING_LINK );

    return (Double)linkedSealingFE.getProperty( NaModelConstants.PARA_LANDUSE_PROP_SEALING );
  }

  /**
   * Returns landuse name that is compatible with the calculation core; mappings are stored so once given ID is used
   * again if requested; for null gml names, the new ID is given without storing it
   */
  public final String getLanduseFeatureShortedName( final Feature landuseClass )
  {
    return m_landuseNameMap.get( landuseClass );
  }

  public Soiltype getSoilType( final String soilTypeID )
  {
    // FIXME: we should only support one of those now...
    final Soiltype soiltype = m_parameter.findSoiltypeByID( soilTypeID );
    if( soiltype != null )
      return soiltype;

    // Feature could not be found by id. For backwards compatibility: search by name as well:
    return m_soilTypeNameHash.get( soilTypeID );
  }

  public Feature getLanduseClass( final String landuseName )
  {
    return m_landuseClassMap.get( landuseName );
  }
}