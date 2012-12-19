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
package org.kalypso.ui.rrm.internal.results.view.base;

import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.core.util.pool.KeyInfo;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.provider.IObsProvider;
import org.kalypso.ogc.sensor.provider.PooledObsProvider;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT_TYPE;
import org.kalypso.zml.core.base.IZmlSourceElement;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dirk Kuch
 */
public class HydrologyResultReference implements IHydrologyResultReference
{
  private final RRM_RESULT m_type;

  private final IFile m_file;

  private String m_identifier;

  private PoolableObjectType m_key;

  private PooledObsProvider m_provider;

  private final Feature m_parent;

  private final RrmSimulation m_simulation;

  private final RrmCalculationResult m_calculation;

  private final boolean m_calculationInput;

  public HydrologyResultReference( final RrmSimulation simulation, final RrmCalculationResult calculation, final Feature feature, final RRM_RESULT result )
  {
    m_simulation = simulation;
    m_calculation = calculation;

    m_parent = feature;
    final RRM_RESULT_TYPE type = result.getType();
    switch( type )
    {
      case eCatchment:
        m_file = getCalculation().getFolder().getFile( String.format( "/%s/%s/%s", RrmCalculationResult.FOLDER_CATCHMENT, feature.getName(), result.getFileName() ) ); //$NON-NLS-1$
        break;

      case eNode:
        m_file = getCalculation().getFolder().getFile( String.format( "/%s/%s/%s", RrmCalculationResult.FOLDER_NODE, feature.getName(), result.getFileName() ) ); //$NON-NLS-1$
        break;

      case eStorage:
        m_file = getCalculation().getFolder().getFile( String.format( "/%s/%s/%s", RrmCalculationResult.FOLDER_STROAGE_CHANNEL, feature.getName(), result.getFileName() ) ); //$NON-NLS-1$
        break;

      default:
        throw new UnsupportedOperationException();
    }

    m_type = result;
    m_calculationInput = false;
  }

  public HydrologyResultReference( final RrmSimulation simulation, final RrmCalculationResult calculation, final Feature parent, final ZmlLink lonk, final RRM_RESULT type )
  {
    m_simulation = simulation;
    m_calculation = calculation;
    m_parent = parent;

    m_file = lonk.getFile();

    m_type = type;
    m_calculationInput = true;
  }

  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter.isAssignableFrom( IHydrologyResultReference.class ) )
      return this;
    else if( adapter.isAssignableFrom( IZmlSourceElement.class ) )
      return this;

    return null;
  }

  @Override
  public URL getUrl( ) throws MalformedURLException
  {
    if( m_file == null )
      return null;

    return m_file.getLocationURI().toURL();
  }

  @Override
  public RRM_RESULT getType( )
  {
    return m_type;
  }

  @Override
  public boolean isValid( )
  {
    if( m_file == null )
      return false;

    try
    {
      return UrlUtilities.checkIsAccessible( getUrl() );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }

    return false;
  }

  @Override
  public void dispose( )
  {
    // nothing to do...b
  }

  @Override
  public IObsProvider getObsProvider( )
  {
    if( !isValid() )
      return null;

    if( Objects.isNotNull( m_provider ) )
      return m_provider;

    m_provider = new PooledObsProvider( getPoolKey() );

    return m_provider;

  }

  @Override
  public IPoolableObjectType getPoolKey( )
  {
    if( !isValid() )
      return null;

    if( Objects.isNotNull( m_key ) )
      return m_key;

    m_key = new PoolableObjectType( "zml", m_file.getName(), getContext(), false ); //$NON-NLS-1$

    return m_key;

  }

  private URL getContext( )
  {
    try
    {
      return m_file.getLocationURI().toURL();
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }

    return null;
  }

  @Override
  public boolean isDirty( )
  {
    final IObservation observation = getObsProvider().getObservation();
    if( Objects.isNull( observation ) )
      return false;

    final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
    final KeyInfo info = pool.getInfo( observation );

    return info.isDirty();
  }

  @Override
  public String getLabel( )
  {
    final String simulation = m_simulation.getName();
    final String parent = m_parent.getName();
    final String label = getType().getLabel();

    if( getCalculation() != null )
      return String.format( "%s (%s): %s\r\n%s", simulation, getCalculation().getName(), parent, label ); //$NON-NLS-1$

    return String.format( "%s: %s\r\n%s", simulation, parent, label ); //$NON-NLS-1$
  }

  @Override
  public String getIdentifier( )
  {
    if( StringUtils.isNotBlank( m_identifier ) )
      return m_identifier;

    // FIXME: move information into the enum type
    final RRM_RESULT type = getType();
    switch( type )
    {
      case catchmentBasisQ:
        return ITimeseriesConstants.TYPE_DISCHARGE;
      case catchmentEvapotranspiration:
        return ITimeseriesConstants.TYPE_EVAPORATION;
      case catchmentGesamtTeilgebietsQ:
        return ITimeseriesConstants.TYPE_DISCHARGE;
      case catchmentGrundwasserQ:
        return ITimeseriesConstants.TYPE_DISCHARGE;
      case catchmentGrundwasserstand:
        return ITimeseriesConstants.TYPE_WATERLEVEL;
      case catchmentInterflow:
        return ITimeseriesConstants.TYPE_DISCHARGE;
      case catchmentNiederschlag:
        return ITimeseriesConstants.TYPE_RAINFALL;
      case catchmentOberflaechenQNatuerlich:
        return ITimeseriesConstants.TYPE_DISCHARGE;
      case catchmentOberflaechenQVersiegelt:
        return ITimeseriesConstants.TYPE_DISCHARGE;
      case catchmentSchneehoehe:
        return ITimeseriesConstants.TYPE_WATERLEVEL;
      case catchmentTemperature:
        return ITimeseriesConstants.TYPE_TEMPERATURE;
      case inputEvaporation:
        return "INPUT_E_LAND"; //$NON-NLS-1$
      case inputInflow:
        return "HYDROLOGY_INPUT_INFLOW"; //$NON-NLS-1$
      case inputTemperature:
        return "HYDROLOGY_INPUT_T"; //$NON-NLS-1$
      case inputRainfall:
        return "HYDROLOGY_INPUT_RAINFALL"; //$NON-NLS-1$
      case inputSeaEvaporation:
        return "HYDROLOGY_INPUT_EVAPORATION"; //$NON-NLS-1$
      case nodeGesamtknotenAbfluss:
        return ITimeseriesConstants.TYPE_DISCHARGE;
      case storageFuellvolumen:
        return ITimeseriesConstants.TYPE_VOLUME;
      case storageSpeicherUeberlauf:
        return ITimeseriesConstants.TYPE_DISCHARGE;
      case storageEvaporation:
        return ITimeseriesConstants.TYPE_EVAPORATION_WATER_BASED;
      case inputGauge:
        return "HYDROLOGY_INPUT_GAUGE"; //$NON-NLS-1$
    }

    return m_identifier;
  }

  @Override
  public void setIdentifier( final String identifier )
  {
    m_identifier = identifier;
  }

  @Override
  public int getIndex( )
  {
    return 0;
  }

  @Override
  public boolean isCalcualtionInput( )
  {
    return m_calculationInput;
  }

  public RrmCalculationResult getCalculation( )
  {
    return m_calculation;
  }
}
