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
import org.eclipse.core.resources.IFolder;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.core.util.pool.KeyInfo;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.model.hydrology.binding.cm.ICatchment;
import org.kalypso.model.hydrology.binding.model.channels.IStorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.provider.IObsProvider;
import org.kalypso.ogc.sensor.provider.PooledObsProvider;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT_TYPE;
import org.kalypso.zml.core.base.IZmlSourceElement;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dirk Kuch
 */
public class HydrologyResultReference implements IHydrologyResultReference, IZmlSourceElement
{
  private final RRM_RESULT m_type;

  private final IFile m_file;

  private String m_identifier;

  private PoolableObjectType m_key;

  private PooledObsProvider m_provider;

  private final Feature m_parent;

  private final RrmSimulation m_simulation;

  public HydrologyResultReference( final RrmSimulation simulation, final IFolder calcCaseFolder, final Feature feature, final RRM_RESULT result )
  {
    m_simulation = simulation;
    m_parent = feature;
    final RRM_RESULT_TYPE type = result.getType();
    switch( type )
    {
    // FIXME i18n - english project template

      case eCatchment:
        m_file = calcCaseFolder.getFile( String.format( "/Teilgebiet/%s/%s", feature.getName(), result.getFileName() ) ); //$NON-NLS-1$
        break;

      case eNode:
        m_file = calcCaseFolder.getFile( String.format( "/Knoten/%s/%s", feature.getName(), result.getFileName() ) ); //$NON-NLS-1$
        break;

      case eStorage:
        m_file = calcCaseFolder.getFile( String.format( "/SpeicherStrang/%s/%s", feature.getName(), result.getFileName() ) ); //$NON-NLS-1$
        break;

      default:
        throw new UnsupportedOperationException();
    }

    m_type = result;
  }

  public HydrologyResultReference( final RrmSimulation simulation, final URL context, final Feature parent, final TimeseriesLinkType link, final RRM_RESULT type ) throws MalformedURLException
  {
    m_simulation = simulation;
    m_parent = parent;
    if( link != null )
    {
      final URL url = UrlResolverSingleton.resolveUrl( context, link.getHref() );

      m_file = ResourceUtilities.findFileFromURL( url );
    }
    else
      m_file = null;

    m_type = type;
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

    m_key = new PoolableObjectType( "zml", m_file.getName(), getContext(), false );

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
    final String label = getType().getLabel();
    final String simulation = m_simulation.getName();
    final String parent = m_parent.getName();

    return String.format( "%s, %s: %s\r\n%s", simulation, getFeatureTypeName(), parent, label );
  }

  private String getFeatureTypeName( )
  {
    if( m_parent instanceof Node )
      return "Knoten";
    else if( m_parent instanceof IStorageChannel )
      return "Speicherstrang";
    else if( m_parent instanceof ICatchment )
      return "Teilgebiet";

    return "";
  }

  @Override
  public String getIdentifier( )
  {
    if( StringUtils.isNotBlank( m_identifier ) )
      return m_identifier;

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
        return "HYDROLOGY_INPUT_EVAPORATION";
      case inputInflow:
        return ITimeseriesConstants.TYPE_DISCHARGE;
      case inputTemperature:
        return "HYDROLOGY_INPUT_T";
      case inputRainfall:
        return "HYDROLOGY_INPUT_RAINFALL";
      case inputSeaEvaporation:
        return "HYDROLOGY_INPUT_EVAPORATION";
      case nodeGesamtknotenAbfluss:
        return ITimeseriesConstants.TYPE_DISCHARGE;
      case storageFuellvolumen:
        return ITimeseriesConstants.TYPE_VOLUME;
      case storageSpeicherUeberlauf:
        return ITimeseriesConstants.TYPE_DISCHARGE;

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
}
