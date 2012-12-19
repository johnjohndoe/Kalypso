/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 *
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 *
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 *
 * and
 *
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 * Contact:
 *
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 *
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.model.hydrology.internal.preprocessing.hydrotope;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.model.sort.JSISpatialIndex;
import org.kalypsodeegree_impl.model.sort.SpatialIndexExt;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;

/**
 * @author Dejan Antanascovic
 */
public class HydroHash
{
  private final Comparator< ? super Catchment> m_catchmentSorter;

  private final Map<Catchment, CatchmentInfo> m_catchmentInfos;

  private final ParameterHash m_parameterHash;

  private final Catchment[] m_catchments;

  private SpatialIndexExt m_catchmentIndex;

  private final boolean m_doAttributeDissolve;

  public HydroHash( final ParameterHash landuseHash, final Catchment[] catchments, final IDManager idManager )
  {
    this( landuseHash, catchments, idManager, true );
  }

  /**
   * @param doAttributeDissolve
   *          If hydrotopes with same attributes should be combined into a single one. Set to <code>false</code> for
   *          debug purpose only.
   */
  public HydroHash( final ParameterHash landuseHash, final Catchment[] catchments, final IDManager idManager, final boolean doAttributeDissolve )
  {
    m_parameterHash = landuseHash;
    m_catchments = catchments;
    m_doAttributeDissolve = doAttributeDissolve;

    m_catchmentSorter = new CatchmentByAsciiIdSorter( idManager );
    m_catchmentInfos = new TreeMap<>( m_catchmentSorter );
  }

  public void initHydrotopes( final HydrotopeCollection hydrotopeCollection ) throws GM_Exception, NAPreprocessorException
  {
    final IFeatureBindingCollection<IHydrotope> hydrotopes = hydrotopeCollection.getHydrotopes();

    for( final IHydrotope hydrotope : hydrotopes )
    {
      final Catchment catchment = findCatchment( hydrotope );

      if( catchment == null )
      {
        // TODO: we cannot throw an exception, because sometimes not all catchments are calculated, in this case,
        // finding no catchment is ok

        // final String message = String.format( Messages.getString("HydroHash_0"), hydrotope.getName() ); //$NON-NLS-1$
        // throw new NAPreprocessorException( message );
      }
      else
        addHydrotope( catchment, hydrotope );
    }
  }

  private Catchment findCatchment( final IHydrotope hydrotope ) throws GM_Exception
  {
    final IXLinkedFeature catchmentLink = hydrotope.getCatchmentLink();
    if( catchmentLink != null )
      return (Catchment) catchmentLink.getFeature();

    final SpatialIndexExt catchmentIndex = getCatchmentIndex();

    final Geometry hydrotopGeometry = JTSAdapter.export( hydrotope.getGeometry() );
    if( hydrotopGeometry == null )
      return null;

    final Envelope boundingBox = hydrotopGeometry.getEnvelopeInternal();

    final List<Catchment> query = catchmentIndex.query( boundingBox );
    for( final Catchment catchment : query )
    {
      final Geometry catchmentGeometry = JTSAdapter.export( catchment.getGeometry() );
      if( catchmentGeometry.contains( hydrotopGeometry.getInteriorPoint() ) )
        return catchment;
    }

    return null;
  }

  /* Lazy, because often this is never needed */
  private SpatialIndexExt getCatchmentIndex( )
  {
    if( m_catchmentIndex == null )
      m_catchmentIndex = indexCatchments( m_catchments );

    return m_catchmentIndex;
  }

  private SpatialIndexExt indexCatchments( final Catchment[] catchments )
  {
    final SpatialIndexExt index = new JSISpatialIndex();
    for( final Catchment catchment : catchments )
    {
      final GM_Envelope envelope = catchment.getEnvelope();
      final Envelope boundingBox = JTSAdapter.export( envelope );

      index.insert( boundingBox, catchment );
    }

    return index;
  }

  private void addHydrotope( final Catchment catchment, final IHydrotope hydrotop ) throws NAPreprocessorException
  {
    final CatchmentInfo info = getHydrotopInfo( catchment );
    info.add( hydrotop );
  }

  public CatchmentInfo getHydrotopInfo( final Catchment catchment )
  {
    final CatchmentInfo info = m_catchmentInfos.get( catchment );
    if( info != null )
      return info;

    final CatchmentInfo newInfo = new CatchmentInfo( catchment, m_parameterHash, m_doAttributeDissolve );
    m_catchmentInfos.put( catchment, newInfo );
    return newInfo;
  }

  public String getHydroFeatureId( final Catchment catchment, final int pos )
  {
    final CatchmentInfo info = getHydrotopInfo( catchment );

    return info.getHydroFeatureId( pos );
  }

  public Collection<Catchment> getCatchments( )
  {
    return Collections.unmodifiableCollection( m_catchmentInfos.keySet() );
  }

  public Collection<HydrotopeInfo> getHydrotops( final Catchment catchment )
  {
    final CatchmentInfo hydrotopInfo = getHydrotopInfo( catchment );
    return hydrotopInfo.getHydrotops();
  }

  public ParameterHash getParameterHash( )
  {
    return m_parameterHash;
  }
}