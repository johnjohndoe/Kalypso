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
package org.kalypso.kalypso1d2d.pjt.map;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResultCollection;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Thomas Jung
 *
 */
public final class HydrographProcessResultOperation implements ICoreRunnableWithProgress
{
  private static DatatypeFactory DATATYPE_FACTORY;

  static
  {
    try
    {
      DATATYPE_FACTORY = DatatypeFactory.newInstance();
    }
    catch( final DatatypeConfigurationException e )
    {
      e.printStackTrace();
    }
  }

  private final IHydrographCollection m_hydrographs;

  private final Map<IPath, Date> m_resultMap;

  private final IFolder m_scenarioFolder;

  public HydrographProcessResultOperation( final IHydrographCollection hydrographs, final Map<IPath, Date> resultMap, final IFolder scenarioFolder )
  {
    m_hydrographs = hydrographs;
    m_resultMap = resultMap;
    m_scenarioFolder = scenarioFolder;

  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      final Set<Entry<IPath, Date>> entrySet = m_resultMap.entrySet();

      final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographProcessResultOperation.0" ), entrySet.size() ); //$NON-NLS-1$

      final Map<GM_Point, IObservation<TupleResult>> obsMap = new HashMap<>();

      for( final IHydrograph hydrograph : m_hydrographs.getHydrographs() )
      {

        final GM_Object location = hydrograph.getLocation();
        if( location instanceof GM_Point )
        {
          final GM_Point point = (GM_Point) location;
          final Feature wrappedFeature = hydrograph;
          final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( wrappedFeature );

          /* clear existing results */
          final TupleResult result = obs.getResult();
          result.clear();

          obsMap.put( point, obs );
        }
      }

      int count = 0;
      final int resultSize = entrySet.size();

      for( final Entry<IPath, Date> entry : entrySet )
      {
        final Date date = entry.getValue();
        count++;
        progress.subTask( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographProcessResultOperation.1", count, resultSize ) ); //$NON-NLS-1$

        /* get the observation */

        /* get the date for which this result is valid */
        final GregorianCalendar calendar = new GregorianCalendar();

        // TODO: check for right time zone
        calendar.setTime( date );

        /* get the node result gml */
        final IPath docPath = entry.getKey();
        if( docPath == null )
          return null;

        final URL scenarioURL = ResourceUtilities.createURL( m_scenarioFolder );
        final URL resultURL = UrlUtilities.resolveWithZip( scenarioURL, docPath.toPortableString() );

        if( !UrlUtilities.checkIsAccessible( resultURL ) )
          continue;

        final GMLWorkspace w = GmlSerializer.createGMLWorkspace( resultURL, null );

        final Feature feature = w.getRootFeature();
        final INodeResultCollection nodeResultCollection = (INodeResultCollection) feature.getAdapter( INodeResultCollection.class );

        final FeatureList nodeList = nodeResultCollection.getNodeResults().getFeatureList();

        int hyd = 0;

        /* get the hydrograph locations and observations */
        for( final IHydrograph hydrograph : m_hydrographs.getHydrographs() )
        {
          hyd++;

          final GM_Object location = hydrograph.getLocation();
          if( location instanceof GM_Point )
          {
            final GM_Point point = (GM_Point) location;

            // TODO: check for right time zone
            progress.subTask( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographProcessResultOperation.2", count, resultSize, date.toString(), hyd, m_hydrographs.getHydrographs().size() ) ); //$NON-NLS-1$
            addResult( obsMap, calendar, nodeList, point );
          }
        }
        ProgressUtilities.worked( progress, 1 );
      }

      saveToHydrographFeature( obsMap );

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      monitor.done();
    }
  }

  private void addResult( final Map<GM_Point, IObservation<TupleResult>> obsMap, final GregorianCalendar calendar, final FeatureList nodeList, final GM_Point point )
  {
    final IObservation<TupleResult> o = obsMap.get( point );
    final TupleResult tuples = o.getResult();

    final IComponent[] components = tuples.getComponents();

    final IComponent dateComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    final IComponent waterlevelComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
    final IComponent depthComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_DEPTH );
    final IComponent velocityComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_VELOCITY );
    final IComponent velocityDirComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_VELOCITY_DIRECTION );
    final IComponent dischargeComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );

    final IComponent waveHsigComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_HSIG );
    final IComponent wavePerComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_PER );
    final IComponent waveDirComp = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_DIR );

    tuples.setSortComponents( new IComponent[] { dateComp } );

    /* find the nearest node */
    final Feature nearestFeature = GeometryUtilities.findNearestFeature( point, 0.1, nodeList, GMLNodeResult.QNAME_PROP_LOCATION );
    if( nearestFeature != null )
    {
      /* get the data of that node */
      final INodeResult nodeResult = (INodeResult) nearestFeature.getAdapter( INodeResult.class );
      final double depth = nodeResult.getDepth();
      final Double waterlevel = nodeResult.getWaterlevel();
      final Double discharge = nodeResult.getDischarge();
      final double absoluteVelocity = nodeResult.getAbsoluteVelocity();
      final List<Double> velocityList = nodeResult.getVelocity();
      double velocityDir = 0;
      if( !(velocityList == null || velocityList.size() != 2) )
        velocityDir = GeometryUtilities.directionFromVector( velocityList.get( 0 ), velocityList.get( 1 ) );
      if( Double.isNaN( velocityDir ) )
      {
        velocityDir = 0;
      }

      final Double lDoubleHsig = nodeResult.getWaveHsig();
      final Double lDoublePer = nodeResult.getWavePeriod();
      final Double lDoubleDir = nodeResult.getWaveDirection();

      /* add the data to the observation */
      final IRecord newRecord = tuples.createRecord();

      newRecord.setValue( dateComp, DATATYPE_FACTORY.newXMLGregorianCalendar( calendar ) );
      newRecord.setValue( waterlevelComp, new BigDecimal( waterlevel ).setScale( 10, BigDecimal.ROUND_HALF_UP ) );
      newRecord.setValue( depthComp, new BigDecimal( depth ).setScale( 10, BigDecimal.ROUND_HALF_UP ) );
      newRecord.setValue( velocityComp, new BigDecimal( absoluteVelocity ).setScale( 10, BigDecimal.ROUND_HALF_UP ) );

      if( velocityDirComp != null )
        newRecord.setValue( velocityDirComp, new BigDecimal( velocityDir ).setScale( 10, BigDecimal.ROUND_HALF_UP ) );

      if( discharge != null )
        newRecord.setValue( dischargeComp, new BigDecimal( discharge ).setScale( 10, BigDecimal.ROUND_HALF_UP ) );
      else
        newRecord.setValue( dischargeComp, new BigDecimal( 0.0 ).setScale( 10, BigDecimal.ROUND_HALF_UP ) );

      // Wave parameter

      if( lDoubleHsig != null && !Double.isNaN( lDoubleHsig ) )
        newRecord.setValue( waveHsigComp, new BigDecimal( lDoubleHsig ).setScale( 10, BigDecimal.ROUND_HALF_UP ) );
      else
        System.out.println( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographProcessResultOperation.12" ) ); //$NON-NLS-1$

      if( lDoublePer != null && !Double.isNaN( lDoublePer ) )
        newRecord.setValue( wavePerComp, new BigDecimal( lDoublePer ).setScale( 10, BigDecimal.ROUND_HALF_UP ) );
      else
        System.out.println( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographProcessResultOperation.12" ) ); //$NON-NLS-1$

      if( lDoubleDir != null && !Double.isNaN( lDoubleDir ) )
        newRecord.setValue( waveDirComp, new BigDecimal( lDoubleDir ).setScale( 10, BigDecimal.ROUND_HALF_UP ) );
      else
        System.out.println( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.HydrographProcessResultOperation.12" ) ); //$NON-NLS-1$

      tuples.add( newRecord );
    }
  }

  private void saveToHydrographFeature( final Map<GM_Point, IObservation<TupleResult>> obsMap )
  {
    /* save the obs in the feature */
    for( final IHydrograph hydrograph : m_hydrographs.getHydrographs() )
    {
      final GM_Object location = hydrograph.getLocation();
      final Feature feature = hydrograph;
      if( location instanceof GM_Point )
      {
        final GM_Point point = (GM_Point) location;

        final IObservation<TupleResult> o = obsMap.get( point );
        ObservationFeatureFactory.toFeature( o, feature );
      }
    }
  }
}
