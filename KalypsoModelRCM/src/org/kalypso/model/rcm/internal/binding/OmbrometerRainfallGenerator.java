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
package org.kalypso.model.rcm.internal.binding;

import java.net.MalformedURLException;
import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.tokenreplace.IStringResolver;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.rcm.binding.AbstractRainfallGenerator;
import org.kalypso.model.rcm.binding.IOmbrometer;
import org.kalypso.model.rcm.binding.IOmbrometerCollection;
import org.kalypso.model.rcm.internal.KalypsoModelRcmActivator;
import org.kalypso.model.rcm.internal.UrlCatalogRcm;
import org.kalypso.model.rcm.internal.i18n.Messages;
import org.kalypso.model.rcm.util.BufferBoundaryCalculator;
import org.kalypso.model.rcm.util.IBoundaryCalculator;
import org.kalypso.model.rcm.util.OmbrometerUtils;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.model.rcm.util.ThiessenAreaOperation;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypso.utils.log.LogUtilities;
import org.kalypso.zml.core.filter.binding.IZmlFilter;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Implementation that generates rainfall from ombrometer stations with Thiessen.
 *
 * @author Gernot Belger
 */
public class OmbrometerRainfallGenerator extends AbstractRainfallGenerator
{
  static final QName FEATURE_OMBROMETER_RAINFALL_GENERATOR = new QName( UrlCatalogRcm.NS_RCM, "OmbrometerRainfallGenerator" ); //$NON-NLS-1$

  static final QName MEMBER_ombrometerCollection = new QName( UrlCatalogRcm.NS_RCM, "ombrometerCollection" ); //$NON-NLS-1$

  static final QName PROPERTY_ombrometerFeaturePath = new QName( UrlCatalogRcm.NS_RCM, "ombrometerFeaturePath" ); //$NON-NLS-1$

  static final QName PROPERTY_timeseriesLinkPath = new QName( UrlCatalogRcm.NS_RCM, "timeseriesLinkPath" ); //$NON-NLS-1$

  static final QName PROPERTY_areaPath = new QName( UrlCatalogRcm.NS_RCM, "areaPath" ); //$NON-NLS-1$

  static final QName PROPERTY_catchmentAreaPath = new QName( UrlCatalogRcm.NS_RCM, "catchmentAreaPath" ); //$NON-NLS-1$

  public OmbrometerRainfallGenerator( final Object parent, final IRelationType parentRelation, final IFeatureType featureType, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, featureType, id, propValues );
  }

  @Override
  public IObservation[] createRainfall( final Feature[] catchmentFeatures, final IStringResolver variables, final ILog log, IProgressMonitor monitor ) throws CoreException
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      /* Monitor. */
      monitor.beginTask( Messages.getString("OmbrometerRainfallGenerator_6"), 1000 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString("OmbrometerRainfallGenerator_7") ); //$NON-NLS-1$

      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.INFO, KalypsoModelRcmActivator.PLUGIN_ID, Messages.getString("OmbrometerRainfallGenerator_8"), null ) ); //$NON-NLS-1$

      /* Get the needed properties. */
      final IXLinkedFeature ombrometerCollectionLink = getProperty( MEMBER_ombrometerCollection, IXLinkedFeature.class );
      final IOmbrometerCollection ombrometerCollection = (IOmbrometerCollection) ombrometerCollectionLink.getFeature();
      final String collectionPath = getProperty( PROPERTY_ombrometerFeaturePath, String.class );
      final String areaPath = getProperty( PROPERTY_areaPath, String.class );
      final String linkPath = getProperty( PROPERTY_timeseriesLinkPath, String.class );
      final String catchmentAreaPath = getProperty( PROPERTY_catchmentAreaPath, String.class );

      /* Create the paths. */
      final GMLXPath collectionXPath = new GMLXPath( collectionPath, getWorkspace().getNamespaceContext() );
      final GMLXPath areaXPath = new GMLXPath( areaPath, getWorkspace().getNamespaceContext() );
      final GMLXPath linkXPath = new GMLXPath( linkPath, getWorkspace().getNamespaceContext() );
      final GMLXPath catchmentAreaXPath = new GMLXPath( catchmentAreaPath, getWorkspace().getNamespaceContext() );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString("OmbrometerRainfallGenerator_9") ); //$NON-NLS-1$

      /* Get the ombrometers. */
      final FeatureList ombrometerList = (FeatureList) GMLXPathUtilities.query( collectionXPath, ombrometerCollection );

      // Find the ombrometer-areas
      final Feature[] ombrometerFeatures = FeatureHelper.toArray( ombrometerList );
      if( ombrometerFeatures.length < ombrometerList.size() )
      {
        // TODO: log problem
      }

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString("OmbrometerRainfallGenerator_10") ); //$NON-NLS-1$

      /* Read the observations. */
      final IZmlFilter[] filters = getFilters().toArray( new IZmlFilter[] {} );
      final DateRange range = getPeriod( variables );
      final IObservation[] ombrometerObservations = RainfallGeneratorUtilities.readObservations( ombrometerFeatures, linkXPath, filters, range );

      /* Apply thiessen, if not yet done. */
      if( !ombrometerCollection.hasBeenProcessed() )
        analyseOmbrometerAndProcessThiessen( ombrometerFeatures, ombrometerObservations, new NullProgressMonitor() );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString("OmbrometerRainfallGenerator_11") ); //$NON-NLS-1$

      final GM_Polygon[] ombrometerAreas = FeatureHelper.getProperties( ombrometerFeatures, areaXPath, new GM_Polygon[ombrometerFeatures.length] );

      final IGeoTransformer transformer = GeoTransformerFactory.getGeoTransformer( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      final GM_Polygon[] transformedAreas = GeometryUtilities.transform( ombrometerAreas, transformer );

      /* Convert to JTS geometries. */
      final Polygon[] ombrometerPolygons = JTSAdapter.export( transformedAreas, Polygon.class );
      monitor.worked( 100 );

      /* Monitor. */
      monitor.subTask( Messages.getString("OmbrometerRainfallGenerator_12") ); //$NON-NLS-1$

      /* Get all catchment areas. */
      final GM_MultiSurface[] areas = RainfallGeneratorUtilities.findCatchmentAreas( catchmentFeatures, catchmentAreaXPath );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString("OmbrometerRainfallGenerator_13") ); //$NON-NLS-1$

      /* Iterate through all catchments. */
      final IObservation[] result = new IObservation[areas.length];
      for( int i = 0; i < areas.length; i++ )
      {
        /* Monitor. */
        monitor.subTask( String.format( Messages.getString("OmbrometerRainfallGenerator_14"), i + 1, areas.length ) ); //$NON-NLS-1$

        final GM_MultiSurface area = areas[i];
        if( area == null )
        {
          monitor.worked( 400 / areas.length );
          continue;
        }

        final Geometry areaGeometry = JTSAdapter.export( area );
        final double[] weights = JTSUtilities.fractionAreasOf( areaGeometry, ombrometerPolygons );
        result[i] = RainfallGeneratorUtilities.combineObses( ombrometerObservations, weights, "ombrometer://thiessen" ); //$NON-NLS-1$

        /* Monitor. */
        monitor.worked( 400 / areas.length );
      }

      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.OK, KalypsoModelRcmActivator.PLUGIN_ID, Messages.getString("OmbrometerRainfallGenerator_16"), null ) ); //$NON-NLS-1$

      return result;
    }
    catch( final GM_Exception e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( Messages.getString("OmbrometerRainfallGenerator_17"), e.getLocalizedMessage() ), e ) ); //$NON-NLS-1$

      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, "Failed to convert geometry: " + e.toString(), e ) ); //$NON-NLS-1$
    }
    catch( final SensorException e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( Messages.getString("OmbrometerRainfallGenerator_19"), e.getLocalizedMessage() ), e ) ); //$NON-NLS-1$

      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, "Failed to combine observations: " + e.toString(), e ) ); //$NON-NLS-1$
    }
    catch( final MalformedURLException e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( Messages.getString("OmbrometerRainfallGenerator_21"), e.getLocalizedMessage() ), e ) ); //$NON-NLS-1$

      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, Messages.getString("OmbrometerRainfallGenerator_22") + e.toString(), e ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( Messages.getString("OmbrometerRainfallGenerator_23"), e.getLocalizedMessage() ), e ) ); //$NON-NLS-1$

      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, Messages.getString("OmbrometerRainfallGenerator_24") + e.toString(), e ) ); //$NON-NLS-1$
    }
    finally
    {
      /* Monitor. */
      monitor.done();

      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.INFO, KalypsoModelRcmActivator.PLUGIN_ID, Messages.getString("OmbrometerRainfallGenerator_25"), null ) ); //$NON-NLS-1$
    }
  }

  private void analyseOmbrometerAndProcessThiessen( final Feature[] ombrometerFeatures, final IObservation[] ombrometerObservations, final IProgressMonitor monitor ) throws CoreException, GM_Exception
  {
    Assert.isTrue( ombrometerFeatures.length == ombrometerObservations.length );

    /* Set used flag of each ombrometer */
    for( int i = 0; i < ombrometerObservations.length; i++ )
    {

      final IObservation obs = ombrometerObservations[i];
      final Boolean use = analyseObservation( obs );

      final IOmbrometer ombro = (IOmbrometer) ombrometerFeatures[i];
      ombro.setUsed( use );
    }

    // REMARK: huge buffer ratio. Does not really change the result (as long as all catchments are covered), but we do
    // not need to worry where to get the buffer as we don't save the result here. Mainly needed for the visualisation.
    final IBoundaryCalculator bufferCalculator = new BufferBoundaryCalculator( 10.0 );

    /* Recalculate Thiessen */
    final ThiessenAreaOperation worker = new ThiessenAreaOperation( IOmbrometer.QNAME_PROP_STATIONLOCATION, IOmbrometer.QNAME_PROP_ISUSED );
    final Map<Feature, GM_Polygon> areas = worker.execute( Arrays.asList( ombrometerFeatures ), bufferCalculator, monitor );
    for( final Entry<Feature, GM_Polygon> entry : areas.entrySet() )
    {
      final IOmbrometer ombrometer = (IOmbrometer) entry.getKey();
      ombrometer.setAffectedArea( entry.getValue() );
    }
  }

  private Boolean analyseObservation( final IObservation obs )
  {
    try
    {
      final String description = OmbrometerUtils.analyseOmbrometer( obs );
      return OmbrometerUtils.getUsedFromDescription( description );
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      return Boolean.FALSE;
    }
  }
}