/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.rcm.binding.AbstractRainfallGenerator;
import org.kalypso.model.rcm.binding.IOmbrometerCollection;
import org.kalypso.model.rcm.binding.IOmbrometerCollection.GenerationStrategy;
import org.kalypso.model.rcm.internal.KalypsoModelRcmActivator;
import org.kalypso.model.rcm.internal.UrlCatalogRcm;
import org.kalypso.model.rcm.util.IOmbrometerGeneratorStrategy;
import org.kalypso.model.rcm.util.RainfallExtensionUtilities;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
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
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Implementation that generates rainfall from ombrometer stations with Thiessen.
 * 
 * @author Gernot Belger
 */
public class OmbrometerRainfallGenerator extends AbstractRainfallGenerator
{
  static final QName FEATURE_OMBROMETER_RAINFALL_GENERATOR = new QName( UrlCatalogRcm.NS_RCM, "OmbrometerRainfallGenerator" );

  static final QName MEMBER_ombrometerCollection = new QName( UrlCatalogRcm.NS_RCM, "ombrometerCollection" );

  static final QName PROPERTY_ombrometerFeaturePath = new QName( UrlCatalogRcm.NS_RCM, "ombrometerFeaturePath" );

  static final QName PROPERTY_timeseriesLinkPath = new QName( UrlCatalogRcm.NS_RCM, "timeseriesLinkPath" );

  static final QName MEMBER_FILTER = new QName( UrlCatalogRcm.NS_RCM, "filterMember" );

  static final QName PROPERTY_areaPath = new QName( UrlCatalogRcm.NS_RCM, "areaPath" );

  static final QName PROPERTY_catchmentAreaPath = new QName( UrlCatalogRcm.NS_RCM, "catchmentAreaPath" );

  private final FeatureBindingCollection<IZmlFilter> m_filters = new FeatureBindingCollection<IZmlFilter>( this, IZmlFilter.class, MEMBER_FILTER, true );

  public OmbrometerRainfallGenerator( final Object parent, final IRelationType parentRelation, final IFeatureType featureType, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, featureType, id, propValues );
  }

  @Override
  public IObservation[] createRainfall( final Feature[] catchmentFeatures, final DateRange range, final ILog log, IProgressMonitor monitor ) throws CoreException
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      /* Monitor. */
      monitor.beginTask( "Führe Generator Ombrometer (Thiessen) aus...", 1000 );
      monitor.subTask( "Prüfe Voraussetzungen..." );

      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.INFO, KalypsoModelRcmActivator.PLUGIN_ID, "Generator Ombrometer (Thiessen) wurde gestartet.", null ) );

      /* Get the needed properties. */
      final XLinkedFeature_Impl ombrometerCollectionLink = getProperty( MEMBER_ombrometerCollection, XLinkedFeature_Impl.class );
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
      monitor.subTask( "Lade Ombrometer..." );

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
      monitor.subTask( "Konvertiere und wende die Thiessenmethode an..." );

      final IZmlFilter[] filters = m_filters.toArray( new IZmlFilter[m_filters.size()] );

      final IObservation[] ombrometerObservations = RainfallGeneratorUtilities.readObservations( ombrometerFeatures, linkXPath, filters, range );

      /* Apply generator strategy, if not yet done. */
      if( !ombrometerCollection.hasBeenProcessed() )
      {
        final GenerationStrategy strategy = ombrometerCollection.getGenerationStrategy();
        final IOmbrometerGeneratorStrategy generatorStrategy = RainfallExtensionUtilities.createOmbrometerGeneratorStrategy( strategy.name() );
        generatorStrategy.processFirstTime( ombrometerFeatures, ombrometerObservations, new NullProgressMonitor() );
      }

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( "Erzeuge Ombrometerflächen..." );

      final GM_Surface< ? >[] ombrometerAreas = FeatureHelper.getProperties( ombrometerFeatures, areaXPath, new GM_Surface[ombrometerFeatures.length] );

      /* Convert to JTS geometries. */
      final Polygon[] ombrometerPolygons = new Polygon[ombrometerAreas.length];
      final IGeoTransformer transformer = GeoTransformerFactory.getGeoTransformer( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      for( int i = 0; i < ombrometerAreas.length; i++ )
      {
        if( ombrometerAreas[i] != null )
        {
          final GM_Surface< ? > ombrometerArea = ombrometerAreas[i];
          final GM_Object ombrometerTransformed = transformer.transform( ombrometerArea );
          ombrometerPolygons[i] = (Polygon) JTSAdapter.export( ombrometerTransformed );
        }

        /* Monitor. */
        monitor.worked( 200 / ombrometerAreas.length );
      }

      /* Monitor. */
      monitor.subTask( "Hole Einzugsgebiete..." );

      /* Get all catchment areas. */
      final GM_MultiSurface[] areas = RainfallGeneratorUtilities.findCatchmentAreas( catchmentFeatures, catchmentAreaXPath );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( "Bearbeite Einzugsgebiete..." );

      /* Iterate through all catchments. */
      final IObservation[] result = new IObservation[areas.length];
      for( int i = 0; i < areas.length; i++ )
      {
        /* Monitor. */
        monitor.subTask( String.format( "Bearbeite Einzugsgebiet %d / %d...", i + 1, areas.length ) );

        final GM_MultiSurface area = areas[i];
        if( area == null )
        {
          monitor.worked( 400 / areas.length );
          continue;
        }

        final Geometry areaGeometry = JTSAdapter.export( area );
        final double[] weights = JTSUtilities.fractionAreasOf( areaGeometry, ombrometerPolygons );
        result[i] = RainfallGeneratorUtilities.combineObses( ombrometerObservations, weights, "ombrometer://thiessen" );

        /* Monitor. */
        monitor.worked( 400 / areas.length );
      }

      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.OK, KalypsoModelRcmActivator.PLUGIN_ID, "Berechnet", null ) );

      return result;
    }
    catch( final GM_Exception e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( "Generator Ombrometer (Thiessen) wurde mit einem Fehler beendet: %s", e.getLocalizedMessage() ), e ) );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to convert geometry: " + e.toString(), e ) );
    }
    catch( final SensorException e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( "Generator Ombrometer (Thiessen) wurde mit einem Fehler beendet: %s", e.getLocalizedMessage() ), e ) );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to combine observations: " + e.toString(), e ) );
    }
    catch( final MalformedURLException e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( "Generator Ombrometer (Thiessen) wurde mit einem Fehler beendet: %s", e.getLocalizedMessage() ), e ) );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to load Observations: " + e.toString(), e ) );
    }
    catch( final Exception e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( "Generator Ombrometer (Thiessen) wurde mit einem Fehler beendet: %s", e.getLocalizedMessage() ), e ) );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to create the rainfall: " + e.toString(), e ) );
    }
    finally
    {
      /* Monitor. */
      monitor.done();

      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.INFO, KalypsoModelRcmActivator.PLUGIN_ID, "Generator Ombrometer (Thiessen) wurde beendet.", null ) );
    }
  }
}