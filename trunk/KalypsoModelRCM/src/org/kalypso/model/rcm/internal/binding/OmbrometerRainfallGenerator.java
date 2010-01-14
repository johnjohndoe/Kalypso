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
import java.net.URL;
import java.util.Date;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.rcm.KalypsoModelRcmActivator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.internal.UrlCatalogRcm;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.transformation.GeoTransformer;
import org.kalypso.utils.log.LogUtilities;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Implementation that generates rainfall from ombrometer stations.
 * 
 * @author Gernot Belger
 */
public class OmbrometerRainfallGenerator extends Feature_Impl implements IRainfallGenerator
{
  public static final QName QNAME = new QName( UrlCatalogRcm.NS_RCM, "OmbrometerRainfallGenerator" );

  public static final QName QNAME_PROP_ombrometerCollection = new QName( UrlCatalogRcm.NS_RCM, "ombrometerCollection" );

  public static final QName QNAME_PROP_ombrometerFeaturePath = new QName( UrlCatalogRcm.NS_RCM, "ombrometerFeaturePath" );

  public static final QName QNAME_PROP_timeseriesLinkPath = new QName( UrlCatalogRcm.NS_RCM, "timeseriesLinkPath" );

  public static final QName QNAME_PROP_areaPath = new QName( UrlCatalogRcm.NS_RCM, "areaPath" );

  public static final QName QNAME_PROP_catchmentAreaPath = new QName( UrlCatalogRcm.NS_RCM, "catchmentAreaPath" );

  /**
   * The log.
   */
  private ILog m_log;

  /**
   * The constructor.
   * 
   * @param parent
   *          The parent.
   * @param parentRelation
   *          The parent relation.
   * @param featureType
   *          The feature type.
   * @param id
   *          The feature id.
   * @param propValues
   *          The property values.
   */
  public OmbrometerRainfallGenerator( Object parent, IRelationType parentRelation, IFeatureType featureType, String id, Object[] propValues )
  {
    super( parent, parentRelation, featureType, id, propValues );

    m_log = null;
  }

  /**
   * @see org.kalypso.model.rcm.binding.IRainfallGenerator#setLog(org.eclipse.core.runtime.ILog)
   */
  @Override
  public void setLog( ILog log )
  {
    m_log = log;
  }

  /**
   * @see org.kalypso.model.rcm.binding.IRainfallGenerator#createRainfall(org.kalypsodeegree.model.feature.Feature[],
   *      java.util.Date, java.util.Date, org.eclipse.core.runtime.IProgressMonitor)
   */
  public IObservation[] createRainfall( Feature[] catchmentFeatures, Date from, Date to, IProgressMonitor monitor ) throws org.eclipse.core.runtime.CoreException
  {
    /* Update the log. */
    LogUtilities.logQuietly( m_log, new Status( IStatus.INFO, KalypsoModelRcmActivator.PLUGIN_ID, "Generator Ombrometer (Thiessen) wurde gestartet.", null ) );

    /* Get the needed properties. */
    Feature ombrometerCollection = getProperty( QNAME_PROP_ombrometerCollection, Feature.class );
    String collectionPath = getProperty( QNAME_PROP_ombrometerFeaturePath, String.class );
    String areaPath = getProperty( QNAME_PROP_areaPath, String.class );
    String linkPath = getProperty( QNAME_PROP_timeseriesLinkPath, String.class );
    String catchmentAreaPath = getProperty( QNAME_PROP_catchmentAreaPath, String.class );

    /* Create the paths. */
    GMLXPath collectionXPath = new GMLXPath( collectionPath, getWorkspace().getNamespaceContext() );
    GMLXPath areaXPath = new GMLXPath( areaPath, getWorkspace().getNamespaceContext() );
    GMLXPath linkXPath = new GMLXPath( linkPath, getWorkspace().getNamespaceContext() );
    GMLXPath catchmentAreaXPath = new GMLXPath( catchmentAreaPath, getWorkspace().getNamespaceContext() );

    try
    {
      /* Get the ombrometers. */
      FeatureList ombrometerList = (FeatureList) GMLXPathUtilities.query( collectionXPath, ombrometerCollection );

      // Find the ombrometer-areas
      Feature[] ombrometerFeatures = FeatureHelper.toArray( ombrometerList );
      if( ombrometerFeatures.length < ombrometerList.size() )
      {
        // TODO: log problem
      }

      GM_Surface< ? >[] ombrometerAreas = FeatureHelper.getProperties( ombrometerFeatures, areaXPath, new GM_Surface[ombrometerFeatures.length] );
      TimeseriesLinkType[] ombrometerLinks = FeatureHelper.getProperties( ombrometerFeatures, linkXPath, new TimeseriesLinkType[ombrometerFeatures.length] );
      URL sourceContext = ombrometerList.getParentFeature().getWorkspace().getContext();

      IObservation[] ombrometerObservations = RainfallGeneratorUtilities.readObservations( ombrometerLinks, from, to, sourceContext );

      // Convert to JTS-Geometries
      Polygon[] ombrometerPolygons = new Polygon[ombrometerAreas.length];
      GeoTransformer transformer = new GeoTransformer( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      for( int i = 0; i < ombrometerAreas.length; i++ )
      {
        if( ombrometerAreas[i] != null )
        {
          GM_Surface< ? > ombrometerArea = ombrometerAreas[i];
          GM_Object ombrometerTransformed = transformer.transform( ombrometerArea );
          ombrometerPolygons[i] = (Polygon) JTSAdapter.export( ombrometerTransformed );
        }
      }

      /* Get all catchment areas. */
      GM_MultiSurface[] areas = RainfallGeneratorUtilities.findCatchmentAreas( catchmentFeatures, catchmentAreaXPath );

      // Iterate through all catchments
      IObservation[] result = new IObservation[areas.length];
      for( int i = 0; i < areas.length; i++ )
      {
        GM_MultiSurface area = areas[i];
        if( area == null )
          continue;

        Geometry areaGeometry = JTSAdapter.export( area );
        double[] weights = JTSUtilities.fractionAreasOf( areaGeometry, ombrometerPolygons );
        result[i] = RainfallGeneratorUtilities.combineObses( ombrometerObservations, weights );
      }

      /* Update the log. */
      LogUtilities.logQuietly( m_log, new Status( IStatus.OK, KalypsoModelRcmActivator.PLUGIN_ID, "Berechnet", null ) );

      return result;
    }
    catch( GM_Exception e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( m_log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( "Generator Ombrometer (Thiessen) wurde mit einem Fehler beendet: %s", e.getLocalizedMessage() ), e ) );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to convert Geometrie: " + e.toString(), e ) );
    }
    catch( SensorException e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( m_log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( "Generator Ombrometer (Thiessen) wurde mit einem Fehler beendet: %s", e.getLocalizedMessage() ), e ) );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to combine Observations: " + e.toString(), e ) );
    }
    catch( MalformedURLException e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( m_log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( "Generator Ombrometer (Thiessen) wurde mit einem Fehler beendet: %s", e.getLocalizedMessage() ), e ) );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to load Observations: " + e.toString(), e ) );
    }
    catch( Exception e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( m_log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( "Generator Ombrometer (Thiessen) wurde mit einem Fehler beendet: %s", e.getLocalizedMessage() ), e ) );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to create the rainfall: " + e.toString(), e ) );
    }
    finally
    {
      /* Update the log. */
      LogUtilities.logQuietly( m_log, new Status( IStatus.INFO, KalypsoModelRcmActivator.PLUGIN_ID, "Generator Ombrometer (Thiessen) wurde beendet.", null ) );
    }
  }
}