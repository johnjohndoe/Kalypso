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
package org.kalypso.transformation;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.util.Date;

import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.ogc.util.CopyObservationFeatureVisitor;
import org.kalypso.ogc.util.CopyObservationHandler;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.UrlCatalogUpdateObservationMapping;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;

/**
 * Helper class to generate a gml that can be used for converting timeseries. The generated gml includes a list map
 * features. Each map feature has a input-TimeseriesLink and output-TimeseriesLink. The resulting gml can be used as
 * input for the CopyObservationTask.
 * 
 * TODO also add a geometry property to the map feature, so that this gml can be also used with a mapview (e.g. in a
 * wizard), where the user can select the timeseries from the map.
 * 
 * @author doemming
 */
public class CopyObservationMappingHelper
{
  public static final String RESULT_LIST_PROP = "mappingMember";

  public static final String RESULT_TS_IN_PROP = "inObservationLink";

  public static final String RESULT_TS_OUT_PROP = "outObservationLink";

  /**
   * 
   * @param context
   *          that should be used by the workspace
   * @return GMLWorkspace that represents the mapping
   * @throws Exception
   */
  public static GMLWorkspace createMappingWorkspace( final URL context ) throws Exception
  {
    final GMLSchema schema = GMLSchemaCatalog.getSchema( UrlCatalogUpdateObservationMapping.NS );
    if( schema == null )
      throw new Exception( "could not load schema with namespace: " + UrlCatalogUpdateObservationMapping.NS );
    final FeatureType mapColFT = schema.getFeatureType( "MappingCollection" );
    final Feature rootFE = FeatureFactory.createFeature( "1", mapColFT );
    return new GMLWorkspace_Impl( schema.getFeatureTypes(), rootFE, context, null, schema.getTargetNS(), schema
        .getNamespaceMap() );
  }

  /**
   * 
   * @param workspace
   *          the mapping will be added to the given workspace, usually a workspace created by this class
   * @param inHref
   *          reference to the ZML-input (also a filter),usually used as source by a CopyObservationTask
   * @param outHref
   *          reference to the ZML-output, usually used as target by a CopyObservationTask
   * @throws Exception
   */
  public static void addMapping( GMLWorkspace workspace, String inHref, String outHref ) throws Exception
  {
    final org.kalypso.zml.obslink.ObjectFactory obsLinkFac = new org.kalypso.zml.obslink.ObjectFactory();

    final FeatureType mapFT = workspace.getFeatureType( "MappingObservation" );
    final Feature mapFE = workspace.createFeature( mapFT );
    // in
    final TimeseriesLink inLink = obsLinkFac.createTimeseriesLink();
    inLink.setHref( inHref );
    final FeatureProperty inProp = FeatureFactory.createFeatureProperty(
        CopyObservationMappingHelper.RESULT_TS_IN_PROP, inLink );
    mapFE.setProperty( inProp );

    // out
    final TimeseriesLink outLink = obsLinkFac.createTimeseriesLink();
    outLink.setHref( outHref );
    final FeatureProperty outProp = FeatureFactory.createFeatureProperty( RESULT_TS_OUT_PROP, outLink );
    mapFE.setProperty( outProp );
    workspace.addFeatureAsComposition( workspace.getRootFeature(), RESULT_LIST_PROP, 0, mapFE );
  }

  /**
   * this mapping updates only the measured time periode, the forecast periode will be taken from the target before
   * overwriting it. So only measured periode will update.
   * 
   * @param workspace
   *          the workspace to process, usually a workspace created by this class
   * @param resolver
   *          resolver
   * @param srcContext
   *          context
   * @param logger
   *          logger
   * @param from
   *          begin of measure periode
   * @param forecastStart
   *          begin of forecast periode (end of measure periode)
   * @param end
   *          end of forecast periode
   */
  public static void runMapping( final GMLWorkspace workspace, final UrlResolver resolver, final URL srcContext,
      final ILogger logger, Date from, Date forecastStart, Date end, boolean keepForecast )
  {
    final StringWriter stringWriter = new StringWriter();
    final PrintWriter writer = new PrintWriter( stringWriter );
    final CopyObservationHandler.Source[] sources;
    if( keepForecast )
      sources = new CopyObservationHandler.Source[]
      {
          new CopyObservationHandler.Source( RESULT_TS_IN_PROP, from, forecastStart ), // measured
          new CopyObservationHandler.Source( RESULT_TS_OUT_PROP, forecastStart, end ) // forecast
      };
    else
      sources = new CopyObservationHandler.Source[]
      { new CopyObservationHandler.Source( RESULT_TS_IN_PROP, from, forecastStart ), // measured
      };
    final CopyObservationFeatureVisitor visitor = new CopyObservationFeatureVisitor( srcContext, resolver,
        RESULT_TS_OUT_PROP, sources, null, null, writer );
    workspace.accept( visitor, RESULT_LIST_PROP, 1 );
    logger.log( stringWriter.toString() );
  }
}
