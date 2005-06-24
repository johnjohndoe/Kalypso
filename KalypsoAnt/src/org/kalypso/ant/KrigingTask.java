/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ant;

import java.io.File;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.util.logging.Logger;

import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.transformation.dwd.KrigingReader;
import org.kalypso.transformation.dwd.SourceObservationProvider;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.UrlCatalogUpdateObservationMapping;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.cs.CoordinateSystem;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.model.feature.visitors.ResortVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Ein Ant Task, der Zeitreihen-Links in GMLs kopiert. Die generelle Idee ist es, alle Features eines GML durchzugehen,
 * und für jedes Feature eine Zeitreihe (definiert über einen Link) zu lesen und an eine andere Stelle (definiert durch
 * eine andere Property des Features) zu schreiben.
 * 
 * <code>
 * TODO
 * </code>
 * 
 * @author doemming
 */

public class KrigingTask extends Task
{
  private static final String RESULT_LIST_PROP = "mappingMember";

  private static final String RESULT_TS_IN_PROP = "inObservationLink";

  private static final String RESULT_TS_OUT_PROP = "outObservationLink";

  /** context used for non qualified hrefs */
  private URL m_context;

  /** epsg-code for spatial reference system, that is used in kriging.txt */
  private String m_epsg;

  /** href to the mapping gml to generate */
  private String m_hrefGeneratesGml;

  /** href to rastermapping (Format Krässig) (mandatory) */
  private String m_hrefKrigingTXT;

  /** href to modellGML (mandatory) */
  private String m_modellGML;

  /**
   * featurepath to the features that have the location property (mus be a polygon) (mandatory)
   */
  private String m_modellGMLFeaturePath;

  /** propertyName of the location property (mus be a polygon) (mandatory) */
  private String m_modellGMLpolygonPropname;

  /** property name, where to generate the taget files (mandatory) */
  private String m_modellGMLTargetObservationlinkPropname;

  /** timestep in minutes */
  private double m_timeStepMinutes;

  /** modell, that contains links to source observations (mandatory) */
  private String m_sourceGML;

  /**
   * featurepath to features that contains links to source observations (mandatory)
   */
  private String m_sourceGMLFeaturePath;

  /**
   * feature property name that contains links to source observations (mandatory)
   */
  private String m_sourceGMLObservationLinkProperty;

  /**
   * feature property name that contains ids that are used for mapping to indentifiers in "kriging.txt" (optional, fid
   * used f unset )
   */
  private String m_sourceGMLIDLinkProperty;

  public final void setHrefGeneratesGml( String hrefGeneratesGml )
  {
    m_hrefGeneratesGml = hrefGeneratesGml;
  }

  public final void setHrefKrigingTXT( String hrefKrigingTXT )
  {
    m_hrefKrigingTXT = hrefKrigingTXT;
  }

  public final void setModellGML( String modellGML )
  {
    m_modellGML = modellGML;
  }

  public final void setModellGMLFeaturePath( String modellGMLFeaturePath )
  {
    m_modellGMLFeaturePath = modellGMLFeaturePath;
  }

  public final void setModellGMLpolygonPropname( String modellGMLpolygonPropname )
  {
    m_modellGMLpolygonPropname = modellGMLpolygonPropname;
  }

  public final void setEpsg( String epsg )
  {
    m_epsg = epsg;
  }

  public final void setContext( URL context )
  {
    m_context = context;
  }

  public final void setTimeStepMinutes( double timeStepMinutes )
  {
    m_timeStepMinutes = timeStepMinutes;
  }

  public final void setModellGMLTargetObservationlinkPropname( String modellGMLTargetObservationlinkPropname )
  {
    m_modellGMLTargetObservationlinkPropname = modellGMLTargetObservationlinkPropname;
  }

  public final void setSourceGML( String sourceGML )
  {
    m_sourceGML = sourceGML;
  }

  public final void setSourceGMLFeaturePath( String sourceGMLFeaturePath )
  {
    m_sourceGMLFeaturePath = sourceGMLFeaturePath;
  }

  public final void setSourceGMLIDLinkProperty( String sourceGMLIDLinkProperty )
  {
    m_sourceGMLIDLinkProperty = sourceGMLIDLinkProperty;
  }

  public final void setSourceGMLObservationLinkProperty( String sourceGMLObservationLinkProperty )
  {
    m_sourceGMLObservationLinkProperty = sourceGMLObservationLinkProperty;
  }

  public void execute() throws BuildException
  {
    try
    {
      Logger logger = Logger.getAnonymousLogger();
      logger.info( "load mapping schema NS=" + UrlCatalogUpdateObservationMapping.NS );
      final GMLSchema schema = GMLSchemaCatalog.getSchema( UrlCatalogUpdateObservationMapping.NS );
      if( schema == null )
        throw new Exception( "could not load schema with namespace: " + UrlCatalogUpdateObservationMapping.NS );
      final FeatureType mapColFT = schema.getFeatureType( "MappingCollection" );
      final FeatureType mapFT = schema.getFeatureType( "MappingObservation" );

      final Feature rootFE = FeatureFactory.createFeature( "1", mapColFT );
      
      
      final GMLWorkspace resultWorkspace = new GMLWorkspace_Impl( schema.getFeatureTypes(), rootFE, m_context, null,
          schema.getTargetNS(), schema.getNamespaceMap() );

      logger.info( "check coordinatessystem" );
      // crs stuff
      final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
      final String targetCRSName = m_epsg;
      if( targetCRSName == null )
        throw new Exception( "coordinatesystem not set" );
      final CoordinateSystem cs = csFac.getCSByName( targetCRSName );
      if( cs == null )
        throw new Exception( "unknown coordinatesystem" );
      final CS_CoordinateSystem targetCRS = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export( cs );
      logger.info( "use coordinatessystem " + m_epsg + " OK" );

      final UrlResolver urlResolver = new UrlResolver();

      if( m_hrefKrigingTXT == null )
        throw new Exception( "kriging raster not set" );
      final URL krigingMapURL = urlResolver.resolveURL( m_context, m_hrefKrigingTXT );
      if( krigingMapURL == null )
        throw new Exception( "kriging raster not found" );
      logger.info( "use kriging ratser (mapping) " + krigingMapURL.toExternalForm() );

      logger.info( "load modell..." );
      // load catchment model
      if( m_modellGML == null )
        throw new Exception( "modell not set" );
      final URL modellURL = urlResolver.resolveURL( m_context, m_modellGML );
      if( modellURL == null )
        throw new Exception( "modell not sound" );
      final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( modellURL );
      if( modellWorkspace == null )
        throw new Exception( "could not load modell" );
      modellWorkspace.accept( new TransformVisitor( targetCRS ), modellWorkspace.getRootFeature(),
          FeatureVisitor.DEPTH_INFINITE );
      modellWorkspace.accept( new ResortVisitor(), modellWorkspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

      if( m_modellGMLFeaturePath == null )
        throw new Exception( "modell feature path not set" );
      final Object modellFeatureFromPath = modellWorkspace.getFeatureFromPath( m_modellGMLFeaturePath );
      final Feature[] modelFeatures = FeatureHelper.getFeaturess( modellFeatureFromPath );

      logger.info( "load src modell..." );
      // load src model
      if( m_sourceGML == null )
        throw new Exception( "source modell not set" );
      final URL srcModellURL = urlResolver.resolveURL( m_context, m_sourceGML );
      final GMLWorkspace srcModellWorkspace = GmlSerializer.createGMLWorkspace( srcModellURL );
      if( srcModellWorkspace == null )
        throw new Exception( "could not load source modell" );
      srcModellWorkspace.accept( new TransformVisitor( targetCRS ), srcModellWorkspace.getRootFeature(),
          FeatureVisitor.DEPTH_INFINITE );
      srcModellWorkspace.accept( new ResortVisitor(), srcModellWorkspace.getRootFeature(),
          FeatureVisitor.DEPTH_INFINITE );
      final Object srcFeatureFromPath = srcModellWorkspace.getFeatureFromPath( m_sourceGMLFeaturePath );
      final Feature[] srcFeatures = FeatureHelper.getFeaturess( srcFeatureFromPath );
      final SourceObservationProvider provider = new SourceObservationProvider( srcFeatures, m_sourceGMLIDLinkProperty,
          m_sourceGMLObservationLinkProperty );

      logger.info( "calculate mapping ..." );
      final Reader inputStreamReader = new InputStreamReader( krigingMapURL.openStream() );

      final KrigingReader kReader = new KrigingReader( Logger.global, inputStreamReader, provider, targetCRS );

      final org.kalypso.zml.obslink.ObjectFactory obsLinkFac = new org.kalypso.zml.obslink.ObjectFactory();
      final ObjectFactory o = new ObjectFactory();
      final Marshaller marshaller = o.createMarshaller();
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      for( int i = 0; i < modelFeatures.length; i++ )
      {
        final Feature feature = modelFeatures[i];
        final AbstractFilterType inFilter = kReader.createFilter( feature, m_modellGMLpolygonPropname );

        final Writer writer = new StringWriter();
        marshaller.marshal( inFilter, writer );
        // TODO close writer
        final String string = XMLUtilities.removeXMLHeader( writer.toString() );
        final String filterInline = XMLUtilities.prepareInLine( string )+"#useascontext";
        
        //        final URL zmlURL = new URL( href + "?" + filterInline );
        final Feature mapFE = resultWorkspace.createFeature( mapFT );
        // in
        final TimeseriesLink inLink = obsLinkFac.createTimeseriesLink();
        //        inLink.setHref( writer.toString() );
        inLink.setHref( filterInline );
        final FeatureProperty inProp = FeatureFactory.createFeatureProperty( RESULT_TS_IN_PROP, inLink );
        mapFE.setProperty( inProp );

        // out
        final TimeseriesLink copyLink = (TimeseriesLink)feature.getProperty( m_modellGMLTargetObservationlinkPropname );
        final TimeseriesLink outLink = obsLinkFac.createTimeseriesLink();
        outLink.setHref( copyLink.getHref() );
        final FeatureProperty outProp = FeatureFactory.createFeatureProperty( RESULT_TS_OUT_PROP, outLink );
        mapFE.setProperty( outProp );
        resultWorkspace.addFeatureAsComposition( rootFE, RESULT_LIST_PROP, 0, mapFE );
      }
      final File result = new File( m_hrefGeneratesGml );
      final Writer resultWriter = new FileWriter( result );
      GmlSerializer.serializeWorkspace( resultWriter, resultWorkspace, "UTF-8" );
      IOUtils.closeQuietly( resultWriter );

      //      System.out.println( "TEST: " + m_test );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new BuildException( e.getLocalizedMessage(), e );
    }
  }
}
