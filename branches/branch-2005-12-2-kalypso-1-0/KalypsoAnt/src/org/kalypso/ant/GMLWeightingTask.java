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
package org.kalypso.ant;

import java.io.File;
import java.io.FileWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;

import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.contribs.java.util.logging.LoggerUtilities;
import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.filter.FilterFactory;
import org.kalypso.transformation.CopyObservationMappingHelper;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.InterpolationFilter;
import org.kalypso.zml.filters.NOperationFilter;
import org.kalypso.zml.filters.ObjectFactory;
import org.kalypso.zml.filters.OperationFilter;
import org.kalypso.zml.filters.ZmlFilter;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeaturePath;
import org.w3._1999.xlinkext.SimpleLinkType;
import org.xml.sax.InputSource;

/**
 * This Task generates from a number of input zml files new zml output files. <br>
 * It uses a combination of operation and n-operation filters like this: <br>
 * <center><b>ZMLout = sum( f(i)*ZMLin(i) ) </b> </center> <br>
 * f(i): factor <br>
 * ZMLin(i): input timeseries <br>
 * the parameters are given by a gml model and some configuration strings (featurepath and property names)
 * 
 * @author doemming
 */
public class GMLWeightingTask extends Task
{
  private File m_targetMapping = null;

  private URL m_modelURL;

  private URL m_targetContext;

  private String m_featurePathTarget; // e.g. "PegelCollectionAssociation/PegelMember"

  private String m_propZMLTarget; // e.g. "Niederschlag"

  private String m_propRelationWeightMember; // e.g. "gewichtung"

  private String m_propWeight; // e.g. "faktor"

  private String m_propOffset; // e.g. "faktor"

  private String m_propRelationSourceFeature; //e.g. "ombrometerMember"

  private String m_propZMLSource;//e.g. Niederschlag_gemessen

  private String m_propSourceUsed;

  private String m_sourceFilter;

  private long m_from;

  private long m_to;

  private Long m_forecastFrom;

  private Long m_forecastTo;

  private Long m_sourceFrom;

  private Long m_sourceTo;

  private Long m_targetFrom;

  private Long m_targetTo;

  /**
   * @see org.apache.tools.ant.Task#execute()
   */
  public void execute() throws BuildException
  {
    try
    {
      final Project antProject = getProject();
      // REMARK: It is NOT possible to put this inner class into an own .class file (at least not inside the plugin
      // code) else we get an LinkageError when accessing the Project class.
      final ILogger logger = new ILogger()
      {
        /**
         * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.util.logging.Level, int, java.lang.String)
         */
        public void log( final Level level, final int msgCode, final String message )
        {
          final String outString = LoggerUtilities.formatLogStylish( level, msgCode, message );

          if( antProject == null )
            System.out.println( outString );
          else
            antProject.log( outString );
        }
      };

      final String message = getDescription();
      if( message != null && message.length() > 0 )
        logger.log( Level.INFO, LoggerUtilities.CODE_NEW_MSGBOX, message );

      final UrlResolver urlResolver = new UrlResolver();
      // create needed factories
      final ObjectFactory filterFac = new ObjectFactory();
      final Marshaller marshaller = filterFac.createMarshaller();

      final org.w3._1999.xlinkext.ObjectFactory linkFac = new org.w3._1999.xlinkext.ObjectFactory();

      // workspace for results
      final GMLWorkspace resultWorkspace = CopyObservationMappingHelper.createMappingWorkspace( m_modelURL );

      // 1. load srcgml
      logger.log( Level.INFO, -1, "Lade Modell " + m_modelURL );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( m_modelURL );

      // 2. locate features to process
      final Feature[] targetFeatures = getTargetFeatures( workspace );
      if( targetFeatures == null )
        throw new BuildException( "Kein(e) Ziel-Feature(s) gefunden f�r FeaturePath: " + m_featurePathTarget );

      // loop all features
      for( int i = 0; i < targetFeatures.length; i++ )
      {
        final Feature targetFE = targetFeatures[i];
        // 3. find target
        final TimeseriesLink targetLink = (TimeseriesLink)targetFE.getProperty( m_propZMLTarget );
        final String targetHref = targetLink.getHref();
        final URL targetURL = urlResolver.resolveURL( m_targetContext, targetHref );

        // 4. build n-operation filter
        final NOperationFilter nOperationFilter = filterFac.createNOperationFilter();
        nOperationFilter.setOperator( "+" );
        final List filterList = nOperationFilter.getFilter();

        // 5. resolve weights
        final Feature[] weightFEs = getWeightFeatures( workspace, targetFE );
        if( weightFEs == null )
          throw new BuildException( "Kein(e) Gewichts-Feature(s) gefunden f�r FeaturePath: "
              + m_propRelationWeightMember );

        // 6. loop weights
        for( int j = 0; j < weightFEs.length; j++ )
        {
          final Feature weightFE = weightFEs[j];

          final double factor = getFactor( weightFE );
          final double offset = getOffset( weightFE );

          // 7. resolve sources
          final Feature[] sourceFeatures = getSourceFeatures( workspace, weightFE );
          if( sourceFeatures == null )
            throw new BuildException( "Kein(e) Quell-Feature(s) gefunden f�r FeaturePath: "
                + m_propRelationSourceFeature );

          final OperationFilter offsetFilter = filterFac.createOperationFilter();
          filterList.add( offsetFilter );
          offsetFilter.setOperator( "+" );
          offsetFilter.setOperand( Double.toString( offset ) );

          final NOperationFilter weightSumFilter = filterFac.createNOperationFilter();
          weightSumFilter.setOperator( "+" );

          offsetFilter.setFilter( weightSumFilter );

          final List offsetSummands = weightSumFilter.getFilter();

          // 8. loop source features
          for( int k = 0; k < sourceFeatures.length; k++ )
          {
            // 7. resolve feature that has source zml reference
            final Feature sourceFE = sourceFeatures[k];

            if( sourceFE == null )
            {
              logger.log( Level.WARNING, -1, "Linked source feature missing in Feature: " + weightFE.getId() );

              // IMPORTANT: just skips this weight; leads probably to wrong results
              continue;
            }

            // 9. resolve property that is source zml reference
            final TimeseriesLink zmlLink = (TimeseriesLink)sourceFE.getProperty( m_propZMLSource );
            final Boolean useThisSource;
            if( m_propSourceUsed != null && m_propSourceUsed.length() > 0 )
              useThisSource = (Boolean)sourceFE.getProperty( m_propSourceUsed );
            else
              useThisSource = Boolean.TRUE;

            if( !useThisSource.booleanValue() )
            {
              logger.log( Level.INFO, LoggerUtilities.CODE_NONE, "Ignoriere: " + sourceFE.getId() );
              continue;
            }

            if( zmlLink == null )
            {
              logger.log( Level.WARNING, LoggerUtilities.CODE_SHOW_DETAILS,
                  "Linked timeserie link missing in Feature: " + weightFE.getId() );

              // IMPORTANT: just skips this weight; leads probably to wrong results
              continue;
            }

            // 10. build operation filter with parameters from gml
            final OperationFilter filter = filterFac.createOperationFilter();
            offsetSummands.add( filter );
            filter.setOperator( "*" );
            filter.setOperand( Double.toString( factor ) );

            /* Innermost filter part */
            final ZmlFilter zmlFilter = filterFac.createZmlFilter();
            final SimpleLinkType simpleLink = linkFac.createSimpleLinkType();
            final String sourceHref = zmlLink.getHref();
            simpleLink.setHref( sourceHref );
            zmlFilter.setZml( simpleLink );

            if( m_sourceFilter != null )
            {
              final String strFilterXml = FilterFactory.getFilterPart( m_sourceFilter );

              final StringReader sr = new StringReader( strFilterXml );
              final AbstractFilterType af = (AbstractFilterType)FilterFactory.OF_FILTER.createUnmarshaller().unmarshal(
                  new InputSource( sr ) );
              filter.setFilter( af );

              // HACK
              if( af instanceof InterpolationFilter )
                ( (InterpolationFilter)af ).setFilter( zmlFilter );
              else
                throw new UnsupportedOperationException(
                    "Only InterpolationFilter as source-filter supported at the moment." );

              sr.close();
            }
            else
              filter.setFilter( zmlFilter );
          }

          /* Empty NOperation filter is forbidden */
          if( offsetSummands.isEmpty() )
          {
            logger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_MSGBOX, "Leere Summe f�r Feature: " + targetFE.getId() );
            return;
          }
        }

        /* Empty NOperation filter is forbidden */
        if( filterList.isEmpty() )
        {
          logger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_MSGBOX, "Leere Summe f�r Feature: " + targetFE.getId() );
          return;
        }

        // 11. serialize filter to string
        final Writer writer = new StringWriter();
        marshaller.marshal( nOperationFilter, writer );
        writer.close();
        final String string = XMLUtilities.removeXMLHeader( writer.toString() );
        final String filterInline = XMLUtilities.prepareInLine( string );

        // 12. add mapping to result workspace
        CopyObservationMappingHelper.addMapping( resultWorkspace, filterInline, targetURL.toExternalForm() );
        logger.log( Level.INFO, -1, "Ziel-ZML " + targetURL );
      }

      // 14. do the mapping
      final Date sourceFrom = m_sourceFrom == null ? new Date( m_from ) : new Date( m_sourceFrom.longValue() );
      final Date sourceTo = m_sourceTo == null ? new Date( m_forecastFrom.longValue() ) : new Date( m_sourceTo
          .longValue() );
      final Date targetFrom = m_targetFrom == null ? new Date( m_forecastFrom.longValue() ) : new Date( m_targetFrom
          .longValue() );
      final Date targetTo = m_targetTo == null ? new Date( m_to ) : new Date( m_targetTo.longValue() );
      final Date forecastFrom = new Date( m_forecastFrom.longValue() );
      final Date forecastTo = m_forecastTo == null ? new Date( m_to ) : new Date( m_forecastTo.longValue() );

      CopyObservationMappingHelper.runMapping( resultWorkspace, urlResolver, m_modelURL, logger, true, sourceFrom,
          sourceTo, targetFrom, targetTo, forecastFrom, forecastTo );

      // 15. serialize result workspace to file
      if( m_targetMapping != null )
      {
        FileWriter writer = null;
        try
        {
          writer = new FileWriter( m_targetMapping );
          GmlSerializer.serializeWorkspace( writer, resultWorkspace );
          writer.close();
        }
        finally
        {
          IOUtils.closeQuietly( writer );
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new BuildException( e );
    }
  }

  private double getOffset( final Feature weightFE )
  {
    if( m_propOffset == null || m_propOffset.length() == 0 )
      return 0.0;

    return ( (Double)weightFE.getProperty( m_propOffset ) ).doubleValue();
  }

  private double getFactor( final Feature weightFE )
  {
    if( m_propWeight == null || m_propWeight.length() == 0 )
      return 1.0;

    return ( (Double)weightFE.getProperty( m_propWeight ) ).doubleValue();
  }

  private Feature[] getTargetFeatures( final GMLWorkspace workspace )
  {
    final FeaturePath path = new FeaturePath( m_featurePathTarget );
    final Object property = path.getFeature( workspace );
    if( property instanceof FeatureList )
      return ( (FeatureList)property ).toFeatures();

    if( property instanceof Feature )
      return new Feature[]
      { (Feature)property };

    return null;
  }

  private Feature[] getWeightFeatures( final GMLWorkspace workspace, final Feature targetFE )
  {
    if( m_propRelationWeightMember == null || m_propRelationWeightMember.length() == 0 )
      return new Feature[]
      { targetFE };

    return workspace.resolveLinks( targetFE, m_propRelationWeightMember );
  }

  private Feature[] getSourceFeatures( final GMLWorkspace workspace, final Feature weightFE )
  {
    if( m_propRelationSourceFeature == null || m_propRelationSourceFeature.length() == 0 )
      return new Feature[]
      { weightFE };

    final Object property = weightFE.getProperty( m_propRelationSourceFeature );
    if( property instanceof FeatureList )
      return ( (FeatureList)property ).toFeatures();

    if( property instanceof Feature )
      return new Feature[]
      { (Feature)property };

    if( property instanceof String )
      return new Feature[]
      { workspace.resolveLink( weightFE, m_propRelationSourceFeature ) };

    return null;
  }

  /**
   * @param targetMapping
   *          gml file that will be generated and includes the mapping that will be generated from the model
   */
  public final void setTargetMapping( final File targetMapping )
  {
    m_targetMapping = targetMapping;
  }

  /**
   * @param modelURL
   *          reference to the model that describes the mapping in a gml structure
   */
  public final void setModelURL( URL modelURL )
  {
    m_modelURL = modelURL;
  }

  /**
   * @param targetContext
   *          context to use
   */
  public final void setTargetContext( URL targetContext )
  {
    m_targetContext = targetContext;
  }

  /**
   * @param featurePathTarget
   *          path to the features that contain the ZML-target properties and the references to weighting features
   */
  public final void setFeaturePathTarget( String featurePathTarget )
  {
    m_featurePathTarget = featurePathTarget;
  }

  /**
   * @param propRelationSourceFeature
   *          name of property that links from weighting feature to zml source feature
   */
  public final void setPropRelationSourceFeature( String propRelationSourceFeature )
  {
    m_propRelationSourceFeature = propRelationSourceFeature;
  }

  /**
   * @param propRelationWeightMember
   *          name of property that links from zml target feature to the list of weighting features
   */
  public final void setPropRelationWeightMember( String propRelationWeightMember )
  {
    m_propRelationWeightMember = propRelationWeightMember;
  }

  /**
   * @param propWeight
   *          property name of the weighting property, feature property type must be double
   */
  public final void setPropWeight( String propWeight )
  {
    m_propWeight = propWeight;
  }

  /**
   * @param propOffset
   *          property name of the offset property, feature property type must be double
   */
  public void setPropOffset( String propOffset )
  {
    m_propOffset = propOffset;
  }

  /**
   * @param propZMLSource
   *          property name of the zml source property, feature property type must be TimeSeriesLink
   */
  public final void setPropZMLSource( String propZMLSource )
  {
    m_propZMLSource = propZMLSource;
  }

  /**
   * @param propSourceUsed
   *          property name of the zml sourceUsed property, feature property type must be Boolean. If set, the property
   *          is used to determined if this particular source is used or not.
   */
  public void setPropSourceUsed( final String propSourceUsed )
  {
    m_propSourceUsed = propSourceUsed;
  }

  /**
   * @param sourceFilter
   *          If non- <code>null</code>, this filter will be applied to every source-zml
   */
  public void setSourceFilter( String sourceFilter )
  {
    m_sourceFilter = sourceFilter;
  }

  /**
   * @param propZMLTarget
   *          property name of the zml target property, feature property type must be TimeSeriesLink
   */
  public final void setPropZMLTarget( String propZMLTarget )
  {
    m_propZMLTarget = propZMLTarget;
  }

  /**
   * @param from
   *          beginning of measure periode
   * @deprecated Use sourceFrom, targetFrom or forecastFrom instead
   */
  public final void setFrom( long from )
  {
    m_from = from;
  }

  /**
   * @param forecastFrom
   *          beginning of forecast periode (end of measure periode)
   */
  public final void setForecastFrom( final Long forecastFrom )
  {
    m_forecastFrom = forecastFrom;
  }

  /**
   * @param forecastTo
   *          end of forecast periode (end of measure periode)
   */
  public final void setForecastTo( final Long forecastTo )
  {
    m_forecastTo = forecastTo;
  }

  /**
   * @param to
   *          end of forecast periode
   * @deprecated Use sourceTo, targetTo or forecastTo instead
   */
  public final void setTo( long to )
  {
    m_to = to;
  }

  /**
   * @param sourceFrom
   *          start of request for source - observations
   */
  public final void setSourceFrom( final Long sourceFrom )
  {
    m_sourceFrom = sourceFrom;
  }

  /**
   * @param sourceTo
   *          end of request for source - observations
   */
  public final void setSourceTo( final Long sourceTo )
  {
    m_sourceTo = sourceTo;
  }

  /**
   * @param targetFrom
   *          start of request for source - observations
   */
  public final void setTargetFrom( final Long targetFrom )
  {
    m_targetFrom = targetFrom;
  }

  /**
   * @param targetTo
   *          end of request for source - observations
   */
  public final void setTargetTo( final Long targetTo )
  {
    m_targetTo = targetTo;
  }

}
