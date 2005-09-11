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
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.util.Date;
import java.util.List;

import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.transformation.CopyObservationMappingHelper;
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

/**
 * This Task generates from a numer of input zml files new zml output files. <br>
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

  private String m_propRelationSourceFeature; //e.g. "ombrometerMember"

  private String m_propZMLSource;//e.g. Niederschlag_gemessen

  private long m_from;

  private long m_forecastFrom;

  private long m_to;

  /**
   * @see org.apache.tools.ant.Task#execute()
   */
  public void execute() throws BuildException
  {
    //    super.execute();
    try
    {
      final Task me = this;
      final ILogger logger = new ILogger()
      {
        /**
         * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.lang.String)
         */
        public void log( String message )
        {
          me.log( message );
        }
      };
      final UrlResolver urlResolver = new UrlResolver();
      // create needed factories
      final ObjectFactory filterFac = new ObjectFactory();
      final Marshaller marshaller = filterFac.createMarshaller();

      final org.w3._1999.xlinkext.ObjectFactory linkFac = new org.w3._1999.xlinkext.ObjectFactory();

      // workspace for results
      final GMLWorkspace resultWorkspace = CopyObservationMappingHelper.createMappingWorkspace( m_modelURL );

      // 1. load srcgml
      logger.log( " lade Modell " + m_modelURL );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( m_modelURL );

      // 2. locate features to process
      final FeaturePath path = new FeaturePath( m_featurePathTarget );
      final FeatureList feList = (FeatureList)path.getFeature( workspace );
      // loop all features
      for( int i = 0; i < feList.size(); i++ )
      {
        final Feature targetFE = (Feature)feList.get( i );
        // 3. find target
        final TimeseriesLink targetLink = (TimeseriesLink)targetFE.getProperty( m_propZMLTarget );
        final URL targetURL = urlResolver.resolveURL( m_targetContext, targetLink.getHref() );
        // 4. build n-operation filter
        final NOperationFilter nOperationFilter = filterFac.createNOperationFilter();
        nOperationFilter.setOperator( "+" );
        final List filterList = nOperationFilter.getFilter();

        // 5. resolve weights
        final Feature[] weightFEs = workspace.resolveLinks( targetFE, m_propRelationWeightMember );
        
        // 6. loop weights
        for( int j = 0; j < weightFEs.length; j++ )
        {
          // 7. resolve feature that has source zml reference
          final Feature sourceFE = workspace.resolveLink( weightFEs[j], m_propRelationSourceFeature );
          if( sourceFE == null )
          {
            logger.log( "Linked source feature missing in Feature: " + weightFEs[j].getId() );
            
            // IMPORTANT: just skips this weight; leads probably to wrong results
            continue;
          }
          
          // 8. resolve property that is source zml reference
          final TimeseriesLink zmlLink = (TimeseriesLink)sourceFE.getProperty( m_propZMLSource );
          if( zmlLink == null )
          {
            logger.log( "Linked timeserie link missing in Feature: " + weightFEs[j].getId() );
            
            // IMPORTANT: just skips this weight; leads probably to wrong results
            continue;
          }
          
          // 9. build operation filter with parameters from gml
          final OperationFilter filter = filterFac.createOperationFilter();
          filterList.add( filter );
          filter.setOperator( "*" );
          double factor = ( (Double)weightFEs[j].getProperty( m_propWeight ) ).doubleValue();
          filter.setOperand( Double.toString( factor ) );
          final ZmlFilter zmlFilter = filterFac.createZmlFilter();
          filter.setFilter( zmlFilter );
          final SimpleLinkType simpleLink = linkFac.createSimpleLinkType();
          simpleLink.setHref( zmlLink.getHref() );
          zmlFilter.setZml( simpleLink );
        }
        // 10. serialize filter to string
        final Writer writer = new StringWriter();
        marshaller.marshal( nOperationFilter, writer );
        writer.close();
        final String string = XMLUtilities.removeXMLHeader( writer.toString() );
        final String filterInline = XMLUtilities.prepareInLine( string ) + "#useascontext";
        
        // 11. add mapping to result workspace
        CopyObservationMappingHelper.addMapping( resultWorkspace, filterInline, targetURL.toExternalForm() );
        logger.log( " Ziel-ZML " + targetURL );
      }
      // 13. do the mapping
      CopyObservationMappingHelper.runMapping( resultWorkspace, urlResolver, m_modelURL, logger, new Date( m_from ),
          new Date( m_forecastFrom ), new Date( m_to ), true );

      // 14. serialize result workspace to file
      if( m_targetMapping != null )
      {
        FileWriter writer = null;
        try
        {
          writer = new FileWriter( m_targetMapping );
          GmlSerializer.serializeWorkspace( writer, resultWorkspace );
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

  /**
   * 
   * @param targetMapping
   *          gml file that will be generated and includes the mapping that will be generated from the model
   */
  public final void setTargetMapping( final File targetMapping )
  {
    m_targetMapping = targetMapping;
  }

  /**
   * 
   * @param modelURL
   *          reference to the model that describes the mapping in a gml structure
   */
  public final void setModelURL( URL modelURL )
  {
    m_modelURL = modelURL;
  }

  /**
   * 
   * @param targetContext
   *          context to use
   */
  public final void setTargetContext( URL targetContext )
  {
    m_targetContext = targetContext;
  }

  /**
   * 
   * @param featurePathTarget
   *          path to the features that contain the ZML-target properties and the references to weighting features
   */
  public final void setFeaturePathTarget( String featurePathTarget )
  {
    m_featurePathTarget = featurePathTarget;
  }

  /**
   * 
   * @param propRelationSourceFeature
   *          name of property that links from weighting feature to zml source feature
   */
  public final void setPropRelationSourceFeature( String propRelationSourceFeature )
  {
    m_propRelationSourceFeature = propRelationSourceFeature;
  }

  /**
   * 
   * @param propRelationWeightMember
   *          name of property that links from zml target feature to the list of weighting features
   */
  public final void setPropRelationWeightMember( String propRelationWeightMember )
  {
    m_propRelationWeightMember = propRelationWeightMember;
  }

  /**
   * 
   * @param propWeight
   *          property name of the weighting property, feature property type must be double
   */
  public final void setPropWeight( String propWeight )
  {
    m_propWeight = propWeight;
  }

  /**
   * 
   * @param propZMLSource
   *          property name of the zml source property, feature property type must be TimeSeriesLink
   */
  public final void setPropZMLSource( String propZMLSource )
  {
    m_propZMLSource = propZMLSource;
  }

  /**
   * 
   * @param propZMLTarget
   *          property name of the zml target property, feature property type must be TimeSeriesLink
   */
  public final void setPropZMLTarget( String propZMLTarget )
  {
    m_propZMLTarget = propZMLTarget;
  }

  /**
   * 
   * @param from
   *          beginning of measure periode
   */
  public final void setFrom( long from )
  {
    m_from = from;
  }

  /**
   * 
   * @param forecastFrom
   *          beginning of forecast periode (end of measure periode)
   *  
   */
  public final void setForecastFrom( long forecastFrom )
  {
    m_forecastFrom = forecastFrom;
  }

  /**
   * 
   * @param to
   *          end of forecast periode
   */
  public final void setTo( long to )
  {
    m_to = to;
  }

}
