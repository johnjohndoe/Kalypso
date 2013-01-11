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
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.NATimeSettings;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.NaCatchmentData;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Gernot Belger
 */
public class LzsToGml
{
  private static final Pattern PATTERN_HEADER_BODF = Pattern.compile( "([0-9]{8} [0-9]{2}) h ([0-9]+?) bodf" ); //$NON-NLS-1$

  private static enum CatchmentStatus
  {
    SEARCH_HEADER,
    READ_SNOW,
    READ_GWSP,
    READ_BODF,
  }

  private static enum ChannelStatus
  {
    SEARCH_HEADER,
    READ_QGS;
  }

  // REMARK: we omit any hour here, as the calculation core does not support it. Probably this is a bug of the
  // calculation core, even if the people responsible for this do not recognize it. In the input file of the
  // calculation core the hour is specified, but the produced date is always written without hour information ('00').
  private final DateFormat m_formatFileName = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyyMMdd'.gml'" ) ); //$NON-NLS-1$

  // 19960521 00 h 125 bodf
  private final DateFormat m_dateFormat = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyyMMdd 00" ) ); //$NON-NLS-1$

  private final Date m_initialDate;

  private final GMLWorkspace m_lzWorkspace;

  private final Logger m_logger;

  private final IDManager m_idManager;

  private final File m_lzsimDir;

  private final NaCatchmentData m_catchmentData;

  public LzsToGml( final File lzsimDir, final Date initialDate, final IDManager idManager, final NaCatchmentData catchmentData, final Logger logger ) throws GMLSchemaException
  {
    m_lzsimDir = lzsimDir;
    m_initialDate = initialDate;
    m_idManager = idManager;
    m_catchmentData = catchmentData;
    m_logger = logger;
    m_lzWorkspace = FeatureFactory.createGMLWorkspace( new QName( NaModelConstants.NS_INIVALUES, "InitialValues" ), null, null ); //$NON-NLS-1$
  }

  public void readLzs( ) throws Exception
  {
    // create new GMLworkspace for lzsim results
    final Feature lzRootFE = m_lzWorkspace.getRootFeature();
    final XMLGregorianCalendar xmlIniDate = DateUtilities.toXMLGregorianCalendar( m_initialDate );
    lzRootFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "iniDate" ), xmlIniDate ); //$NON-NLS-1$

    final List<Feature> catchments = m_idManager.getAllFeaturesFromType( IDManager.CATCHMENT );

    for( final Feature feature : catchments )
      readCatchmentStartCondition( (Catchment)feature );

    final List<Feature> channels = m_idManager.getAllFeaturesFromType( IDManager.CHANNEL );
    for( final Feature feature : channels )
      readChannelStartCondition( (Channel)feature );
  }

  public void writeGml( final File outputDir ) throws IOException, GmlSerializeException
  {
    // TODO: check, if we read any data for this date, else do not write gml

    final String resultFilename = m_formatFileName.format( m_initialDate ); //$NON-NLS-1$
    final File resultFile = new File( outputDir, resultFilename );
    resultFile.getParentFile().mkdirs();
    GmlSerializer.serializeWorkspace( resultFile, m_lzWorkspace, "UTF-8" ); //$NON-NLS-1$

    final String iniDate = m_dateFormat.format( m_initialDate );
    m_logger.info( Messages.getString( "org.kalypso.convert.namodel.manager.LzsimManager.42", iniDate ) ); //$NON-NLS-1$
  }

  private void readCatchmentStartCondition( final Catchment catchment ) throws Exception
  {
    final InitialValues initialValues = (InitialValues)m_lzWorkspace.getRootFeature();

    final IFeatureBindingCollection<org.kalypso.model.hydrology.binding.initialValues.Catchment> catchments = initialValues.getCatchments();
    final org.kalypso.model.hydrology.binding.initialValues.Catchment iniCatchment = catchments.addNew( org.kalypso.model.hydrology.binding.initialValues.Catchment.FEATURE_CATCHMENT );
    iniCatchment.setNaCatchmentID( catchment );

    final int asciiID = m_idManager.getAsciiID( catchment );
    iniCatchment.setName( Integer.toString( asciiID ) ); //$NON-NLS-1$ //$NON-NLS-2$
    final String fileName = String.format( "we%s.lzs", asciiID ); //$NON-NLS-1$
    final File lzsFile = new File( m_lzsimDir, fileName );

    FileReader fileReader = null;
    try
    {
      fileReader = new FileReader( lzsFile );
      readLzsFile( fileReader, catchment, iniCatchment );
      fileReader.close();
    }
    catch( final NumberFormatException e )
    {
      m_logger.severe( String.format( Messages.getString( "LzsToGml.0" ), e.getLocalizedMessage() ) ); //$NON-NLS-1$
    }
    catch( final IOException e )
    {
      m_logger.warning( Messages.getString( "org.kalypso.convert.namodel.manager.LzsimManager.27", catchment.getName() ) ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( fileReader );
    }
  }

  private void readLzsFile( final FileReader fileReader, final Catchment catchment, final Feature lzCatchmentFE ) throws Exception
  {
    final LineNumberReader reader = new LineNumberReader( fileReader );

    final IFeatureType lzCatchmentFT = GMLSchemaUtilities.getFeatureTypeQuiet( new QName( NaModelConstants.NS_INIVALUES, "Catchment" ) ); //$NON-NLS-1$
    final IRelationType lzinitHydMemberRT = (IRelationType)lzCatchmentFT.getProperty( NaModelConstants.INI_CATCHMENT_LINK_HYD_PROP );

    final String iniDate = m_dateFormat.format( m_initialDate );
    int maxHydros = 0;
    int counterHydros = 0;
    // TODO: why compare the datum? IT would be more robust to just read all data that is in the lsz file, and append
    // it to the features!

    CatchmentStatus status = CatchmentStatus.SEARCH_HEADER;
    while( reader.ready() )
    {
      final String line = reader.readLine();
      if( line == null )
        break;

      final String cleanLine = line.trim().replaceAll( "\\s+", " " ); //$NON-NLS-1$ //$NON-NLS-2$
      switch( status )
      {
        case SEARCH_HEADER:
          final Matcher matcherBODF = PATTERN_HEADER_BODF.matcher( cleanLine );
          if( cleanLine.endsWith( "snow" ) && cleanLine.startsWith( iniDate ) ) //$NON-NLS-1$
            status = CatchmentStatus.READ_SNOW;
          else if( cleanLine.endsWith( "gwsp" ) && cleanLine.startsWith( iniDate ) ) //$NON-NLS-1$
            status = CatchmentStatus.READ_GWSP;
          else if( matcherBODF.matches() && cleanLine.startsWith( iniDate ) )
          {
            status = CatchmentStatus.READ_BODF;
            maxHydros = Integer.parseInt( matcherBODF.group( 2 ) );
          }
          break;

        case READ_BODF:
        {
          final Feature lzHydFE = m_lzWorkspace.createFeature( lzCatchmentFE, lzinitHydMemberRT, lzinitHydMemberRT.getTargetFeatureType() );
          m_lzWorkspace.addFeatureAsComposition( lzCatchmentFE, lzinitHydMemberRT, 0, lzHydFE );
          final String[] strings = cleanLine.split( " " ); //$NON-NLS-1$
          final int pos = Integer.parseInt( strings[0] ) - 1;
          final String hydroID = m_catchmentData.getHydroFeatureId( catchment, pos );
          lzHydFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "featureId" ), hydroID ); //$NON-NLS-1$
          final Double interception = Double.valueOf( strings[1] );
          final List<Double> bofs = new ArrayList<>();
          for( int i = 2; i < strings.length; i++ )
          {
            final Double bf = Double.valueOf( strings[i] );
            bofs.add( bf );
          }
          lzHydFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "bi" ), interception ); //$NON-NLS-1$
          lzHydFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "bofs" ), bofs ); //$NON-NLS-1$
          counterHydros++;
          if( counterHydros >= maxHydros )
          {
            status = CatchmentStatus.SEARCH_HEADER;
            counterHydros = 0;
          }
        }
          break;

        case READ_GWSP:
        {
          final String[] strings = cleanLine.split( " " ); //$NON-NLS-1$
          final Double hgws = Double.valueOf( strings[1] );// hoehe gw
          final Double qb = Double.valueOf( strings[2] );// basisabfluss
          lzCatchmentFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "hgws" ), hgws ); //$NON-NLS-1$
          lzCatchmentFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "qb" ), qb ); //$NON-NLS-1$
          status = CatchmentStatus.SEARCH_HEADER;
        }
          break;

        case READ_SNOW:
          final String[] strings = cleanLine.split( " " ); //$NON-NLS-1$
          final Double h = Double.valueOf( strings[1] );// hoehe schnee
          final Double ws = Double.valueOf( strings[2] );// wassergehalt
          lzCatchmentFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "h" ), h ); //$NON-NLS-1$
          lzCatchmentFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "ws" ), ws ); //$NON-NLS-1$
          status = CatchmentStatus.SEARCH_HEADER;
          break;
      }
    }// TODO: if we reach this line without any read data, something is wrong!
  }

  private void readChannelStartCondition( final Channel channel ) throws Exception
  {
    final int asciiID = m_idManager.getAsciiID( channel );
    final String fileName = String.format( "we%s.lzg", asciiID ); //$NON-NLS-1$//$NON-NLS-2$
    final File lzgFile = new File( m_lzsimDir, fileName );
    if( !lzgFile.exists() )
      // FIXME: shoudn't we throw some exception here?
      return;

    final InitialValues initialValues = (InitialValues)m_lzWorkspace.getRootFeature();

    final IFeatureBindingCollection<org.kalypso.model.hydrology.binding.initialValues.Channel> channels = initialValues.getChannels();
    final org.kalypso.model.hydrology.binding.initialValues.Channel iniChannel = channels.addNew( org.kalypso.model.hydrology.binding.initialValues.Channel.FEATURE_CHANNEL );
    iniChannel.setNaChannelID( channel );
    iniChannel.setName( Integer.toString( asciiID ) ); //$NON-NLS-1$ //$NON-NLS-2$

    FileReader fileReader = null;
    try
    {
      fileReader = new FileReader( lzgFile );
      final double qgs = readLzgFile( fileReader );
      fileReader.close();

      if( !Double.isNaN( qgs ) )
        iniChannel.setQgs( qgs );
    }
    finally
    {
      IOUtils.closeQuietly( fileReader );
    }
  }

  private double readLzgFile( final FileReader fileReader ) throws NumberFormatException, IOException
  {
    final LineNumberReader reader = new LineNumberReader( fileReader );

    ChannelStatus status = ChannelStatus.SEARCH_HEADER;

    final String iniDate = m_dateFormat.format( m_initialDate );

    while( reader.ready() )
    {
      final String line = reader.readLine();
      if( line == null )
        break;

      final String cleanLine = line.trim().replaceAll( "\\s+", " " ); //$NON-NLS-1$ //$NON-NLS-2$
      switch( status )
      {
        case SEARCH_HEADER:
          if( cleanLine.endsWith( "qgs" ) && cleanLine.startsWith( iniDate ) ) //$NON-NLS-1$
            // 19960521 00 h 1 qgs
            // 1 0.000
            status = ChannelStatus.READ_QGS;
          break;

        case READ_QGS: // Gesamtabfluss
          final String[] strings = cleanLine.split( " " ); //$NON-NLS-1$
          return Double.valueOf( strings[1] );
      }
    }

    return Double.NaN;
  }
}
