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
package org.kalypso.convert.namodel.manager;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.LineNumberReader;
import java.io.Writer;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author huebsch
 */

public class LzsimManager
{
  private final static int STATUS_SEARCH_HEADER = 0;

  private static final int STATUS_READ_SNOW = 1;

  private static final int STATUS_READ_GWSP = 2;

  private static final int STATUS_READ_BODF = 3;

  private static final int STATUS_READ_QGS = 4;

  private final Date[] m_initialDates;

  public LzsimManager( Date[] initialDates )
  {
    m_initialDates = initialDates;
  }

  /**
   * Reads the initial values back from the ascii files, if any have been ordered.
   */
  public void readInitialValues( final IDManager idManager, final File tmpDir, final Logger logger, final File outputDir ) throws Exception
  {
    if( m_initialDates.length == 0 )
      return;

    // REMARK: we omit any hour here, as the calculation core does not support it. Probably this is a bug of the calculation core, 
    // even if the people responsible for this do not recognise it. In the input file of the calculation core the hour is specified, but
    // the produced date is always written without hour information ('00').
    final DateFormat formatFileName = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyyMMdd" ) ); //$NON-NLS-1$
    // 19960521 00 h 125 bodf
    final DateFormat dateFormat = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyyMMdd 00" ) ); //$NON-NLS-1$
    final Pattern patternHeaderBODF = Pattern.compile( "([0-9]{8} [0-9]{2}) h ([0-9]+?) bodf" ); //$NON-NLS-1$

    for( Date initialDate : m_initialDates )
    {
      final String iniDate = dateFormat.format( initialDate );

      // create new GMLworkspace for lzsim results
      final File lzsimDir = new File( tmpDir, "lzsim" ); //$NON-NLS-1$
      final GMLWorkspace lzWorkspace = FeatureFactory.createGMLWorkspace( new QName( NaModelConstants.NS_INIVALUES, "InitialValues" ), null, null ); //$NON-NLS-1$
      final Feature lzRootFE = lzWorkspace.getRootFeature();
      final XMLGregorianCalendar xmlIniDate = DateUtilities.toXMLGregorianCalendar( initialDate );
      lzRootFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "iniDate" ), xmlIniDate ); //$NON-NLS-1$

      final IFeatureType lzCatchmentFT = lzWorkspace.getGMLSchema().getFeatureType( new QName( NaModelConstants.NS_INIVALUES, "Catchment" ) ); //$NON-NLS-1$
      final IFeatureType lzChannelFT = lzWorkspace.getGMLSchema().getFeatureType( new QName( NaModelConstants.NS_INIVALUES, "Channel" ) ); //$NON-NLS-1$
      final IFeatureType lzrootFT = lzWorkspace.getGMLSchema().getFeatureType( new QName( NaModelConstants.NS_INIVALUES, "InitialValues" ) ); //$NON-NLS-1$

      final IRelationType lzCatchmentMemberRT = (IRelationType) lzrootFT.getProperty( NaModelConstants.INI_CATCHMENT_MEMBER_PROP );
      final IRelationType lzChannelMemberRT = (IRelationType) lzrootFT.getProperty( NaModelConstants.INI_CHANNEL_MEMBER_PROP );

      final IRelationType lzinitHydMemberRT = (IRelationType) lzCatchmentFT.getProperty( NaModelConstants.INI_CATCHMENT_LINK_HYD_PROP );
      final List<Feature> CatchmentFEs = idManager.getAllFeaturesFromType( IDManager.CATCHMENT );
      final List<Feature> ChannelFEs = idManager.getAllFeaturesFromType( IDManager.CHANNEL );

      // iterate over catchments / lzsim-files
      for( final Feature feature : CatchmentFEs )
      {
        final Feature lzCatchmentFE = lzWorkspace.createFeature( lzRootFE, lzCatchmentMemberRT, lzCatchmentFT );
        lzWorkspace.addFeatureAsComposition( lzRootFE, lzCatchmentMemberRT, 0, lzCatchmentFE );
        lzCatchmentFE.setProperty( NaModelConstants.INI_HYD_FEATUREID_PROP, feature.getId() );

        final int asciiID = idManager.getAsciiID( feature );
        lzCatchmentFE.setProperty( new QName( "http://www.opengis.net/gml", "name" ), Integer.toString( asciiID ) ); //$NON-NLS-1$ //$NON-NLS-2$
        final String fileName = "we" + asciiID + ".lzs";  //$NON-NLS-1$//$NON-NLS-2$
        try
        {
          final File lzsFile = new File( lzsimDir, fileName );
          final FileReader fileReader = new FileReader( lzsFile );
          final LineNumberReader reader = new LineNumberReader( fileReader );

          int status = STATUS_SEARCH_HEADER;
          String line = null;
          int maxHydros = 0;
          int counterHydros = 0;
// TODO: why compare the datum? IT would be more robust to just read all whta is in the lsz file, and append it to the
          // features!
          // iterate over lines in file
          while( (line = reader.readLine()) != null )
          {
            line = line.trim().replaceAll( "\\s+", " " ); //$NON-NLS-1$ //$NON-NLS-2$
            switch( status )
            {
              case STATUS_SEARCH_HEADER:
                final Matcher matcherBODF = patternHeaderBODF.matcher( line );
                if( line.endsWith( "snow" ) && line.startsWith( iniDate ) ) //$NON-NLS-1$
                  status = STATUS_READ_SNOW;
                else if( line.endsWith( "gwsp" ) && line.startsWith( iniDate ) ) //$NON-NLS-1$
                  status = STATUS_READ_GWSP;
                else if( matcherBODF.matches() && line.startsWith( iniDate ) )
                {
                  // System.out.println( RegexpUtilities.toGroupInfoString( matcherBODF ) );
                  status = STATUS_READ_BODF;
                  maxHydros = Integer.parseInt( matcherBODF.group( 2 ) );
                }
                break;
              case STATUS_READ_BODF:
              {
                final Feature lzHydFE = lzWorkspace.createFeature( lzCatchmentFE, lzinitHydMemberRT, lzinitHydMemberRT.getTargetFeatureType() );
                lzWorkspace.addFeatureAsComposition( lzCatchmentFE, lzinitHydMemberRT, 0, lzHydFE );
                final String[] strings = line.split( " " ); //$NON-NLS-1$
                final int pos = Integer.parseInt( strings[0] ) - 1;
                final String hydroID = idManager.getHydroFeatureId( feature, pos );
                lzHydFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "featureId" ), hydroID ); //$NON-NLS-1$
                final Double interception = Double.valueOf( strings[1] );
                final List<Double> bofs = new ArrayList<Double>();
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
                  status = STATUS_SEARCH_HEADER;
                  counterHydros = 0;
                }
              }
                break;
              case STATUS_READ_GWSP:
              {
                final String[] strings = line.split( " " ); //$NON-NLS-1$
                final Double hgws = Double.valueOf( strings[1] );// hoehe gw
                final Double qb = Double.valueOf( strings[2] );// basisabfluss
                lzCatchmentFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "hgws" ), hgws ); //$NON-NLS-1$
                lzCatchmentFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "qb" ), qb ); //$NON-NLS-1$
                status = STATUS_SEARCH_HEADER;
              }
                break;
              case STATUS_READ_SNOW:
                final String[] strings = line.split( " " ); //$NON-NLS-1$
                final Double h = Double.valueOf( strings[1] );// hoehe schnee
                final Double ws = Double.valueOf( strings[2] );// wassergehalt
                lzCatchmentFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "h" ), h ); //$NON-NLS-1$
                lzCatchmentFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "ws" ), ws ); //$NON-NLS-1$
                status = STATUS_SEARCH_HEADER;
                break;
            }
          }// TODO: if we reach this line without any read data, something is wrong!
        }
        catch( final Exception e )
        {
          System.out.println( Messages.getString("org.kalypso.convert.namodel.manager.LzsimManager.27", asciiID,feature.getProperty( NaModelConstants.GML_FEATURE_NAME_PROP ) )); //$NON-NLS-1$ 
        }
      }

      // iterate over channels / lzsim-files
      for( final Feature feature : ChannelFEs )
      {
        final int asciiID = idManager.getAsciiID( feature );
        final String fileName = "we" + asciiID + ".lzg";  //$NON-NLS-1$//$NON-NLS-2$
        final File lzgFile = new File( lzsimDir, fileName );
        if( !lzgFile.exists() )
        {
          continue;
        }
        final Feature lzChannelFE = lzWorkspace.createFeature( lzRootFE, lzChannelMemberRT, lzChannelFT );
        lzWorkspace.addFeatureAsComposition( lzRootFE, lzChannelMemberRT, 0, lzChannelFE );
        lzChannelFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "featureId" ), feature.getId() ); //$NON-NLS-1$
        lzChannelFE.setProperty( new QName( "http://www.opengis.net/gml", "name" ), Integer.toString( asciiID ) ); //$NON-NLS-1$ //$NON-NLS-2$
        final FileReader fileReader = new FileReader( lzgFile );
        final LineNumberReader reader = new LineNumberReader( fileReader );

        int status = STATUS_SEARCH_HEADER;
        String line = null;

        // iterate over lines in file
        while( (line = reader.readLine()) != null )
        {
          line = line.trim().replaceAll( "\\s+", " " ); //$NON-NLS-1$ //$NON-NLS-2$
          switch( status )
          {
            case STATUS_SEARCH_HEADER:
              if( line.endsWith( "qgs" ) && line.startsWith( iniDate ) ) //$NON-NLS-1$
                // 19960521 00 h 1 qgs
                // 1 0.000
                status = STATUS_READ_QGS;
              break;
            case STATUS_READ_QGS:
            {
              final String[] strings = line.split( " " ); //$NON-NLS-1$
              final Double qgs = Double.valueOf( strings[1] );// Gesamtabfluss
              lzChannelFE.setProperty( new QName( NaModelConstants.NS_INIVALUES, "qgs" ), qgs ); //$NON-NLS-1$
              status = STATUS_SEARCH_HEADER;
            }
              break;
          }
        }
      }// TODO: check, if we read any data for this date
      final String resultPathRelative = "Ergebnisse/Aktuell/Anfangswerte/" + formatFileName.format( initialDate ) + ".gml";   //$NON-NLS-1$//$NON-NLS-2$
      final File resultFile = new File( outputDir, resultPathRelative );
      resultFile.getParentFile().mkdirs();
      GmlSerializer.serializeWorkspace( resultFile, lzWorkspace, "UTF-8" ); //$NON-NLS-1$
      logger.info( Messages.getString("org.kalypso.convert.namodel.manager.LzsimManager.42", iniDate )); //$NON-NLS-1$
    }
  }

  public static void writeLzsimFiles( final IDManager idManager, final File tmpDir, final GMLWorkspace iniValuesWorkspace )
  {
    final List<Feature> allNAChannelFeatures = idManager.getAllFeaturesFromType( IDManager.CHANNEL );
    final Hashtable<String, Feature> channelIDToFeatureHash = new Hashtable<String, Feature>();
    for( final Feature feature : allNAChannelFeatures )
      channelIDToFeatureHash.put( feature.getId(), feature );

    final List<Feature> allNACatchmentFeatures = idManager.getAllFeaturesFromType( IDManager.CATCHMENT );
    final Hashtable<String, Feature> catchmentIDToFeatureHash = new Hashtable<String, Feature>();
    for( final Feature feature : allNACatchmentFeatures )
      catchmentIDToFeatureHash.put( feature.getId(), feature );

    final File lzsimDir = new File( tmpDir, "lzsim" ); //$NON-NLS-1$
    final Feature iniValuesRootFeature = iniValuesWorkspace.getRootFeature();
    // Initial value date
    final Date initialDate = DateUtilities.toDate( (XMLGregorianCalendar) iniValuesRootFeature.getProperty( new QName( NaModelConstants.NS_INIVALUES, "iniDate" ) ) ); //$NON-NLS-1$
    final DateFormat dateFormat = NATimeSettings.getInstance().getLzsLzgDateFormat();
    final String iniDate = dateFormat.format( initialDate );

    // write initial conditions for the strands
    // TODO:write only for strands of the actual calculation
    final List<?> channelList = (List<?>) iniValuesRootFeature.getProperty( NaModelConstants.INI_CHANNEL_MEMBER_PROP );
    for( int i = 0; i < channelList.size(); i++ )
    {
      final Feature channelFE = (Feature) channelList.get( i );
      final StringBuffer lzgBuffer = new StringBuffer();
      final String naChannelID = (String) channelFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "featureId" ) ); //$NON-NLS-1$

      final Feature naChannelFE = channelIDToFeatureHash.get( naChannelID );
      // in the channelIDToFeatureHash (HashMap<featureID, feature>) are all channels in the model. if this run is only
      // a subnet of the total model the idManager only knows the featuers in the submodel just skip this one.
      if( naChannelFE == null )
        continue;
      final int asciiChannelID = idManager.getAsciiID( naChannelFE );
      final String fileName = "we" + asciiChannelID + ".lzg";  //$NON-NLS-1$//$NON-NLS-2$

      try
      {
        final File lzgFile = new File( lzsimDir, fileName );
        lzgBuffer.append( iniDate + " h   1 qgs" + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
        final Double h = (Double) channelFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "qgs" ) ); //$NON-NLS-1$
        lzgBuffer.append( "   1" + FortranFormatHelper.printf( h, "f9.3" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        final Writer lzgWriter = new FileWriter( lzgFile );
        lzgWriter.write( lzgBuffer.toString() );
        lzgWriter.close();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        System.out.println( Messages.getString("org.kalypso.convert.namodel.manager.LzsimManager.55", channelFE.getId()) ); //$NON-NLS-1$
      }
    }

    // for all catchments in the calculation - in the hydrohash(catchmentsIDs, list of hydrotopesIDs)
    final List<?> catchmentList = (List<?>) iniValuesRootFeature.getProperty( NaModelConstants.INI_CATCHMENT_MEMBER_PROP );
    final Set<String> catchmentIdsFromLzsim = idManager.getCatchmentIdsFromLzsim();
    for( final String catchmentID : catchmentIdsFromLzsim )
    {
      final StringBuffer lzsBuffer = new StringBuffer();

      final Feature naCatchmentFE = catchmentIDToFeatureHash.get( catchmentID );
      // in the catchmentIDToFeatureHash (HashMap<featureID, feature>) are all channels in the model. if this run is
      // only
      // a subnet of the total model the idManager only knows the featuers in the submodel just skip this one.
      if( naCatchmentFE == null )
        continue;
      final int asciiCatchmentID = idManager.getAsciiID( naCatchmentFE );
      final String fileName = "we" + asciiCatchmentID + ".lzs";  //$NON-NLS-1$//$NON-NLS-2$

      final File lzsFile = new File( lzsimDir, fileName );
      final List<String> sortedHydrosIDsfromLzsim = idManager.getSortedHydrosIDsfromLzsim( catchmentID );
      // find catchmentID in the iniValues
      for( int i = 0; i < catchmentList.size(); i++ )
      {
        final Feature catchmentFE = (Feature) catchmentList.get( i );
        // write lzs for the catchment
        if( catchmentFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "featureId" ) ).equals( catchmentID ) ) //$NON-NLS-1$
        {
          try
          {
            // snow
            lzsBuffer.append( iniDate + " h     1 snow" + "\n" );  //$NON-NLS-1$//$NON-NLS-2$
            final Double h = (Double) catchmentFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "h" ) ); //$NON-NLS-1$
            final Double ws = (Double) catchmentFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "ws" ) ); //$NON-NLS-1$
            lzsBuffer.append( "   1" + FortranFormatHelper.printf( h, "f9.2" ) + FortranFormatHelper.printf( ws, "f9.2" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
            // groundwater
            lzsBuffer.append( iniDate + " h     1 gwsp" + "\n" );  //$NON-NLS-1$//$NON-NLS-2$
            final Double hgws = (Double) catchmentFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "hgws" ) ); //$NON-NLS-1$
            final Double qb = (Double) catchmentFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "qb" ) ); //$NON-NLS-1$
            lzsBuffer.append( "   1" + FortranFormatHelper.printf( hgws, "f9.2" ) + FortranFormatHelper.printf( qb, "f9.2" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
            // hydrotops (interception storage content& soil moisture)
            int hydroPos = 0;
            final List<?> iniHydsList = (List<?>) catchmentFE.getProperty( new QName( NaModelConstants.NS_INIVALUES, "hyd" ) ); //$NON-NLS-1$
            lzsBuffer.append( iniDate + " h  " + FortranFormatHelper.printf( Integer.toString( iniHydsList.size() ), "i4" ) + " bodf" + "\n" );  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$//$NON-NLS-4$
            for( final String hydroID : sortedHydrosIDsfromLzsim )
            {
              hydroPos++;
              final Iterator<?> iter = iniHydsList.iterator();
              while( iter.hasNext() )
              {
                final Feature iniHydFe = (Feature) iter.next();
                // write initial parameters for the hydrotop
                final String hydFeatureId = (String) iniHydFe.getProperty( new QName( NaModelConstants.NS_INIVALUES, "featureId" ) ); //$NON-NLS-1$
                if( hydFeatureId.equals( hydroID ) )
                {
                  final Double bi = (Double) iniHydFe.getProperty( new QName( NaModelConstants.NS_INIVALUES, "bi" ) ); //$NON-NLS-1$
                  lzsBuffer.append( FortranFormatHelper.printf( hydroPos, "i4" ) + FortranFormatHelper.printf( bi, "f7.2" ) );  //$NON-NLS-1$//$NON-NLS-2$
                  final List<Double> bofs = (List<Double>) iniHydFe.getProperty( new QName( NaModelConstants.NS_INIVALUES, "bofs" ) ); //$NON-NLS-1$
                  for( final Double bof : bofs )
                  {
                    lzsBuffer.append( FortranFormatHelper.printf( bof, "f7.2" ) ); //$NON-NLS-1$
                  }
                  lzsBuffer.append( "\n" ); //$NON-NLS-1$
                }
              }
            }
            final Writer lzsWriter = new FileWriter( lzsFile );
            lzsWriter.write( lzsBuffer.toString() );
            lzsWriter.close();
          }
          catch( final Exception e )
          {
            System.out.println( Messages.getString("org.kalypso.convert.namodel.manager.LzsimManager.87",catchmentFE.getId() ) ); //$NON-NLS-1$
          }
        }
      }
    }
  }

  /**
   * Read initial dates from gml-configuration.
   */
  public static Date[] getInitialDates( final Feature controlFE )
  {
    final List< ? > dateList = (List< ? >) controlFE.getProperty( NaModelConstants.NACONTROL_INITIALVALUEDATE_PROP );
    if( dateList == null )
      return new Date[0];

    final TreeSet<Date> dateWriteSet = new TreeSet<Date>();
    for( final Object object : dateList )
    {
      final Feature fe = (Feature) object;
      final Boolean write = (Boolean) fe.getProperty( NaModelConstants.NACONTROL_WRITE_PROP );
      // by default it is false now, but for backward compatibility check if there is any value
      if( write != null && write.booleanValue() )
      {
        final Date initialDate = DateUtilities.toDate( (XMLGregorianCalendar) fe.getProperty( NaModelConstants.NACONTROL_INITIALDATE_PROP ) );
        dateWriteSet.add( initialDate );
      }
    }

    return dateWriteSet.toArray( new Date[dateWriteSet.size()] );
  }}