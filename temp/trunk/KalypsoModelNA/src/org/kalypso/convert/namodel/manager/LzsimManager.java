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
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
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

import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.java.io.filter.MultipleWildCardFileFilter;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.contribs.java.util.regex.RegexpUtilities;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

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

  private void loadIniValues( final File tmpDir, final Logger logger, final ISimulationResultEater resultEater )
  {
    try
    {
      final String[] wildcards = new String[] { "*" + "lzs", "*" + "lzg" };
      File lzsimDir = new File( tmpDir, "lzsim" );
      final MultipleWildCardFileFilter filter = new MultipleWildCardFileFilter( wildcards, false, false, true );
      final File[] lzsimFiles = lzsimDir.listFiles( filter );
      File lzsimZIP = new File( tmpDir, "lzsim.zip" );
      try
      {
        ZipUtilities.zip( lzsimZIP, lzsimFiles, lzsimDir );
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
      resultEater.addResult( NaModelConstants.LZSIM_OUT_ID, lzsimZIP );
    }
    catch( SimulationException e )
    {
      e.printStackTrace();
      logger.info( e.getMessage() );
    }
  }

  public static void initialValues( final IDManager idManager, final File tmpDir, final Logger logger, final ISimulationResultEater resultEater ) throws Exception
  {
    //TODO: implement for different dates!
    // create new GMLworkspace for lzsim results
    File lzsimDir = new File( tmpDir, "lzsim" );
    final String ns = "http://www.tuhh.de/initialValues";
    final GMLWorkspace lzWorkspace = FeatureFactory.createGMLWorkspace( ns, new QName( ns, "InitialValues" ) );
    final Feature lzRootFE = lzWorkspace.getRootFeature();
    
    final IFeatureType lzCatchmentFT = lzWorkspace.getGMLSchema().getFeatureType( new QName( ns, "Catchment" ) );
    final IFeatureType lzChannelFT = lzWorkspace.getGMLSchema().getFeatureType( new QName( ns, "Channel" ) );
    final IFeatureType lzrootFT = lzWorkspace.getGMLSchema().getFeatureType( new QName( ns, "InitialValues" ) );

    final IRelationType lzCatchmentMemberRT = (IRelationType) lzrootFT.getProperty( new QName( ns, "catchmentMember" ) );
    final IRelationType lzChannelMemberRT = (IRelationType) lzrootFT.getProperty( new QName( ns, "channelMember" ) );
    
    final IRelationType lzinitHydMemberRT = (IRelationType) lzCatchmentFT.getProperty( new QName( ns, "hyd" ) );
    final List<Feature> CatchmentFEs = idManager.getAllFeaturesFromType( IDManager.CATCHMENT );
    final List<Feature> ChannelFEs = idManager.getAllFeaturesFromType( IDManager.CHANNEL );
    DateFormat dateFormat = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyyMMdd HH" ) );
    // 19960521 00 h 125 bodf
    final Pattern patternHeaderBODF = Pattern.compile( "([0-9]{8} [0-9]{2}) h ([0-9]+?) bodf" );
    // iterate over catchments / lzsim-files
    for( final Feature feature : CatchmentFEs )
    {
      final Feature lzCatchmentFE = lzWorkspace.createFeature( lzRootFE, lzCatchmentFT );
      lzWorkspace.addFeatureAsComposition( lzRootFE, lzCatchmentMemberRT, 0, lzCatchmentFE );
      lzCatchmentFE.setProperty( new QName( ns, "featureId" ), feature.getId() );

      final int asciiID = idManager.getAsciiID( feature );
      final String fileName = "we" + asciiID + ".lzs";
      final File lzsFile = new File( lzsimDir, fileName );
      final FileReader fileReader = new FileReader( lzsFile );
      final LineNumberReader reader = new LineNumberReader( fileReader );

      int status = STATUS_SEARCH_HEADER;
      String line = null;
      int maxHydros = 0;
      int counterHydros = 0;

      // iterate over lines in file
      while( (line = reader.readLine()) != null )
      {
        line = line.trim().replaceAll("\\s+"," ");
        switch( status )
        {
          case STATUS_SEARCH_HEADER:
            final Matcher matcherBODF = patternHeaderBODF.matcher( line );
            if( line.endsWith( "snow" ) && line.startsWith("19960824"))
              status = STATUS_READ_SNOW;
            else if( line.endsWith( "gwsp" ) && line.startsWith("19960824"))
              status = STATUS_READ_GWSP;
            else if( matcherBODF.matches() && line.startsWith("19960824"))
            {
              System.out.println( RegexpUtilities.toGroupInfoString( matcherBODF ) );
              status = STATUS_READ_BODF;
              String dateString = matcherBODF.group( 1 );
              Date date = dateFormat.parse(dateString);
              XMLGregorianCalendar calendarDate = DateUtilities.toXMLGregorianCalendar(date);
              lzRootFE.setProperty(new QName(ns,"iniDate"),calendarDate);
              maxHydros = Integer.parseInt( matcherBODF.group( 2 ) );
            }
            break;
          case STATUS_READ_BODF:
          {
            final Feature lzHydFE = lzWorkspace.createFeature( lzCatchmentFE, lzinitHydMemberRT.getTargetFeatureType() );
            lzWorkspace.addFeatureAsComposition( lzCatchmentFE, lzinitHydMemberRT, 0, lzHydFE );
            final String[] strings = line.split( " " );
            final int pos = Integer.parseInt( strings[0] )-1;
            final String hydroID = idManager.getHydroFeatureId( feature, pos );
            lzHydFE.setProperty( new QName( ns, "featureId" ), hydroID );
            final Double interception = Double.valueOf( strings[1] );
            final List<Double> bofs = new ArrayList<Double>();
            for( int i = 2; i < strings.length; i++ )
            {
              final Double bf = Double.valueOf( strings[i] );
              bofs.add( bf );
            }
            lzHydFE.setProperty( new QName( ns, "bi" ), interception );
            lzHydFE.setProperty( new QName( ns, "bofs" ), bofs );
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
            final String[] strings = line.split( " " );
            final Double hgws = Double.valueOf( strings[1] );// hoehe gw
            final Double qb = Double.valueOf( strings[2]);// basisabfluss
            lzCatchmentFE.setProperty( new QName( ns, "hgws" ), hgws );
            lzCatchmentFE.setProperty( new QName( ns, "qb" ), qb );
            status = STATUS_SEARCH_HEADER;
          }
            break;
          case STATUS_READ_SNOW:
            final String[] strings = line.split( " " );
            final Double h = Double.valueOf( strings[1] );// hoehe schnee
            final Double ws = Double.valueOf( strings[2] );// wassergehalt
            lzCatchmentFE.setProperty( new QName( ns, "h" ), h );
            lzCatchmentFE.setProperty( new QName( ns, "ws" ), ws );
            status = STATUS_SEARCH_HEADER;
            break;
        }
      }

    }

    // iterate over channels / lzsim-files
    for( final Feature feature : ChannelFEs )
    {
      final int asciiID = idManager.getAsciiID( feature );
      final String fileName = "we" + asciiID + ".lzg";
      final File lzgFile = new File( lzsimDir, fileName );
      if( !lzgFile.exists() )
      {
        continue;
      }
      final Feature lzChannelFE = lzWorkspace.createFeature( lzRootFE, lzChannelFT );
      lzWorkspace.addFeatureAsComposition( lzRootFE, lzChannelMemberRT, 0, lzChannelFE );
      lzChannelFE.setProperty( new QName( ns, "featureId" ), feature.getId() );
      final FileReader fileReader = new FileReader( lzgFile );
      final LineNumberReader reader = new LineNumberReader( fileReader );

      int status = STATUS_SEARCH_HEADER;
      String line = null;

      // iterate over lines in file
      while( (line = reader.readLine()) != null )
      {
        line = line.trim().replaceAll("\\s+"," ");
        switch( status )
        {
          case STATUS_SEARCH_HEADER:
            // TODO read date
            if( line.endsWith( "qgs" )&& line.startsWith("19960824") )
              // 19960521 00 h 1 qgs
              // 1 0.000
              status = STATUS_READ_QGS;
            break;
          case STATUS_READ_QGS:
          {
            final String[] strings = line.split( " " );
            final Double qgs = Double.valueOf( strings[1] );// Gesamtabfluss
            lzChannelFE.setProperty( new QName( ns, "qgs" ), qgs );
            status = STATUS_SEARCH_HEADER;
          }
            break;
        }
      }
    }
    final File lzsimGML = new File( tmpDir, "lzsim.gml" );
    GmlSerializer.serializeWorkspace( lzsimGML, lzWorkspace, "UTF-8" );
    resultEater.addResult( NaModelConstants.LZSIM_OUT_ID, lzsimGML );
  }
}
