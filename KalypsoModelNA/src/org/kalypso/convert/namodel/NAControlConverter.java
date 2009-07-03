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
package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

import javax.xml.datatype.XMLGregorianCalendar;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

public class NAControlConverter
{
  // graphicTool: types
  public static final int LINE = 0;

  public static final int BLOCK = 1;

  public static final int P = 2;

  public static final int M = 3;

  public static final int T = 4;

  // graphictool: sides for y-axis
  public static final int LEFT = 0;

  public static final int RIGHT = 1;

  private NAControlConverter( )
  {
    // never instantiate
  }

  public static void featureToASCII( NAConfiguration conf, File projectPath, GMLWorkspace controlWorkspace, GMLWorkspace modellWorkspace ) throws IOException
  {
    final Feature controlFE = controlWorkspace.getRootFeature();
    final File startDir = new File( projectPath, "start" );
    startDir.mkdirs();

    final File startFile = new File( startDir, "we_nat_start.txt" );
    final File falStartFile = new File( startDir, "falstart.lst" );

    // write FalStart
    final StringBuffer b1 = new StringBuffer();
    writeFalstart( conf, startFile, b1 );
    // write it
    final FileWriter writer1 = new FileWriter( falStartFile );
    writer1.write( b1.toString() );
    writer1.close();

    // generate Start
    final StringBuffer b = new StringBuffer();
    appendResultsToGenerate( conf, controlFE, b );
    appendResultInformation( modellWorkspace, controlWorkspace, b, conf.getIdManager() );
    // Startwerte fuer die kurzzeitsimulation
    appendInitailDates( controlFE, b, conf );
    // write it
    final FileWriter writer = new FileWriter( startFile );
    writer.write( b.toString() );
    writer.close();
  }

  private static void appendInitailDates( final Feature controlFE, final StringBuffer b, NAConfiguration conf )
  {
    final TreeSet<Date> dateWriteSet = new TreeSet<Date>();
    final DateFormat format = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyyMMdd  HH" ) );
    final List< ? > dateList = (List< ? >) controlFE.getProperty( NaModelConstants.NACONTROL_INITIALVALUEDATE_PROP );
    if( dateList != null )
    {
      final Iterator< ? > iter = dateList.iterator();
      while( iter.hasNext() )
      {
        final Feature fe = (Feature) iter.next();
        final Boolean write = (Boolean) fe.getProperty( NaModelConstants.NACONTROL_WRITE_PROP );
        // by default it is false now, but for backward compatibility check if there is any value
        if( write != null && write.booleanValue() )
        {
          final Date initialDate = DateUtilities.toDate( (XMLGregorianCalendar) fe.getProperty( NaModelConstants.NACONTROL_INITIALDATE_PROP ) );
          dateWriteSet.add( initialDate );
          conf.setIniWrite( true );
        }
      }
    }

    final Iterator<Date> iniIter = dateWriteSet.iterator();
    while( iniIter.hasNext() )
    {
      final String iniDate = format.format( iniIter.next() );
      b.append( iniDate + "\n" );
    }
    b.append( "99999\n" );
    conf.setInitalValues( dateWriteSet );
  }

  private static void appendResultsToGenerate( NAConfiguration conf, Feature controlFE, StringBuffer b )
  {
    int minutesOfTimeStep = conf.getMinutesOfTimeStep();
    double hoursOfTimeStep = minutesOfTimeStep / 60d;
    b.append( " " + hoursOfTimeStep + "\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_TMP_PROP ) ) + "       Temperatur                 .tmp\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_PRE_PROP ) ) + "       Niederschlag               .pre\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_SCH_PROP ) ) + "       Schnee                     .sch\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_BOF_PROP ) ) + "       Bodenfeuchte               .bof\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_BSP_PROP ) ) + "       Bodenspeicher              .bsp\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_GWS_PROP ) ) + "       Grundwasserstand           .gws\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_QGS_PROP ) ) + "       Gesamtabfluss Knoten       .qgs\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_QGG_PROP ) ) + "       Gesamtabfluss TG           .qgg\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_QNA_PROP ) ) + "       nat. Oberflaechenabfluss   .qna\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_QIF_PROP ) ) + "       Interflow                  .qif\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_QVS_PROP ) ) + "       Abfluss vers. Flaechen     .qvs\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_QBS_PROP ) ) + "       Basisabfluss               .qbs\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_QT1_PROP ) ) + "       Kluftgrundw1               .qt1\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_QTG_PROP ) ) + "       Kluftgrundw                .qtg\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_QGW_PROP ) ) + "       Grundwasserabfluss         .qgw\n" );
    // sollte nicht bei der Ausgabe erzeugt werden, da Berechnung mit kap. Aufstieg noch nicht implementiert!
    b.append( "n" + "       Kapil.Aufstieg/Perkolation .kap\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_VET_PROP ) ) + "       Evapotranspiration         .vet\n" );
    // FIXME die mulden-rigolen sind abhänging von der version der exe. muss erst noch angepasst werden. rechnet jetzt
    // nur mit der v2.5 (ask Christoph)
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_QMR_PROP ) ) + "       Ausgabe MRS                .qmr\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_HYD_PROP ) ) + "       Ausgabe Hydrotope          .hyd\n" );
    // if "2": output of *.txt and *.bil
    if( ((Boolean) (controlFE.getProperty( NaModelConstants.NACONTROL_BIL_PROP ))).booleanValue() )
      b.append( "2" + "       Abflussbilanz              .bil\n" );
    else
      b.append( "n" + "       Abflussbilanz              .bil\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_NMQ_PROP ) ) + "       Statistische Abflusswerte  .nmq\n" );
    // Folgende Dateien werden zusätzlich mit Speicherinhalt generiert .sph, .spv, .spn, .spb
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_SPI_PROP ) ) + "       Speicherinhalt             .spi\n" );
    b.append( getBoolean( controlFE.getProperty( NaModelConstants.NACONTROL_SUP_PROP ) ) + "       Speicherueberlauf          .sup\n" );
  }

  private static void appendResultInformation( final GMLWorkspace modellWorkspace, final GMLWorkspace controlWorkspace, final StringBuffer b, final IDManager idManager )
  {
    // knoten
    final IFeatureType nodeFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final Feature[] nodeFEs = modellWorkspace.getFeatures( nodeFT );
    // boolean onlyRootNodeResult = FeatureHelper.booleanIsTrue( controlWorkspace.getRootFeature(),
    // "resultForRootNodeOnly", true );
    final String rootNodeID = (String) controlWorkspace.getRootFeature().getProperty( NaModelConstants.NACONTROL_ROOTNODE_PROP );
    for( int i = 0; i < nodeFEs.length; i++ )
    {
      // fuer root node immer ein ergebnis generieren
      if( rootNodeID != null && rootNodeID.equals( nodeFEs[i].getId() ) )
        b.append( idManager.getAsciiID( nodeFEs[i] ) + "\n" );
      // b.append( FeatureHelper.getAsString( nodeFEs[i], "num" ) + "\n" );
      // fuer nicht root node nur ergebnisse generieren wenn gewuenscht
      else if( rootNodeID == null && FeatureHelper.booleanIsTrue( nodeFEs[i], NaModelConstants.GENERATE_RESULT_PROP, false ) )
        b.append( idManager.getAsciiID( nodeFEs[i] ) + "\n" );
      // b.append( FeatureHelper.getAsString( nodeFEs[i], "num" ) + "\n" );
    }
    b.append( "99999\n" );
    // teilgebiete
    final IFeatureType catchmentFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    final Feature[] catchmentFEs = modellWorkspace.getFeatures( catchmentFT );
    for( int i = 0; i < catchmentFEs.length; i++ )
    {
      if( FeatureHelper.booleanIsTrue( catchmentFEs[i], NaModelConstants.GENERATE_RESULT_PROP, false ) )
        b.append( idManager.getAsciiID( catchmentFEs[i] ) + "\n" );
      // b.append( FeatureHelper.getAsString( catchmentFEs[i], "inum" ) + "\n" );
    }
    b.append( "99999\n" );
  }

  private static void writeFalstart( NAConfiguration conf, File startFile, StringBuffer b )
  {

    String system = "we";// "sys";
    String zustand = "nat";
    final DateFormat format = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyy MM dd HH" ) );

    String startDate = format.format( conf.getSimulationStart() );
    String endDate = format.format( conf.getSimulationEnd() );

    b.append( "xxx\n" );
    b.append( "x einzugsgebiet\n" );
    if( conf.isUsePrecipitationForm().equals( true ) )
    {
      String preFormText = conf.getPrecipitationForm();
      int preForm = 0;
      if( preFormText.equals( "Blockregen" ) )
      {
        preForm = 1;
      }
      else if( preFormText.equals( "linksschiefer Regen" ) )
      {
        preForm = 2;
      }
      else if( preFormText.equals( "Zentralregen" ) )
      {
        preForm = 3;
      }
      else if( preFormText.equals( "rechtsschiefer Regen" ) )
      {
        preForm = 4;
      }
      else
        System.out.println( "Falsche Wahl der synthetische Niederschlagsform!" );

      b.append( "x Niederschlagsform (2-nat; 1-syn); projektverzeichnis; System(XXXX); Zustand (YYY); Dateiname: Wahrscheinlichkeit [1/a]; Dauer [h]; Verteilung; Konfigurationsdatei mit Pfad\n" );
      b.append( "1 .. " + system + " " + zustand + " " + "synth.st" + " " + FortranFormatHelper.printf( conf.getAnnuality(), "f6.3" ) + " " + FortranFormatHelper.printf( conf.getDuration(), "f9.3" )
          + " " + Integer.toString( preForm ) + " " + "start" + File.separator + startFile.getName() + "\n" );
    }
    else
    {
      b.append( "x Niederschlagsform (2-nat; 1-syn); projektverzeichnis; System(XXXX); Zustand (YYY); Simulationsbeginn(dat+Zeit); Simulationsende; Konfigurationsdatei mit Pfad\n" );
      b.append( "2 .. " + system + " " + zustand + "  " + startDate + " " + endDate + " " + "start" + File.separator + startFile.getName() + "\n" );
    }
  }

  private static String getBoolean( Object object )
  {
    if( object == null || (!(object instanceof Boolean)) )
      return "n";
    boolean flag = ((Boolean) object).booleanValue();
    if( flag )
      return "j";
    return "n";
  }

}