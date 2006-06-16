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
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.convert.namodel.manager.IDManager;
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
    List dateList = (List) controlFE.getProperty( "InitialValueDate" );
    if( dateList != null )
    {
      Iterator iter = dateList.iterator();
      while( iter.hasNext() )
      {
        Feature fe = (Feature) iter.next();
        Boolean write = (Boolean) fe.getProperty( "write" );
        if( write )
        {
          final Date initialDate = DateUtilities.toDate( (XMLGregorianCalendar) fe.getProperty( "initialDate" ) );
//          Date initialDate = (Date) fe.getProperty( "initialDate" );
          SimpleDateFormat format = new SimpleDateFormat( "yyyyMMdd  HH" );
          String iniDate = format.format( initialDate );
          b.append( iniDate + "\n" );
          conf.setIniWrite(true);
        }
      }
    }
    b.append( "99999\n" );
  }

  private static void appendResultsToGenerate( NAConfiguration conf, Feature controlFE, StringBuffer b )
  {
    int minutesOfTimeStep = conf.getMinutesOfTimeStep();
    double hoursOfTimeStep = minutesOfTimeStep / 60d;
    b.append( " " + hoursOfTimeStep + "\n" );
    b.append( getBoolean( controlFE.getProperty( "tmp" ) ) + "       Temperatur                 .tmp\n" );
    b.append( getBoolean( controlFE.getProperty( "pre" ) ) + "       Niederschlag               .pre\n" );
    b.append( getBoolean( controlFE.getProperty( "sch" ) ) + "       Schnee                     .sch\n" );
    b.append( getBoolean( controlFE.getProperty( "bof" ) ) + "       Bodenfeuchte               .bof\n" );
    b.append( getBoolean( controlFE.getProperty( "bsp" ) ) + "       Bodenspeicher              .bsp\n" );
    b.append( getBoolean( controlFE.getProperty( "gws" ) ) + "       Grundwasserstand           .gws\n" );
    b.append( getBoolean( controlFE.getProperty( "qgs" ) ) + "       Gesamtabfluss Knoten       .qgs\n" );
    b.append( getBoolean( controlFE.getProperty( "qgg" ) ) + "       Gesamtabfluss TG           .qgg\n" );
    b.append( getBoolean( controlFE.getProperty( "qna" ) ) + "       nat. Oberflaechenabfluss   .qna\n" );
    b.append( getBoolean( controlFE.getProperty( "qif" ) ) + "       Interflow                  .qif\n" );
    b.append( getBoolean( controlFE.getProperty( "qvs" ) ) + "       Abfluss vers. Flaechen     .qvs\n" );
    b.append( getBoolean( controlFE.getProperty( "qbs" ) ) + "       Basisabfluss               .qbs\n" );
    b.append( getBoolean( controlFE.getProperty( "qt1" ) ) + "       Kluftgrundw1               .qt1\n" );
    b.append( getBoolean( controlFE.getProperty( "qtg" ) ) + "       Kluftgrundw                .qtg\n" );
    b.append( getBoolean( controlFE.getProperty( "qgw" ) ) + "       Grundwasserabfluss         .qgw\n" );
    // sollte nicht bei der Ausgabe erzeugt werden, da Berechnung mit kap. Aufstieg noch nicht implementiert!
    b.append( "n" + "       Kapil.Aufstieg/Perkolation .kap\n" );
    b.append( getBoolean( controlFE.getProperty( "vet" ) ) + "       Evapotranspiration         .vet\n" );
    b.append( getBoolean( controlFE.getProperty( "hyd" ) ) + "       Ausgabe Hydrotope          .hyd\n" );
    // if "2": output of *.txt and *.bil
    if( ((Boolean) (controlFE.getProperty( "bil" ))).booleanValue() )
      b.append( "2" + "       Abflussbilanz              .bil\n" );
    else
      b.append( "n" + "       Abflussbilanz              .bil\n" );
    b.append( getBoolean( controlFE.getProperty( "nmq" ) ) + "       Statistische Abflusswerte  .nmq\n" );
    // Folgende Dateien werden zusätzlich mit Speicherinhalt generiert .sph, .spv, .spn, .spb
    b.append( getBoolean( controlFE.getProperty( "spi" ) ) + "       Speicherinhalt             .spi\n" );
    b.append( getBoolean( controlFE.getProperty( "sup" ) ) + "       Speicherueberlauf          .sup\n" );
  }

  private static void appendResultInformation( final GMLWorkspace modellWorkspace, final GMLWorkspace controlWorkspace, final StringBuffer b, final IDManager idManager )
  {
    // knoten
    final IFeatureType nodeFT = modellWorkspace.getFeatureType( "Node" );
    final Feature[] nodeFEs = modellWorkspace.getFeatures( nodeFT );
    // boolean onlyRootNodeResult = FeatureHelper.booleanIsTrue( controlWorkspace.getRootFeature(),
    // "resultForRootNodeOnly", true );
    final String rootNodeID = (String) controlWorkspace.getRootFeature().getProperty( "rootNode" );
    for( int i = 0; i < nodeFEs.length; i++ )
    {
      // fuer root node immer ein ergebnis generieren
      if( rootNodeID != null && rootNodeID.equals( nodeFEs[i].getId() ) )
        b.append( idManager.getAsciiID( nodeFEs[i] ) + "\n" );
      // b.append( FeatureHelper.getAsString( nodeFEs[i], "num" ) + "\n" );
      // fuer nicht root node nur ergebnisse generieren wenn gewuenscht
      else if( rootNodeID == null && FeatureHelper.booleanIsTrue( nodeFEs[i], "generateResult", false ) )
        b.append( idManager.getAsciiID( nodeFEs[i] ) + "\n" );
      // b.append( FeatureHelper.getAsString( nodeFEs[i], "num" ) + "\n" );
    }
    b.append( "99999\n" );
    // teilgebiete
    final IFeatureType catchmentFT = modellWorkspace.getFeatureType( "Catchment" );
    final Feature[] catchmentFEs = modellWorkspace.getFeatures( catchmentFT );
    for( int i = 0; i < catchmentFEs.length; i++ )
    {
      if( FeatureHelper.booleanIsTrue( catchmentFEs[i], "generateResult", false ) )
        b.append( idManager.getAsciiID( catchmentFEs[i] ) + "\n" );
      // b.append( FeatureHelper.getAsString( catchmentFEs[i], "inum" ) + "\n" );
    }
    b.append( "99999\n" );
  }

  private static void writeFalstart( NAConfiguration conf, File startFile, StringBuffer b )
  {

    String system = "we";// "sys";
    String zustand = "nat";
    SimpleDateFormat format = new SimpleDateFormat( "yyyy MM dd HH" );

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
      b.append( "1 .. " + system + " " + zustand + " " + "synth.st" + " " + conf.getAnnuality().toString() + " " + conf.getDuration().toString() + " " + Integer.toString( preForm ) + " " + "start"
          + File.separator + startFile.getName() + "\n" );
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