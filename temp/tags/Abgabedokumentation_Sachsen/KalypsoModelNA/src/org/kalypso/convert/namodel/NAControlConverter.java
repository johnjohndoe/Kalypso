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

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
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

  public static void featureToASCII(NAConfiguration conf, File projectPath, GMLWorkspace controlWorkspace,
      GMLWorkspace modellWorkspace ) throws IOException
  {
    final Feature controlFE = controlWorkspace.getRootFeature();
    final File startDir = new File( projectPath, "start" );
    startDir.mkdirs();

    final File startFile = new File( startDir, "we_nat_start.txt" );
    final File falStartFile = new File( startDir, "falstart.lst" );

    // write FalStart
    final StringBuffer b1 = new StringBuffer();
    writeFalstart( conf,startFile, b1 );
    //write it
    final FileWriter writer1 = new FileWriter( falStartFile );
    writer1.write( b1.toString() );
    writer1.close();

    // generate Start
    final StringBuffer b = new StringBuffer();
    appendResultsToGenerate( controlFE, b );
    appendResultInformation( modellWorkspace,controlWorkspace, b );
    //write it
    final FileWriter writer = new FileWriter( startFile );
    writer.write( b.toString() );
    writer.close();
  }

  private static void appendResultsToGenerate( Feature controlFE, StringBuffer b )
  {
    b.append( " " + FeatureHelper.getAsString( controlFE, "timeStep" ) + "\n" );

    b.append( getBoolean( controlFE.getProperty( "tmp" ) )
        + "       Temperatur                 .tmp\n" );
    b.append( getBoolean( controlFE.getProperty( "pre" ) )
        + "       Niederschlag               .pre\n" );
    b.append( getBoolean( controlFE.getProperty( "sch" ) )
        + "       Schnee                     .sch\n" );
    b.append( getBoolean( controlFE.getProperty( "bof" ) )
        + "       Bodenfeuchte               .bof\n" );
    b.append( getBoolean( controlFE.getProperty( "bsp" ) )
        + "       Bodenspeicher              .bsp\n" );
    b.append( getBoolean( controlFE.getProperty( "gws" ) )
        + "       Grundwasserstand           .gws\n" );
    b.append( getBoolean( controlFE.getProperty( "qgs" ) )
        + "       Gesamtabfluss Knoten       .qgs\n" );
    b.append( getBoolean( controlFE.getProperty( "qgg" ) )
        + "       Gesamtabfluss TG           .qgg\n" );
    b.append( getBoolean( controlFE.getProperty( "qna" ) )
        + "       Oberflaechenabfluss        .qna\n" );
    b.append( getBoolean( controlFE.getProperty( "qif" ) )
        + "       Interflow                  .qif\n" );
    b.append( getBoolean( controlFE.getProperty( "qvs" ) )
        + "       Abfluss vers. Flaechen     .qvs\n" );
    b.append( getBoolean( controlFE.getProperty( "qbs" ) )
        + "       Basisabfluss               .qbs\n" );
    b.append( getBoolean( controlFE.getProperty( "qt1" ) )
        + "       Kluftgrundw1               .qt1\n" );
    b.append( getBoolean( controlFE.getProperty( "qtg" ) )
        + "       Kluftgrundw                .qtg\n" );
    b.append( getBoolean( controlFE.getProperty( "qgw" ) )
        + "       Krundwasser                .qgw\n" );
    b.append( getBoolean( controlFE.getProperty( "kap" ) )
        + "       Kapil.Aufstieg/Perkolation .kap\n" );
    b.append( getBoolean( controlFE.getProperty( "vet" ) )
        + "       Evapotranspiration         .vet\n" );
    b.append( getBoolean( controlFE.getProperty( "hyd" ) )
        + "       Ausgabe hydrotope          .hyd\n" );
    b.append( getBoolean( controlFE.getProperty( "bil" ) )
        + "       Abflussbilanz              .bil\n" );
    b.append( getBoolean( controlFE.getProperty( "nmq" ) )
        + "       Statistische Abflusswerte  .nmq\n" );
    b.append( getBoolean( controlFE.getProperty( "spi" ) )
        + "       Speicherinhalt             .spi\n" );
    b.append( getBoolean( controlFE.getProperty( "sub" ) )
        + "       Speicherueberlauf          .sup\n" );
    b.append( getBoolean( controlFE.getProperty( "sph" ) )
        + "       Wasserstand Speicher       .sph\n" );
    b.append( getBoolean( controlFE.getProperty( "spv" ) )
        + "       Talsperrenverdunstung      .spv\n" );
    b.append( getBoolean( controlFE.getProperty( "spn" ) )
        + "       Zehrung                    .spn\n" );
    b.append( getBoolean( controlFE.getProperty( "vep" ) )
        + "       Evaporation                .vep\n" );
  }

  private static void appendResultInformation(GMLWorkspace modellWorkspace, GMLWorkspace controlWorkspace, StringBuffer b )
  {
    // knoten
    final FeatureType nodeFT = modellWorkspace.getFeatureType( "Node" );
    final Feature[] nodeFEs = modellWorkspace.getFeatures( nodeFT );
    boolean onlyRootNodeResult=FeatureHelper.booleanIsTrue(controlWorkspace.getRootFeature(),"resultForRootNodeOnly",true);
    final String rootNodeID=(String)controlWorkspace.getRootFeature().getProperty("rootNode");
    for( int i = 0; i < nodeFEs.length; i++ )
    {
      // fuer root node immer ein ergebnis generieren
      if(rootNodeID!=null && rootNodeID.equals(nodeFEs[i].getId()))
        b.append( FeatureHelper.getAsString( nodeFEs[i], "num" ) + "\n" );
      // fuer nicht root node nur ergebnisse generieren wenn gewuenscht
      else if( !onlyRootNodeResult && FeatureHelper.booleanIsTrue( nodeFEs[i], "generateResult", false ) )
        b.append( FeatureHelper.getAsString( nodeFEs[i], "num" ) + "\n" );
    }
    b.append( "99999\n" );
    // teilgebiete
    final FeatureType catchmentFT = modellWorkspace.getFeatureType( "Catchment" );
    final Feature[] catchmentFEs = modellWorkspace.getFeatures( catchmentFT );
    for( int i = 0; i < catchmentFEs.length; i++ )
    {
      if( FeatureHelper.booleanIsTrue( catchmentFEs[i], "generateResult", false ) )
        b.append( FeatureHelper.getAsString( catchmentFEs[i], "inum" ) + "\n" );
    }
    b.append( "99999\n" );
    // TODO startwerte fuer die kurzzeitsimulation
    b.append( "99999\n" );
  }

  private static void writeFalstart(NAConfiguration conf, File startFile,
      StringBuffer b )
  {

    String system = "we";//"sys";
    String zustand = "nat";
    SimpleDateFormat format = new SimpleDateFormat("yyyy MM dd HH");

    String startDate=format.format(conf.getSimulationStart());
    String endDate=format.format(conf.getSimulationEnd());

    b.append( "xxx\n" );
    b.append( "x einzugsgebiet\n" );
    b.append( "x Niederschlagsform (2-nat; 1-syn); projektverzeichnis; System(XXXX); Zustand (YYY); Simulationsbeginn(dat+Zeit); Simulationsende; Konfigurationsdatei mit Pfad\n" );
    b.append( "2 .. " + system + " " + zustand + "  " + startDate + " " + endDate + " " + "start"
        + File.separator + startFile.getName() + "\n" );
  }

  private static String getBoolean( Object object )
  {
    if( object == null || ( !( object instanceof Boolean ) ) )
      return "n";
    boolean flag = ( (Boolean)object ).booleanValue();
    if( flag )
      return "j";
    return "n";
  }
}