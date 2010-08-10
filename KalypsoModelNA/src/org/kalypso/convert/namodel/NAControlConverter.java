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
import java.util.List;
import java.util.Locale;

import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.binding.NAModellControl;
import org.kalypso.model.hydrology.internal.binding.suds.Greenroof;
import org.kalypso.model.hydrology.internal.binding.suds.Swale;
import org.kalypso.model.hydrology.internal.binding.suds.SwaleInfiltrationDitch;
import org.kalypso.model.hydrology.internal.i18n.Messages;
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

  public static void featureToASCII( final NAConfiguration conf, final File startDir, final NAModellControl naControl, final GMLWorkspace modellWorkspace ) throws IOException
  {
    startDir.mkdirs();

    final File startFile = new File( startDir, "we_nat_start.txt" ); //$NON-NLS-1$
    final File falStartFile = new File( startDir, "falstart.lst" ); //$NON-NLS-1$

    // write FalStart
    final StringBuffer b1 = new StringBuffer();
    writeFalstart( conf, startFile, b1 );
    // write it
    final FileWriter writer1 = new FileWriter( falStartFile );
    writer1.write( b1.toString() );
    writer1.close();

    // generate Start
    final StringBuffer b = new StringBuffer();
    appendResultsToGenerate( conf, naControl, b );
    appendResultInformation( modellWorkspace, naControl, b, conf.getIdManager() );
    // Startwerte fuer die kurzzeitsimulation
    appendInitialDates( naControl, b );
    // write it
    final FileWriter writer = new FileWriter( startFile );
    writer.write( b.toString() );
    writer.close();
  }

  private static void appendInitialDates( final NAModellControl controlFE, final StringBuffer b )
  {
    final DateFormat format = NATimeSettings.getInstance().getLzsLzgDateFormat();

    final Date[] initialDates = controlFE.getInitialDatesToBeWritten();
    for( final Date iniDate : initialDates )
    {
      final String iniString = format.format( iniDate );
      b.append( iniString + "\n" ); //$NON-NLS-1$
    }
    b.append( "99999\n" ); //$NON-NLS-1$
  }

  private static void appendResultsToGenerate( final NAConfiguration conf, final NAModellControl controlFE, final StringBuffer b )
  {
    final int minutesOfTimeStep = conf.getMinutesOfTimeStep();
    final double hoursOfTimeStep = minutesOfTimeStep / 60d;
    b.append( " " + hoursOfTimeStep + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    b.append( getBoolean( controlFE.doGenerateTMP() ) + "       Temperatur                 .tmp\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGeneratePRE() ) + "       Niederschlag               .pre\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateSCH() ) + "       Schnee                     .sch\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateBOF() ) + "       Bodenfeuchte               .bof\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateBSP() ) + "       Bodenspeicher              .bsp\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateGWS() ) + "       Grundwasserstand           .gws\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateQGS() ) + "       Gesamtabfluss Knoten       .qgs\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateQGG() ) + "       Gesamtabfluss TG           .qgg\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateQNA() ) + "       nat. Oberflaechenabfluss   .qna\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateQIF() ) + "       Interflow                  .qif\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateQVS() ) + "       Abfluss vers. Flaechen     .qvs\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateQBS() ) + "       Basisabfluss               .qbs\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateQT1() ) + "       Kluftgrundw1               .qt1\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateQTG() ) + "       Kluftgrundw                .qtg\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateQGW() ) + "       Grundwasserabfluss         .qgw\n" ); //$NON-NLS-1$
    // sollte nicht bei der Ausgabe erzeugt werden, da Berechnung mit kap. Aufstieg noch nicht implementiert!
    b.append( "n" + "       Kapil.Aufstieg/Perkolation .kap\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    b.append( getBoolean( controlFE.doGenerateVET() ) + "       Evapotranspiration         .vet\n" ); //$NON-NLS-1$

    b.append( getBoolean( controlFE.doGenerateHYD() ) + "       Ausgabe Hydrotope          .hyd\n" ); //$NON-NLS-1$
    // if "2": output of *.txt and *.bil
    if( controlFE.doGenerateBIL() )
      b.append( "2" + "       Abflussbilanz              .bil\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      b.append( "n" + "       Abflussbilanz              .bil\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    b.append( getBoolean( controlFE.doGenerateNMQ() ) + "       Statistische Abflusswerte  .nmq\n" ); //$NON-NLS-1$
    // Folgende Dateien werden zusätzlich mit Speicherinhalt generiert .sph, .spv, .spn, .spb
    b.append( getBoolean( controlFE.doGenerateSPI() ) + "       Speicherinhalt             .spi\n" ); //$NON-NLS-1$
    b.append( getBoolean( controlFE.doGenerateSUP() ) + "       Speicherueberlauf          .sup\n" ); //$NON-NLS-1$

    // FIXME: bad abstraction, put into helper class
    final GMLWorkspace sudsWorkspace = conf.getSudsWorkspace();
    boolean hasGreenRoofs = false;
    boolean hasSwaleInfiltrationDitches = false;
    boolean hasSwales = false;
    if( sudsWorkspace != null )
    {
      final List< ? > suds = (List< ? >) sudsWorkspace.getRootFeature().getProperty( NaModelConstants.SUDS_PROP_SUDS_MEMBER ); //$NON-NLS-1$ //$NON-NLS-2$
      for( final Object sudsItem : suds )
      {
        hasGreenRoofs |= sudsItem instanceof Greenroof;
        hasSwaleInfiltrationDitches |= sudsItem instanceof SwaleInfiltrationDitch;
        hasSwales |= sudsItem instanceof Swale;
      }
    }

    b.append( String.format( Locale.US, "%-8s%-27s%s\n", hasGreenRoofs ? "j" : "n", "Gründach Überlauf", ".qgu" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    b.append( String.format( Locale.US, "%-8s%-27s%s\n", hasGreenRoofs ? "j" : "n", "Gründach Drainrohr", ".qgr" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    b.append( String.format( Locale.US, "%-8s%-27s%s\n", hasSwaleInfiltrationDitches ? "j" : "n", "Überlauf Mulden-Rigolen", ".que" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    b.append( String.format( Locale.US, "%-8s%-27s%s\n", hasSwaleInfiltrationDitches ? "j" : "n", "Drainrohr Mulden-Rigolen", ".qmr" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    b.append( String.format( Locale.US, "%-8s%-27s%s\n", hasSwales ? "j" : "n", "Überlauf Mulden", ".mul" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
  }

  private static void appendResultInformation( final GMLWorkspace modellWorkspace, final NAModellControl naControl, final StringBuffer b, final IDManager idManager )
  {
    // knoten
    final IFeatureType nodeFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final Feature[] nodeFEs = modellWorkspace.getFeatures( nodeFT );
    // boolean onlyRootNodeResult = FeatureHelper.booleanIsTrue( controlWorkspace.getRootFeature(),
    // "resultForRootNodeOnly", true );
    final String rootNodeID = naControl.getRootNodeID();
    for( final Feature nodeFE : nodeFEs )
    {
      // fuer root node immer ein ergebnis generieren
      if( rootNodeID != null && rootNodeID.equals( nodeFE.getId() ) )
        b.append( idManager.getAsciiID( nodeFE ) + "\n" ); //$NON-NLS-1$
      // b.append( FeatureHelper.getAsString( nodeFEs[i], "num" ) + "\n" );
      // fuer nicht root node nur ergebnisse generieren wenn gewuenscht
      else if( rootNodeID == null && FeatureHelper.booleanIsTrue( nodeFE, NaModelConstants.GENERATE_RESULT_PROP, false ) )
        b.append( idManager.getAsciiID( nodeFE ) + "\n" ); //$NON-NLS-1$
      // b.append( FeatureHelper.getAsString( nodeFEs[i], "num" ) + "\n" );
    }
    b.append( "99999\n" ); //$NON-NLS-1$
    // teilgebiete
    final IFeatureType catchmentFT = modellWorkspace.getGMLSchema().getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    final Feature[] catchmentFEs = modellWorkspace.getFeatures( catchmentFT );
    for( final Feature catchmentFE : catchmentFEs )
    {
      if( FeatureHelper.booleanIsTrue( catchmentFE, NaModelConstants.GENERATE_RESULT_PROP, false ) )
        b.append( idManager.getAsciiID( catchmentFE ) + "\n" ); //$NON-NLS-1$
      // b.append( FeatureHelper.getAsString( catchmentFEs[i], "inum" ) + "\n" );
    }
    b.append( "99999\n" ); //$NON-NLS-1$
  }

  private static void writeFalstart( final NAConfiguration conf, final File startFile, final StringBuffer b )
  {
    final String system = "we";// "sys"; //$NON-NLS-1$
    final String zustand = "nat"; //$NON-NLS-1$
    final DateFormat format = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyy MM dd HH" ) ); //$NON-NLS-1$

    final String startDate = format.format( conf.getSimulationStart() );
    final String endDate = format.format( conf.getSimulationEnd() );

    b.append( "xxx\n" ); //$NON-NLS-1$
    b.append( "x einzugsgebiet\n" ); //$NON-NLS-1$
    if( conf.isUsePrecipitationForm() )
    {
      final String preFormText = conf.getPrecipitationForm();
      int preForm = 0;
      if( preFormText.equals( "Blockregen" ) ) //$NON-NLS-1$
      {
        preForm = 1;
      }
      else if( preFormText.equals( "linksschiefer Regen" ) ) //$NON-NLS-1$
      {
        preForm = 2;
      }
      else if( preFormText.equals( "Zentralregen" ) ) //$NON-NLS-1$
      {
        preForm = 3;
      }
      else if( preFormText.equals( "rechtsschiefer Regen" ) ) //$NON-NLS-1$
      {
        preForm = 4;
      }
      else
        System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAControlConverter.49" ) ); //$NON-NLS-1$

      b.append( "x Niederschlagsform (2-nat; 1-syn); projektverzeichnis; System(XXXX); Zustand (YYY); Dateiname: Wahrscheinlichkeit [1/a]; Dauer [h]; Verteilung; Konfigurationsdatei mit Pfad\n" ); //$NON-NLS-1$
      b.append( "1 .. " + system + " " + zustand + " " + "synth.st" + " " + FortranFormatHelper.printf( conf.getAnnuality(), "f6.3" ) + " " + FortranFormatHelper.printf( conf.getDuration(), "f9.3" ) //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
          + " " + Integer.toString( preForm ) + " " + "start" + File.separator + startFile.getName() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    }
    else
    {
      b.append( "x Niederschlagsform (2-nat; 1-syn); projektverzeichnis; System(XXXX); Zustand (YYY); Simulationsbeginn(dat+Zeit); Simulationsende; Konfigurationsdatei mit Pfad\n" ); //$NON-NLS-1$
      b.append( "2 .. " + system + " " + zustand + "  " + startDate + " " + endDate + " " + "start" + File.separator + startFile.getName() + "\n" ); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    }
  }

  private static String getBoolean( final boolean property )
  {
    if( property )
      return "j"; //$NON-NLS-1$

    return "n"; //$NON-NLS-1$
  }

}