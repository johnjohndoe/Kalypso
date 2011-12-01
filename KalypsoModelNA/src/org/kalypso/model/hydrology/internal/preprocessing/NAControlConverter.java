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
package org.kalypso.model.hydrology.internal.preprocessing;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.suds.Greenroof;
import org.kalypso.model.hydrology.binding.suds.Swale;
import org.kalypso.model.hydrology.binding.suds.SwaleInfiltrationDitch;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.NATimeSettings;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

public class NAControlConverter
{
  private static final String FILENAME_START_FILE = "we_nat_start.txt"; //$NON-NLS-1$

  private static final String FILENAME_FALSTART = "falstart.lst";//$NON-NLS-1$

  private final NAControl m_metaControl;

  private final File m_startDir;

  private final File m_falStartFile;

  private final File m_startFile;

  public NAControlConverter( final NAControl metaControl, final File startDir )
  {
    m_metaControl = metaControl;
    m_startDir = startDir;

    startDir.mkdirs();

    m_falStartFile = new File( startDir, FILENAME_FALSTART );
    m_startFile = new File( startDir, FILENAME_START_FILE );
  }

  public void writeFalstart( ) throws IOException
  {
    PrintWriter writer = null;
    try
    {
      writer = new PrintWriter( m_falStartFile );
      writeFalstart( writer );
      writer.close();
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  private void writeFalstart( final PrintWriter writer )
  {
    final String system = "we";// "sys"; //$NON-NLS-1$
    final String zustand = "nat"; //$NON-NLS-1$
    final DateFormat format = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyy MM dd HH" ) ); //$NON-NLS-1$

    final Date simulationStart = m_metaControl.getSimulationStart();
    final Date simulationEnd = m_metaControl.getSimulationEnd();

    final String startDate = format.format( simulationStart );
    final String endDate = format.format( simulationEnd );

    writer.append( "xxx\n" ); //$NON-NLS-1$
    writer.append( "x einzugsgebiet\n" ); //$NON-NLS-1$

    final String pathStartFile = m_startDir.getName() + File.separator + FILENAME_START_FILE;

    if( m_metaControl.isUsePrecipitationForm() )
    {
      final String preFormText = m_metaControl.getPrecipitationForm();
      final int preForm = convertPreform( preFormText );
      if( preForm == 0 )
        // FIXME: use logger
        System.out.println( Messages.getString( "org.kalypso.convert.namodel.NAControlConverter.49" ) ); //$NON-NLS-1$

      final double annuality = m_metaControl.getAnnuality();
      final double duration = m_metaControl.getDurationHours();
      writer.append( "x Niederschlagsform (2-nat; 1-syn); projektverzeichnis; System(XXXX); Zustand (YYY); Dateiname: Wahrscheinlichkeit [1/a]; Dauer [h]; Verteilung; Konfigurationsdatei mit Pfad\n" ); //$NON-NLS-1$
      writer.append( "1 .. " + system + " " + zustand + " " + "synth.st" + " " + FortranFormatHelper.printf( annuality, "f6.3" ) + " " + FortranFormatHelper.printf( duration, "f9.3" ) //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
          + " " + Integer.toString( preForm ) + " " + pathStartFile + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    }
    else
    {
      writer.append( "x Niederschlagsform (2-nat; 1-syn); projektverzeichnis; System(XXXX); Zustand (YYY); Simulationsbeginn(dat+Zeit); Simulationsende; Konfigurationsdatei mit Pfad\n" ); //$NON-NLS-1$
      writer.append( "2 .. " + system + " " + zustand + "  " + startDate + " " + endDate + " " + pathStartFile + "\n" ); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    }
  }

  private int convertPreform( final String preFormText )
  {
    if( "Blockregen".equals( preFormText ) ) //$NON-NLS-1$
      return 1;

    if( "linksschiefer Regen".equals( preFormText ) ) //$NON-NLS-1$
    {
      return 2;
    }

    if( "Zentralregen".equals( preFormText ) ) //$NON-NLS-1$
      return 3;

    if( "rechtsschiefer Regen".equals( preFormText ) ) //$NON-NLS-1$
      return 4;

    return 0;
  }

  public void writeStartFile( final NAModellControl naControl, final Node rootNode, final NaModell naModel, final GMLWorkspace sudsWorkspace, final IDManager idManager ) throws IOException
  {
    PrintWriter writer = null;
    try
    {
      writer = new PrintWriter( m_startFile );
      writeStartFile( writer, naControl, rootNode, naModel, sudsWorkspace, idManager );
      writer.close();
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  private void writeStartFile( final PrintWriter writer, final NAModellControl naControl, final Node rootNode, final NaModell naModel, final GMLWorkspace sudsWorkspace, final IDManager idManager )
  {
    writeResultsToGenerate( naControl, sudsWorkspace, writer );
    writeResultInformation( naModel, rootNode, idManager, writer );
    writeInitialDates( naControl, writer );
  }

  // Startwerte fuer die kurzzeitsimulation
  private static void writeInitialDates( final NAModellControl controlFE, final PrintWriter writer )
  {
    final DateFormat format = NATimeSettings.getInstance().getLzsLzgDateFormat();

    final Date[] initialDates = controlFE.getInitialDatesToBeWritten();
    for( final Date iniDate : initialDates )
    {
      final String iniString = format.format( iniDate );
      writer.append( iniString + "\n" ); //$NON-NLS-1$
    }
    writer.append( "99999\n" ); //$NON-NLS-1$
  }

  private void writeResultsToGenerate( final NAModellControl controlFE, final GMLWorkspace sudsWorkspace, final PrintWriter writer )
  {
    final int minutesOfTimeStep = m_metaControl.getMinutesOfTimestep();
    final double hoursOfTimeStep = minutesOfTimeStep / 60d;
    writer.append( " " + hoursOfTimeStep + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    writer.append( getBoolean( controlFE.doGenerateTMP() ) + "       Temperatur                 .tmp\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGeneratePRE() ) + "       Niederschlag               .pre\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateSCH() ) + "       Schnee                     .sch\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateBOF() ) + "       Bodenfeuchte               .bof\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateBSP() ) + "       Bodenspeicher              .bsp\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateGWS() ) + "       Grundwasserstand           .gws\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateQGS() ) + "       Gesamtabfluss Knoten       .qgs\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateQGG() ) + "       Gesamtabfluss TG           .qgg\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateQNA() ) + "       nat. Oberflaechenabfluss   .qna\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateQIF() ) + "       Interflow                  .qif\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateQVS() ) + "       Abfluss vers. Flaechen     .qvs\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateQBS() ) + "       Basisabfluss               .qbs\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateQT1() ) + "       Kluftgrundw1               .qt1\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateQTG() ) + "       Kluftgrundw                .qtg\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateQGW() ) + "       Grundwasserabfluss         .qgw\n" ); //$NON-NLS-1$
    // sollte nicht bei der Ausgabe erzeugt werden, da Berechnung mit kap. Aufstieg noch nicht implementiert!
    writer.append( "n" + "       Kapil.Aufstieg/Perkolation .kap\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    writer.append( getBoolean( controlFE.doGenerateVET() ) + "       Evapotranspiration         .vet\n" ); //$NON-NLS-1$

    // FIXME: depending on the calc core, the next line must be present or not.... grrrr...
    // We leave that for now.... (it is not clear when exactly this was changed, probably Summer 2009)
    // writer.append( "n       Ausgabe MRS                .qmr\n" ); //$NON-NLS-1$

    writer.append( getBoolean( controlFE.doGenerateHYD() ) + "       Ausgabe Hydrotope          .hyd\n" ); //$NON-NLS-1$
    // if "2": output of *.txt and *.bil
    if( controlFE.doGenerateBIL() )
      writer.append( "2" + "       Abflussbilanz              .bil\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      writer.append( "n" + "       Abflussbilanz              .bil\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    writer.append( getBoolean( controlFE.doGenerateNMQ() ) + "       Statistische Abflusswerte  .nmq\n" ); //$NON-NLS-1$
    // Folgende Dateien werden zusätzlich mit Speicherinhalt generiert .sph, .spv, .spn, .spb
    writer.append( getBoolean( controlFE.doGenerateSPI() ) + "       Speicherinhalt             .spi\n" ); //$NON-NLS-1$
    writer.append( getBoolean( controlFE.doGenerateSUP() ) + "       Speicherueberlauf          .sup\n" ); //$NON-NLS-1$

    // FIXME: bad abstraction, put into helper class
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

    writer.append( String.format( Locale.US, "%-8s%-27s%s\n", hasGreenRoofs ? "j" : "n", "Gründach Überlauf", ".qgu" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    writer.append( String.format( Locale.US, "%-8s%-27s%s\n", hasGreenRoofs ? "j" : "n", "Gründach Drainrohr", ".qgr" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    writer.append( String.format( Locale.US, "%-8s%-27s%s\n", hasSwaleInfiltrationDitches ? "j" : "n", "Überlauf Mulden-Rigolen", ".que" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    writer.append( String.format( Locale.US, "%-8s%-27s%s\n", hasSwaleInfiltrationDitches ? "j" : "n", "Drainrohr Mulden-Rigolen", ".qmr" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    writer.append( String.format( Locale.US, "%-8s%-27s%s\n", hasSwales ? "j" : "n", "Überlauf Mulden", ".mul" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
  }

  private static void writeResultInformation( final NaModell naModel, final Node rootNode, final IDManager idManager, final PrintWriter writer )
  {
    final IFeatureBindingCollection<Node> nodes = naModel.getNodes();
    for( final Node node : nodes )
    {
      if( rootNode != null && rootNode.equals( node ) )
      {
        // fuer root node immer ein ergebnis generieren
        writer.append( idManager.getAsciiID( node ) + "\n" ); //$NON-NLS-1$
      }
      else if( rootNode == null && node.isGenerateResults() )
      {
        // fuer nicht root node nur ergebnisse generieren wenn gewuenscht
        writer.append( idManager.getAsciiID( node ) + "\n" ); //$NON-NLS-1$
      }
    }

    writer.append( "99999\n" ); //$NON-NLS-1$
    // teilgebiete
    final IFeatureBindingCollection<Catchment> catchments = naModel.getCatchments();
    for( final Catchment catchment : catchments )
    {
      if( catchment.isGenerateResults() )
        writer.append( idManager.getAsciiID( catchment ) + "\n" ); //$NON-NLS-1$
    }
    writer.append( "99999\n" ); //$NON-NLS-1$
  }


  private static String getBoolean( final boolean property )
  {
    if( property )
      return "j"; //$NON-NLS-1$

    return "n"; //$NON-NLS-1$
  }

}