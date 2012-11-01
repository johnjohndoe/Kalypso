/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.wspwin.core;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.Locale;

/**
 * represents the contents of one calculation (.001, .002, ... file)
 * 
 * @author Belger
 */
public class CalculationContentBeanPasche extends AbstractCalculationContentBean
{
  // REMARK: Important: we use the ordinal of the enum constants, to ORDER of constants IS IMPORTANT, do not change
  public static enum KIND
  {
    WATERLEVEL,
    BF_UNIFORM,
    BF_NON_UNIFORM;
  }

  public static enum FLIESSGESETZ
  {
    DARCY_WEISBACH_OHNE_FORMEINFLUSS,
    DARCY_WEISBACH_MIT_FORMEINFLUSS,
    MANNING_STRICKLER;
  }

  public static enum OUTPUT
  {
    SIMLPE,
    WITH_KONTROLFILE;
  }

  public static enum VERZOEGERUNSVERLUST
  {
    DVWK,
    BJOERNSEN,
    DFG;
  }

  public static enum ART_ANFANGS_WSP
  {
    GRENZTIEFE,
    STATIONAER_GLEICHFOERMIGES_GEFAELLE,
    DIREKTEINGABE;
  }

  private final KIND m_calcKind;

  private final FLIESSGESETZ m_fliessgesetz;

  private final OUTPUT m_output;

  private final boolean m_ausgabeProfilnummer;

  private final boolean m_isSimpleBerechnungWSPInt;

  private final boolean m_reibungsverlustNachTrapezformel;

  private final VERZOEGERUNSVERLUST m_verzoegerungsVerlust;

  private final ART_ANFANGS_WSP m_artAnfangswasserspiegel;

  private final boolean m_berechneBruecken;

  private final boolean m_berechneWehre;

  private final boolean m_ausgabewerteInLiter;

  private final boolean m_erstelleWQDateien;

  private final boolean m_ergebnislistenErstellen;

  private final BigDecimal m_gefaelle;

  private final BigDecimal m_min;

  private final BigDecimal m_step;

  private final BigDecimal m_max;

  private final int m_abfluss;

  private final BigDecimal m_anfang;

  private final BigDecimal m_ende;

  private final String m_strInfo;

  private final BigDecimal m_hoehe;

  private final boolean m_nhmo;

  private final boolean m_nwsfq;

  private final boolean m_nwsfl;

  private final String m_strQ;

  private final boolean m_nusg;

  private final boolean m_kalMin;

  /**
   * @param min
   *          minimal runoff [dl/s]
   * @param max
   *          maximal runoff [dl/s]
   * @param step
   *          runoff step [dl/s]
   */
  public CalculationContentBeanPasche( final CalculationBean bean, final KIND calcKind, final FLIESSGESETZ fliessgesetz, final OUTPUT output, final boolean ausgabeProfilnummer, final boolean isSimpleBerechnungWSPInt, final boolean reibungsverlustNachTrapezformal, final VERZOEGERUNSVERLUST verzoegerungsVerlust, final ART_ANFANGS_WSP artAnfangswasserspiegel, final boolean berechneBruecken, final boolean berechneWehre, final boolean ausgabewerteInLiter, final boolean erstelleWQDateien, final boolean ergebnislistenErstellen, final BigDecimal gefaelle, final BigDecimal min, final BigDecimal step, final BigDecimal max, final int abfluss, final BigDecimal anfang, final BigDecimal ende, final String strInfo, final BigDecimal hoehe, final boolean nhmo, final boolean nwsfq, final boolean nwsfl, final String strQ, final boolean nusg, final boolean kalMin )
  {
    super( bean );

    m_calcKind = calcKind;
    m_fliessgesetz = fliessgesetz;
    m_output = output;
    m_ausgabeProfilnummer = ausgabeProfilnummer;
    m_isSimpleBerechnungWSPInt = isSimpleBerechnungWSPInt;
    m_reibungsverlustNachTrapezformel = reibungsverlustNachTrapezformal;
    m_verzoegerungsVerlust = verzoegerungsVerlust;
    m_artAnfangswasserspiegel = artAnfangswasserspiegel;
    m_berechneBruecken = berechneBruecken;
    m_berechneWehre = berechneWehre;
    m_ausgabewerteInLiter = ausgabewerteInLiter;
    m_erstelleWQDateien = erstelleWQDateien;
    m_ergebnislistenErstellen = ergebnislistenErstellen;
    m_gefaelle = gefaelle;
    m_min = min;
    m_step = step;
    m_max = max;
    m_abfluss = abfluss;
    m_anfang = anfang;
    m_ende = ende;
    m_strInfo = strInfo;
    m_hoehe = hoehe;
    m_nhmo = nhmo;
    m_nwsfq = nwsfq;
    m_nwsfl = nwsfl;
    m_strQ = strQ;
    m_nusg = nusg;
    m_kalMin = kalMin;
  }

  public int getAbfluss( )
  {
    return m_abfluss;
  }

  public BigDecimal getAnfang( )
  {
    return m_anfang;
  }

  public ART_ANFANGS_WSP getArtAnfangswasserspiegel( )
  {
    return m_artAnfangswasserspiegel;
  }

  public boolean isAusgabeProfilnummer( )
  {
    return m_ausgabeProfilnummer;
  }

  public boolean isAusgabewerteInLiter( )
  {
    return m_ausgabewerteInLiter;
  }

  public boolean isBerechneBruecken( )
  {
    return m_berechneBruecken;
  }

  public boolean isBerechneWehre( )
  {
    return m_berechneWehre;
  }

  public KIND getCalcKind( )
  {
    return m_calcKind;
  }

  public BigDecimal getEnde( )
  {
    return m_ende;
  }

  public boolean isErgebnislistenErstellen( )
  {
    return m_ergebnislistenErstellen;
  }

  public boolean isErstelleWQDateien( )
  {
    return m_erstelleWQDateien;
  }

  public FLIESSGESETZ getFliessgesetz( )
  {
    return m_fliessgesetz;
  }

  public BigDecimal getGefaelle( )
  {
    return m_gefaelle;
  }

  public BigDecimal getHoehe( )
  {
    return m_hoehe;
  }

  public boolean isSimpleBerechnungWSPInt( )
  {
    return m_isSimpleBerechnungWSPInt;
  }

  public boolean isKalMin( )
  {
    return m_kalMin;
  }

  public BigDecimal getMax( )
  {
    return m_max;
  }

  public BigDecimal getMin( )
  {
    return m_min;
  }

  public boolean isNhmo( )
  {
    return m_nhmo;
  }

  public boolean isNusg( )
  {
    return m_nusg;
  }

  public boolean isNwsfl( )
  {
    return m_nwsfl;
  }

  public boolean isNwsfq( )
  {
    return m_nwsfq;
  }

  public OUTPUT getOutput( )
  {
    return m_output;
  }

  public boolean isReibungsverlustNachTrapezformel( )
  {
    return m_reibungsverlustNachTrapezformel;
  }

  public BigDecimal getStep( )
  {
    return m_step;
  }

  public String getStrInfo( )
  {
    return m_strInfo;
  }

  public String getStrQ( )
  {
    return m_strQ;
  }

  public VERZOEGERUNSVERLUST getVerzoegerungsVerlust( )
  {
    return m_verzoegerungsVerlust;
  }

  public static CalculationContentBeanPasche read( final CalculationBean bean, final File file ) throws IOException
  {
    try (LineNumberReader lnr = new LineNumberReader( new FileReader( file ) ))
    {
      // ZEILE 0 //
      final int calcKindInt = readInt( lnr );
      final KIND calcKind;
      switch( calcKindInt )
      {
        case 1:
          calcKind = KIND.WATERLEVEL;
          break;
        case 2:
          calcKind = KIND.BF_UNIFORM;
          break;
        case 3:
          calcKind = KIND.BF_NON_UNIFORM;
          break;

        default:
          calcKind = KIND.WATERLEVEL;
          break;
      }

      // ZEILE 1 //
      final int fliessgesetzInt = readInt( lnr );
      final FLIESSGESETZ fliessgesetz;
      switch( fliessgesetzInt )
      {
        case 1:
          fliessgesetz = FLIESSGESETZ.DARCY_WEISBACH_OHNE_FORMEINFLUSS;
          break;
        case 2:
          fliessgesetz = FLIESSGESETZ.DARCY_WEISBACH_MIT_FORMEINFLUSS;
          break;
        case 3:
          fliessgesetz = FLIESSGESETZ.MANNING_STRICKLER;
          break;

        default:
          fliessgesetz = FLIESSGESETZ.DARCY_WEISBACH_OHNE_FORMEINFLUSS;
          break;
      }

      // ZEILE 2 //
      final Integer outputInt = readInt( lnr );
      final OUTPUT output = outputInt == 2 ? OUTPUT.WITH_KONTROLFILE : OUTPUT.SIMLPE;

      // ZEILE 3 //
      final boolean ausgabeProfilnummer = readBoolean( lnr );

      // ZEILE 4 //
      final boolean isSimpleBerechnungWSPInt = readInt( lnr ) == 1;

      // ZEILE 5 //
      final boolean reibungsverlustNachTrapezformal = readInt( lnr ) == 1;
      // m_RVerlust

      // ZEILE 6 //
      final int verzoegerungsVerlustInt = readInt( lnr );
      final VERZOEGERUNSVERLUST verzoegerungsVerlust;
      switch( verzoegerungsVerlustInt )
      {
        case 1:
          verzoegerungsVerlust = VERZOEGERUNSVERLUST.DVWK;
          break;

        case 2:
          verzoegerungsVerlust = VERZOEGERUNSVERLUST.BJOERNSEN;
          break;
        case 3:
          verzoegerungsVerlust = VERZOEGERUNSVERLUST.DFG;
          break;

        default:
          verzoegerungsVerlust = VERZOEGERUNSVERLUST.BJOERNSEN;
          break;
      }

      // ZEILE 7 //
      final int anfangswasserspiegelInt = readInt( lnr );
      final ART_ANFANGS_WSP artAnfangswasserspiegel;
      switch( anfangswasserspiegelInt )
      {
        case 1:
          artAnfangswasserspiegel = ART_ANFANGS_WSP.DIREKTEINGABE;
          break;

        case 2:
        default:
          artAnfangswasserspiegel = ART_ANFANGS_WSP.GRENZTIEFE;
          break;

        case 3:
          artAnfangswasserspiegel = ART_ANFANGS_WSP.STATIONAER_GLEICHFOERMIGES_GEFAELLE;
          break;
      }

      // ZEILE 8 //
      final boolean berechneBruecken = readBoolean( lnr );

      // ZEILE 9 //
      final boolean berechneWehre = readBoolean( lnr );

      // ZEILE 10 //
      final boolean ausgabewerteInLiter = readBoolean( lnr );

      // ZEILE 11 //
      // bei Berechnungsart WSP bedeutet dieser Parameter: WSP-Fixierung in Längsschnitt eintragen
      // aber wiederspruch: wird auch noch mal unten extra gelesen?? -> nWSFL
      final boolean erstelleWQDateien = readBoolean( lnr );

      // ZEILE 12 //
      // bei Berechnungsart WSP bedeutet dieser Parameter: WSP-Fixierung in Querprofile eintragen
      // aber wiederspruch: wird auch noch mal unten extra gelesen?? -> nWSFQ
      final boolean ergebnislistenErstellen = readBoolean( lnr );

      final BigDecimal dGefaelle = readBigDecimal( lnr );

      final BigDecimal dQMin = readBigDecimal( lnr );

      final BigDecimal dQStep = readBigDecimal( lnr );

      final BigDecimal dQMax = readBigDecimal( lnr );

      final int nAbfluss = readInt( lnr );

      final BigDecimal dAnfang = readBigDecimal( lnr );

      final BigDecimal dEnde = readBigDecimal( lnr );

      final String strInfo = expectNextLine( lnr ).trim();

      final BigDecimal dHoehe = readBigDecimal( lnr );

      final boolean nHMO = readBoolean( lnr );

      final boolean nWSFQ = readBoolean( lnr );

      final boolean nWSFL = readBoolean( lnr );

      final String strQ = expectNextLine( lnr ).trim();

      final boolean nUSG = readBoolean( lnr );

      final boolean nKalMin = readBoolean( lnr );

      return new CalculationContentBeanPasche( bean, calcKind, fliessgesetz, output, ausgabeProfilnummer, isSimpleBerechnungWSPInt, reibungsverlustNachTrapezformal, verzoegerungsVerlust, artAnfangswasserspiegel, berechneBruecken, berechneWehre, ausgabewerteInLiter, erstelleWQDateien, ergebnislistenErstellen, dGefaelle, dQMin, dQStep, dQMax, nAbfluss, dAnfang, dEnde, strInfo, dHoehe, nHMO, nWSFQ, nWSFL, strQ, nUSG, nKalMin );
    }
  }

  @Override
  public void write( final File profDir ) throws IOException
  {
    final CalculationBean calculationBean = getCalculationBean();
    final String fileName = calculationBean.getFileName();
    final File file = new File( profDir, fileName );

    try (PrintWriter pw = new PrintWriter( file ))
    {
      // ZEILE 0 //
      pw.println( m_calcKind.ordinal() + 1 );

      // ZEILE 1 //
      pw.println( m_fliessgesetz.ordinal() + 1 );

      // ZEILE 2 //
      pw.println( m_output.ordinal() + 1 );

      // ZEILE 3 //
      printBoolean( pw, m_ausgabeProfilnummer );

      // ZEILE 4 //
      printBoolean( pw, m_isSimpleBerechnungWSPInt );

      // ZEILE 5 //
      printBoolean( pw, m_reibungsverlustNachTrapezformel );

      // ZEILE 6 //
      pw.println( m_verzoegerungsVerlust.ordinal() + 1 );

      // ZEILE 7 //
      pw.println( m_artAnfangswasserspiegel.ordinal() + 1 );

      // ZEILE 8 //
      printBoolean( pw, m_berechneBruecken );

      // ZEILE 9 //
      printBoolean( pw, m_berechneWehre );

      // ZEILE 10 //
      printBoolean( pw, m_ausgabewerteInLiter );

      // ZEILE 11 //
      // bei Berechnungsart WSP bedeutet dieser Parameter: WSP-Fixierung in Längsschnitt eintragen
      // aber wiederspruch: wird auch noch mal unten extra gelesen?? -> nWSFL
      printBoolean( pw, m_erstelleWQDateien );

      // ZEILE 12 //
      // bei Berechnungsart WSP bedeutet dieser Parameter: WSP-Fixierung in Querprofile eintragen
      // aber wiederspruch: wird auch noch mal unten extra gelesen?? -> nWSFQ
      printBoolean( pw, m_ergebnislistenErstellen );

      pw.format( Locale.US, "%f", m_gefaelle ); //$NON-NLS-1$

      pw.format( Locale.US, "%f%n", m_min ); //$NON-NLS-1$
      pw.format( Locale.US, "%f%n", m_step ); //$NON-NLS-1$
      pw.format( Locale.US, "%f%n", m_max ); //$NON-NLS-1$

      pw.println( m_abfluss );

      pw.format( Locale.US, "%f%n", m_anfang ); //$NON-NLS-1$
      pw.format( Locale.US, "%f%n", m_ende ); //$NON-NLS-1$

      pw.println( m_strInfo );

      pw.format( Locale.US, "%f%n", m_hoehe ); //$NON-NLS-1$

      printBoolean( pw, m_nhmo );

      printBoolean( pw, m_nwsfq );

      printBoolean( pw, m_nwsfl );

      pw.println( m_strQ );

      printBoolean( pw, m_nusg );

      printBoolean( pw, m_kalMin );
    }
  }

  private static void printBoolean( final PrintWriter pw, final boolean flag )
  {
    if( flag )
      pw.println( 1 );
    else
      pw.println( 0 );
  }
}