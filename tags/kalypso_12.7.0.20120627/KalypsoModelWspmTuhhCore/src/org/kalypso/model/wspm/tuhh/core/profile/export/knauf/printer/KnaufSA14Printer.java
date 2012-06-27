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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.printer;

import java.util.Locale;

import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA14Bean;

import com.google.common.base.Strings;

/**
 * @author Dirk Kuch
 */
public class KnaufSA14Printer extends AbstractKnaufPrinter
{

  public KnaufSA14Printer( final KnaufSA14Bean bean )
  {
    super( bean );
  }

  @Override
  protected KnaufSA14Bean getBean( )
  {
    return (KnaufSA14Bean) super.getBean();
  }

  @Override
  public int getMaxRowSize( )
  {
    return 78;
  }

  @Override
  public String getContent( )
  {
    final StringBuilder builder = new StringBuilder();

    /**
     * <pre>
     * char [3-4], type I2, Ausdruckparameter IA
     * 0 kein Datenausdruck
     * </pre>
     */
    builder.append( String.format( "%2d", 0 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [5-6], type I2,  Steuerparameter NHYD für das Fließgesetzes
     * </pre>
     */
    builder.append( String.format( "%2d", getBean().getNHyd() ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [7-8], type I2,  Steuerparameter für Erweiterungsverluste<br>
     * abgeminderter Stoßverlust nach BORDA-CARNOT
     * </pre>
     */
    builder.append( String.format( "%2d", 1 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [9-12], type I4,   Anzahl der Profile eines Berechnungsabschnittes IE
     * </pre>
     */
    builder.append( String.format( "%4d", getBean().getNumberOfProfiles() ) ); //$NON-NLS-1$

    /**
     * char [13-16], type I4, Steuerparameter IPR für die Ausgabe von Zwischenergebnissen,
     */
    builder.append( String.format( "%4d", 0 ) ); //$NON-NLS-1$

    /**
     * char [17-20], type I4, Steuerparameter IPAU für die Ausgabe von Zwischenergebnissen bei Verzweigungsberechnungen
     */
    builder.append( String.format( "%4d", 0 ) ); //$NON-NLS-1$

    /**
     * char [21], empty
     */
    builder.append( " " ); //$NON-NLS-1$

    /**
     * <pre>
     * char [22], type I1, Wahl der Ergebnislisten
     * IFP = 0 Ausgabe LUA-NRW - DOS-Format
     * </pre>
     */
    builder.append( "0" ); //$NON-NLS-1$

    /**
     * <pre>
     * char [23], type I1, Stellung der Überschrift (nur für IFP = 0)
     * “STATIONAERE WASSERSPIEGELLAGEN“
     * IDR=0 Matrixdrucker, IDR=1 HP-Laserdrucker
     * </pre>
     */
    builder.append( "0" ); //$NON-NLS-1$

    /**
     * <pre>
     * char [24], type I1, Steuerparameter IDAT für die Dateneingabe
     * IDAT=0 Kennung der Profile über Stationierungsangaben
     * </pre>
     */
    builder.append( "0" ); //$NON-NLS-1$

    builder.append( Strings.repeat( " ", 3 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [28], type I1,Steuerparameter IAUTO für die automatische Umkehrung der Berechnungsrichtung
     * IAUTO=0 normale Berechnung ohne Umkehrung
     * </pre>
     */
    builder.append( "0" ); //$NON-NLS-1$

    builder.append( Strings.repeat( " ", 3 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [32], type I1, Steuerparameter IPUNKT für die Ergänzung fehlender Nullen bei
     * formatierter Dateneingabe (nur für spezielle alte Datensätze)
     * Default : IPUNKT=0
     * </pre>
     */
    builder.append( "0" ); //$NON-NLS-1$

    /**
     * <pre>
     * char [33-36], type I4, Steuerparameter IZMAX für die maximale Zeilenanzahl im Resultatausdruck
     * Default : IZMAX = 67, bei HTML IZMAX = 88
     * </pre>
     */
    builder.append( String.format( "%4d", 67 ) ); //$NON-NLS-1$

    builder.append( Strings.repeat( " ", 3 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [40], type I1, Steuerparameter NPOSEY
     * Korrektur des benetzten Umfanges bei gegliederten Querschnitten (nur bei NHYD=1 - 4 )
     * NPOSEY = 0 ohne Korrektur
     * </pre>
     */
    builder.append( "0" ); //$NON-NLS-1$

    builder.append( Strings.repeat( " ", 3 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [44], type I1, Berechnung der maßgebenden Energiehöhe
     * NBETA = 0 mit Geschwindigkeitsverteilungsbeiwerten ALPHA
     * </pre>
     */
    builder.append( "0" ); //$NON-NLS-1$

    builder.append( Strings.repeat( " ", 3 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [48], type I1, Steuerparameter IFORM für die Rechnung mit Formbeiwerten
     * IFORM= 0 keine Berücksichtigung von Formbeiwerten
     * </pre>
     */
    builder.append( "0" ); //$NON-NLS-1$

    builder.append( Strings.repeat( " ", 3 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [52], type I1, Wahl der Bezugshöhe
     * NN = 0 Höhenangaben in NN + m (Pegel Amsterdam)
     * </pre>
     */
    builder.append( "0" ); //$NON-NLS-1$

    /**
     * <pre>
     * char [53-54], type I2, IQPO < 0 Datei *.QPO wird nicht erstellt
     * </pre>
     */
    builder.append( String.format( "%2d", 0 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [55-56], type I2, ILPO < 0 Datei *.LPO wird nicht erstellt
     * </pre>
     */
    builder.append( String.format( "%2d", 0 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [57-58], type I2,IUFG ≤ 0 Datei *.UFG wird nicht erstellt
     * </pre>
     */
    builder.append( String.format( "%2d", 0 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [59-60], type I2, ISMG ≤ 0 Datei *.SMG wird nicht erstellt
     * </pre>
     */
    builder.append( String.format( "%2d", 0 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [61-62], type I2, IHTM < 0 Datei *.HTM wird nicht erstellt
     * </pre>
     */
    builder.append( String.format( "%2d", 0 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [63-64], type I2, IE94 ≤ 0 Datei *.E94 wird nicht erstellt
     * </pre>
     */
    builder.append( String.format( "%2d", 0 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [65-66], type I2, IE97 < 0 Datei *.E97 wird nicht erstellt
     * </pre>
     */
    builder.append( String.format( "%2d", 0 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [67-68], type I2, IKTAU = 2 alle Profile erhalten Kennzeichen "K"
     * (Ausgabe K-Tau-Tabellen für alle Profile)
     * </pre>
     */
    builder.append( String.format( "%2d", 0 ) ); //$NON-NLS-1$

    builder.append( Strings.repeat( " ", 2 ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [71 - 76], type F6.0, Sinuosität von Mäanderabflüssen SM
     * </pre>
     */
    builder.append( String.format( Locale.US, "%6.0f", 0.0 ) ); //$NON-NLS-1$

    builder.append( " " ); //$NON-NLS-1$

    /**
     * <pre>
     * char [78], type I1, Steuerparameter NFROU zur Berechnung der Froude'schen Zahl
     * NFROU=0 Berechnung nach Gl. 2.5-9
     * </pre>
     */
    builder.append( "0" ); //$NON-NLS-1$

    return builder.toString();
  }

}
