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

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA20Bean;

/**
 * @author Dirk Kuch
 */
public class KnaufSA20Printer extends AbstractKnaufPrinter
{

  public KnaufSA20Printer( final KnaufSA20Bean bean )
  {
    super( bean );
  }

  @Override
  protected KnaufSA20Bean getBean( )
  {
    return (KnaufSA20Bean) super.getBean();
  }

  @Override
  public int getMaxRowSize( )
  {
    return 110;
  }

  @Override
  public String getContent( )
  {
    final StringBuilder builder = new StringBuilder();

    /**
     * char [3-12], type F10.4 changed-> %10.1f, Station Number
     */
    builder.append( String.format( Locale.US, "%10.1f", getBean().getStation() ) ); //$NON-NLS-1$

    /**
     * char [13-16], I4 Anzahl der einzulesenden Querprofilpunkte bzw. Anzahl der folgenden SA30
     */
    builder.append( String.format( Locale.US, "%4d", getBean().getNumberOfProfilePoints() ) ); //$NON-NLS-1$

    final Double distance = getBean().getDistanceNextProfile();

    /**
     * char [17-24], type F8.0 changed-> %8.2f, Abstand zum nächsten Querprofil, in der Achse des linken Vorlandes
     * gemessen
     */
    builder.append( String.format( Locale.US, "%8.2f", distance ) ); //$NON-NLS-1$

    /**
     * char [25-32], type F8.0 changed-> %8.2f, Abstand zum nächsten Querprofil , in der Flussachse gemessen
     */
    builder.append( String.format( Locale.US, "%8.2f", distance ) ); //$NON-NLS-1$

    /**
     * char [33-40], type F8.0 changed-> %8.2f, Abstand zum nächsten Querprofil, in der Achse des rechten Vorlandes
     * gemessen
     */
    builder.append( String.format( Locale.US, "%8.2f", distance ) ); //$NON-NLS-1$

    /**
     * char [41-48], type F8.0 changed-> %8.2f, Integral eingegebene Querschnittsfläche
     */
    builder.append( String.format( Locale.US, "%8.2f", 0.0 ) ); //$NON-NLS-1$

    /**
     * char [49-56], type F8.0 changed-> %8.2f, Integral eingegebener Anteil des benetzten Umfanges
     */
    builder.append( String.format( Locale.US, "%8.2f", 0.0 ) ); //$NON-NLS-1$

    /**
     * char [57-64], type F8.0 changed-> %8.2f, Integral eingegebener Anteil der Spiegelbreite
     */
    builder.append( String.format( Locale.US, "%8.2f", 0.0 ) ); //$NON-NLS-1$

    /** char [65] frei */
    builder.append( " " ); //$NON-NLS-1$

    /**
     * char [66] KZTAU = Kennzeichnung eines Profiles als Knoten für Wellenablaufberechnung (nur Eintrag von "K" für
     * Knoten möglich)
     */
    builder.append( " " ); //$NON-NLS-1$

    /**
     * char [67-70], type I4, Teilstreckennummer IVZ; wenn ein Wert für IVZ eingegeben wird, so wird das betreffende
     * Profil einer Teilstrecke einer Stromverzweigung zugeordnet. Als Teilstreckennummer sind nur Werte zwischen 1 und
     * 30 zulässig. IVZ=0 bedeutet, dass das betreffende Querprofil keiner Flussteilstrecke zuzuordnen ist.
     */
    builder.append( String.format( Locale.US, "%4d", 0 ) ); //$NON-NLS-1$

    /**
     * char [71-77], type F7.0 changed -> %7.2 - Pfeilerformbeiwert DELTA nach REHBOCK oder YARNELL anzugeben im OW der
     * Brücke Hinweis: falls DELTA(i) = 0 wird DELTA(i) aus DELTA(i-1) übernommen<br>
     * Alternativ bei Schützen : Neigungswinkel ß (a°)
     */
    builder.append( String.format( Locale.US, "%7.2f", getBean().getPfeilerFormBeiwert() ) ); //$NON-NLS-1$

    builder.append( " " ); //$NON-NLS-1$

    /**
     * char [79-80] - Kennzeichnung von Mehrfeldbrücken<br>
     * MFB MFB= LL Zuordnung zum linken Vorland<br>
     * MFB= FF " " Flussbett<br>
     * MFB= RR " " rechten Vorland
     */
    builder.append( StringUtils.repeat( " ", 2 ) ); //$NON-NLS-1$

    final IProfileRecord point = getBean().findLowestPoint();

    /**
     * char [81-95], type F15.0 changed -> %15.2 - Rechtswert Gauss-Krüger für tiefsten Punkt
     */
    builder.append( String.format( Locale.US, "%15.3f", point.getRechtswert() ) ); //$NON-NLS-1$

    /**
     * char [96-110], type F15.0 changed -> %15.2 - Hochwert Gauss-Krüger für tiefsten Punkt
     */
    builder.append( String.format( Locale.US, "%15.3f", point.getHochwert() ) ); //$NON-NLS-1$

    return builder.toString();
  }
}
