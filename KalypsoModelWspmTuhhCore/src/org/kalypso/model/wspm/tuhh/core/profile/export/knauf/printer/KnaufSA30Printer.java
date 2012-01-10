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

import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA30Bean;

/**
 * @author Dirk Kuch
 */
public class KnaufSA30Printer extends AbstractKnaufPrinter
{
  public KnaufSA30Printer( final KnaufSA30Bean bean )
  {
    super( bean );
  }

  @Override
  protected KnaufSA30Bean getBean( )
  {
    return (KnaufSA30Bean) super.getBean();
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
     * char [13-24], type F12.0 changed-> %12.3f, Abstand des Profilpunktes von der Bezugsachse
     */
    builder.append( String.format( Locale.US, "%12.3f", getBean().getBreite() ) ); //$NON-NLS-1$

    /**
     * char [25-36], type F12.0 changed-> %12.3f, m+NN z = Höhe des Profilpunktes
     */
    builder.append( String.format( Locale.US, "%12.3f", getBean().getHoehe() ) ); //$NON-NLS-1$

    /** char [37] 1X leer */
    builder.append( " " ); //$NON-NLS-1$

    /** char [38-39] A2 KZ = Kennziffer für Grenzpunkte */
    builder.append( getBean().getKennziffer() ); //$NON-NLS-1$

    /** char [40] A1 AG = Ausuferungsgrenze */
    builder.append( getBean().getAusuferungsgrenze() ); //$NON-NLS-1$

    /** char [41-49] A9 - beliebiger Text */
    builder.append( getBean().getDescription() ); //$NON-NLS-1$

    /** char [50-57] F8.3 - dp = Breite eines Bewuchselementes */
    builder.append( String.format( Locale.US, "%8.3f", getBean().getBewuchsDp() ) ); //$NON-NLS-1$

    /** char [58-65] F8.3 - ax = Bewuchselementabstand in Fließrichtung */
    builder.append( String.format( Locale.US, "%8.3f", getBean().getBewuchsAx() ) ); //$NON-NLS-1$

    /** char [66-73] F8.3 - ay = Bewuchselementabstand quer zu ax */
    builder.append( String.format( Locale.US, "%8.3f", getBean().getBewuchsAy() ) ); //$NON-NLS-1$

    /** char [74-80] F7.1 mm K-Wert, Reichweite bis neuer Wert eingegeben wird */
    builder.append( String.format( Locale.US, "%7.1f", getBean().getRoughness() ) ); //$NON-NLS-1$

    /** char [81-95] F15.0 change to %15.2f m Rechtswert Gauss-Krüger */
    builder.append( String.format( Locale.US, "%15.3f", getBean().getRechtswert() ) ); //$NON-NLS-1$

    /** char [96-110] F15.0 change to %15.2f m Hochwert Gauss-Krüger */
    builder.append( String.format( Locale.US, "%15.3f", getBean().getHochwert() ) ); //$NON-NLS-1$

    return builder.toString();
  }
}
