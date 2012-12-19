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
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA40Bean;

/**
 * @author Dirk Kuch
 */
public class KnaufSA40Printer extends AbstractKnaufPrinter
{

  public KnaufSA40Printer( final KnaufSA40Bean bean )
  {
    super( bean );
  }

  @Override
  protected KnaufSA40Bean getBean( )
  {
    return (KnaufSA40Bean) super.getBean();
  }

  @Override
  public int getMaxRowSize( )
  {
    return 79;
  }

  @Override
  public String getContent( )
  {
    final StringBuilder builder = new StringBuilder();

    /**
     * char [3-4], type I2, Steuerparameter final NPR
     */
    builder.append( String.format( Locale.US, "%2d", 2 ) ); //$NON-NLS-1$

    /**
     * char [5-6], type I2, Steuerparameter IQ (siehe unten)
     */
    builder.append( String.format( Locale.US, "%2d", 0 ) ); //$NON-NLS-1$

    /**
     * char [7-14], type F8.0 changed %8.2f, Anfangswasserspiegelhöhe <br>
     * a) exakter Wert, wenn NPR= 1,2,3,8 oder-1 <br>
     * b) Schätzwert, wenn NPR= 4,5,-4,-5,44,55
     */
    builder.append( String.format( Locale.US, "%8.2f", 0.0 ) ); //$NON-NLS-1$ //TODO

    /**
     * char [15-22], type F8.0 changed %8.2f,Endwasserspiegellage für die Eichung des Rauheitsbeiwertes
     */
    builder.append( String.format( Locale.US, "%8.2f", 0.0 ) ); //$NON-NLS-1$ //TODO

    /**
     * char [23-32], type F10.0 changed %10.2f,Sohlgefälle oder Energieliniengefälle im Anfangsprofil, wenn NPR= 5,-5
     * oder 55,8
     */
    builder.append( String.format( Locale.US, "%10.2f", 0.0 ) ); //$NON-NLS-1$ //TODO

    builder.append( StringUtils.repeat( " ", 2 ) ); //$NON-NLS-1$

    /**
     * char [35], type I1, Steuerparameter IDRUCK (nur für IVZ <> 0)<br>
     * IDRUCK=0 Berechnung final mit Startwerten wird final nicht ausgegeben<br>
     * IDRUCK=1 Resultate final für Anfangsrechnung final werden ausgedruckt
     */
    builder.append( String.format( Locale.US, "%1d", 0 ) ); //$NON-NLS-1$

    builder.append( StringUtils.repeat( " ", 2 ) ); //$NON-NLS-1$

    /**
     * char [38-39], type I2, Kennzeichnung der Varianten für die Hintereinanderschaltung von mehreren
     * Berechnungsabschnitten (NPR= 7) Für die Kennzeichnung sind nur Zahlen zwischen 1 und 99 zulässig, bei Eingabe von
     * IVA=0 wird der Kennzeichnungsparameter in der Reihenfolge der Dateneingabe gesetzt
     */
    builder.append( String.format( Locale.US, "%2d", 1 ) ); //$NON-NLS-1$

    /**
     * char [40-79], type A40, Überschrift (4.Zeile im Tabellenkopf) zur Variantenkennzeichnung, beliebiger Text
     * anstelle von SA13 wenn Spalten 40+41 leer sind, wird neuer Text nicht übernommen
     */
    builder.append( String.format( Locale.US, "%s", Messages.getString("KnaufSA40Printer_2") ) ); //$NON-NLS-1$ //$NON-NLS-2$

    return builder.toString();
  }
}
