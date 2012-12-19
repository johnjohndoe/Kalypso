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

import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA15Bean;

/**
 * @author Dirk Kuch
 */
public class KnaufSA15Printer extends AbstractKnaufPrinter
{

  public KnaufSA15Printer( final KnaufSA15Bean bean )
  {
    super( bean );
  }

  @Override
  protected KnaufSA15Bean getBean( )
  {
    return (KnaufSA15Bean) super.getBean();
  }

  @Override
  public int getMaxRowSize( )
  {
    return 80;
  }

  @Override
  public String getContent( )
  {
    final StringBuilder builder = new StringBuilder();

    /**
     * char [3-10], type F8.0 changed-> %8.2f, Länge des Berechnungsabschnittes
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getCalculationDistance() ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [11 - 18], type F8.0 changed-> %8.2f, m+NN Höhe der Wehrkrone
     * </pre>
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getWeirHeight() ) ); //$NON-NLS-1$

    /**
     * char [19-26], type F8.0 changed-> %8.2f, Wehrbreite
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getWeirWidth() ) ); //$NON-NLS-1$

    /**
     * char [27-34], type F8.0 changed-> %8.2f, Überfallbeiwert für die Form des Wehres
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getWeirUeberfallBeiwert() ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [35-42], type F8.0 changed-> %8.3f, Genauigkeitsschranke für die Wasserspiegellage
     *         Vorbesetzung : EPSH=0.005
     * </pre>
     */
    builder.append( String.format( Locale.US, "%8.3f", getBean().getEPSH() ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [43-50], type F8.0 changed-> %8.3f, Genauigkeitsschranke für den Energiehöhenvergleich
     *         Vorbesetzung : EPSV=0.01
     * </pre>
     */
    builder.append( String.format( Locale.US, "%8.3f", getBean().getEPSV() ) ); //$NON-NLS-1$

    /**
     * <pre>
     * char [51-58], type F8.0 changed-> %8.3f, Erweiterungsverlustfaktor für NCAR=2
     *        Vorbesetzung : DELCAR = 2/3
     * </pre>
     */
    builder.append( String.format( Locale.US, "%8.3f", 0.0 ) ); //$NON-NLS-1$ //TODO at the moment NCAR=2 is not supported!

    /**
     * <pre>
     * char [59-70], type F12.0 changed-> %12.3f, kinematische Zähigkeit der Flüssigkeit
     * </pre>
     */
    builder.append( String.format( Locale.US, "%12.3f", getBean().getRNY() ) ); //$NON-NLS-1$ //TODO

    /**
     * <pre>
     * char [71-80], type F10.0 changed-> %10.3f, Widerstandszahl für durchströmten Bewuchs
     * </pre>
     */
    builder.append( String.format( Locale.US, "%10.3f", getBean().getCWR() ) ); //$NON-NLS-1$ //TODO

    return builder.toString();
  }
}
