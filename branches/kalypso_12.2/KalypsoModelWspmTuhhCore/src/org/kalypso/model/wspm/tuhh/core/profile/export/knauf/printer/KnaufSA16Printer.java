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

import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA16Bean;

/**
 * @author Dirk Kuch
 */
public class KnaufSA16Printer extends AbstractKnaufPrinter
{

  public KnaufSA16Printer( final KnaufSA16Bean bean )
  {
    super( bean );
  }

  @Override
  protected KnaufSA16Bean getBean( )
  {
    return (KnaufSA16Bean) super.getBean();
  }

  @Override
  public int getMaxRowSize( )
  {
    return 58;
  }

  @Override
  public String getContent( )
  {
    final StringBuilder builder = new StringBuilder();

    /**
     * char [3-10], type F8.0 changed-> %8.2f,DHWMAX, maximale Differenz zwischen den Wasserspiegelhöhen
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getDHWMax() ) ); //$NON-NLS-1$

    /**
     * char [11-18], type F8.0 changed-> %8.2f, VFMAX, maximale Fließgeschwindigkeit im Hauptgerinne
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getVFMax() ) ); //$NON-NLS-1$

    /**
     * char [19-26], type F8.0 changed-> %8.2f, HZVMAX, maximale örtliche Verlusthöhe
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getHZVMax() ) ); //$NON-NLS-1$

    /**
     * char [27-34], type F8.0 changed-> %8.2f, FAKLHG, maximaler Abstand zum OW-Querprofil bei einem Fließwechsel
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getFAKLHG() ) ); //$NON-NLS-1$

    /**
     * char [35-42], type F8.0 changed-> %8.2f, FFMAX, max. Fliessquerschnitt im Hauptgerinne
     */
    builder.append( String.format( Locale.US, "%8.1f", getBean().getFFMax() ) ); //$NON-NLS-1$

    /**
     * char [43-50], type F8.0 changed-> %8.2f, BBRMAX, max. Profilabstand bei Brücken
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getBBRMax() ) ); //$NON-NLS-1$

    /**
     * char [51-58], type F8.0 changed-> %8.2f, FAKRHYD zur Berechnung der maximal zulässigen Rauheit
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getFAKRHYD() ) ); //$NON-NLS-1$

    return builder.toString();
  }
}
