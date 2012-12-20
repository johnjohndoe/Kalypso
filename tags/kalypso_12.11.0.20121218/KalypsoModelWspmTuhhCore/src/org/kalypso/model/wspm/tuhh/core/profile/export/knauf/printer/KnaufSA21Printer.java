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

import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA21Bean;

/**
 * @author Dirk Kuch
 */
public class KnaufSA21Printer extends AbstractKnaufPrinter
{
  public KnaufSA21Printer( final KnaufSA21Bean bean )
  {
    super( bean );
  }

  @Override
  protected KnaufSA21Bean getBean( )
  {
    return (KnaufSA21Bean) super.getBean();
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
     * char [3-12], type F10.4 changed-> %10.1f, Station Number
     */
    builder.append( String.format( Locale.US, "%10.1f", getBean().getStation() ) ); //$NON-NLS-1$

    /**
     * char [13-20], type F8.0 changed-> %8.2f, Berechnungsabfluss Q
     */
    builder.append( String.format( Locale.US, "%8.2f", 0.0 ) ); //$NON-NLS-1$ 

    /**
     * char [21-28], type F8.0 changed-> %8.2f, Rauheitsbeiwerte linkes Vorland
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getLeftForelandRoughness() ) ); //$NON-NLS-1$ 

    /**
     * char [29-36], type F8.0 changed-> %8.2f, Rauheitsbeiwert Flussbett
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getRiverbedRoughness() ) ); //$NON-NLS-1$ 

    /**
     * char [37-44], type F8.0 changed-> %8.2f, Rauheitsbeiwert rechtes Vorland
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getRightForelandRoughness() ) ); //$NON-NLS-1$ 

    /** char [45] 1X frei */
    builder.append( " " ); //$NON-NLS-1$ 

    /** char [46] A1 ABSZETA */
    builder.append( getBean().getABSZeta() );

    /**
     * char [47-42], F6.0 changed %6.2 - örtlicher Verlustbeiwert ZETA,
     */
    builder.append( String.format( Locale.US, "%6.2f", getBean().getLocalVerlustBeiwertZeta() ) ); //$NON-NLS-1$ 

    /**
     * char [53-60], F8.0 changed %8.2 - Kote DKUK (höchster Punkt des möglichen Fließquerschnittes bei geschlossenen
     * Profilen)
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getDKUK() ) ); //$NON-NLS-1$

    /**
     * char [61-68], F8.0 changed %8.2 - Bezugsradius des Durchlasses RHK
     */
    builder.append( String.format( Locale.US, "%8.2f", 0.0 ) ); //$NON-NLS-1$

    /**
     * char [69-76], F8.0 changed %8.2 - Sohlneigung SJOD (Durchlass, Streichwehr)
     */
    builder.append( String.format( Locale.US, "%8.2f", 0.0 ) ); //$NON-NLS-1$

    /**
     * char [77-78], I2 - Steuerparameter IDP für Sonderprofile
     */
    builder.append( String.format( Locale.US, "%2d", getBean().getIDP() ) ); //$NON-NLS-1$

    /**
     * char [77-78], I2 - Steuerparameter KZD für Durchströmungsart
     */
    builder.append( String.format( Locale.US, "%2d", getBean().getKZD() ) ); //$NON-NLS-1$

    return builder.toString();
  }
}
