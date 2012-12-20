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
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA29Bean;

/**
 * @author Dirk Kuch
 */
public class KnaufSA29Printer extends AbstractKnaufPrinter
{

  public KnaufSA29Printer( final KnaufSA29Bean bean )
  {
    super( bean );
  }

  @Override
  protected KnaufSA29Bean getBean( )
  {
    return (KnaufSA29Bean) super.getBean();
  }

  @Override
  public int getMaxRowSize( )
  {
    return 56;
  }

  @Override
  public String getContent( )
  {
    final StringBuilder builder = new StringBuilder();

    /**
     * char [3-12], type F10.4 changed-> %10.1f, Station Number
     */
    builder.append( String.format( Locale.US, "%10.1f", getBean().getStation() ) ); //$NON-NLS-1$

    builder.append( StringUtils.repeat( " ", 4 ) ); //$NON-NLS-1$

    /**
     * char [17-24], F8.0 changed -> %8.2f Höhe der Wehrkrone HKRONE
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getHKrone() ) ); //$NON-NLS-1$

    /**
     * char [25-32], F8.0 changed -> %8.2f Wehrbreite WEBE
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getWebe() ) ); //$NON-NLS-1$

    /**
     * char [33-40], F8.0 changed -> %8.2f Überfallbeiwert Mue
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getMueBeiwert() ) ); //$NON-NLS-1$

    /**
     * char [41-48], F8.0 changed -> %8.2f Wehrhöhe WHOEHE (im OW gemessen)
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getWHoehe() ) ); //$NON-NLS-1$

    /**
     * char [49-56], F8.0 changed -> %8.2fAusuferungshöhe im UW des Wehres HUFER
     */
    builder.append( String.format( Locale.US, "%8.2f", getBean().getHUfer() ) ); //$NON-NLS-1$

    return builder.toString();
  }
}
