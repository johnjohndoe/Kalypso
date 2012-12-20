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

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.AbstractKnaufProjectBean;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractKnaufPrinter implements IKnaufPrinter
{
  private final AbstractKnaufProjectBean m_bean;

  public AbstractKnaufPrinter( final AbstractKnaufProjectBean bean )
  {
    m_bean = bean;
  }

  protected AbstractKnaufProjectBean getBean( )
  {
    return m_bean;
  }

  @Override
  public CharSequence println( )
  {
    final StringBuilder builder = new StringBuilder();

    builder.append( String.format( "%2d", m_bean.getSatzart() ) ); //$NON-NLS-1$
    builder.append( getContent() );

    final String output = builder.toString();
    if( StringUtils.length( output ) > getMaxRowSize() )
    {
      final CharSequence cutted = output.subSequence( 0, getMaxRowSize() );
      System.out.println( "Knauf Export - too long output string has been detected - cutted:" ); //$NON-NLS-1$
      System.out.println( output );
      System.out.println( " -> " ); //$NON-NLS-1$
      System.out.println( cutted );

      return cutted;
    }

    return String.format( "%s\r\n", output ); //$NON-NLS-1$
  }

  protected abstract String getContent( );

  protected abstract int getMaxRowSize( );
}
