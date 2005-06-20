/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.filter.filters.valuecomp;

import java.lang.reflect.UndeclaredThrowableException;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.util.parser.IParser;
import org.kalypso.util.parser.ParserException;
import org.kalypso.util.parser.ParserFactory;

/**
 * AbstractValueComp
 * 
 * @author schlienger
 */
public abstract class AbstractValueComp implements IValueComp
{
  protected final String m_axisType;

  protected final IAxis m_axis;

  protected final IParser m_parser;

  /**
   * Constructor
   * 
   * @param axes
   * @param axisType
   */
  public AbstractValueComp( final IAxis[] axes, final String axisType )
  {
    m_axisType = axisType;

    m_axis = ObservationUtilities.findAxisByType( axes, axisType );

    m_parser = ParserFactory.createParser( m_axis.getDataClass() );
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.valuecomp.IValueComp#getAxis()
   */
  public IAxis getAxis()
  {
    return m_axis;
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.valuecomp.IValueComp#validates(java.lang.Object)
   */
  public boolean validates( Object element )
  {
    try
    {
      return internalValidates( element );
    }
    catch( ParserException e )
    {
      e.printStackTrace();

      throw new UndeclaredThrowableException( e );
    }
  }

  /**
   * @param element
   * @return true if comparison validates
   * @throws ParserException
   * 
   * @see org.kalypso.ogc.sensor.filter.filters.valuecomp.IValueComp#validates(java.lang.Object)
   */
  protected abstract boolean internalValidates( Object element ) throws ParserException;
}