/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.ui.wizards.results;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Thomas Jung
 * 
 */
public class ResultThemeData
{
  IResultMeta m_result;

  THEMETYPE m_type;

  enum THEMETYPE
  {
    vectors,
    tinWspLine,
    tinWspArea,
    tinVeloLine,
    tinVeloArea,
    tinDepthLine,
    tinDepthArea,
    tinShearStressLine,
    tinShearStressArea,
    hydrograph,
    log,
    coreDataZip
  }

  public ResultThemeData( IResultMeta result, THEMETYPE type )
  {
    m_result = result;
    m_type = type;
  }

  public void setDocumentType( THEMETYPE themeType )
  {
    m_type = themeType;
  }

  public THEMETYPE getDocumentType( )
  {
    return m_type;
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( Object obj )
  {
    return EqualsBuilder.reflectionEquals( this, obj );
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return HashCodeBuilder.reflectionHashCode( this );
  }

}
