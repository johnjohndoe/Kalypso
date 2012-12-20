/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.ewawi.data;

import java.io.File;

import org.apache.commons.io.FilenameUtils;
import org.kalypso.model.wspm.ewawi.utils.EwawiKey;

/**
 * Represents one data set of ewawi files (i.e. .pro, .sta, .epl file with same key).
 * 
 * @author Gernot Belger
 */
public class EwawiPlus
{
  private final EwawiKey m_key;

  private final EwawiPro m_proIndex = new EwawiPro();

  private final EwawiSta m_staIndex = new EwawiSta();

  private final EwawiEpl m_eplIndex = new EwawiEpl();

  public EwawiPlus( final EwawiKey key )
  {
    m_key = key;
  }

  public EwawiKey getKey( )
  {
    return m_key;
  }

  public EwawiPro getProIndex( )
  {
    return m_proIndex;
  }

  public EwawiSta getStaIndex( )
  {
    return m_staIndex;
  }

  public EwawiEpl getEplIndex( )
  {
    return m_eplIndex;
  }

  public void setSourceFile( final File sourceFile )
  {
    /* REMARK: If there are two source files (e.g. .pro files) which generate the same key, only the last will be remembered. */
    /* REMARK: The lines however will be all stored in the corresponding data object. */
    final String fileName = sourceFile.getName();
    final String extension = FilenameUtils.getExtension( fileName ).toLowerCase();
    switch( extension )
    {
      case "pro": //$NON-NLS-1$
      {
        m_proIndex.setSourceFile( sourceFile );
        break;
      }
      case "sta": //$NON-NLS-1$
      {
        m_staIndex.setSourceFile( sourceFile );
        break;
      }
      case "epl": //$NON-NLS-1$
      {
        m_eplIndex.setSourceFile( sourceFile );
        break;
      }
      default:
      {
        break;
      }
    }
  }

  public void addProLine( final EwawiProLine proLine )
  {
    m_proIndex.addProLine( proLine );
  }

  public void addStaLine( final EwawiStaLine staLine )
  {
    m_staIndex.addStaLine( staLine );
  }

  public void addEplLine( final EwawiEplLine eplLine )
  {
    m_eplIndex.addEplLine( eplLine );
  }
}