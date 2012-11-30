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
package org.kalypso.model.wspm.pdb.wspm;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.gml.classifications.ICodeClass;
import org.kalypso.model.wspm.pdb.gaf.GafCode;

/**
 * @author Gernot Belger
 */
public class CheckoutCodeUpdater
{
  private final GafCode m_code;

  private final ICodeClass m_codeClass;

  private boolean m_changed = false;

  public CheckoutCodeUpdater( final GafCode code, final ICodeClass codeClass )
  {
    m_code = code;
    m_codeClass = codeClass;
  }

  public boolean update( )
  {
    updateCode();
    updateDescription();
    updateColor();
    return m_changed;
  }

  private void updateCode( )
  {
    final String gafCode = m_code.getCode();
    final String oldGafCode = m_codeClass.getDescription();
    if( ObjectUtils.equals( gafCode, oldGafCode ) )
      return;

    m_codeClass.setDescription( gafCode );
    m_changed = true;
  }

  private void updateDescription( )
  {
    final String description = m_code.getDescription();
    final String oldDescription = m_codeClass.getComment();

    if( StringUtils.isEmpty( oldDescription ) && StringUtils.isEmpty( description ) )
      return;

    // gml resets to null if empty -> prevent to many false changes
    if( ObjectUtils.equals( description, oldDescription ) )
      return;

    m_codeClass.setComment( description );
    m_changed = true;
  }

  private void updateColor( )
  {
    final RGB color = m_code.getColor();
    final RGB oldColor = m_codeClass.getColor();
    if( ObjectUtils.equals( color, oldColor ) )
      return;

    m_codeClass.setColor( color );
    m_changed = true;
  }
}
