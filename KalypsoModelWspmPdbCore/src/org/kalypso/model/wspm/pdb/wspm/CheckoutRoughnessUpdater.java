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
package org.kalypso.model.wspm.pdb.wspm;

import java.awt.Color;
import java.math.BigDecimal;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.gml.classifications.IRoughnessClass;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;

/**
 * @author Gernot Belger
 */
public class CheckoutRoughnessUpdater
{
  private final Roughness m_roughness;

  private final IRoughnessClass m_roughnessClass;

  private boolean m_changed = false;

  public CheckoutRoughnessUpdater( final Roughness roughness, final IRoughnessClass roughnessClass )
  {
    m_roughness = roughness;
    m_roughnessClass = roughnessClass;
  }

  public boolean update( )
  {
    updateDescription();
    updateKst();
    updateKs();
    updateComment();
    updateColor();

    // TODO: we have no equivalent for the following properties
    // roughness.getSource();
    // roughness.getValidity();

    return m_changed;
  }

  private void updateDescription( )
  {
    final String description = m_roughness.getLabel();
    final String oldDescription = m_roughnessClass.getDescription();
    if( ObjectUtils.equals( description, oldDescription ) )
      return;

    m_roughnessClass.setDescription( description );
    m_changed = true;
  }

  private void updateKst( )
  {
    final BigDecimal kst = m_roughness.getKstValue();
    final BigDecimal oldKst = m_roughnessClass.getKstValue();
    if( ObjectUtils.equals( kst, oldKst ) )
      return;

    m_roughnessClass.setKstValue( kst );
    m_changed = true;
  }

  private void updateKs( )
  {
    final BigDecimal ks = m_roughness.getKValue();
    final BigDecimal oldKs = m_roughnessClass.getKsValue();
    if( ObjectUtils.equals( ks, oldKs ) )
      return;

    m_roughnessClass.setKsValue( ks );
    m_changed = true;
  }

  private void updateComment( )
  {
    final String comment = m_roughness.getDescription();
    final String oldComment = m_roughnessClass.getComment();
    if( ObjectUtils.equals( comment, oldComment ) )
      return;

    m_roughnessClass.setComment( comment );
    m_changed = true;
  }

  private void updateColor( )
  {
    final String colorAsString = m_roughness.getColor();
    if( StringUtils.isEmpty( colorAsString ) )
      return;
    final Color color = Color.decode( colorAsString );
    final RGB oldRGB = m_roughnessClass.getColor();
    final Color oldColor = new Color( oldRGB.red, oldRGB.green, oldRGB.blue );
    if( ObjectUtils.equals( color, oldColor ) )
      return;

    final RGB newRGB = new RGB( color.getRed(), color.getGreen(), color.getBlue() );
    m_roughnessClass.setColor( newRGB );
    m_changed = true;
  }
}