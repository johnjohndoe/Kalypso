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
import org.kalypso.model.wspm.core.gml.classifications.IVegetationClass;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;

/**
 * @author Gernot Belger
 */
public class CheckoutVegetationUpdater
{
  private final Vegetation m_vegetation;

  private final IVegetationClass m_vegetationClass;

  private boolean m_changed = false;

  public CheckoutVegetationUpdater( final Vegetation vegetation, final IVegetationClass vegetationClass )
  {
    m_vegetation = vegetation;
    m_vegetationClass = vegetationClass;
  }

  public boolean update( )
  {
    updateDescription();
    updateAx();
    updateAy();
    updateDp();
    updateComment();
    updateColor();

    // TODO: we have no equivalent for the following properties
    // vegetation.getSource();
    // vegetation.getValidity();
    return m_changed;
  }

  private void updateDescription( )
  {
    final String description = m_vegetation.getLabel();
    final String oldDescription = m_vegetationClass.getDescription();
    if( ObjectUtils.equals( description, oldDescription ) )
      return;

    m_vegetationClass.setDescription( description );
    m_changed = true;
  }

  private void updateAx( )
  {
    final BigDecimal ax = m_vegetation.getAx();
    final BigDecimal oldAx = m_vegetationClass.getAx();
    if( ObjectUtils.equals( ax, oldAx ) )
      return;

    m_vegetationClass.setAx( ax );
    m_changed = true;
  }

  private void updateAy( )
  {
    final BigDecimal ay = m_vegetation.getAy();
    final BigDecimal oldAy = m_vegetationClass.getAy();
    if( ObjectUtils.equals( ay, oldAy ) )
      return;

    m_vegetationClass.setAy( ay );
    m_changed = true;
  }

  private void updateDp( )
  {
    final BigDecimal dp = m_vegetation.getDp();
    final BigDecimal oldDp = m_vegetationClass.getDp();
    if( ObjectUtils.equals( dp, oldDp ) )
      return;

    m_vegetationClass.setDp( dp );
    m_changed = true;
  }

  private void updateComment( )
  {
    final String comment = m_vegetation.getDescription();
    final String oldComment = m_vegetationClass.getComment();
    if( ObjectUtils.equals( comment, oldComment ) )
      return;

    m_vegetationClass.setComment( comment );
    m_changed = true;
  }

  private void updateColor( )
  {

    final String colorAsString = m_vegetation.getColor();
    if( StringUtils.isEmpty( colorAsString ) )
      return;
    final Color color = Color.decode( colorAsString );
    final RGB oldRGB = m_vegetationClass.getColor();
    final Color oldColor = new Color( oldRGB.red, oldRGB.green, oldRGB.blue );
    if( ObjectUtils.equals( color, oldColor ) )
      return;

    final RGB newRGB = new RGB( color.getRed(), color.getGreen(), color.getBlue() );
    m_vegetationClass.setColor( newRGB );
    m_changed = true;
  }
}