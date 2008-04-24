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
package org.kalypso.ogc.gml.gui;

import java.math.BigDecimal;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.restriction.IRestriction;
import org.kalypso.gmlschema.property.restriction.RestrictionUtilities;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandler;

/**
 * Handler for displaying the big decimals.
 * 
 * @author Holger Albert
 */
public class XsdDecimalGuiTypeHandler extends XsdBaseGuiTypeHandler
{
  /**
   * The constructor.
   * 
   * @param handler
   *            The base type handler.
   */
  public XsdDecimalGuiTypeHandler( final XsdBaseTypeHandler< ? > handler )
  {
    super( handler );
  }

  /**
   * @see org.kalypso.ogc.gml.gui.XsdBaseGuiTypeHandler#createFeatureModifier(org.kalypso.gmlschema.property.IPropertyType,
   *      org.kalypso.ogc.gml.selection.IFeatureSelectionManager,
   *      org.kalypso.ogc.gml.featureview.IFeatureChangeListener, java.lang.String)
   */
  @Override
  public IFeatureModifier createFeatureModifier( final IPropertyType ftp, final IFeatureSelectionManager selectionManager, final IFeatureChangeListener fcl, final String format )
  {
    final String fmt;
    if( format == null )
    {
      // bit of a HACK: set format according to fraction digits, if any are set.
      // Maybe change this later to support fraction digits within the modifier stuff
      final IValuePropertyType vpt = (IValuePropertyType) ftp;
      final IRestriction[] restrictions = vpt.getRestriction();
      final Integer fractionDigits = RestrictionUtilities.findFractionDigits( restrictions );
      if( fractionDigits == null )
        fmt = null;
      else
        fmt = "%." + fractionDigits + "f";
    }
    else
      fmt = format;

    return super.createFeatureModifier( ftp, selectionManager, fcl, fmt );
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    if( element instanceof BigDecimal )
    {
      final BigDecimal decimal = (BigDecimal) element;
      return String.format( "%.2f", decimal.doubleValue() ).replace( ",", "." );
    }

    return super.getText( element );
  }
}