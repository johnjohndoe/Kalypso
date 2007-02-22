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
package org.kalypso.ogc.gml.featureview.maker;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.kalypso.contribs.javax.xml.namespace.ListQName;
import org.kalypso.contribs.javax.xml.namespace.MixedQName;
import org.kalypso.core.jaxb.TemplateUtilitites;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.gui.GuiTypeRegistrySingleton;
import org.kalypso.ogc.gml.gui.IGuiTypeHandler;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This is one control maker tries to create the control via the {@link org.kalypso.ogc.gml.gui.IGuiTypeHandler}.
 * 
 * @author Gernot Belger
 */
public class GuiHandlerControlMaker extends AbstractValueControlMaker
{
  public GuiHandlerControlMaker( final boolean addValidator )
  {
    super( addValidator );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.maker.AbstractValueControlMaker#createControlType(org.kalypso.gmlschema.property.IValuePropertyType,
   *      javax.xml.bind.JAXBElement)
   */
  @Override
  protected JAXBElement< ? extends ControlType> createControlType( final Feature feature, final IFeatureType ft, final IPropertyType pt, final GridDataType griddata )
  {
    if( !(pt instanceof IValuePropertyType) )
      return null;

    final IValuePropertyType vpt = (IValuePropertyType) pt;

    final QName valueQName = vpt.getValueQName();
    /* We try to delegate to the gui type handlers. If this is not possible, we are not responsible. */
    if( !GuiTypeRegistrySingleton.getTypeRegistry().hasTypeName( valueQName ) )
      return null;

    if( valueQName instanceof ListQName )
    {
      return null;
    }
    else if( valueQName instanceof MixedQName )
    {
      // TODO: what to do?
      return null;
    }
    
    final IGuiTypeHandler handler = GuiTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( valueQName );

    final JAXBElement< ? extends ControlType> controlElement = handler.createFeatureviewControl( pt, TemplateUtilitites.OF_FEATUREVIEW );

    final ControlType type = controlElement.getValue();

    if( type instanceof CompositeType )
    {
      // TODO Is it safe to remove the following two lines, because the composite should not grab more space than
      // needed. */
      griddata.setHorizontalAlignment( "GridData.FILL" ); //$NON-NLS-1$
      griddata.setGrabExcessHorizontalSpace( true );
      griddata.setHorizontalSpan( 2 );
    }
    else
    {
      griddata.setHorizontalAlignment( "GridData.BEGINNING" ); //$NON-NLS-1$
      griddata.setHorizontalSpan( 1 );

      // TODO is this ok for all controls?
      //griddata.setWidthHint( FeatureviewHelper.STANDARD_TEXT_FIELD_WIDTH_HINT );
      griddata.setHorizontalAlignment( "GridData.FILL" ); //$NON-NLS-1$
      griddata.setGrabExcessHorizontalSpace( true );

      // if( tweakBorderFlag )
      // {
      // final String style = type.getStyle();
      // final int swtStyle = SWTUtilities.createStyleFromString( style );
      // final int swtStyleWithoutBorder = swtStyle & (0xfffffff ^ SWT.BORDER);
      //
      // // TODO: SWTUtilities.createStringFromStyle( );
      // final String styleWithoutBorder = "SWT.NONE";// SWTUtilities.createStringFromStyle( swtStyleWithoutBorder );
      //
      // type.setStyle( styleWithoutBorder );
      // }
    }

    return controlElement;
  }

}
