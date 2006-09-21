/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.template.featureview.Button;
import org.kalypso.template.featureview.Combo;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayout;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class LinkedFeatureControlMaker extends AbstractValueControlMaker
{
  private final boolean m_showButton;

  public LinkedFeatureControlMaker( final boolean addValidator, final boolean showButton )
  {
    super( addValidator );

    m_showButton = showButton;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.maker.AbstractValueControlMaker#createControlType(org.kalypso.gmlschema.property.IPropertyType,
   *      javax.xml.bind.JAXBElement)
   */
  @Override
  protected JAXBElement< ? extends ControlType> createControlType( Feature feature, IFeatureType ft, final IPropertyType pt, final GridDataType griddata )
  {
    if( pt.isList() )
      return null;

    if( !(pt instanceof IRelationType) )
      return null;

    final IRelationType rt = (IRelationType) pt;
    if( rt.isInlineAble() )
      return null;

    if( !rt.isLinkAble() )
      return null;

    final QName qname = rt.getQName();

    final CompositeType composite = FeatureviewHelper.FACTORY.createCompositeType();
    final List<JAXBElement< ? extends ControlType>> control = composite.getControl();

    final GridLayout layout = FeatureviewHelper.FACTORY.createGridLayout();

    /* If the button should be shown, make two columns, otherwise only one. */
    if( m_showButton )
      layout.setNumColumns( 2 );
    else
      layout.setNumColumns( 1 );

    layout.setMakeColumnsEqualWidth( false );
    layout.setMarginWidth( 1 );
    composite.setLayout( FeatureviewHelper.FACTORY.createGridLayout( layout ) );
    composite.setStyle( "SWT.NONE" ); //$NON-NLS-1$

    griddata.setHorizontalAlignment( "GridData.FILL" ); //$NON-NLS-1$
    griddata.setGrabExcessHorizontalSpace( true );
    griddata.setHorizontalSpan( 2 );

    // Text
    final Combo combo = FeatureviewHelper.FACTORY.createCombo();
    combo.setStyle( "SWT.DROP_DOWN | SWT.READ_ONLY" ); //$NON-NLS-1$
    combo.setProperty( qname );

    final GridDataType comboData = FeatureviewHelper.FACTORY.createGridDataType();
    comboData.setHorizontalAlignment( "GridData.BEGINNING" ); //$NON-NLS-1$
    comboData.setWidthHint( FeatureviewHelper.STANDARD_TEXT_FIELD_WIDTH_HINT );
    combo.setLayoutData( FeatureviewHelper.FACTORY.createGridData( comboData ) );

    control.add( FeatureviewHelper.FACTORY.createCombo( combo ) );

    // Knopf
    if( m_showButton )
    {
      final Button button = FeatureviewHelper.FACTORY.createButton();
      final GridDataType buttonData = FeatureviewHelper.FACTORY.createGridDataType();
      button.setStyle( "SWT.PUSH" ); //$NON-NLS-1$
      button.setProperty( qname );

      buttonData.setHorizontalAlignment( "GridData.BEGINNING" ); //$NON-NLS-1$
      button.setLayoutData( FeatureviewHelper.FACTORY.createGridData( buttonData ) );

      control.add( FeatureviewHelper.FACTORY.createButton( button ) );
    }

    return FeatureviewHelper.FACTORY.createComposite( composite );
  }

}
