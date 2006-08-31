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
package org.kalypso.ogc.gml.featureview.maker;

import java.util.List;

import javax.xml.bind.JAXBElement;

import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.AnnotationUtilities;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayout;
import org.kalypso.template.featureview.Group;
import org.kalypso.template.featureview.LayoutType;
import org.kalypso.template.featureview.SubcompositeType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Control maker for inlined features: displays the feature in a sub-composite.
 * 
 * @author Gernot
 */
public class InlineFeatureControlMaker implements IControlMaker
{
  /**
   * @see org.kalypso.ogc.gml.featureview.maker.IControlMaker#addControls(java.util.List,
   *      org.kalypso.template.featureview.LayoutType, org.kalypso.gmlschema.property.IPropertyType)
   */
  public boolean addControls( final List<JAXBElement< ? extends ControlType>> controlList, final LayoutType parentLayout, final IPropertyType ftp, final Object value )
  {
    if( !(ftp instanceof IRelationType) )
      return false;

    if( !(value instanceof Feature) )
      return false;

    final SubcompositeType compo = FeatureviewHelper.FACTORY.createSubcompositeType();
    compo.setStyle( "SWT.NONE" );
    compo.setProperty( ftp.getQName() );

    final GridDataType griddata = FeatureviewHelper.FACTORY.createGridDataType();
    final JAXBElement<GridDataType> jaxbgriddata = FeatureviewHelper.FACTORY.createGridData( griddata );

    griddata.setHorizontalAlignment( "GridData.FILL" );
    griddata.setVerticalAlignment( "GridData.FILL" );
    griddata.setGrabExcessHorizontalSpace( true );
    griddata.setGrabExcessVerticalSpace( true );
    griddata.setHorizontalSpan( 2 );

    compo.setLayoutData( jaxbgriddata );

    final Group group = FeatureviewHelper.FACTORY.createGroup();

    final GridDataType groupdata = FeatureviewHelper.FACTORY.createGridDataType();
    groupdata.setGrabExcessHorizontalSpace( true );
    groupdata.setGrabExcessVerticalSpace( true );
    groupdata.setHorizontalAlignment( "GridData.FILL" );
    groupdata.setVerticalAlignment( "GridData.FILL" );
    groupdata.setHorizontalSpan( ((GridLayout)parentLayout).getNumColumns() );

    final IAnnotation annotation = AnnotationUtilities.getAnnotation( ftp );
    final String text = annotation == null ? ftp.getQName().getLocalPart() : annotation.getLabel();
    final String tooltip = annotation == null ? null : annotation.getTooltip();

    group.setLayoutData( FeatureviewHelper.FACTORY.createGridData( groupdata ) );
    group.setText( text );
    group.setTooltip( tooltip );
    group.setStyle( "SWT.NONE" );

    final GridLayout gridLayout = FeatureviewHelper.FACTORY.createGridLayout();
    gridLayout.setNumColumns( 2 );
    group.setLayout( FeatureviewHelper.FACTORY.createGridLayout( gridLayout ) );

    group.getControl().add( FeatureviewHelper.FACTORY.createSubcomposite( compo ) );

    controlList.add( FeatureviewHelper.FACTORY.createGroup( group ) );
    
    return true;
  }

}
