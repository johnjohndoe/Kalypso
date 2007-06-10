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

import java.util.List;

import javax.xml.bind.JAXBElement;

import org.kalypso.core.jaxb.TemplateUtilitites;
import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
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
  public boolean addControls( final List<JAXBElement< ? extends ControlType>> controlList, final LayoutType parentLayout, IFeatureType ft, final IPropertyType ftp, final Feature feature )
  {
    if( !(ftp instanceof IRelationType) )
      return false;

    final Object value = feature == null ? null : feature.getProperty( ftp );

    if( !(value instanceof Feature) )
      return false;

    final SubcompositeType compo = TemplateUtilitites.OF_FEATUREVIEW.createSubcompositeType();
    compo.setStyle( "SWT.NONE" ); //$NON-NLS-1$
    compo.setProperty( ftp.getQName() );

    final GridDataType griddata = TemplateUtilitites.OF_FEATUREVIEW.createGridDataType();
    final JAXBElement<GridDataType> jaxbgriddata = TemplateUtilitites.OF_FEATUREVIEW.createGridData( griddata );

    griddata.setHorizontalAlignment( "GridData.FILL" ); //$NON-NLS-1$
    griddata.setVerticalAlignment( "GridData.FILL" ); //$NON-NLS-1$
    griddata.setGrabExcessHorizontalSpace( true );
    griddata.setGrabExcessVerticalSpace( true );
    griddata.setHorizontalSpan( 2 );

    compo.setLayoutData( jaxbgriddata );

    final Group group = TemplateUtilitites.OF_FEATUREVIEW.createGroup();

    final GridDataType groupdata = TemplateUtilitites.OF_FEATUREVIEW.createGridDataType();
    groupdata.setGrabExcessHorizontalSpace( true );
    groupdata.setGrabExcessVerticalSpace( true );
    groupdata.setHorizontalAlignment( "GridData.FILL" ); //$NON-NLS-1$
    groupdata.setVerticalAlignment( "GridData.FILL" ); //$NON-NLS-1$
    groupdata.setHorizontalSpan( ((GridLayout) parentLayout).getNumColumns() );

    final IAnnotation annotation = AnnotationUtilities.getAnnotation( ftp );
    final String text = annotation == null ? ftp.getQName().getLocalPart() : annotation.getLabel();
    final String tooltip = annotation == null ? null : annotation.getTooltip();

    group.setLayoutData( TemplateUtilitites.OF_FEATUREVIEW.createGridData( groupdata ) );
    group.setText( text );
    group.setTooltip( tooltip );
    group.setStyle( "SWT.NONE" ); //$NON-NLS-1$

    final GridLayout gridLayout = TemplateUtilitites.OF_FEATUREVIEW.createGridLayout();
    gridLayout.setNumColumns( 2 );
    group.setLayout( TemplateUtilitites.OF_FEATUREVIEW.createGridLayout( gridLayout ) );

    group.getControl().add( TemplateUtilitites.OF_FEATUREVIEW.createSubcomposite( compo ) );

    controlList.add( TemplateUtilitites.OF_FEATUREVIEW.createGroup( group ) );

    return true;
  }

}
