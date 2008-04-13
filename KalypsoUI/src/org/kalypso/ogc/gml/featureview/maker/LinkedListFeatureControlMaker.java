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

import org.kalypso.core.jaxb.TemplateUtilitites;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.featureview.control.ChecklistOfLinksFeatureControl;
import org.kalypso.ogc.gml.featureview.control.ChecklistOfLinksFeatureviewControlFactory;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.Extensioncontrol;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayout;
import org.kalypso.template.featureview.Group;
import org.kalypso.template.featureview.Extensioncontrol.Param;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Creates feature controls for lists of linked features
 * 
 * @author Gernot Belger
 */
public class LinkedListFeatureControlMaker extends AbstractValueControlMaker
{
  private final boolean m_showSelectButtons;

  public LinkedListFeatureControlMaker( final boolean addValidator, final boolean showSelectButtons )
  {
    super( addValidator );

    m_showSelectButtons = showSelectButtons;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.maker.AbstractValueControlMaker#createControlType(org.kalypso.gmlschema.property.IPropertyType,
   *      javax.xml.bind.JAXBElement)
   */
  @Override
  protected JAXBElement< ? extends ControlType> createControlType( final Feature feature, final IFeatureType ft, final IPropertyType pt, final GridDataType griddata )
  {
    if( !(pt instanceof IRelationType) )
      return null;

    final IRelationType rt = (IRelationType) pt;
    if( rt.isInlineAble() )
      return null;

    if( !rt.isLinkAble() )
      return null;

    if( !pt.isList() )
      return null;

    final QName qname = rt.getQName();

    /* Create the UI components */
    final GridDataType listData = TemplateUtilitites.OF_FEATUREVIEW.createGridDataType();
    listData.setHorizontalAlignment( "GridData.FILL" );
    listData.setVerticalAlignment( "GridData.FILL" );
    listData.setGrabExcessHorizontalSpace( true );

    final Extensioncontrol extensioncontrol = TemplateUtilitites.OF_FEATUREVIEW.createExtensioncontrol();
    extensioncontrol.setEnabled( true );
    extensioncontrol.setExtensionId( ChecklistOfLinksFeatureviewControlFactory.class.getName() );
    extensioncontrol.setLayoutData( TemplateUtilitites.OF_FEATUREVIEW.createGridData( listData ) );
    extensioncontrol.setStyle( "SWT.NONE" );
    extensioncontrol.setVisible( true );
    extensioncontrol.setProperty( qname );

    final Param selectParam = TemplateUtilitites.OF_FEATUREVIEW.createExtensioncontrolParam();
    selectParam.setName( ChecklistOfLinksFeatureControl.PARAM_SELECT_BUTTONS );
    selectParam.setValue( Boolean.toString( m_showSelectButtons ) );
    extensioncontrol.getParam().add( selectParam );

    final GridLayout groupLayout = TemplateUtilitites.OF_FEATUREVIEW.createGridLayout();

    final Group group = TemplateUtilitites.OF_FEATUREVIEW.createGroup();
    group.setLayout( TemplateUtilitites.OF_FEATUREVIEW.createGridLayout( groupLayout ) );
    group.setStyle( "SWT.NONE" );

    group.getControl().add( TemplateUtilitites.OF_FEATUREVIEW.createExtensioncontrol( extensioncontrol ) );

    return TemplateUtilitites.OF_FEATUREVIEW.createGroup( group );
  }
}
