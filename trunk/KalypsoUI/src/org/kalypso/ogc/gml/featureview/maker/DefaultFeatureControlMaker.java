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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.core.jaxb.TemplateUtilitites;
import org.kalypso.gmlschema.adapter.DefaultAnnotation;
import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.i18n.Messages;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.LayoutType;
import org.kalypso.template.featureview.Text;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Control maker for the default feature properties. Replaces the old DefaultControlMaker.
 * 
 * @author Gernot Belger
 */
public class DefaultFeatureControlMaker extends AbstractValueControlMaker
{
  public final static QName QNAME_GML_METADATA = new QName( NS.GML3, "metaDataProperty" ); //$NON-NLS-1$

  public final static QName QNAME_GML_LOCATION = new QName( NS.GML3, "location" ); //$NON-NLS-1$

  public final static QName QNAME_GML_BOUNDEDBY = new QName( NS.GML3, "boundedBy" ); //$NON-NLS-1$

  public final static QName QNAME_GML_DESCRIPTION = new QName( NS.GML3, "description" ); //$NON-NLS-1$

  public final static QName QNAME_GML_NAME = new QName( NS.GML3, "name" ); //$NON-NLS-1$

  private final List<JAXBElement< ? extends ControlType>> m_descControls = new ArrayList<JAXBElement< ? extends ControlType>>();

  public DefaultFeatureControlMaker( final boolean addValidator )
  {
    super( addValidator );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.maker.AbstractValueControlMaker#addControls(java.util.List,
   *      org.kalypso.template.featureview.LayoutType, org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public boolean addControls( final List<JAXBElement< ? extends ControlType>> controlList, final LayoutType parentLayout, IFeatureType ft, final IPropertyType pt, final Feature feature ) throws AbortCreationException
  {
    final QName qname = pt.getQName();

    // HACK: in order to reverse the order of controls, we remeber them in own lists until we get to 'description'.
    // This also implies that name and description must be there in order to have any output at all
    final List<JAXBElement< ? extends ControlType>> list;
    if( QNAME_GML_DESCRIPTION.equals( qname ) )
      list = m_descControls;
    else
      list = controlList;

    final boolean result = super.addControls( list, parentLayout, ft, pt, feature );

    // HACK: se above
    if( QNAME_GML_NAME.equals( qname ) )
      controlList.addAll( m_descControls );

    return result;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.maker.AbstractValueControlMaker#createControlType(org.kalypso.gmlschema.property.IPropertyType)
   */
  @Override
  protected JAXBElement< ? extends ControlType> createControlType( Feature feature, IFeatureType ft, final IPropertyType pt, final GridDataType griddata ) throws AbortCreationException
  {
    final QName qname = pt.getQName();

    /* 'boundedBy', so abort. */
    if( QNAME_GML_BOUNDEDBY.equals( qname ) )
      throw new AbortCreationException();

    if( QNAME_GML_METADATA.equals( qname ) )
      throw new AbortCreationException();

    if( QNAME_GML_LOCATION.equals( qname ) )
      throw new AbortCreationException();

    if( QNAME_GML_DESCRIPTION.equals( qname ) )
    {
      // everything else will be edited in a text field
      final Text editor = TemplateUtilitites.OF_FEATUREVIEW.createText();
      editor.setStyle( "SWT.MULTI | SWT.BORDER" ); //$NON-NLS-1$
      editor.setEditable( true );
      editor.setProperty( qname );

      griddata.setGrabExcessHorizontalSpace( Boolean.TRUE );
      griddata.setHorizontalAlignment( "GridData.FILL" ); //$NON-NLS-1$
      griddata.setVerticalAlignment( "GridData.BEGINNING" ); //$NON-NLS-1$
      // REMARK: this value is random: where do we get a good value (like 2 times the normal height of a text field?).
      griddata.setHeightHint( new Integer( 30 ) );
      griddata.setHorizontalSpan( 1 );

      return TemplateUtilitites.OF_FEATUREVIEW.createText( editor );
    }

    if( QNAME_GML_NAME.equals( qname ) )
    {
      // everything else will be edited in a text field
      final Text editor = TemplateUtilitites.OF_FEATUREVIEW.createText();
      editor.setStyle( "SWT.BORDER" ); //$NON-NLS-1$
      editor.setEditable( true );
      editor.setProperty( qname );

      griddata.setGrabExcessHorizontalSpace( Boolean.TRUE );
      griddata.setHorizontalAlignment( "GridData.FILL" ); //$NON-NLS-1$
      griddata.setHorizontalSpan( 1 );

      return TemplateUtilitites.OF_FEATUREVIEW.createText( editor );
    }

    /* Else we are not responsible for this property. */
    return null;
  }

  @Override
  protected String getLabelVerticalAlignment( )
  {
    return "GridData.BEGINNING"; //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.maker.AbstractValueControlMaker#getAnnotation(org.kalypso.gmlschema.property.IPropertyType)
   */
  @Override
  protected IAnnotation getAnnotation( final IPropertyType ftp )
  {
    final QName qname = ftp.getQName();
    if( QNAME_GML_DESCRIPTION.equals( qname ) )
      return new DefaultAnnotation( "de", Messages.getString( "org.kalypso.ogc.gml.featureview.maker.DefaultFeatureControlMaker.desc" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    if( QNAME_GML_NAME.equals( qname ) )
      return new DefaultAnnotation( "de", Messages.getString( "org.kalypso.ogc.gml.featureview.maker.DefaultFeatureControlMaker.name" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    return super.getAnnotation( ftp );
  }

}
