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
import javax.xml.namespace.QName;

import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.AnnotationUtilities;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayout;
import org.kalypso.template.featureview.LabelType;
import org.kalypso.template.featureview.LayoutDataType;
import org.kalypso.template.featureview.LayoutType;
import org.kalypso.template.featureview.ValidatorLabelType;

/**
 * An abstract control maker with some kind of standard behaviourr: a label followed by a type specific control followed
 * by the validator-rule-label. Normally used for value typed properties.
 * 
 * @author Gernot Belger
 */
public abstract class AbstractValueControlMaker implements IControlMaker
{
  private final boolean m_addValidator;

  public AbstractValueControlMaker( final boolean addValidator )
  {
    m_addValidator = addValidator;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.maker.IControlMaker#addControls(java.util.List,
   *      org.kalypso.template.featureview.LayoutType, org.kalypso.gmlschema.property.IPropertyType)
   */
  public boolean addControls( final List<JAXBElement< ? extends ControlType>> controlList, final LayoutType parentLayout, final IPropertyType ftp, final Object value ) throws AbortCreationException
  {
    /* Create the 'real' control */
    final GridDataType griddata = FeatureviewHelper.FACTORY.createGridDataType();
    final JAXBElement< ? extends ControlType> controlElement = createControlType( ftp, griddata );
    if( controlElement == null )
      return false;
    final JAXBElement<GridDataType> jaxbgriddata = FeatureviewHelper.FACTORY.createGridData( griddata );
    controlElement.getValue().setLayoutData( jaxbgriddata );
    
    /* Some common values i need */
    final QName property = ftp.getQName();
    final IAnnotation annotation = getAnnotation( ftp );
    final String text = annotation == null ? property.getLocalPart() : annotation.getLabel();
    final String tooltip = annotation == null ? null : annotation.getTooltip();

    /* The cellcount is needed to fill the layout afterwards. */
    int cellCount = 0;

    /* Add a label */
    {
      final LabelType label = FeatureviewHelper.FACTORY.createLabelType();
      label.setStyle( "SWT.NONE" );

      label.setText( text );
      label.setTooltip( tooltip );
      label.setVisible( true );

      final GridDataType labelGridData = FeatureviewHelper.FACTORY.createGridDataType();
      labelGridData.setGrabExcessHorizontalSpace( false );
      labelGridData.setHorizontalAlignment( "GridData.BEGINNING" );
      labelGridData.setVerticalAlignment( getLabelVerticalAlignment() );
      label.setLayoutData( FeatureviewHelper.FACTORY.createGridData( labelGridData ) );

      controlList.add( FeatureviewHelper.FACTORY.createLabel( label ) );
      cellCount++;
    }

    /* Add the 'real' control */
    {
      final ControlType type = controlElement.getValue();

      final LayoutDataType layoutData = type.getLayoutData().getValue();
      if( layoutData instanceof GridDataType )
        cellCount += ((GridDataType) layoutData).getHorizontalSpan();

      type.setTooltip( tooltip );

      controlList.add( controlElement );
    }

    /* If a validator is needed, it is added here. */
    if( m_addValidator )
    {
      cellCount++;

      final ValidatorLabelType validatorLabel = FeatureviewHelper.FACTORY.createValidatorLabelType();
      validatorLabel.setStyle( "SWT.NONE" );

      validatorLabel.setVisible( true );
      validatorLabel.setProperty( property );

      final GridDataType labelGridData = FeatureviewHelper.FACTORY.createGridDataType();
      labelGridData.setGrabExcessHorizontalSpace( false );
      labelGridData.setHorizontalAlignment( "GridData.BEGINNING" );
      validatorLabel.setLayoutData( FeatureviewHelper.FACTORY.createGridData( labelGridData ) );
      controlList.add( FeatureviewHelper.FACTORY.createValidatorlabel( validatorLabel ) );
    }

    /* Fill the rest of the line */
    final GridLayout gridLayout = (GridLayout) parentLayout;
    final int numColumns = gridLayout.getNumColumns();
    for( int i = cellCount; i < numColumns; i++ )
    {
      final LabelType label = FeatureviewHelper.FACTORY.createLabelType();
      label.setStyle( "SWT.NONE" );
      label.setVisible( false );

      final GridDataType labelGridData = FeatureviewHelper.FACTORY.createGridDataType();
      labelGridData.setGrabExcessHorizontalSpace( false );
      labelGridData.setHorizontalAlignment( "GridData.BEGINNING" );
      label.setLayoutData( FeatureviewHelper.FACTORY.createGridData( labelGridData ) );

      controlList.add( FeatureviewHelper.FACTORY.createLabel( label ) );
    }

    return true;
  }

  protected String getLabelVerticalAlignment( )
  {
    return "GridData.CENTER";
  }

  /**
   * Bit of a hack: In order to let implementors tweak the label, we get it via this method which may be overwritten.
   * <p>
   * Standard implementation returns <code>AnnotationUtilities.getAnnotation( ftp )</code>.
   */
  protected IAnnotation getAnnotation( final IPropertyType ftp )
  {
    return AnnotationUtilities.getAnnotation( ftp );
  }

  protected abstract JAXBElement< ? extends ControlType> createControlType( final IPropertyType pt, final GridDataType griddata ) throws AbortCreationException;
}
