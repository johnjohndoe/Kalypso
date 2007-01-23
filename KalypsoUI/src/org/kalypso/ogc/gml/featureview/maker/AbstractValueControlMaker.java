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

import org.kalypso.core.jaxb.TemplateUtilitites;
import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.AnnotationUtilities;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayout;
import org.kalypso.template.featureview.LabelType;
import org.kalypso.template.featureview.LayoutDataType;
import org.kalypso.template.featureview.LayoutType;
import org.kalypso.template.featureview.ValidatorLabelType;
import org.kalypsodeegree.model.feature.Feature;

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
  public boolean addControls( final List<JAXBElement< ? extends ControlType>> controlList, final LayoutType parentLayout, IFeatureType ft, final IPropertyType ftp, final Feature feature ) throws AbortCreationException
  {
    /* Create the 'real' control */
    final GridDataType griddata = TemplateUtilitites.OF_FEATUREVIEW.createGridDataType();
    final JAXBElement< ? extends ControlType> controlElement = createControlType( feature, ft, ftp, griddata );
    if( controlElement == null )
      return false;
    final JAXBElement<GridDataType> jaxbgriddata = TemplateUtilitites.OF_FEATUREVIEW.createGridData( griddata );
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
      final LabelType label = TemplateUtilitites.OF_FEATUREVIEW.createLabelType();
      label.setStyle( "SWT.NONE" ); //$NON-NLS-1$

      label.setText( text );
      label.setTooltip( tooltip );
      label.setVisible( true );

      final GridDataType labelGridData = TemplateUtilitites.OF_FEATUREVIEW.createGridDataType();
      labelGridData.setGrabExcessHorizontalSpace( false );
      labelGridData.setHorizontalAlignment( "GridData.BEGINNING" ); //$NON-NLS-1$
      labelGridData.setVerticalAlignment( getLabelVerticalAlignment() );
      label.setLayoutData( TemplateUtilitites.OF_FEATUREVIEW.createGridData( labelGridData ) );

      controlList.add( TemplateUtilitites.OF_FEATUREVIEW.createLabel( label ) );
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

    /* Fill the rest of the line */
    final GridLayout gridLayout = (GridLayout) parentLayout;

    /* Only fill to getNumColumns - 1, because the last column is always filled with the validator or an empty label. */
    final int numColumns = gridLayout.getNumColumns() - 1;
    for( int i = cellCount; i < numColumns; i++ )
    {
      final LabelType label = TemplateUtilitites.OF_FEATUREVIEW.createLabelType();
      label.setStyle( "SWT.NONE" ); //$NON-NLS-1$
      label.setVisible( false );

      final GridDataType labelGridData = TemplateUtilitites.OF_FEATUREVIEW.createGridDataType();
      labelGridData.setGrabExcessHorizontalSpace( false );
      labelGridData.setHorizontalAlignment( "GridData.BEGINNING" ); //$NON-NLS-1$
      label.setLayoutData( TemplateUtilitites.OF_FEATUREVIEW.createGridData( labelGridData ) );

      controlList.add( TemplateUtilitites.OF_FEATUREVIEW.createLabel( label ) );
    }

    /* If a validator is needed, it is added here. */
    if( m_addValidator )
    {
      final ValidatorLabelType validatorLabel = TemplateUtilitites.OF_FEATUREVIEW.createValidatorLabelType();
      validatorLabel.setStyle( "SWT.NONE" ); //$NON-NLS-1$

      validatorLabel.setVisible( true );

      if( property == null )
        System.out.println( "Invalid validator label for feature type property: " + ftp );
      validatorLabel.setProperty( property );

      final GridDataType labelGridData = TemplateUtilitites.OF_FEATUREVIEW.createGridDataType();
      labelGridData.setGrabExcessHorizontalSpace( false );
      labelGridData.setHorizontalAlignment( "GridData.BEGINNING" ); //$NON-NLS-1$
      validatorLabel.setLayoutData( TemplateUtilitites.OF_FEATUREVIEW.createGridData( labelGridData ) );
      controlList.add( TemplateUtilitites.OF_FEATUREVIEW.createValidatorlabel( validatorLabel ) );
    }
    else
    {
      /* Empty label. */
      final LabelType label = TemplateUtilitites.OF_FEATUREVIEW.createLabelType();
      label.setStyle( "SWT.NONE" ); //$NON-NLS-1$
      label.setVisible( false );

      final GridDataType labelGridData = TemplateUtilitites.OF_FEATUREVIEW.createGridDataType();
      labelGridData.setGrabExcessHorizontalSpace( false );
      labelGridData.setHorizontalAlignment( "GridData.BEGINNING" ); //$NON-NLS-1$
      label.setLayoutData( TemplateUtilitites.OF_FEATUREVIEW.createGridData( labelGridData ) );

      controlList.add( TemplateUtilitites.OF_FEATUREVIEW.createLabel( label ) );
    }

    return true;
  }

  protected String getLabelVerticalAlignment( )
  {
    return "GridData.CENTER"; //$NON-NLS-1$
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

  /**
   * This function creates the ControlType for a property of a feature.
   * 
   * @param feature
   *          The feature itself.
   * @param ft
   *          The feature type.
   * @param pt
   *          the property type of the property.
   * @param gridData
   *          the grid data object, which should be used.
   */
  protected abstract JAXBElement< ? extends ControlType> createControlType( Feature feature, IFeatureType ft, final IPropertyType pt, final GridDataType griddata ) throws AbortCreationException;
}
