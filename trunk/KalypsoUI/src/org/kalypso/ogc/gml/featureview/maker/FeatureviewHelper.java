/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.featureview.maker;

import java.util.List;

import javax.xml.bind.JAXBElement;

import org.kalypso.core.jaxb.TemplateUtilitites;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayout;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class FeatureviewHelper implements IFeatureviewFactory
{
  public static final int STANDARD_TEXT_FIELD_WIDTH_HINT = 200;

  private boolean m_showTables = true;

  private boolean m_shouldAddValidator = true;

  private boolean m_shouldShowButton = true;

  /** Generate new templates with or without tables. Cache is cleared. */
  public void setShowTables( final boolean showTable )
  {
    m_showTables = showTable;
  }

  public boolean isShowTables( )
  {
    return m_showTables;
  }

  public void setShouldAddValidator( boolean shouldAddValidator )
  {
    m_shouldAddValidator = shouldAddValidator;
  }

  public boolean isShouldAddValidator( )
  {
    return m_shouldAddValidator;
  }

  public void setShouldShowButton( boolean shouldShowButton )
  {
    m_shouldShowButton = shouldShowButton;
  }

  public boolean isShouldShowButton( )
  {
    return m_shouldShowButton;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.maker.IFeatureviewFactory#get(org.kalypso.gmlschema.feature.IFeatureType,
   *      org.kalypsodeegree.model.feature.Feature)
   */
  public FeatureviewType get( final IFeatureType featureType, final Feature feature )
  {
    final FeatureviewType featureview = TemplateUtilitites.OF_FEATUREVIEW.createFeatureviewType();
    featureview.setTypename( featureType.getQName() );
    featureview.setStyle( "SWT.NONE" ); //$NON-NLS-1$

    final GridLayout gridLayout = TemplateUtilitites.OF_FEATUREVIEW.createGridLayout();
    gridLayout.setNumColumns( 4 );
    featureview.setLayout( TemplateUtilitites.OF_FEATUREVIEW.createGridLayout( gridLayout ) );
    final GridDataType griddata = TemplateUtilitites.OF_FEATUREVIEW.createGridDataType();
    griddata.setGrabExcessHorizontalSpace( Boolean.TRUE );
    griddata.setGrabExcessVerticalSpace( Boolean.TRUE );
    griddata.setHorizontalAlignment( "GridData.FILL" ); //$NON-NLS-1$
    griddata.setVerticalAlignment( "GridData.FILL" ); //$NON-NLS-1$
    featureview.setLayoutData( TemplateUtilitites.OF_FEATUREVIEW.createGridData( griddata ) );

    // REMARK: it is importent that the maker is re-created each time. as the makers sometimes
    // do store state information
    final IControlMaker controlMaker = createControlMaker();
    // PARANOIA: createControlMaker may have been overwritten and so may return null.
    if( controlMaker == null )
      return featureview;

    final List<JAXBElement< ? extends ControlType>> controlList = featureview.getControl();
    for( final IPropertyType ftp : featureType.getProperties() )
    {
      try
      {
        controlMaker.addControls( controlList, gridLayout, featureType, ftp, feature );
      }
      catch( AbortCreationException e )
      {
        // just eat the exception, nothing shall be added for this property
      }
    }

    return featureview;
  }

  /**
   * Creates the control maker, ready to be overwritten by reimplementors.
   * <p>
   * The default implementation creates a {@link DefaultControlMakerStrategy}.
   */
  protected IControlMaker createControlMaker( )
  {
    return new DefaultControlMakerStrategy( m_shouldAddValidator, m_showTables, m_shouldShowButton );
  }
}