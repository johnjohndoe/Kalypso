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

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayout;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class FeatureviewHelper implements IFeatureviewFactory
{
  public static final ObjectFactory FACTORY = new ObjectFactory();

  public static final int STANDARD_TEXT_FIELD_WIDTH_HINT = 200;

  public static final JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  private boolean m_showTables = true;

  private boolean m_shouldAddValidator = false;

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

  /**
   * @see org.kalypso.ogc.gml.featureview.maker.IFeatureviewFactory#get(org.kalypso.gmlschema.feature.IFeatureType,
   *      org.kalypsodeegree.model.feature.Feature)
   */
  public FeatureviewType get( final IFeatureType featureType, final Feature feature )
  {
    final FeatureviewType featureview = FACTORY.createFeatureviewType();
    featureview.setTypename( featureType.getQName() );
    featureview.setStyle( "SWT.NONE" );

    final GridLayout gridLayout = FACTORY.createGridLayout();
    gridLayout.setNumColumns( 4 );
    featureview.setLayout( FACTORY.createGridLayout( gridLayout ) );
    final GridDataType griddata = FACTORY.createGridDataType();
    griddata.setGrabExcessHorizontalSpace( Boolean.TRUE );
    griddata.setGrabExcessVerticalSpace( Boolean.TRUE );
    griddata.setHorizontalAlignment( "GridData.FILL" );
    griddata.setVerticalAlignment( "GridData.FILL" );
    featureview.setLayoutData( FACTORY.createGridData( griddata ) );

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
        final Object value = feature == null ? null : feature.getProperty( ftp );
        controlMaker.addControls( controlList, gridLayout, ftp, value );
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
    return new DefaultControlMakerStrategy( m_shouldAddValidator, m_showTables );
  }
}