/*--------------- Kalypso-Header ----------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 University of Technology Hamburg-Harburg (TUHH)
 Institute of River and Coastal Engineering
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
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

 Contact:

 E-Mail:
 g.belger@bjoernsen.de
 m.schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.gui;

import java.util.List;

import javax.xml.bind.JAXBException;

import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.ogc.gml.featureview.FeatureviewHelper;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.TimeserieLinkFeatureDialog;
import org.kalypso.ogc.gml.featureview.modfier.ButtonModifier;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.template.featureview.ButtonType;
import org.kalypso.template.featureview.CompositeType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.GridLayout;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.TextType;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author bce
 */
public class TimeseriesLinkGuiTypeHandler extends LabelProvider implements IGuiTypeHandler
{
  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureDialog(org.kalypsodeegree.model.feature.GMLWorkspace,
   *      org.kalypsodeegree.model.feature.Feature, org.kalypsodeegree.model.feature.FeatureTypeProperty)
   */
  public IFeatureDialog createFeatureDialog( final GMLWorkspace workspace, final Feature feature,
      final FeatureTypeProperty ftp )
  {
    return new TimeserieLinkFeatureDialog( workspace, feature, ftp );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getClassName()
   */
  public String getClassName()
  {
    return ObservationLinkHandler.CLASS_NAME;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return ObservationLinkHandler.TYPE_NAME;
  }

  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
   */
  public String getText( final Object element )
  {
    if( element == null )
      return "";

    final String href = ( (TimeseriesLink)element ).getHref();
    return href == null ? "" : href;
  }

  /**
   * @throws JAXBException
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureviewControl(java.lang.String,
   *      org.kalypso.template.featureview.ObjectFactory)
   */
  public ControlType createFeatureviewControl( final String propertyName, final ObjectFactory factory ) throws JAXBException
  {
    final CompositeType composite = factory.createComposite();

    final GridLayout layout = factory.createGridLayout();
    layout.setNumColumns( 2 );
    layout.setMakeColumnsEqualWidth( false );
    layout.setMarginWidth( 0 );
    composite.setLayout( layout );
    composite.setStyle( "SWT.NONE" );

    // Text
    final TextType text = factory.createText();
    text.setStyle( "SWT.NONE" );
    text.setProperty( propertyName );

    final GridDataType textData = factory.createGridData();
    textData.setHorizontalAlignment( "GridData.BEGINNING" );
    textData.setWidthHint( FeatureviewHelper.STANDARD_TEXT_FIELD_WIDTH_HINT );
    text.setLayoutData( textData );

    // Knopf
    final ButtonType button = factory.createButton();
    final GridDataType buttonData = factory.createGridData();
    button.setStyle( "SWT.PUSH" );
    button.setProperty( propertyName );

    buttonData.setHorizontalAlignment( "GridData.BEGINNING" );
    button.setLayoutData( buttonData );

    final List children = composite.getControl();
    children.add( text );
    children.add( button );

    return composite;
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureModifier(org.kalypsodeegree.model.feature.GMLWorkspace, org.kalypsodeegree.model.feature.FeatureTypeProperty, org.kalypso.ogc.gml.selection.IFeatureSelectionManager)
   */
  public IFeatureModifier createFeatureModifier( final GMLWorkspace workspace, final FeatureTypeProperty ftp, final IFeatureSelectionManager selectionManager )
  {
    return new ButtonModifier( workspace, ftp, selectionManager );
  }
}
