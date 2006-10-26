/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.gui;

import java.text.ParseException;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.ResourceFileDialog;
import org.kalypso.ogc.gml.featureview.modfier.ButtonModifier;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.typehandler.ResourceFileTypeHandler;
import org.kalypso.template.featureview.Button;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Nadja Peiler
 */
public class ResourceFileGuiTypeHandler extends LabelProvider implements IGuiTypeHandler
{
  public IFeatureDialog createFeatureDialog( final Feature feature, final IPropertyType ftp )
  {
    return new ResourceFileDialog( feature, ftp );
  }

  public JAXBElement< ? extends ControlType> createFeatureviewControl( final IPropertyType property, final ObjectFactory factory )
  {
    final Button button = factory.createButton();
    final GridDataType griddata = factory.createGridDataType();
    button.setStyle( "SWT.PUSH" );
    button.setProperty( property.getQName() );

    griddata.setHorizontalAlignment( "GridData.BEGINNING" );
    button.setLayoutData( factory.createGridData( griddata ) );

    return factory.createButton( button );
  }

  public IFeatureModifier createFeatureModifier( final IPropertyType ftp, final IFeatureSelectionManager selectionManager, final IFeatureChangeListener fcl )
  {
    return new ButtonModifier( ftp, fcl );
  }

  @Override
  public String getText( Object element )
  {
    if( element == null )
      return "";

    final String href = ((IFile) element).getFullPath().toString();
    return href == null ? "" : href;
  }

  public Class getValueClass( )
  {
    return ResourceFileTypeHandler.CLASSNAME;
  }

  public QName getTypeName( )
  {
    return ResourceFileTypeHandler.QNAME;
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    return false;
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#fromText(java.lang.String)
   */
  public Object fromText( final String text ) throws ParseException
  {
    // Standard is to use the parseType method from the corresponding marhsalling type handler
    // In future, this should be directly implemented at this point 
    final IMarshallingTypeHandler marshallingHandler = MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( getTypeName() );
    return marshallingHandler.parseType( text );
  }
}