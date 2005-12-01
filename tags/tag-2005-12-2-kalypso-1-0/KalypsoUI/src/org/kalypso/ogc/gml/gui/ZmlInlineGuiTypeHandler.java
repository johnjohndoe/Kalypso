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

import javax.xml.bind.JAXBException;

import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.ZmlInlineFeatureDialog;
import org.kalypso.ogc.gml.featureview.modfier.ButtonModifier;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.typehandler.ZmlInlineTypeHandler;
import org.kalypso.template.featureview.ButtonType;
import org.kalypso.template.featureview.ControlType;
import org.kalypso.template.featureview.GridDataType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author kuepfer
 */
public class ZmlInlineGuiTypeHandler extends LabelProvider implements IGuiTypeHandler
{

  private final ZmlInlineTypeHandler m_typeHandler;

  public ZmlInlineGuiTypeHandler( ZmlInlineTypeHandler typeHandler )
  {
    m_typeHandler = typeHandler;
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureDialog(org.kalypsodeegree.model.feature.GMLWorkspace,
   *      org.kalypsodeegree.model.feature.Feature, org.kalypsodeegree.model.feature.FeatureTypeProperty)
   */
  public IFeatureDialog createFeatureDialog( GMLWorkspace workspace, Feature feature, FeatureTypeProperty ftp )
  {
    return new ZmlInlineFeatureDialog( feature, ftp, m_typeHandler.getAxisTypes() );
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureviewControl(java.lang.String,
   *      org.kalypso.template.featureview.ObjectFactory)
   */
  public ControlType createFeatureviewControl( String propertyName, ObjectFactory factory ) throws JAXBException
  {

    final ButtonType button = factory.createButton();
    final GridDataType griddata = factory.createGridData();
    button.setStyle( "SWT.PUSH" );
    button.setProperty( propertyName );

    griddata.setHorizontalAlignment( "GridData.BEGINNING" );
    button.setLayoutData( griddata );

    return button;
  }

  /**
   * @see org.kalypso.ogc.gml.gui.IGuiTypeHandler#createFeatureModifier(org.kalypsodeegree.model.feature.GMLWorkspace, org.kalypsodeegree.model.feature.FeatureTypeProperty, org.kalypso.ogc.gml.selection.IFeatureSelectionManager, org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public IFeatureModifier createFeatureModifier( final GMLWorkspace workspace, final FeatureTypeProperty ftp, final IFeatureSelectionManager selectionManager, final IFeatureChangeListener fcl )
  {
    return new ButtonModifier( workspace, ftp, selectionManager, fcl );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getClassName()
   */
  public String getClassName()
  {
    return m_typeHandler.getClassName();
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return m_typeHandler.getTypeName();
  }

  public String getText( Object o )
  {
    //    final String prefix = Arrays.toString( m_typeHandler.getAxisTypes(), "" ) + ": ";
    //    if( o == null )
    //      return prefix + "-";
    //    return prefix + ( (IObservation)o ).getName();
    return "<Editieren...>";
  }
}
