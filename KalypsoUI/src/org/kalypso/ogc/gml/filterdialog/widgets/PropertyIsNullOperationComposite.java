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
package org.kalypso.ogc.gml.filterdialog.widgets;

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.filterdialog.dialog.IErrorMessageReciever;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeContentProvider;
import org.kalypso.ogc.gml.filterdialog.model.FeatureTypeLabelProvider;
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree_impl.filterencoding.PropertyIsNullOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;

class PropertyIsNullOperationComposite extends AbstractFilterComposite
{
  private Label m_firstRowLabel;

  Combo m_fristRowCombo;

  PropertyIsNullOperation m_operation;

  private ComboViewer m_propViewer;

  public PropertyIsNullOperationComposite( final Composite parent, final int style, final PropertyIsNullOperation operation, final IFeatureType ft, final IErrorMessageReciever errorMessageReciever )
  {
    super( parent, style, errorMessageReciever, ft );
    m_operation = operation;
    setControl();
  }

  private void setControl( )
  {
    Expression expression = null;
    if( m_operation != null )
      expression = m_operation.getExpression();
    // TODO add new LiteralType's
    if( expression == null )
    {
      expression = new PropertyName( EMPTY_VALUE );
      m_operation.setExpression( expression );
    }
    // String value = null;
    // if( expression instanceof PropertyName )
    // value = ( (PropertyName)expression ).getValue();
    // if( expression instanceof Literal )
    // value = ( (Literal)expression ).getValue();

    m_firstRowLabel = new Label( this, SWT.NULL );
    m_firstRowLabel.setText( expression.getExpressionName().trim() );
    final Combo firstRowCombo = new Combo( this, SWT.NULL );
    m_propViewer = new ComboViewer( firstRowCombo );
    m_propViewer.setContentProvider( new FeatureTypeContentProvider() );
    m_propViewer.setLabelProvider( new FeatureTypeLabelProvider() );
    // m_propViewer.addFilter( new NonGeometryPropertyFilter() );
    m_propViewer.add( m_ft.getProperties() );
    m_propViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final Object firstElement = ((IStructuredSelection) event.getSelection()).getFirstElement();
        if( firstElement instanceof IValuePropertyType )
        {
          final QName item = ((IValuePropertyType) firstElement).getQName();
          m_operation.setExpression( new PropertyName( item ) );
        }

      }
    } );
    m_propViewer.setSelection( new StructuredSelection( setPropertySelection( m_operation.getExpression() ) ) );
  }
}