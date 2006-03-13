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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.filterdialog.dialog.IErrorMessageReciever;
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree_impl.filterencoding.PropertyIsNullOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;

class PropertyIsNullOperationComposite extends AbstractFilterComposite
{
  private Label m_firstRowLabel;

  private Combo m_fristRowCombo;

  private PropertyIsNullOperation m_operation;

  public PropertyIsNullOperationComposite( final Composite parent, final int style, PropertyIsNullOperation operation, final IFeatureType ft, final IErrorMessageReciever errorMessageReciever )
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
    else if( m_operation == null )
      expression = new PropertyName( EMPTY_VALUE );

    // String value = null;
    // if( expression instanceof PropertyName )
    // value = ( (PropertyName)expression ).getValue();
    // if( expression instanceof Literal )
    // value = ( (Literal)expression ).getValue();

    m_firstRowLabel = new Label( this, SWT.NULL );
    m_firstRowLabel.setText( expression.getExpressionName().trim() );
    m_fristRowCombo = new Combo( this, SWT.FILL | SWT.READ_ONLY );
    for( int i = 0; i < m_ft.getProperties().length; i++ )
    {
      IPropertyType ftp = m_ft.getProperties()[i];
      m_fristRowCombo.add( ftp.getQName().getLocalPart().trim() );
    }
    m_fristRowCombo.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        // empty
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        // empty
      }
    } );
  }
}