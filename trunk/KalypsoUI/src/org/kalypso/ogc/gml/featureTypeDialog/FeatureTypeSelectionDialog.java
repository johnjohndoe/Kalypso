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
package org.kalypso.ogc.gml.featureTypeDialog;

import java.util.ArrayList;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Widget;
import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.AnnotationUtilities;

/**
 * This Dialog only lists FeatureTypes where the substitution group is not _Feature.
 * 
 * @author kuepfer
 */
public class FeatureTypeSelectionDialog extends Dialog
{

  private IFeatureType[] m_ft;

  private String[] m_selectedFeatureTypeName;

  private final int m_multiSelection;

  public FeatureTypeSelectionDialog( Shell parentShell, IFeatureType[] featureTypes, int multiSelection )
  {
    super( parentShell );
    m_ft = featureTypes;

    m_multiSelection = multiSelection;
  }

  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final Composite main = (Composite) super.createDialogArea( parent );
    GridLayout mainLayout = new GridLayout( 1, true );
    GridData gd1 = new GridData( GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL );
    main.setLayout( mainLayout );
    main.setLayoutData( gd1 );

    final Label lable = new Label( main, SWT.NULL );

    lable.setText( "Wählen sie den gewünschten Datentypen aus:" );
    final List list = new List( main, SWT.BORDER | m_multiSelection );
    for( int i = 0; i < m_ft.length; i++ )
    {
      final IFeatureType ft = m_ft[i];
      final IAnnotation annotation = AnnotationUtilities.getAnnotation( ft );
      list.add( annotation.getLabel() );
    }
    final GridData data = new GridData();
    data.widthHint = 250;

    list.setLayoutData( data );

    list.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( final SelectionEvent e )
      {
        handleWidgetSelected( list, e );
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        handleWidgetSelected( list, e );
      }
    } );

    return main;
  }

  protected void handleWidgetSelected( final List list, final SelectionEvent e )
  {
    final Widget w = e.widget;
    if( w == list )
    {
      m_selectedFeatureTypeName = list.getSelection();
    }
  }

  public IFeatureType[] getSelectedFeatureTypes( )
  {
    final ArrayList<IFeatureType> res = new ArrayList<IFeatureType>();
    for( final IFeatureType ft : m_ft )
    {
      final int index = ArrayUtils.indexOf( m_selectedFeatureTypeName, AnnotationUtilities.getAnnotation( ft ).getLabel() );
      if( index > -1 )
        res.add( ft );
    }
    return res.toArray( new IFeatureType[res.size()] );
  }
}
