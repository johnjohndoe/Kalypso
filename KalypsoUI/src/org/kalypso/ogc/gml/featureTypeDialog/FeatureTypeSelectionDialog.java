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
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Annotation;
import org.kalypsodeegree.model.feature.FeatureType;

/**
 * 
 * This Dialog only lists FeatureTypes where the substitution group is not _Feature.
 * 
 * @author kuepfer
 */
public class FeatureTypeSelectionDialog extends Dialog
{

  private FeatureType[] m_ft;

  private String m_lang;

  private String[] m_selectedFeatureTypeName;

  private List m_list;

  public FeatureTypeSelectionDialog( Shell parentShell, FeatureType[] featureTypes )
  {
    super( parentShell );
    m_ft = featureTypes;
    m_lang = KalypsoGisPlugin.getDefault().getLang();

  }

  protected Control createDialogArea( Composite parent )
  {
    Composite main = (Composite)super.createDialogArea( parent );
    GridLayout layout = new GridLayout();
    Label lable = new Label( main, SWT.NULL );
    lable.setText( "Wählen sie die zu impotierenden Datentypen aus:" );
    m_list = new List( main, SWT.BORDER | SWT.MULTI );
    for( int i = 0; i < m_ft.length; i++ )
    {
      FeatureType ft = m_ft[i];
      if( !ft.getName().startsWith( "_" ) )
      {
        Annotation annotation = ft.getAnnotation( m_lang );
        m_list.add( annotation.getLabel() );
      }
    }
    GridData data = new GridData();
    data.widthHint = 250;
    m_list.setLayoutData( data );
    m_list.addSelectionListener( new SelectionListener()
    {

      public void widgetSelected( SelectionEvent e )
      {
        Widget w = e.widget;
        if( w == m_list )
        {
          m_selectedFeatureTypeName = m_list.getSelection();
        }

      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        widgetSelected( e );

      }
    } );

    return main;
  }

  public FeatureType[] getSelectedFeatureTypes()
  {
    ArrayList res = new ArrayList();
    for( int i = 0; i < m_ft.length; i++ )
    {
      FeatureType ft = m_ft[i];
      int index = ArrayUtils.indexOf( m_selectedFeatureTypeName, ft.getAnnotation( m_lang ).getLabel() );
      if( index > -1 )
        res.add( ft );
    }
    return (FeatureType[])res.toArray( new FeatureType[res.size()] );
  }
}
