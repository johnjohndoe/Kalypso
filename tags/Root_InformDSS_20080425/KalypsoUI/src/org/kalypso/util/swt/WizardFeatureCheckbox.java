/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.util.swt;

import java.util.LinkedHashSet;
import java.util.Set;

import javax.xml.namespace.QName;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public class WizardFeatureCheckbox
{
  private String m_label = null;

  protected boolean m_selection;

  private final Feature m_feature;

  private final QName m_qname;

  Set<Runnable> m_listener = new LinkedHashSet<Runnable>();

  public WizardFeatureCheckbox( final Feature feature, final QName qname, final String fallbackLabel )
  {
    m_feature = feature;
    m_qname = qname;

    if( feature == null )
      m_label = fallbackLabel;
    else
    {
      final IAnnotation annotation = AnnotationUtilities.getAnnotation( feature.getFeatureType().getProperty( qname ) );
      m_label = annotation.getLabel();
    }
  }

  public void draw( final Composite body, final GridData gridData, final int layout, final boolean label )
  {
    final Button button = new Button( body, layout | SWT.CHECK );
    button.setLayoutData( gridData );
    if( label )
      button.setText( m_label );

    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_selection = button.getSelection();

        for( final Runnable r : m_listener )
        {
          r.run();
        }
      }
    } );

    if( m_feature != null )
    {
      final Boolean selection = (Boolean) m_feature.getProperty( m_qname );
      button.setSelection( selection );
      m_selection = selection;
    }

  }

  public boolean isSelected( )
  {
    return m_selection;
  }

  public void addSelectionChangeListener( final Runnable r )
  {
    m_listener.add( r );
  }

}
