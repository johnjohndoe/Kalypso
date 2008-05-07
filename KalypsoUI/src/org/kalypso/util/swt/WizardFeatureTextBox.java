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
package org.kalypso.util.swt;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;

/**
 * simple textbox wrapper rendering a qname of a feature
 * 
 * @author kuch
 */
public class WizardFeatureTextBox
{
  private final Feature m_feature;

  private final QName m_qn;

  protected String m_text = ""; //$NON-NLS-1$

  private final Set<Runnable> m_listener = new HashSet<Runnable>();

  protected Text m_textBox;

  private Object m_property;

  public WizardFeatureTextBox( final Feature feature, final QName qn )
  {
    m_feature = feature;
    m_qn = qn;
  }

  public void addModifyListener( final Runnable runnable )
  {
    m_listener.add( runnable );
  }

  public String getLabel( )
  {
    final IAnnotation annotation = AnnotationUtilities.getAnnotation( m_feature.getFeatureType().getProperty( m_qn ) );
    return annotation.getLabel();
  }

  public void draw( final Composite parent, final FormToolkit toolkit, final GridData layout, final int style )
  {
    if( toolkit == null )
    {
      m_textBox = new Text( parent, style );
    }
    else
    {
      m_textBox = toolkit.createText( parent, "", style ); //$NON-NLS-1$
    }

    m_textBox.setLayoutData( layout );

    if( m_feature != null )
    {
      m_property = m_feature.getProperty( m_qn );
      if( m_property != null )
        if( m_property instanceof XMLGregorianCalendar )
        {
          final XMLGregorianCalendar cal = (XMLGregorianCalendar) m_property;
          final GregorianCalendar calendar = cal.toGregorianCalendar();

          final DateFormat df = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.MEDIUM );
          final String date = df.format( calendar.getTime() );

          m_textBox.setText( date );
          m_text = date;

        }
        else if( m_property instanceof List )
        {
          final List names = (List) m_property;
          m_textBox.setText( names.get( 0 ).toString() );
          m_text = names.get( 0 ).toString();

        }
        else if( m_property instanceof Double )
        {
          final Double value = (Double) m_property;

          final String text = String.format( "%.4f", value ); //$NON-NLS-1$
          m_textBox.setText( text );
          m_text = text;
        }
        else if( m_property instanceof BigDecimal )
        {
          final BigDecimal value = (BigDecimal) m_property;

          final String text = String.format( "%.4f", value ); //$NON-NLS-1$
          m_textBox.setText( text );
          m_text = text;
        }
        else
        {
          m_textBox.setText( m_property.toString() );
          m_text = m_property.toString();
        }
    }

    m_textBox.addModifyListener( new ModifyListener()
    {

      public void modifyText( final ModifyEvent e )
      {
        m_text = m_textBox.getText();
        processListener();
      }
    } );

  }

  public void draw( final Composite parent, final GridData layout, final int style )
  {
    draw( parent, null, layout, style );
  }

  public String getText( )
  {
    return m_text;
  }

  protected void processListener( )
  {
    for( final Runnable listener : m_listener )
      listener.run();
  }

  public void setText( final String text )
  {
    m_text = text;

    new UIJob( Messages.getString("org.kalypso.util.swt.WizardFeatureTextBox.4") ) //$NON-NLS-1$
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        m_textBox.setText( text );

        return Status.OK_STATUS;
      }
    }.schedule();

    processListener();
  }

  public Object getLastRenderedProperty( )
  {
    return m_property;
  }

  public void setEnabled( final boolean enabled )
  {
    m_textBox.setEnabled( enabled );

  }

}
